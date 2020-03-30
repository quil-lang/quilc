;;;; linear-paulis.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; This file contains routines for the parametric compilation of gates defined
;;;; by time-independent Hamiltonians via expression as a
;;;; EXP-PAULI-SUM-GATE.

(in-package #:cl-quil)

;; WARNING: this consumes stack space like (* tree-depth fan-out)
(defun tree-substitute (big-tree substitution-table)
  (cond
    ((listp big-tree)
     (mapcar (a:rcurry #'tree-substitute substitution-table) big-tree))
    ((delayed-expression-p big-tree)
     (make-delayed-expression
      (delayed-expression-params big-tree)
      (delayed-expression-lambda-params big-tree)
      (tree-substitute (delayed-expression-expression big-tree) substitution-table)))
    ((assoc big-tree substitution-table)
     (cdr (assoc big-tree substitution-table)))
    (t
     big-tree)))

(defun term->count-and-last-Z-position (term)
  "For TERM a PAULI-TERM, counts the number of Zs and returns the position of the last occuring Z."
  (values (count #\Z (pauli-term-pauli-word term))
          (position #\Z (pauli-term-pauli-word term))))

(defun clone-exp-pauli-sum-gate (gate &key new-name new-terms)
  (with-slots (arguments parameters arity dimension name terms) gate
    (make-instance 'exp-pauli-sum-gate
                   :arguments arguments
                   :parameters parameters
                   :arity arity
                   :dimension dimension
                   :name (or new-name name)
                   :terms (or new-terms terms))))

(defun find-most-frequent-Z (terms arguments &key except)
  (let ((votes (make-array (length arguments) :initial-element 0)))
    (dolist (term terms)
      (loop :for letter :across (pauli-term-pauli-word term)
            :for argument :in (pauli-term-arguments term)
            :when (eql #\Z letter)
              :do (incf (aref votes (position argument arguments :test #'equalp)))))
    (when except
      (setf (aref votes except) 0))
    (vector-argmax votes)))

(defun pauli-instr-of-all-Zs-or-Is-p (instr)
  "Predicate: INSTR is a GATE-APPLICATION defined by a diagonal EXP-PAULI-SUM-GATE."
  (and (typep (gate-application-gate instr) 'exp-pauli-sum-gate)
       (every (lambda (term) (every (lambda (letter) (or (eql letter #\Z) (eql letter #\I)))
                                    (pauli-term-pauli-word term)))
              (exp-pauli-sum-gate-terms (gate-application-gate instr)))))

(define-compiler parametric-diagonal-compiler
    ((instr _ :where (pauli-instr-of-all-Zs-or-Is-p instr)))
  "Decomposes a diagonal Pauli gate by a single step."
  (with-slots (arguments parameters terms arity dimension) (gate-application-gate instr)
    (let ((nonlocal-terms nil))
      ;; first, deal with the words with zero Zs / one Z: they're local gates.
      ;; we'll want to evaluate the gate definition at whatever the gate application says.
      (let ((substitution-table
              (mapcar (lambda (ep ap)
                        (typecase ap
                          (delayed-expression
                           (cons ep (delayed-expression-expression ap)))
                          (otherwise
                           (cons ep ap))))
                      parameters (application-parameters instr))))
        (dolist (term terms)
          (multiple-value-bind (Z-count Z-position)
              (term->count-and-last-Z-position term)
            (case Z-count
              (0
               nil)
              (1
               (inst "RZ"
                     (list (param-* 2.0d0
                                    (tree-substitute (pauli-term-prefactor term)
                                                     substitution-table)))
                     (nth (position (nth Z-position (pauli-term-arguments term))
                                    arguments :test #'equalp)
                          (application-arguments instr))))
              (otherwise
               (push term nonlocal-terms))))))
      ;; we can break nonlocal terms into two collections: those with Zs in
      ;; some spot and those without Zs in that spot. the result will be to
      ;; conjugate the first collection by CNOT, which flips those Zs to Is.
      ;; since we can only emit local gates, almost all such Zs will have to
      ;; be eliminated, and so we'll want to pick the position so that this
      ;; group is as large as possible.
      (let* ((vote (find-most-frequent-Z nonlocal-terms arguments))
             (control-qubit (nth vote (application-arguments instr))))
        ;; now we re-sort the nonlocal terms into two buckets: those with a Z in
        ;; the voted-upon location, and those without
        (let (Is Zs)
          (dolist (term nonlocal-terms)
            (let ((Z-pos (loop :for letter :across (pauli-term-pauli-word term)
                               :for arg :in (pauli-term-arguments term)
                               :for j :from 0
                               :when (and (eql #\Z letter)
                                          (eql vote (position arg arguments :test #'equalp)))
                                 :do (return j))))
              (if Z-pos
                  (push (list term Z-pos) Zs)
                  (push term Is))))
          ;; emit the Is as-is
          (when Is
            (let ((I-gate (clone-exp-pauli-sum-gate (gate-application-gate instr)
                                                    :new-name "Is" :new-terms Is)))
              (inst* I-gate
                     (application-parameters instr)
                     (application-arguments instr))))
          (unless Zs
            (finish-compiler))
          ;; emit the Zs by reducing their Z-counts (for some of them).
          ;; we reduce the Z-count by using the identity CNOT ZI CNOT = ZZ, so we seek
          ;; a second qubit index with a lot of Zs to conjugate away all at once.
          (let* ((subvote-position (find-most-frequent-Z (mapcar #'car Zs) arguments
                                                          :except vote))
                 (subvote-formal (nth subvote-position arguments))
                 (subvote-literal (nth subvote-position (application-arguments instr)))
                 ZZs ZIs)
            ;; cleave the Zs into ZZs and ZIs.
            (dolist (term-pair Zs)
              (let ((term (car term-pair)))
                (if (position subvote-formal (pauli-term-arguments term) :test #'equalp)
                    (push term-pair ZZs)
                    (push term ZIs))))
            ;; for the ZIs: emit them as-is.
            (let ((ZI-gate (clone-exp-pauli-sum-gate (gate-application-gate instr)
                                                     :new-name "ZI-GATE" :new-terms ZIs)))
              (when ZIs
                (inst* ZI-gate
                       (application-parameters instr)
                       (application-arguments instr))))
            ;; for the ZZs: rewrite them as if they would be ZIs ...
            (let* ((ZZs->ZIs
                     (mapcar (lambda (pair)
                               (destructuring-bind (term Z-pos) pair
                                 (let ((new-term (copy-pauli-term term)))
                                   (setf (pauli-term-pauli-word new-term)
                                         (coerce (loop :for letter :across (pauli-term-pauli-word term)
                                                       :for j :from 0
                                                       :if (eql j Z-pos)
                                                         :collect #\I
                                                       :else
                                                         :collect letter)
                                                 'string))
                                   new-term)))
                             ZZs))
                   (ZZ-gate (clone-exp-pauli-sum-gate (gate-application-gate instr)
                                                      :new-name "ZZ-GATE" :new-terms ZZs->ZIs)))
              ;; ... then emit them in a CNOT sandwich.
              (when ZZs
                (inst "CNOT" () control-qubit subvote-literal)
                (inst* ZZ-gate
                       (application-parameters instr)
                       (application-arguments instr))
                (inst "CNOT" () control-qubit subvote-literal)))))))))

;;; Helper functions for special case Paulis.

(defun inst-aXX+bZZ (a b p q)
  "Inst a program for aXX + bZZ on qubits p and q. This is

DEFGATE UXZ(%a, %b) p q AS PAULI-SUM:
    XX(%a) p q
    ZZ(%b) p q
"
  (inst "CNOT" ()                   p q)
  (inst "RX"   (list (param-* 2 a)) p)
  (inst "RZ"   (list (param-* 2 b)) q)
  (inst "CNOT" ()                   p q))

(defun inst-aXX+bYY (a b p q)
  "Inst a program for aXX + bYY on qubits p and q. This is

DEFGATE UXY(%a, %b) p q AS PAULI-SUM:
    XX(%a) p q
    YY(%b) p q
"
  (inst "RZ" `(,-pi/2) p)
  (inst "RZ" `(,-pi/2) q)
  (inst "H"   ()       p)
  (inst "H"   ()       q)
  (inst-aXX+bZZ (param-* -1 a) (param-* -1 b) p q)
  (inst "H"   ()       p)
  (inst "H"   ()       q)
  (inst "RZ" `(,pi/2)  p)
  (inst "RZ" `(,pi/2)  q))

(defun inst-aYY+bZZ (a b p q)
  "Inst a program for aYY + bZZ on qubits p and q. This is

DEFGATE UXY(%a, %b) p q AS PAULI-SUM:
    YY(%a) p q
    ZZ(%b) p q
"
  (declare (ignore a b p q))
  (warn "Unknown identity for YY+ZZ")
  (give-up-compilation))

(defun inst-aMM+bNN (a b p q mm nn)
  (check-type mm (member :XX :YY :ZZ))
  (check-type nn (member :XX :YY :ZZ))
  (assert (not (eq mm nn)))
  (optima:match (list mm nn)
    ((list ':XX ':YY)
     (inst-aXX+bYY a p q p))
    ((list ':XX ':ZZ)
     (inst-aXX+bZZ a b p q))
    ((list ':YY ':ZZ)
     (inst-aYY+bZZ a b p q))
    (otherwise
     (inst-aMM+bNN b a q p nn mm))))

(defun pauli-instr-of-type-aMM+bNN (instr)
  "Test whether INSTR is an EXP-PAULI-SUM-GATE with terms aMM + bNN
where M /= N and M,N in {X, Y, Z}."
  ;; XXX There's gotta be a better way to write this.
  (flet ((PP? (word)
           (and (= 2 (length word))
                (char= (elt word 0) (elt word 1)))))
    (and (typep (gate-application-gate instr) 'exp-pauli-sum-gate)
         (let ((words (map 'list #'pauli-term-pauli-word
                           (exp-pauli-sum-gate-terms (gate-application-gate instr)))))
           (and (= 2 (length words))
                (every #'PP? words)
                ;; Note that these interns are not
                ;; free-for-alls. They'll only be II, XX, YY, or ZZ.
                (let ((type0 (intern (first words) ':keyword))
                      (type1 (intern (second words) ':keyword)))
                  (and (not (eq type0 type1))
                       (not (eq ':II type0))
                       (not (eq ':II type1))
                       (list type0 type1))))))))

;; TODO: also write an orthogonal gate compiler somewhere? approx.lisp will take
;;       care of it in the 2Q case, at least.

(define-compiler parametric-pauli-compiler
    ((instr _ :where (and (typep (gate-application-gate instr) 'exp-pauli-sum-gate)
                          (= 1 (length (application-parameters instr)))
                          (not (typep (first (application-parameters instr)) 'constant)))))
  "Decomposes a gate described by the exponential of a time-independent Hamiltonian into static orthogonal and parametric diagonal components."
  (let ((gate (gate-application-gate instr)))
    (with-slots (arguments parameters terms dimension) gate
      
      ;; make sure that all the pauli terms are all scalar multiples of the unknown parameter.
      ;; we track this by making sure that the unknown parameter appears only once and that
      ;; the surrounding expression takes a particularly nice form.
      (labels ((crawl-parameter (p)
                 (typecase p
                   (list
                    (case (first p)
                      (-
                       (unless (= 2 (length p))
                         (give-up-compilation))
                       (crawl-parameter (second p)))
                      (*
                       (unless (= 3 (length p))
                         (give-up-compilation))
                       (let ((total (+ (crawl-parameter (second p))
                                       (crawl-parameter (third p)))))
                         (unless (<= total 1)
                           (give-up-compilation))
                         total))
                      (otherwise
                       (give-up-compilation))))
                   (number
                    0)
                   (symbol
                    1)
                   (otherwise
                    (give-up-compilation)))))
        (dolist (term terms)
          (unless (= 1 (crawl-parameter (pauli-term-prefactor term)))
            (give-up-compilation))))
      
      ;; test if it's one of the easy cases first.
      (alexandria:when-let ((type (pauli-instr-of-type-aMM+bNN instr)))
        (let ((args (map 'list #'qubit-index (application-arguments instr)))
              (prefactors (map 'list #'pauli-term-prefactor terms)))
          (destructuring-bind (typep typeq) type
            (inst-aMM+bNN
             ;; TODO XXX FIXME: How do I properly use the Pauli prefactors?
             (first (application-parameters instr)) (first (application-parameters instr))
             (nth 0 args)        (nth 1 args)
             typep typeq)
            (finish-compiler))))
      
      ;; Otherwise proceed to the highlighted rout^W^Wgeneral case.
      
      ;; instantiate the Hamiltonian
      (let ((H (zeros (list dimension dimension))))
        (dolist (term terms)
          (setf H (magicl:.+ H (pauli-term->matrix term arguments (list 1d0) parameters))))
        ;; orthogonally diagonalize it: H = O D O^T
        (multiple-value-bind (diagonal O) (magicl:hermitian-eig H)
          ;; convert diagonal into a sum of Z paulis
          (let ((pauli-prefactors (make-array dimension :initial-element 0d0))
                terms diagonal-gate)
            (loop :for d :in diagonal
                  :for i :from 0
                  :do (dotimes (j dimension)
                        (incf (aref pauli-prefactors j)
                              (if (evenp (logcount (logand i j)))
                                  (/    d  dimension)
                                  (/ (- d) dimension)))))
            (setf terms (loop :for prefactor :across pauli-prefactors
                              :for j :from 0
                              :unless (double= 0d0 prefactor)
                                :collect (let ((term-arguments
                                                 (loop :for i :below (length arguments)
                                                       :for arg :in arguments
                                                       :when (logbitp (- (length arguments) i 1) j)
                                                         :collect arg)))
                                           (make-pauli-term
                                            :prefactor (param-* (realpart prefactor)
                                                                (make-delayed-expression
                                                                 nil nil (first parameters)))
                                            :arguments term-arguments
                                            :pauli-word (coerce (make-array (length term-arguments)
                                                                            :initial-element #\Z)
                                                                'string))))
                  diagonal-gate (make-instance 'exp-pauli-sum-gate
                                               :arguments arguments
                                               :parameters parameters
                                               :terms terms
                                               :arity 1
                                               :dimension dimension
                                               :name (string (gensym "DIAG-PAULI-"))))
            ;; emit the instructions
            (inst* "RIGHT-O-T"   (magicl:conjugate-transpose O) (application-arguments instr))
            (inst* diagonal-gate (application-parameters instr) (application-arguments instr))
            (inst* "LEFT-O"      O                              (application-arguments instr))))))))
