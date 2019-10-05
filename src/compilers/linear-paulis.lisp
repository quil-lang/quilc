;;;; linear-paulis.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; This file contains routines for the parametric compilation of gates defined
;;;; by time-independent Hamiltonians via expression as a
;;;; PAULI-SUM-GATE.

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

;; WARNING: i don't know where this method has been considered before, and i
;;          myself haven't worked out a proof that it's correct. caveat compiler
(define-compiler parametric-diagonal-compiler
    ((instr _
            :where (and (typep (gate-application-gate instr) 'pauli-sum-gate)
                        (every (lambda (term) (every (lambda (letter) (or (eql letter #\Z) (eql letter #\I)))
                                                     (pauli-term-pauli-word term)))
                               (pauli-sum-gate-terms (gate-application-gate instr))))))
  "Decomposes a diagonal Pauli gate by a single step."
  (declare (optimize (debug 3) (speed 0)))
  (with-slots (arguments parameters terms arity dimension) (gate-application-gate instr)
    (let ((nonlocal-terms nil))
      ;; first, deal with the words with a zero Zs / one Z: they're local gates
      (dolist (term terms)
        (multiple-value-bind (Z-count Z-position)
            (loop :for letter :across (pauli-term-pauli-word term)
                  :for j :from 0
                  :with pos := 0
                  :with count := 0
                  :when (eql #\Z letter)
                    :do (setf count (1+ count)
                              pos j)
                  :finally (return (values count pos)))
          (case Z-count
            (0
             nil)
            (1
             (inst "RZ"
                   (list (param-* 2.0d0
                                  (tree-substitute (pauli-term-prefactor term)
                                                   (mapcar (lambda (ep ap)
                                                             (typecase ap
                                                               (delayed-expression
                                                                (cons ep (delayed-expression-expression ap)))
                                                               (otherwise
                                                                (cons ep ap))))
                                                           parameters (application-parameters instr)))))
                   (nth (position (nth Z-position (pauli-term-arguments term))
                                  arguments :test #'equalp)
                        (application-arguments instr))))
            (otherwise
             (push term nonlocal-terms)))))
      (let ((votes (make-array (length arguments) :initial-element 0))
            vote)
        ;; we can break nonlocal terms into two collections: those with Zs in
        ;; some spot and those without Zs in that spot. the result will be to
        ;; conjugate the first collection by CNOT, which flips those Zs to Is.
        ;; since we can only emit local gates, almost all such Zs will have to
        ;; be eliminated, and so we'll want to pick the position so that this
        ;; group is as large as possible.
        (dolist (term nonlocal-terms)
          (loop :for letter :across (pauli-term-pauli-word term)
                :for argument :in (pauli-term-arguments term)
                :when (eql #\Z letter)
                  :do (incf (aref votes (position argument arguments :test #'equalp)))))
        (loop :with pos := 0
              :with current-max := (aref votes 0)
              :for j :from 0
              :for item :across votes
              :when (< current-max item)
                :do (setf current-max item
                          pos j)
              :finally (setf vote current-max))
        ;; now we re-sort the nonlocal terms into two buckets: those with a Z in
        ;; the voted-upon location, and those without
        (let ((Is nil) (Zs nil))
          (dolist (term nonlocal-terms)
            (let ((Z-pos (loop :for letter :across (pauli-term-pauli-word term)
                               :for arg :in (pauli-term-arguments term)
                               :for j :from 0
                               :when (and (eql #\Z letter)
                                          (eql vote (position arg arguments :test #'equalp)))
                                 :do (return j))))
              (cond
                (Z-pos
                 (push (list term Z-pos) Zs))
                (t
                 (push term Is)))))
          ;; emit the Is
          (unless (endp Is)
            (let ((I-gate (make-instance 'pauli-sum-gate
                                         :arguments arguments
                                         :parameters parameters
                                         :arity arity
                                         :dimension dimension
                                         :name "Is"
                                         :terms Is)))
              (inst* I-gate
                     (application-parameters instr)
                     (application-arguments instr))))
          ;; emit the Zs
          (unless (endp Zs)
            (let* ((localized-terms
                     (loop :for (term Z-pos) :in Zs
                           :collect (make-pauli-term
                                     :prefactor (pauli-term-prefactor term)
                                     :arguments (pauli-term-arguments term)
                                     :pauli-word (coerce (loop :for letter :across (pauli-term-pauli-word term)
                                                               :for j :from 0
                                                               :if (eql j Z-pos)
                                                                 :collect #\I
                                                               :else
                                                                 :collect letter)
                                                         'string))))
                   (Z-gate (make-instance 'pauli-sum-gate
                                          :arguments arguments
                                          :parameters parameters
                                          :arity arity
                                          :dimension dimension
                                          :terms localized-terms
                                          :name "Zs-GATE"))
                   (control-qubit (nth vote (application-arguments instr)))
                   ;; TODO: there's room here to make an intelligent choice of
                   ;; target qubit. they all act the same, so it'd be best to pick
                   ;; that one that's topologically nearest the control.
                   (target-qubit (if (zerop vote)
                                     (second (application-arguments instr))
                                     (first (application-arguments instr)))))
              (inst "CNOT" () control-qubit target-qubit)
              (inst* Z-gate
                     (application-parameters instr)
                     (application-arguments instr))
              (inst "CNOT" () control-qubit target-qubit))))))))


;; TODO: also write an orthogonal gate compiler somewhere? approx.lisp will take
;;       care of it in the 2Q case, at least.

(define-compiler parametric-pauli-compiler
    ((instr _ :where (and (typep (gate-application-gate instr) 'pauli-sum-gate)
                          (= 1 (length (application-parameters instr)))
                          (not (typep (first (application-parameters instr)) 'constant)))))
  "Decomposes a gate described by the exponential of a time-independent Hamiltonian into static orthogonal and parametric diagonal components."
  (let ((gate (gate-application-gate instr)))
    (with-slots (arguments parameters terms dimension) gate
      ;; XXX: check that every component in the gate has coefficient of the form
      ;;      c*t for c a constant and t the application-parameter.
      
      ;; instantiate the Hamiltonian
      (let ((H (magicl:make-zero-matrix dimension dimension)))
        (dolist (term terms)
          (setf H (m+ H (pauli-term->matrix term arguments (list 1d0) parameters))))
        ;; orthogonally diagonalize it: H = O D O^T
        (multiple-value-bind (diagonal O) (magicl:eig H)
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
                  diagonal-gate (make-instance 'pauli-sum-gate
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
