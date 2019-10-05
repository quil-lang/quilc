;;;; pauli-pair.lisp
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
    ((assoc big-tree substitution-table)
     (cdr (assoc big-tree substitution-table)))
    (t
     big-tree)))

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
                                                   (mapcar #'cons parameters (application-parameters instr)))))
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
            (print (pauli-sum-gate-terms Z-gate))
            (inst "CNOT" () control-qubit target-qubit)
            (inst* Z-gate
                   (application-parameters instr)
                   (application-arguments instr))
            (inst "CNOT" () control-qubit target-qubit)))))))

;; i think that a generic diagonal gate
;;     DIAG(alpha0, ..., alpha(2^n-1)) qn ... q0
;; can be written as
;;     FORKED ... FORKED RZ(a0, ..., a(2^(n-1)-1)) qn ... q0
;;     FORKED ... FORKED RZ(b0, ..., b(2^(n-2)-1)) qn ... q1
;;     ...
;;     FORKED RZ(y0, y1) qn q(n-1)
;;     RZ(z0) qn.
;;
;; the original gate has 2^n - 1 free parameters, neglecting global phase.
;; this sum has 2^(n-1) + 2^(n-2) + ... + 2 + 1 free parameters, which agrees, so that's good.
;;
;; i think i can give a state-prep-style calculation of the roman parameters from the greek parameters.
;; a better question is: is there a decomposition where the Pauli parameters directly show up?
;;
;; let's try a couple of smaller examples.
;; n = 0:  a I + b Z = (a + b, a - b)
;; in this case, b turns into the 'difference' angle RZ(z0), and a is a global shift.
;; n = 1:  a ZI + b IZ + c ZZ = (a + b + c, -a + b - c, a - b - c, -a - b + c)
;;         the goal is to make this look like (e, e, -e, -e).
;;         take e = b; then (a + c, -a - c, a - c, -a + c) + (b, b, -b, -b)
;;                     then (a + c, -a - c, a - c, -a + c) = (a, -a, a, -a) + (c, -c, -c, c)
;;                     then (c, -c, -c, c) = CNOT 1 0 ZI(c) CNOT 1 0  (i.e., the target matches the Z)
;;                          (a, -a, a, -a) = ZI(a), and (b, b, -b, -b) = IZ(b).
;; n = 2: a ZII + b IZI + c IIZ + d ZZI + e ZIZ + f IZZ + g ZZZ =
;;        ZII(a) + IZI(b) + IIZ(c) + (d ZZI + e ZIZ + f IZZ + g ZZZ)
;;        then (d ZZI + e ZIZ + f IZZ + g ZZZ) =
;;             (d + e + f + g, -d - e + f - g, -d + e - f - g, d - e - f + g,
;;              d - e - f - g, -d + e - f + g, -d - e + f + g, d + e + f - g)
;;        ZZI = (1, -1, -1,  1,  1, -1, -1,  1)
;;        ZIZ = (1, -1,  1, -1, -1,  1, -1,  1)
;;        IZZ = (1,  1, -1, -1, -1, -1,  1,  1)
;;        ZZZ = (1, -1, -1,  1, -1,  1,  1, -1) <-- CNOT 2 0 ; CNOT 1 0 ; RZ(g) 0 ; CNOT 1 0 ; CNOT 2 0



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
          (setf m (m+ m (pauli-term->matrix term arguments (list 1d0) parameters))))
        ;; orthogonally diagonalize it: H = O D O^T
        (multiple-value-bind (diagonal O) (magicl:eig H)
          ;; TODO: build a diagonal Pauli sum
          (let ((diagonal-gate (make-instance 'pauli-sum-gate
                                              :arguments arguments
                                              :parameters parameters
                                              :dimension size
                                              :terms ...
                                              :arity 1
                                              :name (string (gensym "DIAG-PAULI-")))))
            (inst* "RIGHT-O-T" (magicl:conjugate-transpose O) (application-arguments instr))
            (inst (make-instance 'gate-application
                                 :gate diagonal-gate
                                 :arguments (application-arguments instr)
                                 :parameters (application-parameters instr)
                                 :operator (named-operator (string (gensym "DIAG-INSTR-")))))
            (inst* "LEFT-O" O (application-arguments instr))))))))



;; TODO: consider whether you should be extracting the Hamiltonian from sampling
;;       the unitary family or if you should be applying EIG to the pauli sum directly.
#+ignore
(define-compiler pauli-pair-compiler
    ((instr (_ (time) q1 q0)            ; name params q1 q0
            :where (and (not (typep time 'number))
                        (typep (gate-application-gate instr) 'pauli-sum-gate))))
  "Rewrites a parametric 2Q gate application described by a time-independent Hamiltonian into canonical form."
  (let ((gate (gate-application-gate instr)))
    ;; XXX: check that the hamiltonian H(t) is time-independent (i.e., time-linear)
    ;; instantiate the hamiltonian H0 = H(1) at a particular time
    (let* ((H0 (gate-matrix gate 1.0d0)))
      ;; diagonalize it: H0 = U D U*
      (multiple-value-bind (dd u) (magicl:eig H0)
        ;; canonicalize d and u so that the phases of d are sorted ascending.
        ;; XXX: also deal with equivalence in d up to multiplication by i
        (let* ((pairs (loop :for d :in dd
                            :for j :below 4
                            :for row := (loop :for i :below 4 :collect (magicl:ref u j i))
                            :collect (list d row)))
               (sorted-pairs (sort pairs #'< :key (a:compose #'phase #'car)))
               (dd (mapcar #'car sorted-pairs))
               (u (make-row-major-matrix 4 4 (loop :for (phase row) :in sorted-pairs
                                                   :nconc row))))
          ;; XXX: if U escaped SU(4), add a phase shift to put it back in.
          (format t "~&diagonal phases: ~a~%" (mapcar #'phase dd))
          (let* (;; infer a presentation of D as a hamiltonian: D = EXPI(a ZI + b IZ + c ZZ)
                 (kernel (print (m* (magicl:inv (make-row-major-matrix 3 3 (list -1  1 -1
                                                                                 -1 -1  1 
                                                                                 1  1  1)))
                                    (make-row-major-matrix 3 1 (mapcar #'phase (rest dd))))))
                 (ZI (magicl:ref kernel 0 0))
                 (IZ (magicl:ref kernel 1 0))
                 (ZZ (magicl:ref kernel 2 0))
                 ;; use +e-basis+ to turn the middle into a canonical gate:
                 ;;     H0 = UDU* = UE* EDE* EU* and EDE* = EXPI(a XX + b YY + c ZZ)
                 (formal-qubit-args (list (formal "q1") (formal "q0")))
                 (formal-parameter-name (make-symbol "time"))
                 (canonical-hamiltonian
                   (make-instance 'pauli-sum-gate
                                  :arguments formal-qubit-args
                                  :parameters (list formal-parameter-name)
                                  :terms (list (make-pauli-term :pauli-word "XX"
                                                                :prefactor `(* ,ZI ,formal-parameter-name)
                                                                :arguments formal-qubit-args)
                                               (make-pauli-term :pauli-word "YY"
                                                                :prefactor `(* ,IZ ,formal-parameter-name)
                                                                :arguments formal-qubit-args)
                                               (make-pauli-term :pauli-word "ZZ"
                                                                :prefactor `(* ,ZZ ,formal-parameter-name)
                                                                :arguments formal-qubit-args))
                                  :dimension 4
                                  :arity 1
                                  :name "CANONICALIZED-HAM"))
                 ;; return: anonymous gate (UE*) canonical hamiltonian (EDE*) anonymous gate (EU*)
                 (left-matrix  (m* u +edag-basis+))
                 (right-matrix (m* +e-basis+ (magicl:conjugate-transpose u))))
            (inst "CONJ-RIGHT"   right-matrix q1 q0)
            (inst (make-instance 'gate-application
                                 :operator (named-operator "CANONICAL-AS-PAULI")
                                 :arguments (list (qubit q1) (qubit q0))
                                 :parameters (list time)
                                 :gate canonical-hamiltonian))
            (inst "CONJ-LEFT"    left-matrix q1 q0)))))))
