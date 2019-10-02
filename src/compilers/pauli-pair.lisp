;;;; pauli-pair.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; This file contains routines for the parametric compilation of two-qubit
;;;; gates determined by time-independent Hamiltonians via expression as a
;;;; PAULI-SUM-GATE.

(in-package #:cl-quil)

#+ignore
(define-compiler canonical-hamiltonian-compiler
    ((instr (_ _ q1 q0)
            :where (and (typep (gate-application-gate instr) 'pauli-sum-gate)
                        (loop :for term :in (pauli-sum-gate-terms (gate-application-gate instr))
                              :always (or (string= "XX" (pauli-term-pauli-word term))
                                          (string= "YY" (pauli-term-pauli-word term))
                                          (string= "ZZ" (pauli-term-pauli-word term)))))))
  "Parametric compiler to CZs for time-independent canonical Hamiltonians."
  
    )

(define-compiler pauli-pair-compiler
    ((instr (_ (time) q1 q0)                 ; name params q1 q0
            :where (and (not (typep time 'number))
                        (typep (gate-application-gate instr) 'pauli-sum-gate))))
  "Rewrites a parametric 2Q gate application described by a time-independent Hamiltonian into canonical form."
  (let ((gate (gate-application-gate instr)))
    ;; XXX: check that the hamiltonian H(t) is time-independent (i.e., time-linear)
    ;; instantiate the hamiltonian H0 = H(1) at a particular time
    (let* ((H0 (gate-matrix gate 1.0d0)))
      ;; diagonalize it: H0 = U D U*
      (multiple-value-bind (d u) (magicl:eig H0)
        (format t "~&diagonal phases: ~a~%" (mapcar #'phase d))
        (let* (;; infer a presentation of D as a hamiltonian: D = EXPI(a ZI + b IZ + c ZZ)
               (kernel (print (m* (magicl:inv (make-row-major-matrix 3 3 (list -1  1 -1
                                                                               -1 -1  1 
                                                                                1  1  1)))
                                  (make-row-major-matrix 3 1 (mapcar #'phase (rest d))))))
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
          (inst "CONJ-LEFT"    left-matrix q1 q0))))))
