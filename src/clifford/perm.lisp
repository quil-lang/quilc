;;;; perm.lisp
;;;;
;;;; Author: Robert Smith (in collab with Charles Hadfield)
;;;;

(in-package #:cl-quil/clifford)

;;; This files deals with the permutation group representation of the
;;; Clifford group.
;;;
;;; The permutation group representation is a direct consequence of
;;; the definition of the Clifford group. Namely, the Clifford group
;;; is the set of unitaries which stabilize the Pauli group under
;;; conjugation. As such, we can just look at how a Clifford group
;;; element permutes the Pauli group, and construct an isomorphism to
;;; a subgroup of S_n, where n ~ number of Paulis.

(defun clifford-perm-degree (n)
  "What is the size of a permutation representing an element of C_n?"
  ;; Half the size of the Pauli group minus 2. We don't need no +i*P,
  ;; -i*P, +I, -I!
  (- (/ (expt 4 (1+ n)) 2) 2))

;;; Here, "details" refers to the set of Paulis
;;;
;;;     {I, -I} U { p : p has phase +i or -i }.
;;;
;;; As indicated in CLIFFORD-PERM-DEGREE, we don't need to represent
;;; these in the integer representation of a Pauli.
(declaim (inline %strip-details %adjoin-details))
(defun %strip-details  (x) (- (/ x 2) 2))
(defun %adjoin-details (x) (* 2 (+ 2 x)))

(defun clifford-to-perm (c)
  "Convert a Clifford group element C into a permutation representation."
  (let* ((num-qubits (num-qubits c))
         (num-points (clifford-perm-degree num-qubits))
         (rep (perm::iota-vector (1+ num-points))))
    (dotimes (i num-points (perm::%make-perm :rep rep))
      ;; The 2*(2 + i) and x/2 - 2 are to convert in/out from integers
      ;; which keep track of +-i/I and which don't.
      (let* ((p (integer-pauli (%adjoin-details i) num-qubits))
             (cp (apply-clifford c p)))
        ;; +1 because CL-PERMUTATION expresses permutations on the set
        ;; of positive integers.
        (setf (aref rep (1+ i)) (1+ (%strip-details (pauli-integer cp))))))))

(defun %nice-cycles (perm)              ; Useful for testing.
  (reverse (mapcar (lambda (cyc)
                     (coerce (perm::cycle-rep cyc) 'list))
                   (remove-if (lambda (cyc)
                                (>= 1 (perm:cycle-length cyc)))
                              (to-cycles perm :canonicalizep t)))))

(defun clifford-group-generators (n)
  "Produce a list of Cliffords that are sufficient to generate C_n."
  (check-type n (integer 1))
  `(,(phase-gate n 0)
    ,@(loop :for i :below n :collect (hadamard n i))
    ,@(loop :for i :below (1- n) :collect (cnot n i (1+ i)))))

(defun clifford-group-as-perm-group (n)
  "Generate the Clifford group C_n as a permutation group."
  (generate-perm-group
   (mapcar #'clifford-to-perm
           (clifford-group-generators n))))
