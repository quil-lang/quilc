;;;; permutation.lisp
;;;;
;;;; Author: Charles Zhang

(in-package #:cl-quil)

;;;; This file implements routines to decompose and synthesize
;;;; permutation matrices representing classical reversible circuits.


;;;; young group decomposition based algorithm for permutation gates

;;; This algorithm follows the description given in "Young Subgroups
;;; for reversible computers", by De Vos and Van Rentergem
;;; (https://www.aimsciences.org/article/exportPdf?id=76a62a03-1559-4506-8945-7b987a5489f4).
(defun decompose! (perm index)
  "This subroutine breaks up the permutation into left and right pieces f = g1 o f' o g2 where g1 and g2 can be realized with a single target gate on the wire W labelled by INDEX and f' is a reversible function which does not change on wire W."
  (let* ((size (length perm))
         (left (make-array size :initial-element 0
                                :element-type '(unsigned-byte 32)))
         (right (make-array size :initial-element 0
                                 :element-type '(unsigned-byte 32)))
         (visited (make-array size :initial-element 0
                                   :element-type 'bit))
         (row 0)
         (1<<index (ash 1 index)))
    (block out
      (loop
        ;; assign 0 to index on the left side
        (setf (aref left row) (logandc2 row 1<<index))
        (setf (aref visited row) 1)
        ;; assign 1 to index on the left side
        (setf (aref left (logxor row 1<<index))
              (logxor (aref left row) 1<<index))
        (setf row (logxor row 1<<index))
        (setf (aref visited row) 1)
        ;; assign 1 to index on the right side
        (setf (aref right
                    (logior (aref perm row)
                            1<<index))
              (aref perm row))
        ;; assign 0 to index on right side
        (setf (aref right
                    (logandc2 (aref perm row)
                              1<<index))
              (logxor (aref perm row) 1<<index))
        (let ((next
                (dotimes (i size size)
                  (when (= (aref perm i)
                           (logxor (aref perm row)
                                   1<<index))
                    (return i)))))
          ;; if the row was already visited, find a new one.
          (when (= (aref visited next) 1)
            (dotimes (i size
                        ;; we're done if everything is visited.
                        (return-from out))
              (when (= (aref visited i) 0)
                (setf next i)
                (return))))
          (setf row next))))
    (let ((old-perm (make-array size :initial-contents perm)))
      (dotimes (i size)
        (setf (aref perm (aref left i))
              (aref right (aref old-perm i))))
      ;; check that the composition is correct.
      (dotimes (i size)
        (assert (= (aref right (aref perm (aref left i)))
                   (aref old-perm i))))
      ;; Return g1 and g2 in "mathematical" or "composition" order.
      (values left right))))

(defstruct single-target-gate
  "A reversible single target gate whose action on the target bit is specified by FUNCTION in the following manner: Invert the target bit when (and only when) FUNCTION on the control lines is 1."
  (function (missing-arg) :type truth-table :read-only t)
  ;; sorted list of qubits
  (control-lines (missing-arg) :type vector :read-only t)
  (target (missing-arg) :type fixnum :read-only t))

(defun single-target-gate-decomposition! (perm)
  "PERM permutes computational basis to computational basis. This algorithm produces at most 2n-1 single target gates (acting on qubits numbered 0 to n-1) where n is the number of bits, which is nearly optimal."
  (assert (power-of-two-p (length perm)))
  (let ((n-qubits (ilog2 (length perm)))
        (left-gates '())
        (right-gates '()))
    (dotimes (index n-qubits)
      (multiple-value-bind (left right)
          (decompose! perm index)
        (flet ((single-target-gate-from-permutation (permutation)
                 (multiple-value-bind (truth-table vars)
                     (truth-table-minimize-base!
                      (make-truth-table n-qubits
                                        :initial-contents
                                        (loop :for value :across permutation
                                              :for row :from 0
                                              :collect (if (= value row) 0 1))))
                   (unless (truth-table-zero-p truth-table)
                     (assert (not (member index vars)))
                     (make-single-target-gate
                      :function truth-table
                      :control-lines (coerce (sort vars #'<) 'vector)
                      :target index)))))
          (let ((left-gate (single-target-gate-from-permutation left))
                (right-gate (single-target-gate-from-permutation right)))
            (when left-gate (push left-gate left-gates))
            (when right-gate (push right-gate right-gates))))))
    ;; Combine the left and right gates in "Quil" order (composition
    ;; applies left-to-right), hence the unintuitive "right" listed
    ;; before "left".
    ;;
    ;; It's possible as an additional optimization to merge the
    ;; right-most left gate and the left-most right gate (assuming
    ;; neither is the identity) because they act on the same
    ;; target. In fact, probably any consecutive gates acting on the
    ;; same target can be merged iteratively.
    (nreconc right-gates left-gates)))

;;; Synthesize a single target gate by using PPRM (positive polarity
;;; Reed-Mueller) form.
(defun simple-single-target-gate-synthesize (gate &optional (index->qubit #'identity))
  "Synthesize a single-target gate GATE, loosely representing the Quil instruction:

    GATE k_0 k_1 ... k_(n-1)

where

    k_i := INDEX->QUBIT(i)

By default, INDEX->QUBIT is the identity function, leading to a reverse convention of the standard Quil order of arguments."
  (let ((control-lines (map 'vector index->qubit (single-target-gate-control-lines gate)))
        (target        (funcall index->qubit (single-target-gate-target gate)))
        (circuit       '()))
    (dolist (cube (truth-table-esop-from-pprm (single-target-gate-function gate)))
      (let ((translated-controls '()))
        (dotimes (index (length cube))
          (let ((trit (aref cube index)))
            (assert (/= trit -1)) ; *PP*RM
            (unless (= trit 0)
              (push (aref control-lines index) translated-controls))))
        (case (length translated-controls)
          (0 (push (build-gate "X" () target) circuit))
          (1 (push (build-gate "CNOT" () (first translated-controls) target) circuit))
          (2 (push (build-gate "CCNOT" ()
                               (first translated-controls)
                               (second translated-controls)
                               target)
                   circuit))
          (t
           (push (apply #'build-multiple-controlled-gate
                        "X" () (append translated-controls (list target)))
                 circuit)))))
    ;; Return the circuit in "Quil" order (left-to-right).
    circuit))

(defun synthesize-permutation (permutation qubits)
  "Synthesize the operator represented by the permutation PERMUTATION acting on the vector of qubits QUBITS.

If permutation represents a matrix mapping the i'th amplitude to the PERMUTATION[i]'th amplitude of a quantum state of qubit indexes {0, 1, ..., n-1}, then QUBITS must be the vector

    #(n-1 n-2 ... 2 1 0)

in accordance with how Quil interprets a gate application."
  (let ((n (length qubits)))
    (flet ((index->qubit (i) (aref qubits (- n i 1))))
      (declare (dynamic-extent #'index->qubit))
      (loop :with permutation := (make-array (length permutation) :initial-contents permutation)
            :for target-gate :in (single-target-gate-decomposition! permutation)
            :nconcing (simple-single-target-gate-synthesize target-gate #'index->qubit)))))

(defun permutation-gate-to-mcx (instr &key context)
  "Compile instructions representing permutation gates to n-qubit Toffoli gates."
  (declare (ignore context))
  (unless (slot-boundp instr 'cl-quil/frontend::name-resolution)
    (give-up-compilation))
  (let ((res (gate-application-resolution instr)))
    (when (typep res 'gate-definition)
      (setf res (gate-definition-to-gate res)))
    (let* ((perm-gate (funcall
                       (operator-description-gate-lifter
                        (application-operator instr))
                       res)))
      (cond
        ((and (typep perm-gate 'permutation-gate)
              (> (gate-dimension perm-gate) 4)) ; N.B. Dimension, not arity!
         (let* ((code (synthesize-permutation
                       (permutation-gate-permutation perm-gate)
                       (map 'vector #'qubit-index (application-arguments instr)))))
           ;; If synthesis produces a 1 instruction sequence, that
           ;; means that the original instruction represents a n-qubit
           ;; controlled Toffoli gate, so we didn't do anything and
           ;; should give up.
           (when (and code (null (rest code)))
             (give-up-compilation :because :acts-trivially))
           code))
        (t
         (give-up-compilation :because :invalid-domain))))))
