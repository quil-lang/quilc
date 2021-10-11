;;;; linear-reversible-circuits.lisp
;;;; This file implements a routine to synthesize linear reversible
;;;; circuits represented as matrices into a sequence of CNOT gates.

(in-package #:cl-quil)

;;; Arithmetic in F_2.
(defmacro incf-mod-2 (place &optional (delta 1) &environment environment)
  (multiple-value-bind (vars vals store-vals writer-form reader-form)
      (get-setf-expansion place environment)
    `(let* (,@(mapcar #'list vars vals)
            (,(first store-vals) (mod (+ ,reader-form ,delta) 2)))
       ,writer-form)))

;;; This is an asymptotically optimal algorithm for decomposing A into
;;; elementary row operations over F_2 (i.e. synthesizing the matrix
;;; into an equivalent CNOT circuit). However, it relies on an
;;; arbitrarily chosen parameter SECTIONS which is chosen to be
;;; log_2(n)/2 by Patel et al. They note this parameter could be
;;; optimized but no optimal solution for it is known yet. The matrix
;;; A represents a linear reversible classical circuit's turth table
;;; on its standard basis inputs. In particular it is a matrix of size
;;; number of bits rather than the much larger permutation matrix used
;;; to represent general quantum gates.
(defun cnot-synth! (a)
  (let ((shape (magicl:shape a)))
    (assert (and (= (length shape) 2)
                 (= (first shape) (second shape))))
    (let* ((n (first shape))
           (sections (max 1 (if (<= 4 n 7)
                                2
                                (round (log n 2) 2))))
           (l-circuit (cnot-synth-lower! a sections))
           (u-circuit (cnot-synth-lower! (magicl:transpose! a) sections)))
      (prog1
          ;; switch the control and target of the gates in the
          ;; u-circuit and append u-circuit onto l-circuit.
          (let ((circuit l-circuit))
            (dolist (gate u-circuit)
              (setf (application-arguments gate)
                    (nreverse (application-arguments gate)))
              (push gate circuit))
            circuit)
        ;; We should have reduced the matrix A to the identity by now.
        (assert (magicl:= (magicl:eye shape :type '(signed-byte 32)) a))))))

;;; Clear out the lower triangular portion of the matrix A in F_2 and
;;; record the row operations as C-NOT gates.
(defun cnot-synth-lower! (a sections)
  (labels ((make-pattern (row start)
             ;; create a fixnum from the sub-row elements
             (let ((number 0))
               (loop :for i :from start :below (+ start sections) :do
                 (setf number (+ (ash number 1)
                                 (magicl:tref a row i))))
               number)))
    (let ((circuit '())
          (n (first (magicl:shape a))))
      (dotimes (sec (ceiling n sections)) ; iterate over column sections
        ;; remove duplicate sub-rows in section sec
        ;; this hash table takes "pattern" bit strings as fixnums and outputs the index of the row first seen with that sub pattern
        (let ((patterns (make-hash-table)))
          (loop for row from (* sec sections) below n do
            ;; This logic would all be much faster if we worked directly on slices instead.
            (let* ((pattern (make-pattern row (* sec sections)))
                   ;; if first copy of pattern save otherwise remove
                   (pattern-row (gethash pattern patterns)))
              (cond (pattern-row
                     (dotimes (i n)
                       (incf-mod-2 (magicl:tref a row i)
                                   (magicl:tref a pattern-row i)))
                     (push (build-gate "CNOT" () pattern-row row) circuit))
                    (t
                     (setf (gethash pattern patterns)
                           row))))))
        ;; use Gaussian elimination for remaining entries in column
        ;; section
        (loop for col from (* sec sections) below (* (1+ sec) sections) do
          (let ((diag (magicl:tref a col col)))
            ;; remove ones in rows below column col
            (loop for row from (1+ col) below n do
              (when (= (magicl:tref a row col) 1)
                ;; force a 1 in the diagonal
                (when (zerop diag)
                  (dotimes (i n)
                    (incf-mod-2 (magicl:tref a col i)
                                (magicl:tref a row i)))
                  (push (build-gate "CNOT" () row col)
                        circuit)
                  (setq diag 1))
                (dotimes (i n)
                  (incf-mod-2 (magicl:tref a row i)
                              (magicl:tref a col i)))
                (push (build-gate "CNOT" () col row) circuit))))))
      circuit)))

;; given a list of CNOT gates and the total number of bits, produce
;; the matrix encoding its truth table.
(defun cnot-circuit-matrix (gates bits)
  (let ((state (magicl:eye (list bits bits) :type 'bit)))
    (dotimes (bit bits)
      (dolist (gate gates)
        (assert (equalp (application-operator gate) (named-operator "CNOT")))
        (destructuring-bind (control target)
            (application-arguments gate)
          (let ((control-index (qubit-index control))
                (target-index (qubit-index target)))
            (when (= (magicl:tref state control-index bit) 1)
              (setf (magicl:tref state target-index bit)
                    (- 1 (magicl:tref state target-index bit))))))))
    state))
