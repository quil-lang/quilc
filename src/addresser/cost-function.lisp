;;;; cost-function.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; This file provides the function cost-function that is used to compare SWAP
;;;; choices by do-greedy-temporal-addressing. the current scheme is to weight a
;;;; given logical-to-physical addressing configuration by how far some set of
;;;; instructions (typically: the next ones available for scheduling) are from
;;;; being physically instantiable, where for each such instruction we count the
;;;; time it will take to SWAP them into place along the shortest path.
;;;;
;;;; there are some subtleties to this: given a pair of instructions like
;;;; CZ 0 3
;;;; CZ 3 7
;;;; on a chip with a linear circuit topology, and (for whatever reason) assuming
;;;; qubits 0 and 7 to be fixed, a naive sum of the SWAP distances will result
;;;; in a standstill. the 0-3 distance is 2 SWAPs and the 3-7 distance is 3 SWAPs,
;;;; and trading 3 for 2 or 3 for 4 preserves the total number of 1+4 = 3+2 = 5
;;;; SWAPs in all. to break this kind of deadlock, we make very distant gates
;;;; suffer a decay penalty, so that gates that are already nearer are preferred
;;;; and executed first.

(in-package #:cl-quil)

(defparameter *cost-fn-tier-decay* 0.5
  "Describes the rate of decay of instruction importance vs. number of intervening instructions before this one is dealt with.")
(defparameter *cost-fn-dist-decay* 0.5
  "Describes the rate of decay of instruction importance vs. number of SWAPs required before this instruction is actionable.")

(defun swap-fidelity (chip-spec link-index)
  "Computes the fidelity of a SWAP operation on a given CHIP-SPECIFICATION and LINK-INDEX."
  (let* ((hardware-object (chip-spec-nth-link chip-spec link-index))
         (permutation-record (vnth 0 (hardware-object-permutation-gates
                                      hardware-object)))
         (swap (apply #'build-gate
                      (permutation-record-operator permutation-record)
                      '()
                      (coerce (chip-spec-qubits-on-link chip-spec link-index) 'list))))
    (calculate-instructions-fidelity (expand-to-native-instructions (list swap) chip-spec) chip-spec)))

(defparameter *cost-fn-weight-style* ':duration)

;; nearly ripped straight out of the Wikipedia article for Floyd-Warshall
(defun precompute-qubit-qubit-distances (chip-spec)
  "Implements Floyd-Warshall to compute the minimum weighted distance between any pair of qubits on a CHIP-SPECification, weighted by swap fidelity."
  (let* ((vertex-count (length (vnth 0 (chip-specification-objects chip-spec))))
         (dist (make-array (list vertex-count vertex-count)
                           :initial-element most-positive-fixnum)))
    ;; all vertices are distance 0 from themselves.
    (dotimes (j vertex-count)
      (setf (aref dist j j) 0))
    ;; write the direct node-to-node weights into the table
    (dotimes (link-index (length (vnth 1 (chip-specification-objects chip-spec))))
      (let ( ; eventually this should look up "SWAP" in this list, but for now
             ; this is guaranteed to be the only 2Q permutation anyway.
            (weight (ecase *cost-fn-weight-style*
                      (:duration (permutation-record-duration (vnth 0 (hardware-object-permutation-gates (chip-spec-nth-link chip-spec link-index)))))
                      (:fidelity (- (log (swap-fidelity chip-spec link-index))))))
            (left-vertex (vnth 0 (chip-spec-qubits-on-link chip-spec link-index)))
            (right-vertex (vnth 1 (chip-spec-qubits-on-link chip-spec link-index))))
        (setf (aref dist right-vertex left-vertex) weight)
        (setf (aref dist left-vertex right-vertex) weight)))
    ;; for each intermediate vertex...
    (dotimes (k vertex-count)
      ;; for each left vertex...
      (dotimes (i vertex-count)
        ;; for each right vertex...
        (dotimes (j vertex-count)
          (let ((ij (aref dist i j))
                (ik (aref dist i k))
                (kj (aref dist k j)))
            ;; is our current best path i->j worse than the concatenation of the
            ;; current best paths i->k and k->j?
            (when (> ij (+ ik kj))
              ;; then replace the old best path with these new ones.
              (setf (aref dist i j) (+ ik kj))
              (setf (aref dist j i) (+ ik kj)))))))
    dist))

;; the basic components of this function are reasonable, but they are weighted
;; by voodoo.
;;
;; the contributing components to badness are...  + effective distance between
;;     qubits which want to be acted on by gates in waiting (+ down-weighted
;;     effective distance between qubits which will be acted on gates in 2, 3,
;;     ...? steps from now?)  (+ more to come?)
;;
;; NOTE: THIS FUNCTION IS WILDLY INAPPROPRIATE (AND SO IS AT LEAST THE WEIGHTING
;; SCHEME IN THE FLOYD-WARSHALL MATRIX) WHEN WORKING WITH NATIVE n-Q GATES, n >
;; 2.
(defun cost-function (qq-distances rewiring instruction-tiers)
  "Computes a badness value associated to a given REWIRING according to a list of logically addressed GATES-IN-WAITING."
  (loop
    :with assigned-qubits := nil
    :with sum := 0d0
    :with gate-count := 0
    :for tier :in instruction-tiers
    :for tier-index :below 3
    :do (dolist (gate tier)
          (when (and (typep gate 'application)
                     (= 2 (length (application-arguments gate))))
            (destructuring-bind (q0 q1) (mapcar #'qubit-index (application-arguments gate))
              (let* ((p0 (apply-rewiring-l2p rewiring q0))
                     (p1 (apply-rewiring-l2p rewiring q1)))
                (unless p0 (rotatef p0 p1) (rotatef q0 q1))
                ;; if both are unassigned, then we gain nothing by changing the
                ;; rewiring, so ignore this gate

                (when p0
                  ;; otherwise at least one is assigned

                  (unless p1
                    ;; find a position for the other qubit
                    (setf p1
                          (loop :with min-p    := nil
                                :with min-dist := double-float-positive-infinity
                                :for p :below (rewiring-length rewiring)
                                :unless (apply-rewiring-p2l rewiring p)
                                  :do (let ((new-dist (aref qq-distances p0 p)))
                                        (when (< new-dist min-dist)
                                          (setf min-p    p
                                                min-dist new-dist)))
                                :finally (return min-p)))
                    (push q1 assigned-qubits)
                    (rewiring-assign rewiring q1 p1))

                  (let ((qq-distance (aref qq-distances p0 p1)))
                    ;; we're using 2^(-depth) * (1 + 2^(1-dist)) so that distant
                    ;; qubits exert weaker forces than nearby ones, encouraging
                    ;; us to execute more quickly accomplishable gates sooner.
                    ;; it's totally possible that dist alone is a good cost fn
                    ;; on its own, and we should experiment with this.
                    (assert (not (= qq-distance most-positive-fixnum)) ()
                            "Multiqubit instruction requested between ~
                                   disconnected components of the QPU graph: ~
                                   ~A ."
                            (print-instruction gate nil))
                    (incf gate-count)
                    (incf sum
                          (* (expt *cost-fn-tier-decay* tier-index) qq-distance))))))))
    :finally (dolist (qubit assigned-qubits) (rewiring-unassign rewiring qubit))
             (return (if (zerop gate-count) 0d0 (/ sum gate-count)))))
