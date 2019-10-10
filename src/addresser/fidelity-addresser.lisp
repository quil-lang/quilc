;;;; fidelity-addresser.lisp
;;;;
;;;; Authors: Eric Peterson, Peter Karalekas

(in-package #:cl-quil)

;;; This file contains a fidelity-based addresser, which solves the problem of
;;; mapping a sequence of Quil instructions acting on logical qubits to a
;;; scheduled sequence of Quil instructions acting on physical qubits.
;;;
;;; It is a close cousin to temporal-addresser.lisp, which uses the schedule
;;; itself as a heuristic for producing "efficient" programs, where "efficient"
;;; is primarily taken to mean "short". In this routine, we make the necessary
;;; modifications to take "efficient" to mean "high-fidelity".
;;;
;;; See DO-FIDELITY-ADDRESSING below for the main entry point.

(defclass fidelity-addresser-state (addresser-state)
  ())

(defstruct fidelity-cost
  value)

(defmethod cost-function ((state fidelity-addresser-state) &key gate-weights instr)
  ;; conglomerate the infidelities in GATE-WEIGHTS, decayed by some decay factor
  ;; add in the infidelity coming from INSTR, using recombination:
  ;;   + calculate the cut point
  ;;   + calculate the infidelity of instructions since the cut point
  ;;   + use only the additional infidelity penalty
  )

(defmethod cost-< ((val1 fidelity-cost) (val2 fidelity-cost))
  (< (- (fidelity-cost-value val1) +double-comparison-threshold-strict+)
     (fidelity-cost-value val2)))

(defmethod cost-= ((val1 fidelity-cost) (val2 fidelity-cost))
  (double= (fidelity-cost-value val1) (fidelity-cost-value val2)))

(defmethod build-worst-cost ((state fidelity-addresser-state))
  (make-fidelity-cost :value most-positive-fixnum))

(defmethod assign-weights-to-gates ((state fidelity-addresser-state))
  (nth-value 1 (addresser-state-logical-schedule state)))

(defun initial-fidelity-addresser-working-state (chip-spec initial-rewiring)
  (let (;; bind the fidelity version of compute-qq-distances or whatever here.
        (state (initial-addresser-working-state chip-spec initial-rewiring)))
    (change-class state 'fidelity-addresser-state)
    state))

(defun do-fidelity-addressing (instrs
                               chip-spec
                               &key
                                 (initial-rewiring nil)
                                 (use-free-swaps nil))
  (let ((state (initial-fidelity-addresser-working-state chip-spec initial-rewiring)))
    (do-greedy-addressing state instrs
      :initial-rewiring initial-rewiring
      :use-free-swaps use-free-swaps)))
