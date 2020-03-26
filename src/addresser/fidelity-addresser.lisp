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

;;; utilitito

(defun swap-fidelity (chip-spec hardware-object)
  "Computes the fidelity of a SWAP operation on a given CHIP-SPECIFICATION and LINK-INDEX."
  (let* ((permutation-record (vnth 0 (hardware-object-permutation-gates
                                      hardware-object)))
         (swap (apply #'build-gate
                      (permutation-record-operator permutation-record)
                      '()
                      (coerce (vnth 0 (hardware-object-cxns hardware-object)) 'list))))
    (calculate-instructions-fidelity (expand-to-native-instructions (list swap) chip-spec) chip-spec)))

;;; ADDRESSER-STATE subclass

(defclass fidelity-addresser-state (addresser-state)
  ((cost-bounds :initform (make-hash-table :test #'eql)
                :accessor fidelity-addresser-state-cost-bounds
                :documentation "Table of maximum costs for a program exercising this hardware object.")))

(defstruct (fidelity-cost (:constructor fidelity-cost (value)))
  (value nil :type real :read-only t))

(defmethod cost-flatten ((cost fidelity-cost))
  (fidelity-cost-value cost))

(defun application-fidelity-cost (state instr)
  "Compute the fidelity cost of INSTR, withr respect to the provided addresser state."
  ;; calculate log-infidelity coming from INSTR, using recombination:
  ;;   + calculate the cut point
  ;;   + calculate the infidelity of instructions since the cut point
  ;;   + use only the additional infidelity penalty
  (let* ((chip-spec (addresser-state-chip-specification state))
         (l2p (addresser-state-working-l2p state))
         (instr-copy (copy-instance instr))
         (instruction-expansion
           (let ((*compress-carefully* nil))
             (when (rewiring-assigned-for-instruction-qubits-p l2p instr)
               (rewire-l2p-instruction l2p instr-copy))
             (expand-to-native-instructions (list instr-copy) chip-spec))))

    ;; compute the naive cost
    (let ((instr-cost (calculate-instructions-log-fidelity instruction-expansion
                                                     chip-spec)))
      ;; then, see if there's a non-naive cost available
      (a:when-let*
          ((hardware-object (and (rewiring-assigned-for-instruction-qubits-p l2p instr)
                                 (lookup-hardware-object chip-spec instr-copy)))
           (cost-bound (gethash hardware-object (fidelity-addresser-state-cost-bounds state)))
           (subschedule (chip-contiguous-subschedule-from-last-instructions
                         (addresser-state-chip-schedule state)
                         (apply #'make-qubit-resource
                                (coerce (vnth 0 (hardware-object-cxns hardware-object)) 'list))))
           (preceding-fidelity
            (calculate-instructions-log-fidelity subschedule
                                                 (addresser-state-chip-specification state))))
        (when (<= cost-bound (+ preceding-fidelity instr-cost))
          (setf instr-cost (- cost-bound preceding-fidelity))))
      instr-cost)))

(defun gate-weights-fidelity-cost (state gate-weights)
  "Compute the total cost of gates in GATE-WEIGHTS, with the cost of individual gates discounted by their 'tier'."
  ;; GATE-WEIGHTS is a hash table mapping gates to their 'tier', which is an
  ;; indication of how far in the future they will need to be addressed. The
  ;; implementation here is similar to that of GATE-WEIGHTS-TEMPORAL-COST,
  ;; except that here gate fidelity information is used.
  (let ((qq-distances (addresser-state-qq-distances state)) ; populated with log-infidelities
        (rewiring (addresser-state-working-l2p state))
        (assigned-qubits nil)
        (gate-count 0)
        (gate-weights-cost 0d0))
    (flet ((1q-cost (gate physical-qubit)
             (a:when-let* ((chip-spec (addresser-state-chip-specification state))
                           (hardware-object (lookup-hardware-object chip-spec gate))
                           (instrs (expand-to-native-instructions (list gate) chip-spec))
                           (rewired-instrs
                            (loop :for instr :in instrs
                                  :for rewired-instr := (copy-instance instr)
                                  :do (setf (application-arguments rewired-instr)
                                            (mapcar (constantly (qubit physical-qubit))
                                                    (application-arguments rewired-instr)))
                                  :collect rewired-instr))
                           (fidelity (calculate-instructions-fidelity rewired-instrs chip-spec)))
               (- (log fidelity))))
           (2q-cost  (gate logical-qubits physical-qubits tier-index)
             (destructuring-bind ((q0 q1) (p0 p1)) (list logical-qubits physical-qubits)
               ;; ensure p0 is present
               (unless p0 (rotatef p0 p1) (rotatef q0 q1))
               ;; if p1 is missing, find a position for it
               (unless p1
                 (setf p1 (nearest-unassigned-qubit p0 rewiring qq-distances))
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
                          ~/cl-quil::instruction-fmt/."
                         gate)
                 (* (expt *cost-fn-tier-decay* tier-index) qq-distance)))))

      (dohash ((gate tier-index) gate-weights)
        (when (and (<= tier-index 3)
                   (typep gate 'application))
          (let* ((logical-qubits (application-qubit-indices gate))
                 (physical-qubits (mapcar (a:curry #'apply-rewiring-l2p rewiring) logical-qubits))
                 (any-assigned? (some #'identity physical-qubits))
                 (all-assigned? (every #'identity physical-qubits)))
            (case (length physical-qubits)
              (1
               (when all-assigned?
                 (a:when-let ((cost (1q-cost gate (first physical-qubits))))
                   (incf gate-count)
                   (incf gate-weights-cost cost))))
              (2
               ;; if both are unassigned, then we gain nothing by changing the
               ;; rewiring, so ignore this gate
               (when any-assigned?
                 (a:when-let ((cost (2q-cost gate logical-qubits physical-qubits tier-index)))
                   (incf gate-count)
                   (incf gate-weights-cost cost)))))))))
    ;; clean up the rewiring
    (dolist (qubit assigned-qubits)
      (rewiring-unassign rewiring qubit))
    ;; normalize actual-cost
    (if (zerop gate-count)
        0d0
        (/ gate-weights-cost gate-count))))

(defmethod cost-function ((state fidelity-addresser-state) &key gate-weights instr)
  (let ((instr-cost 0d0)
        (gate-weights-cost 0d0))

    (when (and instr (typep instr 'gate-application))
      (setf instr-cost (application-fidelity-cost state instr)))

    (when gate-weights
      (setf gate-weights-cost (gate-weights-temporal-cost state gate-weights)))

    (fidelity-cost (+ instr-cost gate-weights-cost))))

(defmethod cost-< ((val1 fidelity-cost) (val2 fidelity-cost))
  (< (- (fidelity-cost-value val1) +double-comparison-threshold-strict+)
     (fidelity-cost-value val2)))

(defmethod cost-= ((val1 fidelity-cost) (val2 fidelity-cost))
  (double= (fidelity-cost-value val1) (fidelity-cost-value val2)))

(defmethod build-worst-cost ((state fidelity-addresser-state))
  (fidelity-cost most-positive-fixnum))

(defparameter *fidelity-1q-descaling* 1/10)

(defmethod unscheduled-gate-weights ((state fidelity-addresser-state))
  (flet ((weight-bumper (instr value)
           (cond
             ((typep instr 'gate-application)
              (case (length (application-arguments instr))
                (1
                 (+ *fidelity-1q-descaling* value))
                (2
                 (+ 1 value))
                (otherwise
                 value)))
             (t
              value))))
    (multiple-value-bind (max-value value-hash)
        (lscheduler-walk-graph (addresser-state-logical-schedule state)
                               :base-value 0
                               :bump-value #'weight-bumper
                               :combine-values #'max)
      (declare (ignore max-value))
      value-hash)))

(defmethod select-and-embed-a-permutation ((state temporal-addresser-state) rewirings-tried)
  ;; randomize cost function weights
  ;; not sure exactly why -- possibly to break symmetry when
  ;; swap selection fails and we rerun?
  (let ((*cost-fn-tier-decay* (+ 0.25d0 (random 0.5d0)))
        (*cost-fn-dist-decay* (+ 0.25d0 (random 0.5d0))))
    (call-next-method)))

(defmethod initialize-instance :after ((instance fidelity-addresser-state)
                                       &rest initargs
                                       &key
                                         chip-spec
                                       &allow-other-keys)
  (declare (ignore initargs))
  ;; set up the qq-distances slot to use log-infidelity as the basic unit
  (setf (addresser-state-qq-distances instance)
        (compute-qubit-qubit-distances chip-spec
                                       (lambda (object)
                                         (if (hardware-object-dead-p object)
                                             most-positive-fixnum
                                             (- (log (swap-fidelity chip-spec object)))))))
  ;; fill the cost-bounds slot
  (flet ((fill-cost-bound (hw)
           (setf (gethash hw (fidelity-addresser-state-cost-bounds instance))
                 (fidelity-cost-value
                  (cost-function instance
                                 :gate-weights (unscheduled-gate-weights instance))))))
    (warm-up-addresser-state instance #'fill-cost-bound)))

