;;;; temporal-addresser.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

;;; This file contains the subroutines specific to the operation of the version
;;; of the addresser that uses overall program runtime to optimize output.

(defclass temporal-addresser-state (addresser-state)
  ((cost-bounds :accessor temporal-addresser-state-cost-bounds
                :initform (make-hash-table :test #'eql)
                :documentation "Mapping from HARDWARE-OBJECTs to the maximal cost of a program exercising that object.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cost function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct temporal-cost
  "Value returned by the duration-based COST-FUNCTION, which is computed from an individual instruction as well as the states of those instructions already scheduled and those instructions yet to be scheduled. Consists of a pair of values: the soonest start time of the proposed instruction, as well as a heuristic evaluation of the opportunity for scheduling future instructions after this one passes."
  (start-time      0d0 :type real :read-only t)
  (heuristic-value 0d0 :type real :read-only t))

(defmethod cost-flatten ((cost temporal-cost))
  (+ (temporal-cost-start-time cost)
     (temporal-cost-heuristic-value cost)))

;;;; The scheme used by the duration-based scheduler is to weight a given
;;;; logical-to-physical addressing configuration by how far some set of
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

(defparameter *cost-fn-tier-decay* 0.5
  "Describes the rate of decay of instruction importance vs. number of intervening instructions before this one is dealt with.")
(defparameter *cost-fn-dist-decay* 0.5
  "Describes the rate of decay of instruction importance vs. number of SWAPs required before this instruction is actionable.")

(defun nearest-unassigned-qubit (p0 rewiring qq-distances)
  "Get the unassigned (relative to REWIRING) qubit with minimum distance to physical qubit P0."
  (loop :with min-p    := nil
        :with min-dist := double-float-positive-infinity
        :for p :below (rewiring-length rewiring)
        :unless (apply-rewiring-p2l rewiring p)
          :do (let ((new-dist (aref qq-distances p0 p)))
                (when (< new-dist min-dist)
                  (setf min-p    p
                        min-dist new-dist)))
        :finally (return min-p)))

(defun application-temporal-cost (state instr)
  "Compute the temporal cost of INSTR, with respect to the provided addresser state."

  ;; The cost is generally computed in two ways: on the one hand, we can naively
  ;; translate INSTR to native gates and then count the total duration, and on
  ;; the other hand, we can try to use the existing chip schedule with
  ;; precomputed cost bounds to get an estimate.
  (let ((chip-spec (addresser-state-chip-specification state))
        (naive-start-time (chip-schedule-resource-end-time
                           (addresser-state-chip-schedule state)
                           (apply #'make-qubit-resource
                                  (mapcar #'qubit-index (application-arguments instr))))))

    ;; early SWAPs are free
    (when (and *addresser-use-free-swaps*
               (swap-application-p instr)
               (double= 0d0 naive-start-time))
      (return-from application-temporal-cost 0))

    (let* ((l2p (addresser-state-working-l2p state))
           (instr-physical (copy-instance instr))
           (lschedule (make-lscheduler))
           (expanded-instructions
             (let ((*compress-carefully* nil))
               (when (rewiring-assigned-for-instruction-qubits-p l2p instr)
                 (rewire-l2p-instruction l2p instr-physical))
               (expand-to-native-instructions (list instr-physical) chip-spec))))

      ;; fill the lschedule with native ops
      (append-instructions-to-lschedule lschedule expanded-instructions)

      ;; compute time from native ops
      (let ((time (+ naive-start-time
                     (lscheduler-calculate-duration lschedule chip-spec))))
        ;; if we have more info, use it
        (a:when-let* ((hardware-object (and (rewiring-assigned-for-instruction-qubits-p l2p instr)
                                            (lookup-hardware-object chip-spec instr-physical)))
                      (cost-bound (gethash hardware-object (temporal-addresser-state-cost-bounds state)))
                      (intelligent-bound
                       (+ cost-bound
                          (chip-schedule-resource-carving-point
                           (addresser-state-chip-schedule state)
                           (apply #'make-qubit-resource
                                  (coerce (vnth 0 (hardware-object-cxns hardware-object)) 'list))))))
          (setf time (min time intelligent-bound)))
        time))))

(defun gate-weights-temporal-cost (state gate-weights)
  "Compute the total cost of gates in GATE-WEIGHTS, with the cost of individual gates discounted by their 'tier'."
  ;; In other words: GATE-WEIGHTS is a hash table mapping gates to their numeric
  ;; 'tiers'. We iterate through them, greedily assigning physical qubits when
  ;; need be, and for each such assignment we tally an "exponentially
  ;; discounted" swap cost. This exponential weighting preferences the
  ;; near-future over the far-future.
  (let ((qq-distances (addresser-state-qq-distances state))
        (rewiring (addresser-state-working-l2p state))
        (assigned-qubits nil)
        (gate-count 0)
        (actual-cost 0))
    (dohash ((gate tier-index) gate-weights)
        (when (and (< tier-index 3)
                   (typep gate 'application)
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
                         ~A ."
                          (print-instruction gate nil))
                  (incf gate-count)
                  (incf actual-cost (* (expt *cost-fn-tier-decay* tier-index) qq-distance))))))))
    ;; clean up the rewiring
    (dolist (qubit assigned-qubits)
      (rewiring-unassign rewiring qubit))
    ;; normalize actual-cost
    (if (zerop gate-count)
        0d0
        (/ actual-cost gate-count))))

;; the basic components of this function are reasonable, but they are weighted
;; by voodoo.
;;
;; the contributing components to badness are...  + effective distance between
;;     qubits which want to be acted on by gates in waiting (+ down-weighted
;;     effective distance between qubits which will be acted on gates in 2, 3,
;;     ...? steps from now?)  (+ more to come?)
;;
;; NOTE: THIS FUNCTION IS WILDLY INAPPROPRIATE (AND SO IS AT LEAST THE WEIGHTING
;; SCHEME IN THE FLOYD-WARSHALL MATRIX) WHEN WORKING WITH NATIVE n-Q GATES, n > 2.
(defmethod cost-function ((state temporal-addresser-state) &key gate-weights instr)
  (let ((time 0) (actual-cost 0))
    (when (and instr (typep instr 'application))
      (setf time (application-temporal-cost state instr)))
    (when gate-weights
      (setf actual-cost (gate-weights-temporal-cost state gate-weights)))
    (make-temporal-cost :start-time time
                        :heuristic-value actual-cost)))

(defmethod cost-< ((val1 temporal-cost) (val2 temporal-cost))
  (or (< (temporal-cost-start-time val1) (temporal-cost-start-time val2))
      (and (double= (temporal-cost-start-time val1) (temporal-cost-start-time val2))
           (< (temporal-cost-heuristic-value val1) (temporal-cost-heuristic-value val2)))))

(defmethod cost-= ((val1 temporal-cost) (val2 temporal-cost))
  (and (double= (temporal-cost-start-time val1) (temporal-cost-start-time val2))
       (double= (temporal-cost-heuristic-value val1) (temporal-cost-heuristic-value val2))))

(defmethod build-worst-cost ((state temporal-addresser-state))
  (make-temporal-cost :start-time most-positive-fixnum
                      :heuristic-value most-positive-fixnum))

(defmethod weighted-future-gates ((state temporal-addresser-state))
  (with-slots (lschedule) state
    (flet
        ((2q-application-p (instr)
           (and (typep instr 'application)
                (= 2 (length (application-arguments instr))))))
      (multiple-value-bind (max-value value-hash)
          (lscheduler-walk-graph lschedule
                                 :base-value 0
                                 :bump-value (lambda (instr value)
                                               (if (2q-application-p instr) (1+ value) value))
                                 :combine-values #'max)
        (declare (ignore max-value))
        (or value-hash (make-hash-table))))))

(defmethod select-and-embed-a-permutation ((state temporal-addresser-state) rewirings-tried)
  ;; randomize cost function weights to break
  ;; symmetry when swap selection fails and we rerun?
  (let ((*cost-fn-tier-decay* (+ 0.25d0 (random 0.5d0)))
        (*cost-fn-dist-decay* (+ 0.25d0 (random 0.5d0))))
    (call-next-method)))

(defmethod initialize-instance :after ((instance temporal-addresser-state)
                                       &rest initargs
                                       &key
                                         chip-spec
                                         prototype
                                       &allow-other-keys)
  (declare (ignore initargs))
  (cond
    (prototype
     (setf (addresser-state-qq-distances instance)
           (addresser-state-qq-distances prototype))
     (setf (temporal-addresser-state-cost-bounds instance)
           (temporal-addresser-state-cost-bounds prototype)))
    (t
          
     ;; set up the qq-distances slot to use runtime as the basic unit
     (setf (addresser-state-qq-distances instance)
           (compute-qubit-qubit-distances
            chip-spec
            (lambda (object)
              ;; TODO: is there a good reason this uses floats over COST objects?
              (if (hardware-object-dead-p object)
                  most-positive-fixnum
                  ;; TODO: prefer the FLEX computation below to digging into RECORD-DURATION?
                  (permutation-record-duration (vnth 0 (hardware-object-permutation-gates object)))))))
     ;; warm the cost-bounds slot
     (flet ((fill-cost-bound (hw)
              (setf (gethash hw (temporal-addresser-state-cost-bounds instance))
                    (temporal-cost-heuristic-value
                     (cost-function instance :gate-weights (weighted-future-gates instance))))))
       (warm-up-addresser-state instance #'fill-cost-bound)))))
