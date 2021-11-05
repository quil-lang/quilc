(in-package #:cl-quil)

;;; The main entry point for addressing is DO-GREEDY-ADDRESSING . However, this
;;; manages a fair amount of state as it navigates the addressing process. The
;;; struct below bundles this together for the sake of convenience. At any given
;;; point of the execution there is only one of these objects, with values being
;;; mutated along the way.

(defclass addresser-state ()
  ((initial-l2p :accessor addresser-state-initial-l2p
                :initarg :initial-l2p
                :documentation "The initial logical-to-physical rewiring."
                :type rewiring)
   (working-l2p :accessor addresser-state-working-l2p
                :documentation "The working / current logical-to-physical rewiring. NOTE: This get mutated a _lot_."
                :type rewiring)
   (l2p-components :accessor addresser-state-l2p-components
                   :initarg :l2p-components
                   :documentation "The association of logical to physical components."
                   :type hash-table)
   (qq-distances :accessor addresser-state-qq-distances
                 :documentation "Precomputed SWAP penalties between separated qubits."
                 :type (array real (* *)))
   (lschedule :accessor addresser-state-logical-schedule
              :documentation "The logical schedule of not-yet-processed instructions."
              :type logical-scheduler)
   (chip-sched :accessor addresser-state-chip-schedule
               :documentation "The outgoing schedule of processed instructions."
               :type chip-schedule)
   (chip-spec :accessor addresser-state-chip-specification
              :initarg :chip-spec
              :documentation "The CHIP-SPECIFICATION governing native-ness."
              :type chip-specification)
   (1q-queues :accessor addresser-state-1q-queues
              :documentation "The family of queues where not-yet-scheduled 1Q instructions live, while we get them out of the way to process 2Q instructions."))
  (:documentation "Common state to be manipulated by addressing routines. Implementations may often opt for more, but this represents the minimum that is expectred."))

(defmethod initialize-instance :after ((instance addresser-state)
                                       &rest initargs
                                       &key
                                         chip-spec
                                         initial-l2p
                                       &allow-other-keys)
  (declare (ignore initargs))
  (let* ((n-qubits (chip-spec-n-qubits chip-spec))
         (initial-l2p (cond
                        (initial-l2p
                         (copy-rewiring initial-l2p))
                        (*addresser-start-with-partial-rewiring*
                         (make-partial-rewiring n-qubits))
                        (t
                         (make-rewiring n-qubits)))))
    (setf (addresser-state-initial-l2p instance) initial-l2p
          (addresser-state-working-l2p instance) (copy-rewiring initial-l2p)
          (addresser-state-logical-schedule instance) (make-lscheduler)
          (addresser-state-chip-schedule instance) (make-chip-schedule chip-spec)
          (addresser-state-chip-specification instance) chip-spec
          (addresser-state-1q-queues instance) (make-array (chip-spec-n-qubits chip-spec) :initial-element (list)))))


(defmacro with-rotatef (places &body body)
  "Evaluate BODY under a rotation of PLACES, then restore these to their original values."
  `(unwind-protect
        (progn (rotatef ,@places)
               ,@body)
     (rotatef ,@(reverse places))))

;;; Current addresser types (of which there are two: the temporal addresser and
;;; fidelity addresser) subclass ADDRESSER-STATE and then implement methods on a
;;; few generics listed below. Mostly this has to do assigning and manipulating
;;; weights and costs.
;;;
;;; NOTE: Addresser implementations MUST also fill the qq-distances slot with
;;; appropriate values. On the one hand, these are necessary for the basic
;;; functioning of the addresser routines and swap selection heuristics. On the
;;; other hand, we do not at present wish to suppose exactly what this looks
;;; like.

(defgeneric weighted-future-gates (state)
  (:documentation "Collect gates from the logical schedule, along with weights which indicate how far into the future the gate occurs. The precise interpretation of this depends on the implementation of COST-FUNCTION, but generally it is expected that a larger weight indicates that the gate occurs 'farther' away in the future.

Returns a hash mapping gates from the logical schedule to numeric values."))

(defgeneric cost-function (state &key gate-weights instr)
  (:documentation "Generic method for extracting a heuristic value.

STATE is an ADDRESSER-STATE, which in particular carries an LSCHEDULER of gates yet to be scheduled.

GATE-WEIGHTS is a hash mapping instructions in STATE->LSCHEDULER to (numerical) weights.  These can be used to precompute values across different runs of COST-FUNCTION (e.g., the depth of an instruction).

INSTR is the \"active instruction\".

- Neither is specified: COST-FUNCTION will return the \"best possible value\".
- GATE-WEIGHTS is specified, INSTR is not: COST-FUNCTION will return a heuristic value suitable for comparing different rewirings, based on the future of instructions to be scheduled.
- INSTR is specified, GATE-WEIGHTS is not: COST-FUNCTION will return a heuristic value suitable for comparing different instructions yet to be scheduled, based on the history of instructions already scheduled.
- GATE-WEIGHTS and INSTR are both specified: COST-FUNCTION will return a heuristic value suitable for comparing different instructions INSTR to be injected (i.e., INSTR is assumed not to participate in LSCHEDULER).
"))

(defgeneric build-worst-cost (state)
  (:documentation "Builds a POSITIVE-INFINITY type value for the COST-FUNCTION associated to STATE."))

;;; Since sometimes costs are compound objects which encode various values, they
;;; must implement the following:

(defgeneric cost-< (x y)
  (:documentation "Generic comparison function for heuristic values.")
  (:method ((x real) (y real))
    (< (- x +double-comparison-threshold-strict+) y)))

(defgeneric cost-= (x y)
  (:documentation "Generic equality function for heuristic values.")
  (:method ((x real) (y real))
    (< (abs (- x y))
       +double-comparison-threshold-strict+)))

(defgeneric cost-flatten (cost)
  (:documentation "Flattens COST to a REAL.  Preserves cost ordering whenever only one of GATE-WEIGHTS or INSTR was provided to COST-FUNCTION.")
  (:method ((cost real))
    cost))

;;; Some utilities

(defun find-physical-component-in-state (state logical)
  (find-physical-component (addresser-state-l2p-components state) logical))

(defun find-physical-component (l2p-components logical)
  "Uses L2P-COMPONENTS to get a physical qubit component from the logical qubit address LOGICAL."
  (maphash (lambda (logical-component physical-component)
             (when (member logical logical-component)
               (return-from find-physical-component physical-component)))
           l2p-components)
  (error "Unable to find a physical connected component associated with the qubit ~a." logical))

(defun compute-qubit-qubit-distances (chip-spec link-cost)
  "Implements Floyd-Warshall to compute the minimum weighted distance between any pair of qubits on a CHIP-SPECification, weighted by LINK-COST."
  ;; nearly ripped straight out of the Wikipedia article for Floyd-Warshall
  (let* ((vertex-count (chip-spec-n-qubits chip-spec))
         (dist (make-array (list vertex-count vertex-count)
                           :initial-element most-positive-fixnum)))
    ;; all vertices are distance 0 from themselves.
    (dotimes (j vertex-count)
      (setf (aref dist j j) 0))
    ;; write the direct node-to-node weights into the table
    (loop :for link :across  (chip-spec-links chip-spec)
          :for value := (funcall link-cost link)
          :do (destructuring-bind (q0 q1)
                  (coerce (vnth 0 (hardware-object-cxns link)) 'list)
                (setf (aref dist q0 q1) value
                      (aref dist q1 q0) value)))
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
              (setf (aref dist i j) (+ ik kj)
                    (aref dist j i) (+ ik kj)))))))
    dist))

(defun warm-up-addresser-state (state hardware-op)
  "'Warm up' the addresser STATE by iterating over each hardware object on the chip, initializing a random gate on the object and then calling HARDWARE-OP with it."
  ;; We snag some of the addresser state here, and restore it after the loop.
  (let ((initial-l2p (addresser-state-initial-l2p state))
        (working-l2p (addresser-state-working-l2p state))
        (lscheduler (addresser-state-logical-schedule state))
        (chip-sched (addresser-state-chip-schedule state)))
    (loop :with chip-spec := (addresser-state-chip-specification state)
          :for order-list :across (chip-specification-objects chip-spec)
          :for qubits :from 1
          :do (dotimes (j (length order-list))
                (let ((hw (vnth j order-list)))
                  (unless (hardware-object-dead-p hw)
                    (let* ((instr (apply #'anon-gate "FLEX" (random-special-unitary (expt 2 qubits))
                                         (or (coerce (vnth 0 (hardware-object-cxns hw)) 'list)
                                             (list j))))
                           (instrs-decomposed (expand-to-native-instructions (list instr) chip-spec))
                           (instrs-compressed (if *compute-tight-recombination-bound*
                                                  (compress-instructions instrs-decomposed chip-spec)
                                                  instrs-decomposed)))
                      (setf (addresser-state-initial-l2p state) (make-rewiring (chip-spec-n-qubits chip-spec))
                            (addresser-state-working-l2p state) (make-rewiring (chip-spec-n-qubits chip-spec))
                            (addresser-state-logical-schedule state) (make-lscheduler)
                            (addresser-state-chip-schedule state) (make-chip-schedule chip-spec))
                      (append-instructions-to-lschedule (addresser-state-logical-schedule state)
                                                        instrs-compressed)
                      (funcall hardware-op hw))))))
    (setf (addresser-state-initial-l2p state) initial-l2p
          (addresser-state-working-l2p state) working-l2p
          (addresser-state-logical-schedule state) lscheduler
          (addresser-state-chip-schedule state) chip-sched)))
