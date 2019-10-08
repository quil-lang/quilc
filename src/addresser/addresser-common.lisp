;;;; addresser-common.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

;;; This file contains the addresser, which solves the problem of mapping a
;;; sequence of Quil instructions acting on logical qubits to a scheduled
;;; sequence of Quil instructions acting on physical qubits.
;;;
;;; Logical qubits are agnostic of any particular QPU. In particular, incoming
;;; Quil programs may presuppose a certain connectivity that does not exist (or
;;; is suboptimal) on a specific chip. Physical qubits are meaningful with
;;; respect to a given chip specification, and one of the responsibilities of
;;; the addesser is to manage the rewiring of logical to physical qubits,
;;; introducing SWAP instructions as needed to allow for the appropriate
;;; connectivity.
;;;
;;; As a very toy example, consider the following logical instruction
;;;
;;;  CNOT 0 4
;;;
;;; with the chip topology given by this diagram
;;;
;;;         (1)
;;;        /   \
;;;      (0)   (2)--(4)
;;;        \   /
;;;         (3)
;;;
;;; Supposing for a brief moment that the chip supports CNOT and SWAP
;;; operations on adjacent physical qubits, the instruction CNOT 0 2
;;; could be translated to
;;;
;;;   SWAP 0 1
;;;   SWAP 1 2
;;;   CNOT 2 4
;;;   SWAP 1 2
;;;   SWAP 0 1
;;;
;;; In this particular example, the presence of the path (0)--(1)--(2)
;;; from physical qubit 0 to a qubit adjacent to 4 is what enabled
;;; this particular sequence of SWAPs. It is one of the responsibilities of the
;;; temporal addresser to identify such paths, and insert the corresponding
;;; SWAP operations. Among the many candidate paths, it must choose
;;; a preferred one according to some heuristic, such as A* (with weights
;;; coming from timing or fidelity data).
;;;
;;; A second responsibility of the addresser is to augment the provided
;;; Quil instructions with timing information, relative to their execution
;;; on a physical device. This information is managed by a chip schedule.
;;;
;;; The potential heuristics used to search for solutions to the addressing
;;; problem are manifold, and different ones are called for in different
;;; situations. This file contains the guts common to each such method, and
;;; different methods will invoke these guts after binding particular behaviors
;;; to various hooks.
;;;
;;; See DO-GREEDY-TEMPORAL-ADDRESSING below for the main entry point.


(defparameter *addresser-swap-lookahead-depth* 2
  "Controls the length of SWAP chains explored by the addresser loop.

WARNING: This value makes the SWAP-selection stage run as
O((# 2Q links)^(*addresser-swap-lookahead-depth*)). Beware making it too
large.")

(defparameter *addresser-max-swap-sequence-length* 1000
  "Controls the maximum number of swaps that can occur in a row.")

(defparameter *prefer-ranged-gates-to-swaps* nil
  "When T, use chains of instructions to simulate long-range gates rather than
SWAPping qubits into place.")

(defparameter *addresser-start-with-partial-rewiring* t
  "When T, starts with a partial rewiring that is filled in gradually.")

(defparameter *addresser-use-2q-tiers* t
  "When T, uses the 2-qubit tiers rather than the general instruction tiers.")

;;; A pseudoinstruction class used to send directives to the addresser ;;;

(defclass application-force-rewiring (application)
  ((target-rewiring :initarg :target
                    :accessor application-force-rewiring-target
                    :documentation "The addresser will move the working rewiring to match this rewiring upon encountering this instruction."))
  (:documentation "A pseudoinstruction used to communicate desired rewiring state across addresser runs."))

;; initialize the other slots of the force-rewiring
(defmethod initialize-instance :after ((inst application-force-rewiring)
                                       &rest initargs)
  (declare (ignore initargs))
  (with-slots (operator arguments target-rewiring) inst
    (setf operator (named-operator "FORCE-REWIRING"))
    (setf arguments (loop :for i :across (rewiring-l2p target-rewiring) :collect (qubit i)))))

(defun application-other-argument (inst qubit)
  "Assumes that INST is a 2-qubit gate. Will get the index of the other qubit
(not equal to QUBIT) used by INST."
  (destructuring-bind (q0 q1) (mapcar #'qubit-index (application-arguments inst))
    (if (= q0 qubit) q1 q0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Addresser State Updates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The main entry point for temporal addressing is DO-GREEDY-TEMPORAL-ADDRESSING
;;; below. However, this manages a fair amount of state as it navigates the
;;; addressing process. The struct below bundles this together for the sake of
;;; convenience. At any given point of the execution there is only one of these
;;; objects, with values being mutated along the way.

(defclass addresser-state ()
  ((initial-l2p :accessor addresser-state-initial-l2p
                :initarg :initial-l2p
                :documentation "The initial logical-to-physical rewiring.")
   (working-l2p :accessor addresser-state-working-l2p
                :initarg :working-l2p
                :documentation "The working / current logical-to-physical rewiring. NOTE: This get mutated a _lot_.")
   (qq-distances :accessor addresser-state-qq-distances
                 :initarg :qq-distances
                 :documentation "Precomputed SWAP penalties between separated qubits.")
   (qubit-cc :accessor addresser-state-qubit-cc
             :initarg :qubit-cc
             :documentation "The connected component where newly-assigned qubits will live.")
   (lschedule :accessor addresser-state-logical-schedule
              :initarg :lschedule
              :documentation "The logical schedule of not-yet-processed instructions.")
   (chip-sched :accessor addresser-state-chip-schedule
               :initarg :chip-sched
               :documentation "The outgoing schedule of processed instructions.")
   (chip-spec :accessor addresser-state-chip-specification
              :initarg :chip-spec
              :documentation "The CHIP-SPECIFICATION governing native-ness.")))

(defun initial-addresser-working-state (chip-spec initial-rewiring)
  "Prepare the initial working state for the logical addresser, given chip specification
CHIP-SPEC and an initial logical-to-physical rewirign INITIAL-REWIRING."
  (let* ((n-qubits (chip-spec-n-qubits chip-spec))
         (initial-l2p (cond
                        (initial-rewiring
                         (copy-rewiring initial-rewiring))
                        (*addresser-start-with-partial-rewiring*
                         (make-partial-rewiring n-qubits))
                        (t
                         (make-rewiring n-qubits)))))
    (make-instance 'addresser-state
                   :initial-l2p initial-l2p
                   :working-l2p (copy-rewiring initial-l2p)
                   :lschedule (make-lscheduler)
                   :qq-distances (precompute-qubit-qubit-distances chip-spec)
                   :qubit-cc (a:extremum (chip-spec-live-qubit-cc chip-spec)
                                         #'>
                                         :key #'length)
                   :chip-sched (make-chip-schedule chip-spec)
                   :chip-spec chip-spec)))

;; TODO: why isn't this part of the state?
(defparameter *addresser-use-free-swaps* nil
  "Does the addresser treat the initial rewiring as something that can be changed?")

(defun select-and-embed-a-permutation (state rewirings-tried)
  "Select a permutation and schedule it for execution. The permutation is selected to lower the
cost-function associated to the current lschedule."
  (with-slots (lschedule initial-l2p working-l2p chip-sched chip-spec qq-distances)
      state
    (format *compiler-noise-stream*
            "SELECT-AND-EMBED-A-PERMUTATION: entering SWAP selection phase.~%")
    (let ((gates-in-waiting (lscheduler-tiers lschedule))
          (*cost-fn-tier-decay* (+ 0.25d0 (random 0.5d0)))
          (*cost-fn-dist-decay* (+ 0.25d0 (random 0.5d0))))
      (ecase *addresser-swap-search-type*
        (:a*
         (flet ((cost-function (rewiring)
                  (* *addresser-a*-swap-search-heuristic-scale*
                     (cost-function qq-distances rewiring gates-in-waiting)))
                (done-function (rewiring)
                  (prog2
                      (rotatef rewiring working-l2p)
                      (dequeue-logical-to-physical state :dry-run t)
                    (rotatef rewiring working-l2p))))
           (dolist (link-index
                    (search-rewiring chip-spec working-l2p
                                     (chip-schedule-qubit-times chip-sched)
                                     #'cost-function #'done-function
                                     :max-iterations *addresser-a*-swap-search-max-iterations*))
             (embed-swap link-index
                         initial-l2p
                         working-l2p
                         chip-spec
                         chip-sched
                         :use-free-swaps *addresser-use-free-swaps*))))
        (:greedy-qubit
         (flet ((cost-function (rewiring)
                  (cost-function qq-distances rewiring gates-in-waiting)))
           (push (copy-rewiring working-l2p) rewirings-tried)
           (embed-swap (select-cost-lowering-swap working-l2p chip-spec chip-sched *addresser-use-free-swaps*
                                                  #'cost-function
                                                  rewirings-tried)
                       initial-l2p
                       working-l2p
                       chip-spec
                       chip-sched
                       :use-free-swaps *addresser-use-free-swaps*)
           rewirings-tried))
        (:greedy-path
         (push (copy-rewiring working-l2p) rewirings-tried)
         (let ((link-index (select-swap-path-gates chip-spec qq-distances gates-in-waiting
                                                   rewirings-tried working-l2p)))
           ;; if we have a nil swap, the greedy scheduler has failed to operate. scary!
           (assert link-index () "Failed to select a SWAP instruction.")
           (embed-swap link-index
                       initial-l2p
                       working-l2p
                       chip-spec
                       chip-sched
                       :use-free-swaps *addresser-use-free-swaps*))
         rewirings-tried)))))

(defgeneric dequeue-classical-instruction (state instr &optional dry-run-escape)
  (:documentation
   "Dispatch for dequeueing classical instructions. Returns a flag indicating whether
we've dirtied up the schedule.")
  (:method ((state addresser-state) instr &optional dry-run-escape)
    (when dry-run-escape  ; every classical instruction can be handled
      (funcall dry-run-escape))
    (with-slots (lschedule chip-spec chip-sched working-l2p) state
      (let (dirty-flag)
        (cond
          ;; is it resourceless?
          ((typep instr 'no-operation)
           ;; if so, discard it and continue.
           (lscheduler-dequeue-instruction lschedule instr)
           (setf dirty-flag t))

          ;; is it maximally resourceful?
          ((global-instruction-p instr)
           ;; dequeue the instruction and set the dirty flag
           (chip-schedule-append chip-sched instr)
           (lscheduler-dequeue-instruction lschedule instr)
           (setf dirty-flag t))

          ;; is it a pure classical instruction?
          ((local-classical-instruction-p instr)
           ;; dequeue the instruction and set the dirty flag
           (chip-schedule-append chip-sched instr)
           (lscheduler-dequeue-instruction lschedule instr)
           (setf dirty-flag t))

          ;; is it a local mixed pure/classical instruction?
          ((or (local-classical-quantum-instruction-p instr)
               (typep instr 'measure-discard)
               (typep instr 'reset-qubit))
           (let ((resources (instruction-resources instr)))
             ;; insert the instruction
             (rewire-l2p-instruction working-l2p instr)
             (chip-schedule-append chip-sched instr)
             ;; dequeue the instruction and set the dirty flag
             (lscheduler-dequeue-instruction lschedule instr)
             (setf dirty-flag t)))

          ;; is it some other kind of PRAGMA not covered above?
          ((typep instr 'pragma)
           ;; just throw it away.
           (lscheduler-dequeue-instruction lschedule instr)
           (setf dirty-flag t))

          ;; otherwise, we don't know what to do
          (t
           (error "The instruction type of \"~/quil:instruction-fmt/\" is not supported by the addresser." instr)))

        dirty-flag))))

(defgeneric dequeue-gate-application (state instr &optional dry-run-escape)
  (:documentation
   "Dequeues the given gate application INSTR, if possible. Returns T if the
schedule gets dirtied in the process, or NIL otherwise. Two other values are
returned: a list of fully rewired 2Q instructions for later scheduling, and a
list of partially-rewired 2Q instructions for later scheduling.")
  (:method (state instr &optional dry-run-escape)
    (with-slots (lschedule working-l2p initial-l2p chip-spec chip-sched qq-distances) state
      (let (dirty-flag
            ready-2q-instrs
            partial-2q-instrs)
        (cond
          ;; is it a rewiring pseudoinstruction?
          ((typep instr 'application-force-rewiring)
           (lscheduler-dequeue-instruction lschedule instr)
           (move-to-expected-rewiring
            working-l2p
            (application-force-rewiring-target instr)
            qq-distances
            chip-spec
            chip-sched
            initial-l2p
            *addresser-use-free-swaps*)
           (setf dirty-flag t))

          ;; is it a 2Q gate?
          ((= 2 (length (application-arguments instr)))
           ;; quick error check on qubit indices
           (assert (every (lambda (q) (< -1 (qubit-index q) (chip-spec-n-qubits chip-spec)))
                          (application-arguments instr))
                   nil
                   "Instruction qubit indices are out of bounds for target QPU: ~/quil:instruction-fmt/"
                   instr)
           (destructuring-bind (p0 p1)
               (mapcar (lambda (q) (apply-rewiring-l2p working-l2p (qubit-index q)))
                       (application-arguments instr))
             (let ((link-line
                     (and p0 p1
                          (nth-value 1 (lookup-hardware-address-by-qubits chip-spec (list p0 p1))))))
               ;; if so, try to find a link that supports it
               (cond
                 ;; the rewiring hasn't been filled out on these logical qubits yet.
                 ;; store this instruction in a bin of to-be-wired possibilities.
                 ((or (not p0) (not p1))
                  (push instr partial-2q-instrs))
                 ;; if we found a link, store this info for later.
                 ;; we make a second pass to deal with 2Q gates after the
                 ;; "easier" gates are handled, since we need to collect info
                 ;; to select the shortest-horizon 2Q gate for insertion.
                 (link-line
                  (push (list link-line instr) ready-2q-instrs))
                 ;; or, if we're not supposed to be SWAPping, we should
                 ;; apply some localizing compilers instead.
                 (*prefer-ranged-gates-to-swaps*
                  (when dry-run-escape
                    (funcall dry-run-escape))
                  (let ((compilation-result (apply-translation-compilers instr chip-spec nil)))
                    (assert compilation-result ()
                            "Failed to apply localizing compilers.")
                    (setf dirty-flag t)
                    (lscheduler-replace-instruction lschedule instr compilation-result)))))))

          ;; is it a 1Q gate?
          ((= 1 (length (application-arguments instr)))
           ;; quick error check on instruction qubits
           (assert (every (lambda (q) (< -1 (qubit-index q) (chip-spec-n-qubits
                                                             (addresser-state-chip-specification state))))
                          (application-arguments instr))
                   nil
                   "Instruction qubit indices are out of bounds for target QPU: ~/quil:instruction-fmt/"
                   instr)
           (when dry-run-escape
             (funcall dry-run-escape))
           (format *compiler-noise-stream*
                   "DEQUEUE-GATE-APPLICATION: ~/quil:instruction-fmt/ is a 1Q ~
instruction, adding to logical queue.~%"
                   instr)
           
           (let ((rewired-instr (copy-instance instr)))
             ;; TODO: this logic makes me think that, if we aren't treating 1Q
             ;; and 2Q instructions instructions separately in the common addresser,
             ;; it should be possible to deal with this 'partially rewired' logic
             ;; in a uniform way, rather than doing it for 1Q here and for 2Q
             ;; later down the road. why not deal with the fully assigned things
             ;; here, bucket the things that are properly partially assigned,
             ;; deal with those by producing potential reassignments, and picking
             ;; the best option from those --- where 'those' ranges over 1Q and
             ;; 2Q gates? seems like it could work.
             (handler-case (rewire-l2p-instruction working-l2p rewired-instr)
               (missing-rewiring-assignment (c)
                 (declare (ignore c))
                 (format *compiler-noise-stream*
                         "DEQUEUE-GATE-APPLICATION: Couldn't rewire ~
~/quil:instruction-fmt/ because assignment is missing"
                         instr)
                 
                 (let* ((q (qubit-index (first (application-arguments instr))))
                        (p (best-qubit-position state q (lscheduler-tiers lschedule))))
                   (rewiring-assign (addresser-state-working-l2p state) q p)
                   (return-from dequeue-gate-application t))))
             
             ;; dequeue and set the dirty bit
             (lscheduler-dequeue-instruction (addresser-state-logical-schedule state) instr)
             (append-instr-to-chip-schedule state rewired-instr)
             (values t nil nil)))

          ;; is it a many-Q gate?
          ((> (length (application-arguments instr)) (length (chip-specification-objects chip-spec)))
           (when dry-run-escape
             (funcall dry-run-escape))
           ;; quick error check on instruction qubits
           (assert (every (lambda (q) (< -1 (qubit-index q) (chip-spec-n-qubits chip-spec)))
                          (application-arguments instr))
                   nil
                   "Instruction qubit indices are out of bounds for target QPU: ~/quil:instruction-fmt/"
                   instr)
           (format *compiler-noise-stream*
                   "DEQUEUE-GATE-APPLICATION: ~/quil:instruction-fmt/ is a ~dQ>2Q instruction, compiling.~%"
                   instr
                   (length (application-arguments instr)))
           ;; then we know we can't find a hardware object to support
           ;; it, so pass it to the chip compiler
           (let ((compilation-result (apply-translation-compilers instr chip-spec nil)))
             (setf dirty-flag t)
             (lscheduler-replace-instruction lschedule instr compilation-result)))
          ;; otherwise, we're helpless
          (t nil))

        (values dirty-flag ready-2q-instrs partial-2q-instrs)))))

(defun dequeue-logical-to-physical (state &key (dry-run nil))
  "Offload instructions from the logical schedule onto the physical hardware, returning T if progress is made.

If DRY-RUN, this returns T as soon as it finds an instruction it can handle."
  ;; Allow for early exit if we are doing a dry run
  (catch 'dry-run-succeeded
    (with-slots (lschedule chip-sched chip-spec working-l2p qq-distances initial-l2p) state
      (format *compiler-noise-stream* "DEQUEUE-LOGICAL-TO-PHYSICAL: entering dequeueing phase.~%")
      (let ((dirty-flag nil)
            (2q-instrs-ready-for-scheduling nil)
            (2q-instrs-partially-assigned nil)
            (dry-run-escape (if dry-run
                                (lambda () (throw 'dry-run-succeeded t))
                                nil)))

        ;; if the lschedule is empty, we're done
        (when (endp (lscheduler-first-instrs lschedule))
          (return-from dequeue-logical-to-physical nil))

        ;; otherwise, the lschedule is nonempty, so we try to dequeue instructions
        (dolist (instr (lscheduler-topmost-instructions lschedule))
          (multiple-value-bind (dirtied ready-2q partial-2q)
              (if (typep instr 'application)
                  (dequeue-gate-application state instr dry-run-escape)
                  (dequeue-classical-instruction instr dry-run-escape))
            (when dirtied
              (setf dirty-flag t))
            (when ready-2q
              (setf 2q-instrs-ready-for-scheduling
                    (append ready-2q 2q-instrs-ready-for-scheduling)))
            (when partial-2q
              (setf 2q-instrs-partially-assigned
                    (append partial-2q 2q-instrs-partially-assigned)))))

        ;; thus ends our loop over topmost-instructions.
        ;; if we dirtied up the schedule, do it all again.
        (when dirty-flag
          (assert (not dry-run) () "Got dirty when the scheduler was supposed to be dry.")
          (return-from dequeue-logical-to-physical t))

        ;; if we didn't dirty up the schedule, see if we collected any
        ;; 2Q gates along the way
        (when (dequeue-soonest-2q-from-list state
                                            2q-instrs-ready-for-scheduling
                                            2q-instrs-partially-assigned
                                            :dry-run dry-run)
          (return-from dequeue-logical-to-physical t))

        nil))))

(defun soonest-2q-instruction (state 2q-instrs partially-assigned-2q-instrs lschedule chip-sched chip-spec)
  "Find the instruction from the given 2Q-INSTRS and PARTIALLY-ASSIGNED-2Q-INSTRS which ends
the soonest. Returns three values: the instruction, the link line, and the optional qubit assignments if
the selected instruction was only partially assigned."
  (let ((horizon most-positive-fixnum)
        instr
        qubit-assignments
        link-line)
    (loop
      :for (candidate-link-line candidate-instr) :in 2q-instrs
      :for (q0 q1) := (coerce (chip-spec-qubits-on-link chip-spec candidate-link-line) 'list)
      :for candidate-horizon
        := (chip-schedule-resource-end-time
            chip-sched
            (make-qubit-resource q0 q1))
      :when (< candidate-horizon horizon)
        :do (setf horizon candidate-horizon
                  instr candidate-instr
                  link-line candidate-link-line))

    (when partially-assigned-2q-instrs
      (loop
        :with gates-in-waiting := (lscheduler-tiers lschedule)
        :for candidate-instr :in partially-assigned-2q-instrs
        :for ((q0 q1) (pos0 pos1)) := (assign-gate state candidate-instr gates-in-waiting)
        :when (and pos0 pos1)
          :do (a:when-let*
                  ((candidate-link-line
                    (nth-value 1 (lookup-hardware-address-by-qubits chip-spec (list pos0 pos1))))
                   (candidate-horizon
                    (chip-schedule-resource-end-time
                     chip-sched
                     (make-qubit-resource pos0 pos1))))
                (when (< candidate-horizon horizon)
                  (setf horizon candidate-horizon
                        link-line candidate-link-line
                        instr candidate-instr
                        qubit-assignments (list (list q0 pos0) (list q1 pos1)))))))
    (values instr link-line qubit-assignments)))

(defgeneric append-instr-to-chip-schedule (state instr)
  (:documentation "Appends INSTR to the CHIP-SCHEDULE housed within STATE.")
  (:method (state instr)
    (chip-schedule-append (addresser-state-chip-schedule state) instr)))

(defun dequeue-soonest-2q-from-list (state 2q-instrs-ready-for-scheduling 2q-instrs-partially-assigned &key dry-run)
  "Dispatches pure 2Q instruction scheduling from a list of 2Q instructions that
are ready to be scheduled."
  (with-slots (chip-sched chip-spec working-l2p lschedule) state
      (multiple-value-bind (instr link-line qubit-assignments)
          (soonest-2q-instruction state
                                  2q-instrs-ready-for-scheduling
                                  2q-instrs-partially-assigned
                                  lschedule chip-sched chip-spec)

        ;; if we didn't find any instructions, then return unsuccessful
        (unless instr (return-from dequeue-soonest-2q-from-list nil))

        ;; from now on, we would schedule something, so bail if on a dry run
        (when dry-run (return-from dequeue-soonest-2q-from-list t))

        ;; ... and dispatch it.
        (format *compiler-noise-stream*
                "DEQUEUE-SOONEST-2Q-FROM-LIST: Elected to schedule ~/quil:instruction-fmt/.~%"
                instr)
        (let ((rewired-instr (copy-instance instr)))
          ;; If a rewiring fails, which it may in e.g. a PARTIAL
          ;; rewiring scheme, then we need to know about it,
          ;; because the gate is native up to remapping, and
          ;; therefore it is gate-native and can be
          ;; instruction-native.
          ;;
          ;; We only want to attempt to rewire gates; other
          ;; objects like circuits (which shouldn't exist), or
          ;; APPLICATION-THREAD-INVOCATION (which can exist)
          ;; shouldn't attempt to be rewired, because they're not
          ;; native anyway. It's an ugly type check, but it is
          ;; what it is.
          (when (or (typep rewired-instr 'gate-application)
                    (and *allow-unresolved-applications*
                         (typep rewired-instr 'unresolved-application)))
            (handler-case (rewire-l2p-instruction working-l2p rewired-instr)
              (missing-rewiring-assignment (c)
                (declare (ignore c))
                ;; the instruction is only partially wired, but remember
                ;; that we already found a putative link. let's do the
                ;; rewiring now, so that on the next pass this instruction
                ;; will fall into the bucket of instructions that are
                ;; ready-2-go.
                (format *compiler-noise-stream*
                        "DEQUEUE-SOONEST-2Q-FROM-LIST: Couldn't rewire ~
~/quil:instruction-fmt/ because assignment is missing"
                        instr)
                (loop
                  :for (logical physical) :in qubit-assignments
                  :unless (apply-rewiring-l2p working-l2p logical)
                    :do (format *compiler-noise-stream*
                                "DEQUEUE-SOONEST-2Q-FROM-LIST: assigning ~
logical qubit ~a to physical qubit ~a~%"
                                logical physical)
                        (rewiring-assign working-l2p logical physical))
                (return-from dequeue-soonest-2q-from-list t))))

          (format *compiler-noise-stream*
                  "DEQUEUE-SOONEST-2Q-FROM-LIST: ~/quil:instruction-fmt/ is ~
~/quil:instruction-fmt/ in the current rewiring~%"
                  instr rewired-instr)

          ;; Figure out if we need to compile the instruction,
          ;; or if we can add it to the schedule.
          (cond
            ;; if we found a link and the instruction is native...
            ((hardware-object-native-instruction-p (chip-spec-nth-link chip-spec link-line)
                                                   rewired-instr)
             (format *compiler-noise-stream*
                     "DEQUEUE-SOONEST-2Q-FROM-LIST: ~/quil:instruction-fmt/ is ~
native in l2p rewiring ~A, flushing 1Q lines and dequeueing.~%"
                     instr
                     (rewiring-l2p working-l2p))

             (destructuring-bind (left-line right-line)
                 (mapcar #'qubit-index (application-arguments instr))
               ;; dequeue the instruction so we can push the
               ;; modified instruction onto the schedule.
               (lscheduler-dequeue-instruction lschedule instr)
               ;; and stack the 2Q gate on top
               (append-instr-to-chip-schedule state rewired-instr)))

            ;; otherwise, we found a link but the instruction is not native
            (t
             (format *compiler-noise-stream*
                     "DEQUEUE-SOONEST-2Q-FROM-LIST: ~/quil:instruction-fmt/ is ~
non-native in the current rewiring, compiling.~%"
                     instr)

             ;; ...release the hounds
             ;;
             ;;
             ;;        ,--._______,-.
             ;;      ,','  ,    .  ,_`-.
             ;;     / /  ,' , _` ``. |  )       `-..
             ;;    (,';'""`/ '"`-._ ` \/ ______    \\
             ;;      : ,o.-`- ,o.  )\` -'      `---.))
             ;;      : , d8b ^-.   '|   `.      `    `.
             ;;      |/ __:_     `. |  ,  `       `    \
             ;;      | ( ,-.`-.    ;'  ;   `       :    ;
             ;;      | |  ,   `.      /     ;      :    \
             ;;      ;-'`:::._,`.__),'             :     ;
             ;;     / ,  `-   `--                  ;     |
             ;;    /  \                   `       ,      |
             ;;   (    `     :              :    ,\      |
             ;;    \   `.    :     :        :  ,'  \    :
             ;;     \    `|-- `     \ ,'    ,-'     :-.-';
             ;;     :     |`--.______;     |        :    :
             ;;      :    /           |    |         |   \
             ;;      |    ;           ;    ;        /     ;
             ;;    _/--' |   -hrr-   :`-- /         \_:_:_|
             ;;  ,',','  |           |___ \
             ;;  `^._,--'           / , , .)
             ;;                     `-._,-'
             (let ((compiled-seq (apply-translation-compilers
                                  instr
                                  chip-spec
                                  (chip-spec-nth-link chip-spec link-line))))
               (lscheduler-replace-instruction lschedule instr compiled-seq)))))
        t)))

(defun lscheduler-tiers (lschedule)
  (if *addresser-use-2q-tiers*
      (lscheduler-2q-tiers lschedule)
      (lscheduler-instruction-tiers lschedule)))

(defun assign-gate (state inst gates-in-waiting)
  "Given a gate application that is unassigned or partially assigned by
the addresser's working logical-to-physical rewiring, compute a best physical qubit
assignment."
  (assert (= 2 (length (application-arguments inst))) () "Expected 2-qubit gate")
  (with-slots (working-l2p chip-spec) state
    (destructuring-bind (q0 q1) (mapcar #'qubit-index (application-arguments inst))
      (let ((p0 (apply-rewiring-l2p working-l2p q0))
            (p1 (apply-rewiring-l2p working-l2p q1)))
        (when p1 (rotatef p0 p1) (rotatef q0 q1))
        (if p0
            (list (list q0 q1)
                  (list p0 (best-qubit-position state q1 gates-in-waiting
                                                :locations (chip-spec-adj-qubits chip-spec p0))))
            (list (list q0 q1) (best-qubit-positions state q0 q1 gates-in-waiting)))))))

(defun best-qubit-position (state logical gates-in-waiting &key locations)
  "Finds the best location for an unassigned logical under the given future schedule."
  (with-slots (working-l2p chip-spec qq-distances qubit-cc) state
    (let ((locations (or locations qubit-cc)))
      (assert (not (apply-rewiring-l2p working-l2p logical)) (logical)
              "Qubit ~a already assigned" logical)
      (a:extremum
       (remove-if (lambda (p)
                    (or (apply-rewiring-p2l working-l2p p)
                        (chip-spec-qubit-dead? chip-spec p)))
                  locations)
       #'<
       :key (lambda (physical)
              (with-rewiring-assign working-l2p logical physical
                (cost-function qq-distances working-l2p gates-in-waiting)))))))

(defun best-qubit-positions (state q0 q1 gates-in-waiting)
  "Find the best location for the pair of qubits Q0 and Q1."
  (let ((pos0 (best-qubit-position state q0 gates-in-waiting)))
    (with-slots (working-l2p chip-spec) state
      (with-rewiring-assign working-l2p q0 pos0
        (list pos0 (best-qubit-position state q1 gates-in-waiting
                                        :locations (chip-spec-adj-qubits chip-spec pos0)))))))

;; todo: eventually we want to modify parts of this to incorporate multi-qubit
;;       hardware objects. a lot of this is already correctly set up for that
;;       eventuality. the main thing that needs to change is that 1Q- and 2Q-
;;       operations should be handled identically. for instance, we should add
;;       queues for the 2Q operations, and they should also be written out by a
;;       flushing call.
(defun do-greedy-addressing (state  ; initial addresser state
                             instrs ; list of instructions to schedule
                             &key
                               (initial-rewiring nil) ; optionally provide an initial rewiring
                               (use-free-swaps nil) ; treat the initial rewiring as virtual
                               )
  "Schedules INSTRS for execution on a QPU specified by CHIP-SPEC. Returns a
values triple (initial-rewiring chip-schedule final-rewiring).

Optional arguments:
 + INITIAL-REWIRING launches with the addresser with a nontrivial qubit
   permutation.
 + USE-FREE-SWAPS treats the initial rewiring as virtual (able to be changed).
   If INITIAL-REWIRING is not provided this option has no effect.
"
  (format *compiler-noise-stream*
          "DO-GREEDY-ADDRESSING: entrance.~%")
  (let ((*addresser-use-free-swaps* (or use-free-swaps (not initial-rewiring))))
    (with-slots (lschedule working-l2p chip-sched initial-l2p) state
      ;; This is governed by an FSM of the shape
      ;;
      ;; [ do-greedy-addressing ]
      ;; [ initialization       ]
      ;;    |
      ;;    v
      ;; [ dequeue-logical-to-physical     and      ]
      ;; [ dequeue-soonest-2q-instruction-from-list ]
      ;; [ inserts available logical instructions   ]
      ;;   |        ^         ^
      ;;   |        |         |
      ;;   |        |        yes
      ;;   v        |         |
      ;; < did we dequeue any instructions? >
      ;;   |        |
      ;;   no       |
      ;;   |        |
      ;;   v        |
      ;; < is the logical schedule empty? > --yes-> [ do-greedy-temporal-addressing ]
      ;;   |        |                               [ flush / cleanup ]
      ;;   no       |
      ;;   |        |
      ;;   v        |
      ;; [ select-and-embed-a-permutation                                   ]
      ;; [ finds a permutation that lowers the cost-function and inserts it ]
      ;;
      ;; These chunks are defined as functions above.
      (flet ((addresser-FSM ()
               (loop
                 :with rewirings-tried := nil
                 :while (lscheduler-first-instrs lschedule)
                 :do (format *compiler-noise-stream* "ADDRESSER-FSM: New pass.~%")
                 :when (dequeue-logical-to-physical state)
                   :do (format *compiler-noise-stream* "ADDRESSER-FSM: LSCHED changed, retrying.~%")
                       (setf rewirings-tried nil)
                 :else
                   :do (format *compiler-noise-stream*
                               "ADDRESSER-FSM: LSCHED unchanged, selecting a permutation.~%")
                       (assert (> *addresser-max-swap-sequence-length* (length rewirings-tried)) ()
                               "Too many SWAP instructions selected in a row: ~a" (length rewirings-tried))
                       (setf rewirings-tried (select-and-embed-a-permutation state rewirings-tried)))))

        ;; build the logically parallelized schedule
        (append-instructions-to-lschedule lschedule instrs)
        ;; actually invoke the FSM
        (addresser-FSM)
        ;; TODO: Consider what happens when initial-l2p has a different logical coverage than final l2p
        ;; TODO: Don't always fully assign the l2p
        ;; XXX: consider how this intertwines with the temporal-addressing finish
        (fill-rewiring working-l2p)
        (setf initial-l2p (copy-rewiring working-l2p))
        (dolist (instr (nreverse (chip-schedule-to-straight-quil chip-sched)))
          (when (swap-application-p instr)
            (apply #'update-rewiring initial-l2p (mapcar #'qubit-index (application-arguments instr)))))
        (format *compiler-noise-stream* "DO-GREEDY-ADDRESSING: departure.~%")
        ;; finally, return what we've constructed
        (values chip-sched initial-l2p working-l2p)))))
