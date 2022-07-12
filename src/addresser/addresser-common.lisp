;;;; addresser-common.lisp
;;;;
;;;; Author: Eric Peterson, Corwin de Boor, Erik Davis

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
;;; See DO-GREEDY-ADDRESSING below for the main entry point.

(defvar *addresser-max-swap-sequence-length* 1000
  "Controls the maximum number of swaps that can occur in a row.")

(defvar *prefer-ranged-gates-to-swaps* nil
  "When T, use chains of instructions to simulate long-range gates rather than
SWAPping qubits into place.")

(defvar *addresser-start-with-partial-rewiring* t
  "When T, starts with a partial rewiring that is filled in gradually.")

;; TODO: consider making this part of the state
(defvar *addresser-use-free-swaps* nil
  "Does the addresser treat the initial rewiring as something that can be changed?")

(defvar *addresser-use-1q-queues* nil
  "A flag indicating whether the addresser should ignore unscheduled 1Q gates for the purposes of computing 2Q costs.")

(defvar *compute-tight-recombination-bound* nil
  "If T, use the compressor to try to precompute a tighter recombination bound.")

;;; The different search strategies implement methods for the following generics

(defgeneric select-swaps-for-rewiring (search-type rewiring target-rewiring addresser-state rewirings-tried)
  (:documentation "Determine links to swap in order to bring the current REWIRING closer to TARGET-REWIRING.

Returns a list of link indices, along with an updated list of rewirings tried."))

(defgeneric select-swaps-for-gates (search-type rewiring gates-in-waiting addresser-state rewirings-tried)
  (:documentation "Determine links to swap in order to schedule GATES-IN-WAITING with respect to the current REWIRING.

Returns a list of link indices, along with an updated list of rewirings tried."))

;;; Search routines are implemented in
;;;   - astar-rewiring-search.lisp (for A*)
;;;   - qubit-heuristic.lisp (for GREEDY-QUBIT)
;;;   - path-heuristic (for GREEDY-PATH)
(deftype addresser-search-type () '(member :a* :greedy-qubit :greedy-path))

(defvar *addresser-gates-swap-search-type* ':greedy-qubit
  "The type of swap search the addresser should use when selecting gates.")

(defvar *addresser-rewiring-swap-search-type* ':a*
  "The type of swap search the addresser should use when doing move-to-rewiring.")

;;; A pseudoinstruction class used to send directives to the addresser
(defclass application-force-rewiring (application)
  ((target-rewiring :initarg :target
                    :accessor application-force-rewiring-target
                    :documentation "The addresser will move the working rewiring to match this rewiring upon encountering this instruction."))
  (:documentation "A pseudoinstruction used to communicate desired rewiring state across addresser runs."))

;; initialize the other slots of the force-rewiring
(defmethod initialize-instance :after ((inst application-force-rewiring)
                                       &rest initargs)
  (declare (ignore initargs))
  (setf (application-operator inst)
        (named-operator "FORCE-REWIRING")
        (application-arguments inst)
        (loop :for i :across (rewiring-l2p (application-force-rewiring-target inst))
              :collect (qubit i))))

(defun application-other-argument (inst qubit)
  "Assumes that INST is a 2-qubit gate. Will get the index of the other qubit
(not equal to QUBIT) used by INST."
  (destructuring-bind (q0 q1) (mapcar #'qubit-index (application-arguments inst))
    (if (= q0 qubit) q1 q0)))

(defun select-swap-links (state rewirings-tried)
  "Determine a list of swap links to embed, given the addresser STATE and a list of REWIRINGS-TRIED.

Returns two values: a list of links, and an updated list of rewirings tried."
  (with-slots (working-l2p chip-sched chip-spec)
      state
    (format-noise "SELECT-SWAP-LINKS: entering SWAP selection phase.")
    (let ((gates-in-waiting (weighted-future-gates state)))
      (select-swaps-for-gates *addresser-gates-swap-search-type*
                              working-l2p
                              gates-in-waiting
                              state
                              rewirings-tried))))

(defun move-to-expected-rewiring (rewiring target-rewiring addresser-state
                                  &key
                                    (use-free-swaps nil)
                                    (rewirings-tried nil))
  "This function inserts the necessary SWAP instructions to move from the working logical-to-physical rewiring REWIRING to the TARGET-REWIRING."
  (with-slots (qq-distances chip-spec chip-sched initial-l2p) addresser-state
    (flet ((done-rewiring (rewiring)
             (zerop (rewiring-distance rewiring target-rewiring qq-distances)))
           (update-rewiring (rewiring)
             (loop
               :for logical :from 0
               :for physical :across (rewiring-l2p target-rewiring)
               :unless (apply-rewiring-l2p rewiring logical)
                 :do (rewiring-assign rewiring logical physical))))
      (loop :do (format-noise "MOVE-TO-EXPECTED-REWIRING: Moving~%~a~%~a" rewiring target-rewiring)
            :until (done-rewiring rewiring)
            :do (assert (> *addresser-max-swap-sequence-length* (length rewirings-tried)) ()
                        "Too many rewirings tried: ~a" (length rewirings-tried))
            :do (let ((links (select-swaps-for-rewiring
                              *addresser-rewiring-swap-search-type*
                              rewiring target-rewiring addresser-state rewirings-tried)))
                  (dolist (link-index links)
                    (embed-swap link-index
                                initial-l2p
                                rewiring
                                chip-spec
                                chip-sched
                                :use-free-swaps use-free-swaps)))
            :finally (update-rewiring rewiring)))))

(defun embed-swap (link-index initial-l2p working-l2p chip-spec chip-sched &key use-free-swaps)
  "Safely insert a SWAP selected by LINK-INDEX into CHIP-SCHED, accounting for the possibility of virtualization."
  ;; we now insert the SWAP selected by LINK-INDEX.
  (destructuring-bind (q0 q1) (coerce (chip-spec-qubits-on-link chip-spec link-index) 'list)
    ;; can we make it a virtual SWAP?
    (cond
     ((and use-free-swaps
           (zerop (chip-schedule-resource-end-time
                   chip-sched
                   (make-qubit-resource q0 q1))))
      ;; yes, we can. apply the link swap to initial-l2p and to working-l2p
      (update-rewiring initial-l2p q0 q1)
      (update-rewiring working-l2p q0 q1)
      (format-noise
       "EMBED-SWAP: This is a free swap. :)~%~
        EMBED-SWAP: New rewiring: ~a~%~
        EMBED-SWAP: New initial rewiring: ~a"
       working-l2p initial-l2p))
     (t
      ;; in this case, this swap has to be performed by the QPU.
      ;; apply the link swap to working-l2p
      (update-rewiring working-l2p q0 q1)
      (format-noise "EMBED-SWAP: New rewiring: ~a" working-l2p)
      ;; insert the relevant 2q instruction
      (chip-schedule-append chip-sched (build-gate "SWAP" '() q0 q1))))))

(defgeneric select-and-embed-a-permutation (state rewirings-tried)
  (:documentation
   "Select a permutation and schedule it for execution. The permutation is selected to lower the cost-function associated to the current lschedule.")
  (:method (state rewirings-tried)
    (multiple-value-bind (swap-links rewirings-tried)
        (select-swap-links state rewirings-tried)
      (dolist (link-index swap-links rewirings-tried)
        (embed-swap link-index
                    (addresser-state-initial-l2p state)
                    (addresser-state-working-l2p state)
                    (addresser-state-chip-specification state)
                    (addresser-state-chip-schedule state)
                    :use-free-swaps *addresser-use-free-swaps*)))))

(defun append-instr-to-chip-schedule (state instr)
  "Appends INSTR to the CHIP-SCHEDULE housed within STATE."
  ;; flush the queues before adding the 2Q instruction
  (when (and *addresser-use-1q-queues*
             (= 2 (length (application-arguments instr))))
    (let ((left-line (apply-rewiring-p2l (addresser-state-working-l2p state)
                                         (qubit-index (first (application-arguments instr)))))
          (right-line (apply-rewiring-p2l (addresser-state-working-l2p state)
                                          (qubit-index (second (application-arguments instr))))))
      ;; flush the 1Q gates down the line
      (flush-1q-instructions-after-wiring state left-line)
      (flush-1q-instructions-after-wiring state right-line)))

  (chip-schedule-append (addresser-state-chip-schedule state) instr))

(defun dequeue-classical-instruction (state instr &optional dry-run-escape)
  "Dispatch for dequeueing classical instructions. Returns a flag indicating whether we've dirtied up the schedule."
  (when dry-run-escape  ; every classical instruction can be handled
    (funcall dry-run-escape))

  (with-slots (lschedule chip-spec chip-sched working-l2p) state
    (when *addresser-use-1q-queues*
        ;; flush the 1Q lines in the relevant events
        (cond
          ;; is it maximally resourceful?
          ((global-instruction-p instr)
           ;; unload the 1Q queues
           (dotimes (qubit (chip-spec-n-qubits chip-spec))
             (flush-1q-instructions-after-wiring state qubit)))

          ;; is it a pure classical instruction?
          ((local-classical-instruction-p instr)
           ;; clear relevant 1Q queues
           (partially-flush-1Q-queues state (instruction-resources instr)))

          ;; is it a local mixed pure/classical instruction?
          ;;
          ;; TODO: this currently does not do the clever 'threading' that
          ;; happens with other 1Q instructions. it probably isn't worth it,
          ;; since MEASUREs are slow instructions.
          ;;
          ;; COUNTERARGUMENT: what's being "threaded" are SWAPs and MEASUREs /
          ;; RESETs. Doing a MEASURE before a SWAP (1) suffers 3 fewer CZs + (2)
          ;; has the potential (if, say, MEASURE happens on both participating
          ;; qubits) to rewrite the post-instruction SWAP as some Xes or to
          ;; elide it entirely -- ECP
          ((or (local-classical-quantum-instruction-p instr)
               (typep instr 'measure-discard)
               (typep instr 'reset-qubit))
           (let ((resources (instruction-resources instr)))
             ;; flush the 1Q queues
             (dotimes (qubit (chip-spec-n-qubits chip-spec))
               (when (resource-subsetp (make-qubit-resource qubit)
                                       resources)
                 (flush-1q-instructions-after-wiring state qubit)))))))

    (cond
        ;; is it resourceless?
        ((typep instr 'no-operation)
         ;; if so, discard it and continue.
         (lscheduler-dequeue-instruction lschedule instr))

        ;; is it maximally resourceful?
        ((global-instruction-p instr)
         ;; dequeue the instruction and set the dirty flag
         (chip-schedule-append chip-sched instr)
         (lscheduler-dequeue-instruction lschedule instr))

        ;; is it a pure classical instruction?
        ((local-classical-instruction-p instr)
         ;; dequeue the instruction and set the dirty flag
         (chip-schedule-append chip-sched instr)
         (lscheduler-dequeue-instruction lschedule instr))

        ;; is it a local mixed pure/classical instruction?
        ((or (local-classical-quantum-instruction-p instr)
             (typep instr 'measure-discard)
             (typep instr 'reset-qubit))
         ;; insert the instruction
         (handler-case (rewire-l2p-instruction working-l2p instr)
           (missing-rewiring-assignment ()
             (return-from dequeue-classical-instruction
               (values nil nil (list instr)))))
         (chip-schedule-append chip-sched instr)
         ;; dequeue the instruction and set the dirty flag
         (lscheduler-dequeue-instruction lschedule instr))

        ;; is it some other kind of PRAGMA not covered above?
        ((typep instr 'pragma)
         ;; just throw it away.
         (lscheduler-dequeue-instruction lschedule instr))

        ;; otherwise, we don't know what to do
        (t
         (error "The instruction type of \"~/cl-quil:instruction-fmt/\" is not supported by the addresser." instr)))
    ;; turns out we're always always dirty
    t))

(defun dequeue-gate-application (state instr &optional dry-run-escape)
  "Dequeues the given gate application INSTR, if possible.

Returns T if the schedule gets dirtied in the process, or NIL otherwise.

Two other values are returned: a list of fully rewired instructions for later scheduling, and a list of partially-rewired instructions for later scheduling."
  (with-slots (lschedule working-l2p initial-l2p chip-spec chip-sched)
      state
    (let ((dirty-flag nil)
          (ready-instrs nil)
          (partial-instrs nil))
      (cond
        ((and *addresser-use-1q-queues*
              (= 1 (length (application-arguments instr))))
         ;; quick error check on instruction qubits
         (assert (every (lambda (q) (< -1 (qubit-index q) (chip-spec-n-qubits
                                                           (addresser-state-chip-specification state))))
                        (application-arguments instr))
                 nil
                 "Instruction qubit indices are out of bounds for target QPU: ~/cl-quil:instruction-fmt/"
                 instr)
         (when dry-run-escape
           (funcall dry-run-escape))

         (add-instruction-to-1q-queue state instr)
         (setf dirty-flag t))
        ;; is it a rewiring pseudoinstruction?
        ((typep instr 'application-force-rewiring)
         (lscheduler-dequeue-instruction lschedule instr)
         (move-to-expected-rewiring working-l2p
                                    (application-force-rewiring-target instr)
                                    state
                                    :use-free-swaps *addresser-use-free-swaps*)
         (setf dirty-flag t))
        ;; is it a small-Q gate?
        ((<= (length (application-arguments instr))
             (length (chip-specification-objects chip-spec)))
           ;; quick error check on qubit indices
           (assert (every (lambda (q) (< -1 (qubit-index q) (chip-spec-n-qubits chip-spec)))
                          (application-arguments instr))
                   nil
                   "Instruction qubit indices are out of bounds for target QPU: ~/cl-quil:instruction-fmt/"
                   instr)
           (let* ((assignments (mapcar (lambda (q) (apply-rewiring-l2p working-l2p (qubit-index q)))
                                       (application-arguments instr)))
                  (all-assigned? (every #'identity assignments))
                  (hardware-index (and all-assigned?
                                       (nth-value 1 (lookup-hardware-address-by-qubits chip-spec assignments)))))
             (cond
               ;; the rewiring hasn't been filled out on these logical qubits yet.
               ;; store this instruction in a bin of to-be-wired possibilities.
               ((not all-assigned?)
                (push instr partial-instrs))
               ;; if we found a hardware object index, save this info for later
               ((and hardware-index
                     (not (hardware-object-dead-p (lookup-hardware-object-by-qubits chip-spec assignments))))
                (push (list hardware-index instr) ready-instrs))
               ;; if neither is true and we're not supposed to be SWAPping, we
               ;; should apply some localizing compilers instead.
               ((and (> 1 (length (application-arguments instr)))
                     *prefer-ranged-gates-to-swaps*)
                (when dry-run-escape
                  (funcall dry-run-escape))
                (let ((compilation-result (apply-translation-compilers instr chip-spec nil)))
                  (assert compilation-result ()
                          "Failed to apply localizing compilers.")
                  (setf dirty-flag t)
                  (lscheduler-replace-instruction lschedule instr compilation-result))))))
        ;; is it a many-Q gate?
        ((> (length (application-arguments instr))
            (length (chip-specification-objects chip-spec)))
         (when dry-run-escape
           (funcall dry-run-escape))
         ;; quick error check on instruction qubits
         (assert (every (lambda (q) (< -1 (qubit-index q) (chip-spec-n-qubits chip-spec)))
                        (application-arguments instr))
                 nil
                 "Instruction qubit indices are out of bounds for target QPU: ~/cl-quil:instruction-fmt/"
                 instr)
         (format-noise
          "DEQUEUE-GATE-APPLICATION: ~/cl-quil:instruction-fmt/ is a ~dQ>2Q instruction, compiling."
          instr
          (length (application-arguments instr)))
         ;; then we know we can't find a hardware object to support
         ;; it, so pass it to the chip compiler
         (let ((compilation-result (apply-translation-compilers instr chip-spec nil)))
           (setf dirty-flag t)
           (lscheduler-replace-instruction lschedule instr compilation-result)))
        ;; otherwise, we're helpless
        (t nil))
      (values dirty-flag ready-instrs partial-instrs))))

(defun dequeue-logical-to-physical (state &key (dry-run nil))
  "Offload instructions from the logical schedule onto the physical hardware, returning T if progress is made.

If DRY-RUN, this returns T as soon as it finds an instruction it can handle."
  ;; Allow for early exit if we are doing a dry run
  (catch 'dry-run-succeeded
    (with-slots (lschedule chip-sched chip-spec working-l2p initial-l2p) state
      (format-noise "DEQUEUE-LOGICAL-TO-PHYSICAL: entering dequeueing phase.")
      (let ((dirty-flag nil)
            (instrs-ready-for-scheduling nil)
            (instrs-partially-assigned nil)
            (dry-run-escape (if dry-run
                                (lambda () (throw 'dry-run-succeeded t))
                                nil)))

        ;; if the lschedule is empty, we're done
        (when (endp (lscheduler-first-instrs lschedule))
          (return-from dequeue-logical-to-physical nil))

        ;; otherwise, the lschedule is nonempty, so we try to dequeue instructions
        (dolist (instr (lscheduler-topmost-instructions lschedule))
          (multiple-value-bind (dirtied ready partial)
              (if (typep instr 'application)
                  (dequeue-gate-application state instr dry-run-escape)
                  (dequeue-classical-instruction state instr dry-run-escape))
            (when dirtied
              (setf dirty-flag t))
            (when ready
              (setf instrs-ready-for-scheduling
                    (append ready instrs-ready-for-scheduling)))
            (when partial
              (setf instrs-partially-assigned
                    (append partial instrs-partially-assigned)))))

        ;; thus ends our loop over topmost-instructions.
        ;; if we dirtied up the schedule, do it all again.
        (when dirty-flag
          (assert (not dry-run) () "Got dirty when the scheduler was supposed to be dry.")
          (return-from dequeue-logical-to-physical t))

        ;; if we didn't dirty up the schedule, see if we collected any
        ;; 2Q gates along the way
        (cond
          (instrs-ready-for-scheduling
           (dequeue-best-instr state
                               instrs-ready-for-scheduling
                               :dry-run dry-run))
          (instrs-partially-assigned
           (try-to-assign-qubits state instrs-partially-assigned))
          (t
           nil))))))

(defun state-with-additional-assignments (state logicals physicals)
  (let ((new-state (copy-instance state))
        (new-l2p (copy-rewiring (addresser-state-working-l2p state))))
    (loop :for l :in logicals
          :for p :in physicals
          :do (rewiring-assign new-l2p l p))
    (setf (addresser-state-working-l2p new-state) new-l2p)
    new-state))

(defun next-best-instruction (state instrs)
  "Find the instruction from the given INSTRS which has the best COST-FUNCTION valuation. Returns three values: the instruction, the index of the hardware object it lies on, and the optional qubit assignments if the selected instruction was only partially assigned."
  (with-slots (chip-spec chip-sched lschedule) state
    (let ((best-cost (build-worst-cost state))
          instr
          hardware-index)

      (loop
        :for (candidate-hardware-index candidate-instr) :in instrs
        :for physical-qubits := (coerce (vnth 0
                                              (hardware-object-cxns
                                               (chip-spec-hw-object chip-spec (1- (length (application-arguments
                                                                                           candidate-instr)))
                                                                    candidate-hardware-index)))
                                        'list)

        :for candidate-cost := (cost-function state :instr candidate-instr)

        :when (cost-< candidate-cost best-cost)
          :do (setf best-cost candidate-cost
                    instr candidate-instr
                    hardware-index candidate-hardware-index))
      (values instr hardware-index))))

(defun dequeue-best-instr (state instrs-ready-for-scheduling &key dry-run)
  "Dispatches pure instruction scheduling from a list of instructions that are ready to be scheduled."
  (with-slots (chip-sched chip-spec working-l2p lschedule) state
    (multiple-value-bind (instr link-line)
        (next-best-instruction state instrs-ready-for-scheduling)
      (declare (ignore link-line))

      ;; if we didn't find any instructions, then return unsuccessful
      (unless instr (return-from dequeue-best-instr nil))

      ;; from now on, we would schedule something, so bail if on a dry run
      (when dry-run (return-from dequeue-best-instr t))

      ;; threads always need expansion
      (when (typep instr 'application-thread-invocation)
        (lscheduler-replace-instruction lschedule instr (apply-translation-compilers
                                                         instr
                                                         chip-spec
                                                         (lookup-hardware-object chip-spec instr)))
        (return-from dequeue-best-instr t))

      ;; ... and dispatch it.
      (format-noise
       "DEQUEUE-BEST-INSTR: Elected to schedule ~/cl-quil:instruction-fmt/."
       instr)
      (let ((rewired-instr (copy-instance instr)))
        (rewire-l2p-instruction working-l2p rewired-instr)
        (format-noise
         "DEQUEUE-BEST-INSTR: ~/cl-quil:instruction-fmt/ is ~/cl-quil:instruction-fmt/ in the current rewiring."
         instr rewired-instr)

        ;; Figure out if we need to compile the instruction,
        ;; or if we can add it to the schedule.
        (cond
          ;; if we found a link and the instruction is native...
          ((hardware-object-native-instruction-p (lookup-hardware-object chip-spec rewired-instr) rewired-instr)
           (format-noise
            "DEQUEUE-BEST-INSTR: ~/cl-quil:instruction-fmt/ is native in l2p rewiring ~A, flushing 1Q lines and dequeueing."
            instr
            (rewiring-l2p working-l2p))
           ;; dequeue the instruction so we can push the
           ;; modified instruction onto the schedule.
           (lscheduler-dequeue-instruction lschedule instr)
           ;; and stack the 2Q gate on top
           (append-instr-to-chip-schedule state rewired-instr))

          ;; otherwise, we found a link but the instruction is not native
          (t
           (format-noise
            "DEQUEUE-BEST-INSTR: ~/cl-quil:instruction-fmt/ is non-native in the current rewiring, compiling."
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
                                (lookup-hardware-object chip-spec rewired-instr))))
             (lscheduler-replace-instruction lschedule instr compiled-seq)))))
      t)))

(defun best-qubit-position (state gates-in-waiting logical)
  "Finds the best location for an unassigned logical under the given future schedule."
  (with-slots (working-l2p chip-spec) state
    (assert (not (apply-rewiring-l2p working-l2p logical)) (logical)
            "Qubit ~a already assigned" logical)
    (let ((best-cost (build-worst-cost state))
          (best-physical nil))
      (dolist (physical (find-physical-component-in-state state logical))
        (unless (or (apply-rewiring-p2l working-l2p physical)
                    (chip-spec-qubit-dead? chip-spec physical))
          (let ((cost (with-rewiring-assign working-l2p logical physical
                        (cost-function state :gate-weights gates-in-waiting))))
            (when (cost-< cost best-cost)
              (setf best-cost cost
                    best-physical physical)))))
      (values best-physical best-cost))))

(defun unassigned-qubits (instrs rewiring)
  "Get all qubits referenced in INSTRS which are not assigned in REWIRING."
  (let ((unassigned-qubits nil))
    (dolist (instr instrs unassigned-qubits)
      (dolist (qubit (cl-quil/resource::resource-qubits-list
                      (instruction-resources instr)))
        (unless (apply-rewiring-l2p rewiring qubit)
          (pushnew qubit unassigned-qubits))))))

(defun try-to-assign-qubits (state instrs)
  "Attempt to assign a logical qubit from INSTRS to a physical qubit, as managed by the addresser state STATE."
  (with-slots (working-l2p) state
    (let ((unassigned-qubits (unassigned-qubits instrs working-l2p))
          (gate-weights (weighted-future-gates state)))
      (flet ((placement-data (logical-qubit)
               "Given a LOGICAL-QUBIT, determine an assigned physical qubit, and the associated cost. Return a list of all three."
               (multiple-value-bind (physical-qubit cost)
                   (best-qubit-position state gate-weights logical-qubit)
                 (list logical-qubit physical-qubit cost))))
        (cond ((endp unassigned-qubits)
               nil)
              (t
               ;; maximize over best-qubit-position
               (destructuring-bind (best-logical best-physical best-cost)
                   (a:extremum (mapcar #'placement-data unassigned-qubits)
                               (complement #'cost-<)
                               :key #'third)
                 (declare (ignore best-cost))
                 (rewiring-assign working-l2p best-logical best-physical))
               t))))))

;; todo: eventually we want to modify parts of this to incorporate multi-qubit
;;       hardware objects. a lot of this is already correctly set up for that
;;       eventuality. the main thing that needs to change is that 1Q- and 2Q-
;;       operations should be handled identically. for instance, we should add
;;       queues for the 2Q operations, and they should also be written out by a
;;       flushing call.
(defgeneric do-greedy-addressing (state ; initial addresser state
                                  instrs ; list of instructions to schedule
                                  &key
                                    use-free-swaps ; treat the initial rewiring as virtual
                                    )
  (:documentation
   "Schedules INSTRS for execution on a QPU specified by CHIP-SPEC. Returns a
values triple (initial-rewiring chip-schedule final-rewiring).

Optional arguments:
 + INITIAL-REWIRING launches with the addresser with a nontrivial qubit
   permutation.
 + USE-FREE-SWAPS treats the initial rewiring as virtual (able to be changed).
   If INITIAL-REWIRING is not provided this option has no effect.
")
  (:method (state instrs &key (initial-rewiring nil) (use-free-swaps nil))
    (format-noise "DO-GREEDY-ADDRESSING: entrance.")
    (with-slots (chip-spec lschedule working-l2p chip-sched initial-l2p) state
      (let ((*addresser-use-free-swaps* (or use-free-swaps initial-l2p)))
        ;; This is governed by an FSM of the shape
        ;;
        ;; [ do-greedy-addressing ]
        ;; [ initialization       ]
        ;;    |
        ;;    v
        ;; [ dequeue-logical-to-physical    and     ]
        ;; [ dequeue-best-instr                     ]
        ;; [ inserts available logical instructions ]
        ;;   |        ^         ^
        ;;   |        |         |
        ;;   |        |        yes
        ;;   v        |         |
        ;; < did we dequeue any instructions? >
        ;;   |        |
        ;;   no       |
        ;;   |        |
        ;;   v        |
        ;; < is the logical schedule empty? > --yes-> [ do-greedy-addressing ]
        ;;   |        |                               [ flush / cleanup      ]
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
                   :do (format-noise "ADDRESSER-FSM: New pass.")
                   :when (dequeue-logical-to-physical state)
                     :do (format-noise "ADDRESSER-FSM: LSCHED changed, retrying.")
                         (setf rewirings-tried nil)
                   :else
                     :do (format-noise "ADDRESSER-FSM: LSCHED unchanged, selecting a permutation.")
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
          (format-noise "DO-GREEDY-ADDRESSING: departure.")
          ;; get rid of any floaters
          (when *addresser-use-1q-queues*
            (dotimes (qubit (chip-spec-n-qubits chip-spec))
              (unless (endp (aref (addresser-state-1q-queues state) qubit))
                (flush-1q-instructions-after-wiring state qubit))))
          ;; finally, return what we've constructed
          (values chip-sched initial-l2p working-l2p))))))
