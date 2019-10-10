;;;; temporal-addresser.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

;;; This file contains the temporal addresser, which solves the problem
;;; of mapping a sequence of Quil instructions acting on logical qubits
;;; to a scheduled sequence of Quil instructions acting on physical qubits.
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

(defparameter *addresser-swap-search-type* :greedy-qubit
  "The type of swap search the addresser should use.

GREEDY-PATH: Assign links values based on whether they are on the shortest path
between two qubits that need to be adjacent
A*: Use A* search algorithm using the cost function as a heuristic
GREEDY-QUBIT: Greedily choose the best link to swap according to the cost function.")

(defparameter *addresser-move-to-rewiring-swap-search-type* :a*
  "The type of swap search the addresser should use when doing move-to-rewiring. ")

(defparameter *addresser-use-2q-tiers* t
  "When T, uses the 2-qubit tiers rather than the general instruction tiers.")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Chip Schedule Management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-ending-1q-line (chip-sched qubit)
  "Find the ending location of QUBIT, following the swaps in CHIP-SCHED."
  (loop
    :for inst := (chip-schedule-last-meeting chip-sched (make-qubit-resource qubit))
      :then (chip-schedule-last-meeting chip-sched (make-qubit-resource qubit) :before-inst inst)
    :unless inst
      :return qubit
    :when (swap-application-p inst)
      :do (setf qubit (application-other-argument inst qubit))))

(defun find-blocking-instruction-on-1q-line (chip-sched qubit &optional (extra-resources (make-null-resource)))
  "Find the latest non-swap instruction making using of QUBIT within CHIP-SCHED.

Return the new qubit line, the found non-swap (or NIL), and the swap immediately after it (or NIL)."
  (loop
    :with prev-inst := nil
    :for inst := (chip-schedule-last-meeting chip-sched (resource-union (make-qubit-resource qubit)
                                                                        extra-resources))
      :then (chip-schedule-last-meeting chip-sched (resource-union (make-qubit-resource qubit)
                                                                   extra-resources) :before-inst inst)
    :unless (and inst (swap-application-p inst))
      :return (values qubit inst prev-inst)
    :do (setf qubit (application-other-argument inst qubit)
              prev-inst inst)))

(defun flush-1q-instructions-forward (chip-sched qubit insts lo-inst hi-inst)
  "Flush the 1Q instructions INSTS on the qubit line QUBIT into the CHIP-SCHED
starting from LO-INST.

If LO-INST is NIL, then no non-swap instructions should have touched qubit
QUBIT. If LO-INST is non-NIL, then every instruction that is not an
pre-ancestor of LO-INST should either not touch QUBIT or be a swap
instruction.

HI-INST must be a swap instruction including QUBIT that succeeds LO-INST (the
first swap-instruction if LO-INST is NIL), or NIL if no such swap exists.
HI-INST would ordinarily be a successor of LO-INST, but they should have been
preemptively disconnected."
  (when (endp insts)
    (return-from flush-1q-instructions-forward))
  (let* ((time (if lo-inst (chip-schedule-end-time chip-sched lo-inst) 0))
         (lschedule (chip-schedule-data chip-sched))
         (inst (first insts))
         (duration (instruction-duration inst (chip-schedule-spec chip-sched) nil)))
    (cond
      ((not duration)
       ;; the instruction is non-native, so compile it
       ;; NOTE: This might produce a sequence of instructions that is not
       ;; actually all executable on this line if we run out of time before the
       ;; next swap. Then, we will move the extra gates onto another qubit line,
       ;; where they aren't native. We'd need to re-apply translation compilers
       ;; there.
       (setf insts (append (apply-translation-compilers
                            inst
                            (chip-schedule-spec chip-sched)
                            (chip-spec-nth-qubit (chip-schedule-spec chip-sched) qubit))
                           (rest insts)))
       (flush-1q-instructions-forward chip-sched qubit insts lo-inst hi-inst))
      ((and hi-inst (> (+ time duration) (chip-schedule-start-time chip-sched hi-inst)))
       ;; instruction doesn't fit between lo-inst and hi-inst
       ;; prepare for next recursive call
       (setf qubit (application-other-argument hi-inst qubit)
             lo-inst hi-inst
             hi-inst (loop
                       :with earliest-inst := nil
                       :with resource := (make-qubit-resource qubit)
                       :for inst :in (gethash hi-inst (lscheduler-later-instrs lschedule))
                       :when (and (resources-intersect-p resource (instruction-resources inst))
                                  (or (not earliest-inst)
                                      (> (chip-schedule-start-time chip-sched earliest-inst)
                                         (chip-schedule-start-time chip-sched inst))))
                         :do (setf earliest-inst inst)
                       :finally (return earliest-inst)))
       (flush-1q-instructions-forward chip-sched qubit insts lo-inst hi-inst))
      (t
       ;; instruction fits between lo-inst and hi-inst
       ;; rewire inst to use the current qubit
       (setf (application-arguments inst) (list (qubit qubit)))
       ;; add inst into the schedule
       (setf (chip-schedule-start-time chip-sched inst) time)
       (lschedule-splice-1q-instruction lschedule lo-inst inst hi-inst)
       ;; note we don't need to separate because inst not connected to hi-inst
       (flush-1q-instructions-forward chip-sched qubit (rest insts) inst hi-inst)))))

(defun flush-1q-instructions (chip-sched qubit insts)
  "Append INSTS (all 1Q instructions) to CHIP-SCHED onto the qubit QUBIT,
following swaps."
  (unless (endp insts)
    (let ((extra-resources (make-null-resource))
          (logical-qubit-resource (make-qubit-resource (qubit-index
                                                        (first (application-arguments (first insts)))))))
      (dolist (instr insts)
        (setf extra-resources (resource-union extra-resources (instruction-resources instr))))
      ;; remove the logical qubit resource
      (setf extra-resources (resource-difference extra-resources
                                                 logical-qubit-resource))
      (multiple-value-bind (qubit lo-inst hi-inst)
          (find-blocking-instruction-on-1q-line chip-sched qubit extra-resources)
        (flush-1q-instructions-forward chip-sched qubit insts lo-inst hi-inst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Swap Selection Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ... all at the service of SELECT-AND-EMBED-A-PERMUTATION below

(defun rewiring-distance (rewiring target-rewiring qq-distances)
  "A measure of the distance between a given REWIRING and a TARGET-REWIRING, based on
the qubit-qubit distance array QQ-DISTANCES."
  (loop :for i :across (rewiring-l2p rewiring)
        :for j :across (rewiring-l2p target-rewiring)
        :when i
          :sum (aref qq-distances i j)))

(defun cost-lowering-candidates (rewiring cost-function rewirings-tried chip-spec depth)
  "Given a rewiring and a cost function, returns a list of swap links for which
the cost of the rewiring is reduced."
  (let ((best-cost-so-far most-positive-fixnum)
        potential-first-links)
    (labels ((depth-first-traversal (depth topmost-link)
               (when (plusp depth)
                 (let ((links-to-search
                         (if topmost-link
                             (chip-spec-adj-links chip-spec topmost-link)
                             (a:iota (chip-spec-n-links chip-spec)))))
                   (dolist (link-index links-to-search)
                     (let ((topmost-link (or topmost-link link-index))
                           (swapped-qubits (chip-spec-qubits-on-link chip-spec link-index)))
                       (with-update-rewiring rewiring (aref swapped-qubits 0) (aref swapped-qubits 1)
                         ;; make sure we haven't been here before
                         (unless (member rewiring rewirings-tried :test #'equalp)
                           ;; compute the cost for this rewiring
                           (let ((new-cost (funcall cost-function rewiring)))
                             (cond
                               ((double= new-cost best-cost-so-far)
                                (push topmost-link potential-first-links))
                               ((< new-cost best-cost-so-far)
                                (setf best-cost-so-far new-cost)
                                (setf potential-first-links (list topmost-link)))
                               (t nil)))
                           ;; recurse on SWAP chains of one lower length
                           (depth-first-traversal (1- depth) topmost-link)))))))))
      (depth-first-traversal depth nil)
      (assert potential-first-links)
      potential-first-links)))

(defun swap-horizon (link-index chip-sched chip-spec &optional use-free-swaps)
  "Estimate the time horizon associated with applying the 'SWAP' instruction
indicated by LINK-INDEX, relative to the given schedule CHIP-SCHED."
  (destructuring-bind (q0 q1) (coerce (chip-spec-qubits-on-link chip-spec link-index) 'list)
    (let* ((swap-duration (permutation-record-duration
                           (vnth 0 (hardware-object-permutation-gates
                                    (chip-spec-nth-link chip-spec link-index)))))
           ;; What follows depends on whether we know that 2Q programs have a bounded length
           (time-bound (gethash "time-bound"
                                (hardware-object-misc-data
                                 (chip-spec-nth-link chip-spec link-index))))
           (horizon-time-fn (if time-bound
                                #'chip-schedule-resource-carving-point
                                #'chip-schedule-resource-end-time))
           (horizon-increment (or time-bound swap-duration))
           (old-horizon (funcall horizon-time-fn
                                 chip-sched
                                 (make-qubit-resource q0 q1))))
      (cond
        ((not (zerop old-horizon))
         (+ old-horizon horizon-increment))
        (use-free-swaps
         0)
        (t
         horizon-increment)))))

(defun select-cost-lowering-swap (rewiring chip-spec chip-sched use-free-swaps cost-function rewirings-tried
                                  &optional
                                    (depth *addresser-swap-lookahead-depth*))
  "Seaches for a 'SWAP' instruction that lowers the objective COST-FUNCTION. Returns such an
instruction if it exists, and errors otherwise."
  (format *compiler-noise-stream* "SELECT-COST-LOWERING-SWAP: Entrance.~%")
  (let* ((current-cost (funcall cost-function rewiring))
         (shortest-horizon most-positive-fixnum)
         (best-cost-so-far most-positive-fixnum)
         (link-index nil)
         (potential-first-links (cost-lowering-candidates rewiring
                                                          cost-function
                                                          rewirings-tried
                                                          chip-spec
                                                          depth)))
    (dolist (index potential-first-links)
      ;; TODO: this assumes only SWAPs exist in the permutation list
      (let ((new-horizon (swap-horizon index chip-sched chip-spec use-free-swaps)))
        (format *compiler-noise-stream* "SELECT-COST-LOWERING-SWAP: Considering ~d: this ~,3f vs best ~,3f.~%"
                (chip-spec-qubits-on-link chip-spec index)
                new-horizon
                shortest-horizon)
        ;; what does swapping this link do to our time horizon? if it
        ;; pushes it out, we do *nothing*, but if it's within our
        ;; horizon then we look at it.
        (when (<= new-horizon shortest-horizon)
          (let ((swapped-qubits (chip-spec-qubits-on-link chip-spec index)))
            (with-update-rewiring rewiring (aref swapped-qubits 0) (aref swapped-qubits 1)
              ;; compute the new cost value
              (let ((new-cost (funcall cost-function rewiring)))
                (format *compiler-noise-stream* "SELECT-COST-LOWERING-SWAP: New cost ~a vs best cost ~a vs current cost ~a.~%"
                        new-cost best-cost-so-far current-cost)
                ;; if it's lower than the best cost we've seen OR if it's the shortest acting swap and it's at least better than nothing...
                (when (or (and (< new-cost best-cost-so-far) (not (zerop new-horizon)))
                          (and (< (+ new-cost +double-comparison-threshold-loose+) current-cost) (< new-horizon shortest-horizon)))
                  ;; ... then we prefer this link to all others.
                  (format *compiler-noise-stream* "SELECT-COST-LOWERING-SWAP: We prefer this SWAP.~%")
                  (setf best-cost-so-far new-cost)
                  (setf link-index index)
                  (setf shortest-horizon new-horizon))))))))
    ;; if we have a nil swap, the greedy scheduler has failed to operate. scary!
    (assert link-index
            nil
            "Failed to select a SWAP instruction. This can be caused by a disconnected qubit graph, a program with a lot of symmetry, or even random chance. You might simply try again, or you might try requesting a different addressing strategy.")
    (format *compiler-noise-stream*
            "SELECT-COST-LOWERING-SWAP: SWAP ~d ~d is best, lowering cost from ~d to ~d.~%"
            (vnth 0 (chip-spec-qubits-on-link chip-spec link-index))
            (vnth 1 (chip-spec-qubits-on-link chip-spec link-index))
            current-cost
            best-cost-so-far)
    link-index))

(defun move-to-expected-rewiring (rewiring target-rewiring qq-distances chip-spec chip-sched initial-l2p use-free-swaps
                                  &optional
                                    (rewirings-tried nil))
  "This function inserts the necessary SWAP instructions to move from the working logical-to-physical
rewiring REWIRING to the TARGET-REWIRING."
  (format *compiler-noise-stream* "MOVE-TO-EXPECTED-REWIRING: Moving~%~a~%~a~%"
          rewiring target-rewiring)
  (flet ((cost-function (rewiring)
           (rewiring-distance rewiring target-rewiring qq-distances))
         (done-moving (rewiring)
           (zerop (rewiring-distance rewiring target-rewiring qq-distances))))
    ;; if we're already properly rewired, stop.
    (when (done-moving rewiring)
      (loop
        :for logical :from 0
        :for physical :across (rewiring-l2p target-rewiring)
        :unless (apply-rewiring-l2p rewiring logical)
          :do (rewiring-assign rewiring logical physical))
      (return-from move-to-expected-rewiring))
    (assert (> *addresser-max-swap-sequence-length* (length rewirings-tried)) ()
            "Too many rewirings tried: ~a" (length rewirings-tried))
    ;; otherwise, pick a SWAP
    (flet ((embed (link-index)
             (embed-swap link-index
                         initial-l2p
                         rewiring
                         chip-spec
                         chip-sched
                         :use-free-swaps nil)))
      (ecase *addresser-move-to-rewiring-swap-search-type*
        (:greedy-path
         (push (copy-rewiring rewiring) rewirings-tried)
         (embed (select-swap-path-target chip-spec qq-distances target-rewiring
                                         rewirings-tried rewiring)))
        (:greedy-qubit
         (push (copy-rewiring rewiring) rewirings-tried)
         (embed (select-cost-lowering-swap rewiring chip-spec chip-sched use-free-swaps
                                           #'cost-function
                                           rewirings-tried)))
        (:a*
         (dolist (link-index (search-rewiring chip-spec rewiring
                                              (chip-schedule-qubit-times chip-sched)
                                              #'cost-function
                                              #'done-moving
                                              :max-iterations *addresser-a*-swap-search-max-iterations*))
           (embed link-index)))))
    ;; and try again
    (move-to-expected-rewiring rewiring target-rewiring qq-distances chip-spec chip-sched initial-l2p use-free-swaps rewirings-tried)))

(defun embed-swap (link-index initial-l2p working-l2p chip-spec chip-sched &key use-free-swaps)
  "Insert a SWAP selected by LINK-INDEX into CHIP-SCHED."
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
      (format *compiler-noise-stream*
              "EMBED-SWAP: This is a free swap. :)~%~
               EMBED-SWAP: New rewiring: ~a~%~
               EMBED-SWAP: New initial rewiring: ~a~%"
              working-l2p initial-l2p))
     (t
      ;; in this case, this swap has to be performed by the QPU.
      ;; apply the link swap to working-l2p
      (update-rewiring working-l2p q0 q1)
      (format *compiler-noise-stream*
              "EMBED-SWAP: New rewiring: ~a~%"
              working-l2p)
      ;; insert the relevant 2q instruction
      ;;
      ;; NB! No QUOTE around q0 and q1.
      (chip-schedule-append chip-sched (build-gate "SWAP" '() q0 q1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Addresser State Updates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The main entry point for temporal addressing is DO-GREEDY-TEMPORAL-ADDRESSING
;;; below. However, this manages a fair amount of state as it navigates the
;;; addressing process. The struct below bundles this together for the sake of
;;; convenience. At any given point of the execution there is only one of these
;;; objects, with values being mutated along the way.

(defstruct temporal-addresser
  initial-l2p         ; the initial logical-to-physical rewiring
  working-l2p         ; the working logical-to-physical rewiring. this gets mutated, a lot
  qq-distances        ; precomputed SWAP distances between separated qubits
  qubit-cc            ; the connected component where newly-assigned qubits will live
  1q-queues           ; a bunch of 1Q instruction queues, INDEXED BY LOGICAL ADDRESS
  lschedule           ; logical schedule
  chip-sched          ; a physical schedule
  chip-spec           ; chip specification
  )

(defparameter *temporal-addresser-use-free-swaps* nil
  "Does the addresser treat the initial rewiring as something that can be changed?")

;; This should have limited extent, e.g. within a call to the addresser
(defvar *temporal-addresser-state-variables*)
(setf (documentation '*temporal-addresser-state-variables* 'variable)
      "The active TEMPORAL-ADDRESSER object, manipulated by DO-GREEDY-TEMPORAL-ADDRESSING and friends.")

(defun select-and-embed-a-permutation (rewirings-tried)
  "Select a permutation and schedule it for execution. The permutation is selected to lower the
cost-function associated to the current lschedule."
  (with-slots (lschedule initial-l2p working-l2p chip-sched chip-spec qq-distances)
      *temporal-addresser-state-variables*
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
                      (dequeue-logical-to-physical :dry-run t)
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
                         :use-free-swaps *temporal-addresser-use-free-swaps*))))
        (:greedy-qubit
         (flet ((cost-function (rewiring)
                  (cost-function qq-distances rewiring gates-in-waiting)))
           (push (copy-rewiring working-l2p) rewirings-tried)
           (embed-swap (select-cost-lowering-swap working-l2p chip-spec chip-sched *temporal-addresser-use-free-swaps*
                                                  #'cost-function
                                                  rewirings-tried)
                       initial-l2p
                       working-l2p
                       chip-spec
                       chip-sched
                       :use-free-swaps *temporal-addresser-use-free-swaps*)
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
                       :use-free-swaps *temporal-addresser-use-free-swaps*))
         rewirings-tried)))))

(defun flush-1q-instructions-after-wiring (qubit)
  "Flush the 1Q queue for QUBIT after potentially assigning it to a physical location."
  (with-slots (working-l2p chip-sched qubit-cc 1q-queues) *temporal-addresser-state-variables* 
    (let ((physical (apply-rewiring-l2p working-l2p qubit)))
      (unless physical
        ;; TODO: Should we try to pick the best one?
        (setf physical (loop
                         :for physical :in qubit-cc
                         :unless (apply-rewiring-p2l working-l2p physical)
                           :return physical))
        (rewiring-assign working-l2p qubit physical))
      (flush-1q-instructions chip-sched physical (nreverse (aref 1q-queues qubit)))
      (setf (aref 1q-queues qubit) nil))))

(defun partially-flush-1Q-queues (resources)
  "Flush any part of any 1Q queue that touches a given set of non-quantum RESOURCES."
  (with-slots (chip-spec 1q-queues) *temporal-addresser-state-variables* 
    ;; for each qubit line
    (dotimes (qubit (chip-spec-n-qubits chip-spec))
      (loop
        ;; walk the associated 1Q queue, latest to earliest
        :for instr :in (aref 1q-queues qubit)
        :for second-segment :on (aref 1q-queues qubit) :by #'cdr
        ;; check this instruction for intersection with RESOURCES
        :when (resources-intersect-p resources (instruction-resources instr))
          ;; if it intersects, segment the queue into
          ;;     [latest ... prev this instr] [this instr ... earliest]
          ;; call flush on the second segment, and store the first back into the 1Q queue
          :do
             (setf (aref 1q-queues qubit) second-segment)
             (flush-1q-instructions-after-wiring qubit)
             (setf (aref 1q-queues qubit) first-segment)
             (return)
             ;; if we didn't intersect, move the pointer on the first segment
        :collect instr :into first-segment))))

(defun dequeue-classical-instruction (instr &optional dry-run-escape)
  "Dispatch for dequeueing classical instructions. Returns a flag indicating whether
we've dirtied up the schedule."
  (when dry-run-escape                         ; every classical instruction can be handled
    (funcall dry-run-escape))
  (with-slots (lschedule chip-spec chip-sched working-l2p) *temporal-addresser-state-variables*
      (let (dirty-flag)
        (cond
          ;; is it resourceless?
          ((typep instr 'no-operation)
           ;; if so, discard it and continue.
           (lscheduler-dequeue-instruction lschedule instr)
           (setf dirty-flag t))

          ;; is it maximally resourceful?
          ((global-instruction-p instr)
           ;; unload the 1Q queues, dequeue the instruction,
           ;; and set the dirty flag
           (dotimes (qubit (chip-spec-n-qubits chip-spec))
             (flush-1q-instructions-after-wiring qubit))
           (chip-schedule-append chip-sched instr)
           (lscheduler-dequeue-instruction lschedule instr)
           (setf dirty-flag t))

          ;; is it a pure classical instruction?
          ((local-classical-instruction-p instr)
           ;; clear relevant 1Q queues, dequeue the instruction
           ;; and set the dirty flag
           (partially-flush-1Q-queues (instruction-resources instr))
           (chip-schedule-append chip-sched instr)
           (lscheduler-dequeue-instruction lschedule instr)
           (setf dirty-flag t))

          ;; is it a local mixed pure/classical instruction?
          ;;
          ;; TODO: this currently does not do the clever 'threading'
          ;; that happens with other 1Q instructions. it probably isn't
          ;; worth it, since MEASUREs are slow instructions.
          ((or (local-classical-quantum-instruction-p instr)
               (typep instr 'measure-discard)
               (typep instr 'reset-qubit))
           (let ((resources (instruction-resources instr)))
             ;; flush the 1Q queues
             (dotimes (qubit (chip-spec-n-qubits chip-spec))
               (when (resource-subsetp (make-qubit-resource qubit)
                                       resources)
                 (flush-1q-instructions-after-wiring qubit)))
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

        dirty-flag)))

(defun dequeue-gate-application (instr &optional dry-run-escape)
  "Dequeues the given gate application INSTR, if possible. Returns T if the
schedule gets dirtied in the process, or NIL otherwise. Two other values are
returned: a list of fully rewired 2Q instructions for later scheduling, and a
list of partially-rewired 2Q instructions for later scheduling."
  (with-slots (lschedule working-l2p initial-l2p chip-spec chip-sched qq-distances 1q-queues)
      *temporal-addresser-state-variables*
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
          *temporal-addresser-use-free-swaps*)
         (setf dirty-flag t))

        ;; is it a 2Q gate?
        ((= 2 (length (application-arguments instr)))
         ;; quick error check on qubit indices
         (assert (every (lambda (q) (< -1 (qubit-index q) (length 1q-queues)))
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
         (assert (every (lambda (q) (< -1 (qubit-index q) (length 1q-queues)))
                        (application-arguments instr))
                 nil
                 "Instruction qubit indices are out of bounds for target QPU: ~/quil:instruction-fmt/"
                 instr)
         (when dry-run-escape
           (funcall dry-run-escape))
         (format *compiler-noise-stream*
                 "DEQUEUE-GATE-APPLICATION: ~/quil:instruction-fmt/ is a 1Q construction, adding to logical queue.~%"
                 instr)
         ;; push it onto the approprite logical 1Q queue
         (push instr (aref 1q-queues (qubit-index (first (application-arguments instr)))))
         ;; dequeue and set the dirty bit
         (lscheduler-dequeue-instruction lschedule instr)
         (setf dirty-flag t))

        ;; is it a many-Q gate?
        ((> (length (application-arguments instr)) (length (chip-specification-objects chip-spec)))
         (when dry-run-escape
           (funcall dry-run-escape))
         ;; quick error check on instruction qubits
         (assert (every (lambda (q) (< -1 (qubit-index q) (length 1q-queues)))
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

      (values dirty-flag ready-2q-instrs partial-2q-instrs))))

(defun dequeue-logical-to-physical (&key (dry-run nil))
  "Offload instructions from the logical schedule onto the physical hardware, returning T if progress is made.

If DRY-RUN, this returns T as soon as it finds an instruction it can handle."
  ;; Allow for early exit if we are doing a dry run
  (catch 'dry-run-succeeded
    (with-slots (lschedule chip-sched chip-spec working-l2p 1q-queues qq-distances initial-l2p)
        *temporal-addresser-state-variables*
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
                  (dequeue-gate-application instr dry-run-escape)
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
        (when (dequeue-soonest-2q-from-list 2q-instrs-ready-for-scheduling
                                            2q-instrs-partially-assigned
                                            :dry-run dry-run)
          (return-from dequeue-logical-to-physical t))

        nil))))

(defun soonest-2q-instruction (2q-instrs partially-assigned-2q-instrs lschedule chip-sched chip-spec)
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
        :for ((q0 q1) (pos0 pos1)) := (assign-gate candidate-instr gates-in-waiting)
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

(defun dequeue-soonest-2q-from-list (2q-instrs-ready-for-scheduling 2q-instrs-partially-assigned &key dry-run)
  "Dispatches pure 2Q instruction scheduling from a list of 2Q instructions that
are ready to be scheduled."
  (with-slots (chip-sched chip-spec working-l2p lschedule) *temporal-addresser-state-variables*
    (multiple-value-bind (instr link-line qubit-assignments)
        (soonest-2q-instruction 2q-instrs-ready-for-scheduling
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
                        "DEQUEUE-SOONEST-2Q-FROM-LIST: Couldn't rewire ~/quil:instruction-fmt/ because assignment is missing"
                        instr)
                (loop
                  :for (logical physical) :in qubit-assignments
                  :unless (apply-rewiring-l2p working-l2p logical)
                    :do (format *compiler-noise-stream*
                                "DEQUEUE-SOONEST-2Q-FROM-LIST: assigning logical qubit ~a to physical qubit ~a~%"
                                logical physical)
                        (rewiring-assign working-l2p logical physical))
                (return-from dequeue-soonest-2q-from-list t))))

          (format *compiler-noise-stream*
                  "DEQUEUE-SOONEST-2Q-FROM-LIST: ~/quil:instruction-fmt/ is ~/quil:instruction-fmt/ in the current rewiring~%"
                  instr rewired-instr)

          ;; Figure out if we need to compile the instruction,
          ;; or if we can add it to the schedule.
          (cond
            ;; if we found a link and the instruction is native...
            ((hardware-object-native-instruction-p (chip-spec-nth-link chip-spec link-line)
                                                   rewired-instr)
             (format *compiler-noise-stream*
                     "DEQUEUE-SOONEST-2Q-FROM-LIST: ~/quil:instruction-fmt/ is native in l2p rewiring ~A, flushing 1Q lines and dequeueing.~%"
                     instr
                     (rewiring-l2p working-l2p))

             (destructuring-bind (left-line right-line)
                 (mapcar #'qubit-index (application-arguments instr))
               ;; dequeue the instruction so we can push the
               ;; modified instruction onto the schedule.
               (lscheduler-dequeue-instruction lschedule instr)
               ;; flush the 1Q gates down the line
               (flush-1q-instructions-after-wiring left-line)
               (flush-1q-instructions-after-wiring right-line)
               ;; and stack the 2Q gate on top
               (chip-schedule-append chip-sched rewired-instr)))

            ;; otherwise, we found a link but the instruction is not native
            (t
             (format *compiler-noise-stream*
                     "DEQUEUE-SOONEST-2Q-FROM-LIST: ~/quil:instruction-fmt/ is non-native in the current rewiring, compiling.~%"
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
  ;; TODO: maybe change global variable to pass function
  (if *addresser-use-2q-tiers*
      (lscheduler-2q-tiers-with-1q-instructions lschedule)
      ;; (lscheduler-2q-tiers lschedule)
      (lscheduler-instruction-tiers lschedule)))

(defun assign-gate (inst gates-in-waiting)
  "Given a gate application that is unassigned or partially assigned by
the addresser's working logical-to-physical rewiring, compute a best physical qubit
assignment."
  (assert (= 2 (length (application-arguments inst))) () "Expected 2-qubit gate")
  (with-slots (working-l2p chip-spec) *temporal-addresser-state-variables*
    (destructuring-bind (q0 q1) (mapcar #'qubit-index (application-arguments inst))
      (let ((p0 (apply-rewiring-l2p working-l2p q0))
            (p1 (apply-rewiring-l2p working-l2p q1)))
        (when p1 (rotatef p0 p1) (rotatef q0 q1))
        (if p0
            (list (list q0 q1)
                  (list p0 (best-qubit-position q1 gates-in-waiting
                                                :locations (chip-spec-adj-qubits chip-spec p0))))
            (list (list q0 q1) (best-qubit-positions q0 q1 gates-in-waiting)))))))

(defun best-qubit-position (logical gates-in-waiting &key locations)
  "Finds the best location for an unassigned logical under the given future schedule."
  (with-slots (working-l2p chip-spec qq-distances qubit-cc) *temporal-addresser-state-variables*
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

(defun best-qubit-positions (q0 q1 gates-in-waiting)
  "Find the best location for the pair of qubits Q0 and Q1."
  (let ((pos0 (best-qubit-position q0 gates-in-waiting)))
    (with-slots (working-l2p chip-spec) *temporal-addresser-state-variables*
      (with-rewiring-assign working-l2p q0 pos0
        (list pos0 (best-qubit-position q1 gates-in-waiting
                                        :locations (chip-spec-adj-qubits chip-spec pos0)))))))

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
    (make-temporal-addresser :initial-l2p initial-l2p
                                   :working-l2p (copy-rewiring initial-l2p)
                                   :lschedule (make-lscheduler)
                                   :qq-distances (precompute-qubit-qubit-distances chip-spec)
                                   :qubit-cc (a:extremum (chip-spec-live-qubit-cc chip-spec)
                                                         #'>
                                                         :key #'length)
                                   :1q-queues (make-array n-qubits
                                                          :initial-element (list))
                                   :chip-sched (make-chip-schedule chip-spec)
                                   :chip-spec chip-spec)))

;; todo: eventually we want to modify parts of this to incorporate multi-qubit
;;       hardware objects. a lot of this is already correctly set up for that
;;       eventuality. the main thing that needs to change is that 1Q- and 2Q-
;;       operations should be handled identically. for instance, we should add
;;       queues for the 2Q operations, and they should also be written out by a
;;       flushing call.
(defun do-greedy-temporal-addressing (instrs ; list of instructions to schedule
                                      chip-spec ; chip-specification qpu description
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
          "DO-GREEDY-TEMPORAL-ADDRESSING: entrance.~%")
  (let* ((*temporal-addresser-use-free-swaps*
           (or use-free-swaps (not initial-rewiring)))
         (*temporal-addresser-state-variables*
           (initial-addresser-working-state chip-spec initial-rewiring)))
    (with-slots (lschedule 1q-queues working-l2p chip-sched initial-l2p) *temporal-addresser-state-variables*
      ;; This is governed by an FSM of the shape
      ;;
      ;; [ do-greedy-temporal-addressing ]
      ;; [ initialization                ]
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
        (flet ((temporal-addresser-FSM ()
                 (loop
                   :with rewirings-tried := nil
                   :while (lscheduler-first-instrs lschedule)
                   :do (format *compiler-noise-stream* "TEMPORAL-ADDRESSER-FSM: New pass.~%")
                   :when (dequeue-logical-to-physical)
                     :do (format *compiler-noise-stream* "TEMPORAL-ADDRESSER-FSM: LSCHED changed, retrying.~%")
                         (setf rewirings-tried nil)
                   :else
                     :do (format *compiler-noise-stream*
                                 "TEMPORAL-ADDRESSER-FSM: LSCHED unchanged, selecting a permutation.~%")
                         (assert (> *addresser-max-swap-sequence-length* (length rewirings-tried)) ()
                                 "Too many SWAP instructions selected in a row: ~a" (length rewirings-tried))
                         (setf rewirings-tried (select-and-embed-a-permutation rewirings-tried)))))

          ;; build the logically parallelized schedule
          (append-instructions-to-lschedule lschedule instrs)
          ;; actually invoke the FSM
          (temporal-addresser-FSM)
          ;; now flush the 1Q queues in preparation for writing out
          (dotimes (qubit (chip-spec-n-qubits chip-spec))
            (unless (endp (aref 1q-queues qubit))
              (flush-1q-instructions-after-wiring qubit)))
          (format *compiler-noise-stream* "DO-GREEDY-TEMPORAL-ADDRESSING: departure.~%")

          ;; TODO: Consider what happens when initial-l2p has a different logical
          ;; coverage than final l2p

          ;; TODO: Don't always fully assign the l2p
          (fill-rewiring working-l2p)
          (setf initial-l2p (copy-rewiring working-l2p))
          (dolist (instr (nreverse (chip-schedule-to-straight-quil chip-sched)))
            (when (swap-application-p instr)
              (apply #'update-rewiring initial-l2p (mapcar #'qubit-index (application-arguments instr)))))

          ;; finally, return what we've constructed
          (values initial-l2p chip-sched working-l2p)))))
