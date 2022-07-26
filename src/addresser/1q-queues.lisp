(in-package #:cl-quil)

;;; 1Q Queue Management
;;;
;;; The addresser optionally (cf. *ADDRESSER-USE-1Q-QUEUES*) prevents 1Q
;;; operations from influencing anything (qubit allocation, SWAP placement, and
;;; so on) since 1Q operations can be easily moved around and are often not very
;;; costly.
;;;
;;; This is done via a system of "1Q Queues", which stash the 1Q operations
;;; during addressing and then are flushed at appropriate times. Routines for
;;; managing these queues are found here, but are mostly called by the
;;; addressing procedures in addresser-common.lisp.

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

(defun flush-1q-instructions-after-wiring (state qubit)
  "Flush the 1Q queue for QUBIT after potentially assigning it to a physical location."
  (check-type state addresser-state)
  (with-slots (working-l2p chip-sched 1q-queues) state
    (let ((physical (apply-rewiring-l2p working-l2p qubit)))
      (unless physical
        ;; TODO: Should we try to pick the best one?
        (setf physical (loop
                         :for physical :in (find-physical-component-in-state state qubit)
                         :unless (apply-rewiring-p2l working-l2p physical)
                           :return physical))
        (rewiring-assign working-l2p qubit physical))
      (flush-1q-instructions chip-sched physical (nreverse (aref 1q-queues qubit)))
      (setf (aref 1q-queues qubit) nil))))

(defun partially-flush-1Q-queues (state resources)
  "Flush any part of any 1Q queue that touches a given set of non-quantum RESOURCES."
  (check-type state addresser-state)
  (with-slots (chip-spec 1q-queues) state
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
             (flush-1q-instructions-after-wiring state qubit)
             (setf (aref 1q-queues qubit) first-segment)
             (return)
             ;; if we didn't intersect, move the pointer on the first segment
        :collect instr :into first-segment))))

(defun add-instruction-to-1Q-queue (state instr)
  "Add the 1Q instruction to the queues."
  (check-type state addresser-state)
  (assert (= 1 (length (application-arguments instr)))
          nil
          "Attempted to add instruction ~/cl-quil:instruction-fmt/ to 1Q queues."
          instr)
  (lscheduler-dequeue-instruction (addresser-state-logical-schedule state) instr)
  (push instr (aref (addresser-state-1q-queues state)
                    (qubit-index (first (application-arguments instr))))))
