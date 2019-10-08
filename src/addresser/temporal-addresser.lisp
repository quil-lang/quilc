;;;; temporal-addresser.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

;;; This file contains the subroutines specific to the operation of the version
;;; of the addresser that uses overall program runtime to optimize output.

(defclass temporal-addresser-state (addresser-state)
  ((1q-queues :accessor temporal-addresser-state-1q-queues
              :initarg :1q-queues
              :documentation "The family of queues where not-yet-scheduled 1Q instructions live, while we get them out of the way to process 2Q instructions.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; 1Q queue management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (with-slots (working-l2p chip-sched qubit-cc 1q-queues) state
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

(defun partially-flush-1Q-queues (state resources)
  "Flush any part of any 1Q queue that touches a given set of non-quantum RESOURCES."
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
             (flush-1q-instructions-after-wiring qubit)
             (setf (aref 1q-queues qubit) first-segment)
             (return)
             ;; if we didn't intersect, move the pointer on the first segment
        :collect instr :into first-segment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; method overrides that augment the common addresser to deal with 1Q queues ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; flush the 1Q lines in the relevant events
(defmethod dequeue-classical-instruction :before
    ((state temporal-addresser-state) instr &optional dry-run-escape)
  ;; handle our 1Q queues
  (when dry-run-escape  ; every classical instruction can be handled
      (funcall dry-run-escape))
    (with-slots (lschedule chip-spec chip-sched working-l2p) state
      (let (dirty-flag)
        (cond
          ;; is it maximally resourceful?
          ((global-instruction-p instr)
           ;; unload the 1Q queues, dequeue the instruction,
           ;; and set the dirty flag
           (dotimes (qubit (chip-spec-n-qubits chip-spec))
             (flush-1q-instructions-after-wiring qubit)))

          ;; is it a pure classical instruction?
          ((local-classical-instruction-p instr)
           ;; clear relevant 1Q queues, dequeue the instruction
           ;; and set the dirty flag
           (partially-flush-1Q-queues state (instruction-resources instr)))

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
                 (flush-1q-instructions-after-wiring qubit)))))))))

;; initialize the 1Q queues
(defun initial-temporal-addresser-working-state (chip-spec initial-rewiring)
  (let* ((state (initial-addresser-working-state chip-spec initial-rewiring)))
    (change-class state 'temporal-addresser-state)
    (setf (temporal-addresser-state-1q-queues state)
          (make-array (chip-spec-n-qubits chip-spec) :initial-element (list)))
    state))

;; defer picking rewiring for single-qubit instructions
(defmethod dequeue-gate-application
    ((state temporal-addresser-state) instr &optional dry-run-escape)
  (cond 
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
     ;; dequeue and set the dirty bit  
     (lscheduler-dequeue-instruction (addresser-state-logical-schedule state) instr)
     (push instr (aref (temporal-addresser-state-1q-queues state)
                       (qubit-index (first (application-arguments instr)))))
     (values t nil nil))
    (t
     (call-next-method))))

;; flush the queues before adding the 2Q instruction
(defmethod append-instr-to-chip-schedule :before
    ((state temporal-addresser-state) (instr gate-application))
  (when (= 2 (length (application-arguments instr)))
    (let ((left-line (apply-rewiring-p2l (addresser-state-working-l2p state)
                                         (qubit-index (first (application-arguments instr)))))
          (right-line (apply-rewiring-p2l (addresser-state-working-l2p state)
                                          (qubit-index (second (application-arguments instr))))))
      ;; flush the 1Q gates down the line
      (flush-1q-instructions-after-wiring state left-line)
      (flush-1q-instructions-after-wiring state right-line))))

(defun do-greedy-temporal-addressing (instrs
                                      chip-spec
                                      &key
                                        (initial-rewiring nil)
                                        (use-free-swaps nil))
  (let ((state (initial-temporal-addresser-working-state chip-spec initial-rewiring)))
    (multiple-value-bind (chip-sched initial-l2p working-l2p)
        (do-greedy-addressing state instrs
          :initial-rewiring initial-rewiring
          :use-free-swaps use-free-swaps)
      
      ;; now flush the 1Q queues in preparation for writing out
      (dotimes (qubit (chip-spec-n-qubits chip-spec))
        (unless (endp (aref (temporal-addresser-state-1q-queues state) qubit))
          (flush-1q-instructions-after-wiring state qubit)))
      
      (values chip-sched initial-l2p working-l2p))))
