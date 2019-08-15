(in-package :cl-quil)

(define-transform expand-fences-to-delays (expand-fences-to-delays)
  "This transform replaces explicit synchronization in the form of FENCE instructions with implicit
synchronization in the form of DELAY instructions on the appropriate qubit lines."
  expand-calibrations
  resolve-waveform-references)

(defun simple-quilt-p (instr)
  "Check whether the given instruction is a simple quilt operation."
  (typep instr '(or pulse capture raw-capture
                    delay fence
                    simple-frame-mutation swap-phase)))

(defun resolved-waveform (instr)
  "Get the resolved waveform of an instruction, if it exists."
  (a:if-let ((wf-ref
              (typecase instr
                (pulse (pulse-waveform instr))
                (capture (capture-waveform instr)))))
    (waveform-ref-name-resolution wf-ref)))

(defun waveform-active-duration (wf-or-wf-defn &optional sample-rate)
  "Get the active duration of the waveform or waveform definition, in seconds.
If WF-OR-WF-DEFN is a waveform definition, SAMPLE-RATE (Hz) must be non-null. "
  (etypecase wf-or-wf-defn
    (standard-waveform (constant-value (waveform-duration wf-or-wf-defn)))
    (waveform-definition
     (unless sample-rate
       (error "Duration of DEFWAVEFORM instance depends on the active sample rate."))
     (/ (length (waveform-definition-entries wf-or-wf-defn))
        sample-rate))))

(defun quilt-instruction-duration (instr &optional sample-rate)
  "Get the duration of the specified quilt instruction INSTR."
  (typecase instr
    ((or pulse capture)
     (waveform-active-duration (resolved-waveform instr) sample-rate))
    (delay
      (constant-value (delay-duration instr)))
    (raw-capture
     (constant-value (raw-capture-duration instr)))
    ((or simple-frame-mutation swap-phase fence)
     0.0)))

(defun quilt-instruction-qubits (instr)
  "Get the list of qubits associated with the quilt instruction INSTR."
  (typecase instr
    (pulse (frame-qubits
            (pulse-frame instr)))
    (capture (frame-qubits
              (capture-frame instr)))
    (raw-capture (frame-qubits
                  (raw-capture-frame instr)))
    (delay (list (delay-qubit instr)))
    (simple-frame-mutation (frame-qubits
                            (frame-mutation-target-frame instr)))
    (swap-phase (append (frame-qubits (swap-phase-left-frame instr))
                        (frame-qubits (swap-phase-right-frame instr))))
    (fence (fence-qubits instr))))


(defun expand-fences-to-delays (parsed-program &optional sample-rate)
  "Expand FENCE instructions to corresponding DELAY instructions on each relevant
qubit line. For custom waveform definitions, SAMPLE-RATE (in Hz) must be provided."
  (let (;; Indexed by qubit indices i.e. numbers
        (qubit-clocks (make-hash-table))
        ;; Non-FENCE instructions
        new-instrs)
    (flet ((process-instr (instr)
             (unless (simple-quilt-p instr)
               (quil-parse-error "Cannot resolve timing information for non-quilt instruction ~A" instr))
             (let* ((qubits (quilt-instruction-qubits instr))
                    (latest (loop :for q :in qubits
                                  :maximize (gethash (qubit-index q) qubit-clocks 0.0)))
                    (duration (quilt-instruction-duration instr sample-rate)))
               ;; We know the involved qubits, the latest time on any of these, and the duration
               ;; of the next instruction. Here we add delay instructions on any qubit lines
               ;; that are not at the latest time (thus synchronizing them), and then advance the clocks
               ;; to account for the next instruction.
               (loop :for q :in qubits
                     :for lag := (- latest
                                    (gethash (qubit-index q) qubit-clocks 0.0))
                     ;; insert DELAY
                     :when (plusp lag)
                       :do (push (make-instance 'delay :qubit q :duration (constant lag))
                                 new-instrs)
                           ;; advance clocks
                     :do (setf (gethash (qubit-index q) qubit-clocks)
                               (+ latest duration)))
               (unless (typep instr 'fence)
                 (push instr new-instrs)))))
      (loop :for instr :across (parsed-program-executable-code parsed-program)
            :do (process-instr instr)))
    (setf (parsed-program-executable-code parsed-program)
          (coerce (nreverse new-instrs) 'vector))
    parsed-program))
