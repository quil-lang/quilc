(in-package :cl-quil)

(define-transform expand-fences-to-delays (expand-fences-to-delays)
  "This transform replaces explicit synchronization in the form of FENCE instructions with implicit
synchronization in the form of DELAY instructions on the appropriate qubit lines."
  expand-calibrations
  resolve-waveform-references)

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
     (* sample-rate (length (waveform-definition-entries wf-or-wf-defn))))))

(defun quilt-instruction-duration (instr &optional sample-rate)
  "Get the duration of the specified quilt instruction INSTR."
  (etypecase instr
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
  (etypecase instr
    (pulse (frame-qubits
            (pulse-frame instr)))
    (capture (frame-qubits
              (capture-frame instr)))
    (raw-capture (frame-qubits
                  (raw-capture-frame instr)))
    (delay (list (delay-qubit instr)))
    (simple-frame-mutation (frame-qubits
                            (target-frame instr)))
    (swap-phase (append (frame-qubits (swap-phase-left-frame instr))
                        (frame-qubits (swap-phase-right-frame instr))))
    (fence (fence-qubits instr))))


(defun expand-fences-to-delays (parsed-program &optional sample-rate)
  "Expand FENCE instructions to corresponding DELAY instructions on each relevant
qubit line. For custom waveform definitions, SAMPLE-RATE (in Hz) must be provided."
  (let ((qubit-clocks (make-hash-table)) ; indexed by qubit indices i.e. numbers
        new-instrs)
    (loop :for instr :across (parsed-program-executable-code parsed-program)
          :for qubits := (quilt-instruction-qubits instr)
          :when qubits
            :do (let ((latest (loop :for q :in qubits
                                    :maximizing (gethash (qubit-index q) qubit-clocks 0.0)))
                      (duration (quilt-instruction-duration instr sample-rate)))
                  (loop :for q :in qubits
                        :for lag := (- latest
                                       (gethash (qubit-index q) qubit-clocks 0.0))
                        ;; insert DELAY
                        :when (plusp lag)
                          :do (push (make-instance 'delay :qubit q :duration (constant lag))
                                    new-instrs)
                        ;; advance clocks
                        :do (setf (gethash (qubit-index q) qubit-clocks)
                                  (+ latest duration))))
          :unless (typep instr 'fence)
            :do (push instr new-instrs))
    (setf (parsed-program-executable-code parsed-program) (nreverse new-instrs))
    parsed-program))
