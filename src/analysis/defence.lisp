(in-package :cl-quil)

;;; Given a 'simple' quilt program, this makes it even simpler: namely, FENCE instructions
;;; are replaced with appropriate DELAY instructions. The basic idea is that something like
;;;
;;; PULSE 0 \"xy\" flat(duration: 1.0, iq: 1.0)
;;; FENCE 0 1
;;; PULSE 1 \"xy\" flat(duration: 1.0, iq: 1.0)
;;;
;;; can get converted to
;;;
;;; PULSE 0 \"xy\" flat(duration: 1.0, iq: 1.0)
;;; DELAY 1 1.0
;;; PULSE 1 \"xy\" flat(duration: 1.0, iq: 1.0)
;;;
;;; where synchronization is encoded in the newly introduced DELAY.
;;;
;;; By a simple quilt program, we mean a program consisting only of instructions
;;; that have an immediately calculable duration. At present, this precludes the
;;; use of control flow operations. See SIMPLE-QUILT-P below for more details.

(define-transform expand-fences-to-delays (expand-fences-to-delays)
  "This transform replaces explicit synchronization in the form of FENCE instructions with implicit synchronization in the form of DELAY instructions on the appropriate qubit lines."
  expand-calibrations
  resolve-waveform-references)

(deftype simple-quilt-instruction ()
  '(or pulse capture raw-capture
    delay fence
    simple-frame-mutation swap-phase))

(defun resolved-waveform (instr)
  "Get the resolved waveform of an instruction, if it exists."
  (a:if-let ((wf-ref
              (typecase instr
                (pulse (pulse-waveform instr))
                (capture (capture-waveform instr)))))
    (waveform-ref-name-resolution wf-ref)))

(defun waveform-active-duration (wf-or-wf-defn)
  "Get the active duration of the waveform or waveform definition, in seconds.
If WF-OR-WF-DEFN is a waveform definition, SAMPLE-RATE (Hz) must be non-null. "
  (etypecase wf-or-wf-defn
    (standard-waveform (constant-value (waveform-duration wf-or-wf-defn)))
    (waveform-definition
     (/ (length (waveform-definition-entries wf-or-wf-defn))
        (constant-value (waveform-definition-sample-rate wf-or-wf-defn))))))

(defun quilt-instruction-duration (instr)
  "Get the duration of the specified quilt instruction INSTR."
  (typecase instr
    ((or pulse capture)
     (waveform-active-duration (resolved-waveform instr)))
    (delay
      (constant-value (delay-duration instr)))
    (raw-capture
     (constant-value (raw-capture-duration instr)))
    ((or simple-frame-mutation swap-phase fence)
     0.0)))

(defun quilt-instruction-frames (instr parsed-program)
  (a:ensure-list
   (typecase instr
     (pulse (pulse-frame instr))
     (capture (capture-frame instr))
     (raw-capture (raw-capture-frame instr))
     (delay-on-frames (delay-frames instr))
     (simple-frame-mutation (frame-mutation-target-frame instr))
     (swap-phase (swap-phase-left-frame instr) (swap-phase-right-frame instr))
     ;; delay affects all frames on precisely the delay qubits
     (delay-on-qubits
      (loop :for defn :in (parsed-program-frame-definitions parsed-program)
            :for frame := (frame-definition-frame defn)
            :when (equalp (frame-qubits frame)
                          (delay-qubits instr))
              :collect frame))
     (fence
      (loop :for defn :in (parsed-program-frame-definitions parsed-program)
            :for frame := (frame-definition-frame defn)
            :when (intersection (frame-qubits frame)
                                (fence-qubits instr)
                                :test #'equalp)
              :collect frame)))))

(defun expand-fences-to-delays (parsed-program)
  "Expand FENCE instructions to corresponding DELAY instructions on each relevant
qubit line. For custom waveform definitions, SAMPLE-RATE (in Hz) must be provided."
  (let ((frame-clocks (make-hash-table :test #'frame=
                                       :hash-function #'frame-hash))
        new-instrs)

    ;; initialize clocks
    (dolist (defn (parsed-program-frame-definitions parsed-program))
      (setf (gethash (frame-definition-frame defn) frame-clocks)
            0.0))

    (flet ((process-instr (instr)
             (unless (typep instr 'simple-quilt-instruction)
               (quil-parse-error "Cannot resolve timing information for non-quilt instruction ~A" instr))
             (let* ((frames (quilt-instruction-frames instr parsed-program))
                    (latest (loop :for f :in frames
                                  :maximize (gethash f frame-clocks)))
                    (duration (quilt-instruction-duration instr)))
               ;; We know the involved frames, the latest time on any of these,
               ;; and the duration of the next instruction. Here we add delay
               ;; instructions on any frames that are not at the latest time
               ;; (thus synchronizing them), and then advance the clocks to
               ;; account for the next instruction.
               (loop :for f :in frames
                     :for lag := (- latest
                                    (gethash f frame-clocks))
                     ;; insert DELAY
                     :when (plusp lag)
                       :do (push (make-instance 'delay-on-frames
                                                :frames (list f)
                                                :duration (constant lag))
                                 new-instrs)
                           ;; advance clocks
                     :do (setf (gethash f frame-clocks)
                               (+ latest duration)))
               (unless (typep instr 'fence)
                 (push instr new-instrs)))))
      (loop :for instr :across (parsed-program-executable-code parsed-program)
            :do (process-instr instr)))

    (setf (parsed-program-executable-code parsed-program)
          (coerce (nreverse new-instrs) 'vector))
    parsed-program))
