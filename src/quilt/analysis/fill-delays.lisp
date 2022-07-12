;;;; fill-delays.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:cl-quil/quilt)

(define-transform fill-delays (fill-delays)
  "This transform fills empty time on Quilt frames with explicit DELAY instructions in a greedy fashion."
  expand-calibrations
  resolve-objects)

;;; Syntactic conveniences

;;; For the purposes of this (or related) analyses, "simple Quilt" is the subset
;;; of straight-line Quilt involving only pulse operations (including captures),
;;; explicit control of timing/synchronization, and frame updates. These are the
;;; basic operations which must be supported by any control hardware which may
;;; be targeted by Quilt, and also reflect a minimal coherent subset of
;;; instructions for which time-related program analyses may be performed.

(deftype simple-quilt-instruction ()
  '(or
    pulse capture raw-capture
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

(defparameter *quilt-seemingly-instantenous-duration* 0.0d0
  "A numerical value representing the duration of seemingly instantenous operations, in seconds. This might be zero, and it might not be!")

(defun quilt-instruction-duration (instr)
  "Get the duration of the specified Quilt instruction INSTR if it is well defined, or NIL otherwise."
  (typecase instr
    ((or pulse capture)
     (waveform-active-duration (resolved-waveform instr)))
    (delay
      (constant-value (delay-duration instr)))
    (raw-capture
     (constant-value (raw-capture-duration instr)))
    (simple-frame-mutation
     *quilt-seemingly-instantenous-duration*)
    ;; FENCE and SWAP-PHASE both impose synchronization, and hence only have a duration that is meaningful on a frame-by-frame basis.
    ((or fence swap-phase)
     nil)))

(defun quilt-instruction-frames (instr parsed-program)
  (a:ensure-list
   (typecase instr
     (pulse (pulse-frame instr))
     (capture (capture-frame instr))
     (raw-capture (raw-capture-frame instr))
     (delay-on-frames (delay-frames instr))
     (simple-frame-mutation (frame-mutation-target-frame instr))
     (swap-phase (list (swap-phase-left-frame instr) (swap-phase-right-frame instr)))
     ;; delay affects all frames on precisely the delay qubits
     (delay-on-qubits
      (loop :for defn :in (parsed-program-frame-definitions parsed-program)
            :for frame := (frame-definition-frame defn)
            :when (frame-on-p frame (delay-qubits instr))
              :collect frame))
     (fence
      (loop :for defn :in (parsed-program-frame-definitions parsed-program)
            :for frame := (frame-definition-frame defn)
            :when (frame-intersects-p frame (fence-qubits instr))
              :collect frame)))))

(defun pulse-operation-frame (instr)
  "If INSTR is a generalized pulse operation (e.g. PULSE, CAPTURE, or RAW-CAPTURE), return the target frame. Otherwise, return NIL."
  (typecase instr
    (pulse (pulse-frame instr))
    (capture (capture-frame instr))
    (raw-capture (raw-capture-frame instr))))

(defun frame-intersects-p (frame qubits)
  "Does the FRAME involve any of the specified QUBITS?"
  (intersection qubits
                (frame-qubits frame)
                :test #'qubit=))

(defun frame-on-p (frame qubits)
  "Does FRAME involve exactly the specified QUBITS in the specified order?"
  (list= (frame-qubits frame)
         qubits
         :test #'qubit=))

;;; Frame Clocks
;;;
;;; We manage a "local time" for each frame, stored in a single hash table keyed
;;; on the frame itself.

(defun make-frame-table ()
  (make-hash-table :test #'frame=
                   :hash-function #'frame-hash))

(defun tracked-frames (frame-table)
  (a:hash-table-keys frame-table))

(defun local-time (frame-clocks frame &optional (default 0.0d0))
  "Get the local time of FRAME with respect to the given CLOCKS."
  (gethash frame frame-clocks default))

(defun (setf local-time) (new-value frame-clocks frame)
  "Set the local time of FRAME with respect to the given CLOCKS."
  (setf (gethash frame frame-clocks) new-value))

(defun latest-time (frame-clocks frames)
  (loop :for frame :in frames :maximize (local-time frame-clocks frame)))

(defgeneric emit-delays (instr clocks)
  (:documentation "Update the local times on CLOCKS according to the specified INSTR, returning a list of implicit DELAYs.")

  (:method ((instr delay-on-frames) clocks)
    (dolist (f (delay-frames instr))
      (incf (local-time clocks f)
            (delay-duration instr)))
    ;; no implicit delays
    nil)

  (:method ((instr delay-on-qubits) clocks)
    (let ((frames (remove-if-not (lambda (f)
                                   (frame-on-p f (delay-qubits instr)))
                                 (tracked-frames clocks)))
          (duration (quilt-instruction-duration instr)))
      (dolist (f frames)
        (incf (local-time clocks f) duration)))
    ;; no implicit delays
    nil)

  (:method ((instr fence) clocks)
    (let* ((frames (remove-if-not (lambda (f)
                                    (frame-intersects-p f (fence-qubits instr)))
                                  (tracked-frames clocks)))
           (latest (latest-time clocks frames)))
      (loop :for f :in frames
            :for lag := (- latest (local-time clocks f))
            ;; handle implicit delays
            :when (plusp lag)
              :do (incf (local-time clocks f) lag)
              :and :collect (make-instance 'delay-on-frames
                                           :frames (list f)
                                           :duration (constant lag)))))

  (:method ((instr swap-phase) clocks)
    (with-slots (left-frame right-frame) instr
      (let ((latest (max (local-time clocks left-frame)
                         (local-time clocks right-frame))))
        (loop :for f :in (list left-frame right-frame)
              :for lag := (- latest (local-time clocks f))
              :when (plusp lag)
                :do (incf (local-time clocks f) lag)
                :and :collect (make-instance 'delay-on-frames
                                             :frames (list f)
                                             :duration (constant lag))))))

  (:method (instr clocks)
    ;; we handle pulse/capture/raw-capture together here. the default case is
    ;; that no clocks are updated
    (a:when-let ((frame (pulse-operation-frame instr)))
      (let* ((start-time (local-time clocks frame))
             (end-time (+ start-time (quilt-instruction-duration instr))))

        (setf (local-time clocks frame) end-time)

        (if (nonblocking-p instr)
            nil
            (loop :for f :in (remove-if-not (lambda (f)
                                              (frame-intersects-p frame (frame-qubits f)))
                                            (tracked-frames clocks))
                  :for lag := (- end-time (local-time clocks f))
                  ;; handle implicit delays
                  :when (plusp lag)
                    :do (incf (local-time clocks f) lag)
                    :and :collect (make-instance 'delay-on-frames
                                                 :frames (list f)
                                                 :duration (constant lag))))))))

(defun fill-delays (parsed-program &key (omit-fences t) (synchronize-at-end t))
  "Introduce any implicit DELAY instructions in the Quilt program PARSED-PROGRAM.

If OMIT-FENCES is T, then FENCE instructions will be removed from the resulting program.
If SYNCHRONIZE-AT-END is T, additional delays will be introduced at the end so that each frame has the same total duration."
  (let ((frame-clocks (make-frame-table))
        new-instrs)

    ;; initialize clocks
    (dolist (defn (parsed-program-frame-definitions parsed-program))
      (setf (local-time frame-clocks (frame-definition-frame defn)) 0.0d0))

    (flet ((process-instr (instr)
             (unless (typep instr 'simple-quilt-instruction)
               (quil-parse-error "Cannot resolve timing information for non-Quilt instruction ~/cl-quil:instruction-fmt/." instr))
             ;; Add delays before the instruction
             (dolist (delay (emit-delays instr frame-clocks))
               (push delay new-instrs))
             (unless (and omit-fences (typep instr 'fence))
               (push instr new-instrs))))
      (loop :for instr :across (parsed-program-executable-code parsed-program)
            :do (process-instr instr))
      (when synchronize-at-end
        (let ((all-qubits (reduce #'union
                                  (tracked-frames frame-clocks)
                                  :key #'frame-qubits)))
          (process-instr (make-instance 'fence :qubits all-qubits)))))

    (setf (parsed-program-executable-code parsed-program)
          (coerce (nreverse new-instrs) 'vector))
    parsed-program))
