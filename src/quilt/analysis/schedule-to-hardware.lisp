;;; quilt/analysis/schedule-to-hardware.lisp
;;;
;;; Author: Erik Davis

(in-package #:cl-quil.quilt)

;;; Syntactic conveniences

;;; For the purposes of this (or related) analyses, "simple Quilt" is the subset
;;; of straight-line Quilt involving only pulse operations or captures, explicit
;;; control of timing/synchronization, and frame updates. These are the basic
;;; operations which must be supported by any control hardware which may be
;;; targeted by Quilt, and also reflect a minimal coherent subset of
;;; instructions for which time-related program analyses may be performed.

(deftype simple-quilt-instruction ()
  '(or
    pulse capture raw-capture
    delay fence fence-all
    simple-frame-mutation swap-phase
    pragma))

(define-condition quilt-scheduling-error ()
  ()
  (:documentation "Representation of an error during scheduling of Quilt."))

(defun quilt-scheduling-error (format-control &rest format-args)
  (error 'quilt-scheduling-error :format-control format-control
                                 :format-arguments format-args))

(defclass hardware-schedule ()
  ((hardware-name :initarg :hardware-name
                  :type string
                  :reader hardware-schedule-hardware-name
                  :documentation "The hardware object targeted by the schedule.")
   (program :initarg :program
            :type parsed-quilt-program
            :reader hardware-schedule-program
            :documentation "A program scheduled to run on a hardware object.")
   (times :initarg :times
          :type hash-table
          :reader hardware-schedule-times
          :documentation "A map from instructions to their scheduled time of execution [seconds]."))
  (:documentation "A Quilt program, together with a mapping from instructions to their time of execution."))

(defun schedule-duration (schedule)
  "Get the length of the hardware schedule SCHEDULE, in seconds."
  (loop :for instr :being :the :hash-keys :of (hardware-schedule-times schedule)
          :using (hash-value start-time)
        :maximize (+ start-time (quilt-instruction-duration instr))))

(defun resolved-waveform (instr)
  "Get the resolved waveform of an instruction, if it exists."
  (a:if-let ((wf-ref
              (typecase instr
                (pulse (pulse-waveform instr))
                (capture (capture-waveform instr)))))
    (waveform-ref-name-resolution wf-ref)))

(defun waveform-active-duration (wf-or-wf-defn frame)
  "Get the active duration of the waveform or waveform definition, in seconds.
If WF-OR-WF-DEFN is a waveform definition, SAMPLE-RATE (Hz) must be non-null. "
  (etypecase wf-or-wf-defn
    (template-waveform (constant-value (template-waveform-duration wf-or-wf-defn)))
    (waveform-definition
     (let ((frame-defn (frame-name-resolution frame)))
       (unless frame-defn
         (quilt-scheduling-error "Unable to determine waveform duration on unresolved frame ~/quilt::instruction-fmt/."
                                 frame))
       (unless (frame-definition-sample-rate frame-defn)
         (quilt-scheduling-error "Unable to determine waveform duration: frame ~/quilt::instruction-fmt/ has no SAMPLE-RATE specified."))
       (/ (length (waveform-definition-entries wf-or-wf-defn))
          (constant-value (frame-definition-sample-rate frame-defn)))))))

(defparameter *quilt-seemingly-instantenous-duration* 0.0d0
  "A numerical value representing the duration of seemingly instantenous operations, in seconds. This might be zero, and it might not be!")

(defun quilt-instruction-duration (instr)
  "Get the duration of the specified Quilt instruction INSTR."
  (etypecase instr
    (pulse
     (waveform-active-duration (resolved-waveform instr) (pulse-frame instr)))
    (capture
     (waveform-active-duration (resolved-waveform instr) (capture-frame instr)))
    (delay
     (constant-value (delay-duration instr)))
    (raw-capture
     (constant-value (raw-capture-duration instr)))
    ((or fence fence-all swap-phase simple-frame-mutation)
     *quilt-seemingly-instantenous-duration*)))

(defun frame-intersects-p (frame qubits)
  "Does the FRAME involve any of the specified QUBITS?"
  (intersection qubits
                (frame-qubits frame)
                :test #'qubit=))

(defun frame-on-p (frame qubits)
  "Does FRAME involve exactly the specified QUBITS?"
  (null (set-exclusive-or qubits
                          (frame-qubits frame)
                          :test #'qubit=)))

(defun frame-subset-p (frame qubits)
  "Does FRAME involve a subset of the specified QUBITS?"
  (subsetp (frame-qubits frame) qubits :test #'qubit=))

(defun all-defined-frames (program)
  "Get all frames defined in the Quilt program PROGRAM."
  (mapcar #'frame-definition-frame
          (parsed-program-frame-definitions program)))

(defun frames-subsumed (qubits program)
  "Get all frames in PROGRAM on any subset of the specified QUBITS."
  (remove-if-not (lambda (f) (frame-subset-p f qubits))
                 (all-defined-frames program)))

(defun frames-on (qubits program)
  "Get all frames in PROGRAM on exactly the specified QUBITS."
  (remove-if-not (lambda (f) (frame-on-p f qubits))
                 (all-defined-frames program)))

(defun frame-hardware (frame)
  "Get the name of the hardware object associated with FRAME."
  (unless (frame-name-resolution frame)
    (quilt-scheduling-error
     "Frame ~/quilt::instruction-fmt/ has not been resolved to a definition."
     frame))
  (frame-definition-hardware-object
   (frame-name-resolution frame)))

(defun resolve-hardware-object (instr)
  "Get the name of the hardware object responsible for executing the simple Quilt instruction INSTR.

NOTE: Scheduling instructions (e.g. DELAY, FENCE) will resolve to NIL."
  (typecase instr
    (pulse
     (frame-hardware (pulse-frame instr)))
    (capture
     (frame-hardware (capture-frame instr)))
    (raw-capture
     (frame-hardware (raw-capture-frame instr)))
    (simple-frame-mutation
     (frame-hardware (frame-mutation-target-frame instr)))
    (swap-phase
     (let ((left (frame-hardware (swap-phase-left-frame instr)))
           (right (frame-hardware (swap-phase-right-frame instr))))
       (unless (string-equal left right)
         (quilt-scheduling-error
          "Instruction ~/quilt::instruction-fmt/ has conflicting hardware objects: ~S and ~S."
          instr left right))
       left))))

;;; Greedy Scheduling
;;;
;;; A basic relationship is that of an instruction 'obstructing' a frame:
;;; informally, if instructions A and B obstruct a common frame, then A and B
;;; must be scheduled on non-overlapping time intervals.
;;;
;;; A naive approach to scheduling is to maintain a clock for each hardware
;;; object, tracking the next available time for an operation. For each
;;; instruction, the set of obstructed frames is computed. The instruction
;;; begins at the next time which synchronizes these, and the associated clocks
;;; are advanced by the instruction duration.
;;;
;;; There is one problem with this approach. Consider
;;;   PULSE 0 "rf" 1.0         # A
;;;   PULSE 1 "rf" 1.0         # B
;;; and suppose that we also have a frame 0 1 "cz". After processing A,
;;; the clocks look like
;;;   0 "rf"    -> 1.0
;;;   0 1 "cz"  -> 1.0          (since A obstructs 0 1 "cz")
;;;   1 "rf"    -> 0.0
;;; and so the naive start time for B is 1.0, due to synchronization with 0 1 "cz".
;;;
;;; The approach adopted here is to only synchronize across 'properly'
;;; obstructed frames (defined below). More or less this implies that a program
;;; without 2Q instructions will have all 1Q operations scheduled independently.

(defun properly-obstructed-frames (instr program)
  "Get all the frames in PROGRAM which are 'properly' obstructed by INSTR.

Here a frame is 'properly' obstructed if it is obstructed by INSTR and the frame qubits are a subset of the qubits on which INSTR acts.

For example, a pulse on 0 \"rf\" does not properly obstruct 0 1 \"cz\"."
  (a:ensure-list
   (typecase instr
     (fence
      (frames-subsumed (fence-qubits instr) program))
     (fence-all
      (all-defined-frames program))
     (delay-on-qubits
      (frames-on (delay-qubits instr) program))
     (delay-on-frames
      (delay-frames instr))
     (pulse
      (if (nonblocking-p instr)
          (pulse-frame instr)
          (frames-subsumed (frame-qubits (pulse-frame instr))
                           program)))
     (capture
      (if (nonblocking-p instr)
          (capture-frame instr)
          (frames-subsumed (frame-qubits (capture-frame instr))
                           program)))
     (raw-capture
      (if (nonblocking-p instr)
          (raw-capture-frame instr)
          (frames-subsumed (frame-qubits (raw-capture-frame instr))
                           program)))
     (simple-frame-mutation
      (frame-mutation-target-frame instr))
     (swap-phase
      (list (swap-phase-left-frame instr)
            (swap-phase-right-frame instr))))))

(defun dependent-frame-definitions (instr)
  (check-type instr simple-quilt-instruction)
  (let ((frames
          (a:ensure-list
           (typecase instr
             (delay-on-frames
              (delay-frames instr))
             (pulse (pulse-frame instr))
             (capture (capture-frame instr))
             (raw-capture (raw-capture-frame instr))
             (simple-frame-mutation
              (frame-mutation-target-frame instr))
             (swap-phase
              (list (swap-phase-left-frame instr)
                    (swap-phase-right-frame instr)))))))
    (remove-duplicates
     (mapcar #'frame-name-resolution frames))))

(defun dependent-waveform-definitions (instr)
  (check-type instr simple-quilt-instruction)
  (a:if-let ((wf (resolved-waveform instr)))
    (typecase wf
      (waveform-definition (list wf)))))

;;; TODO: handle parametric programs
(defun dependent-memory-definitions (instr)
  (check-type instr simple-quilt-instruction)
  (a:ensure-list
   (typecase instr
     (capture (quil::memory-ref-descriptor
               (capture-memory-ref instr)))
     (raw-capture (quil::memory-ref-descriptor
                   (raw-capture-memory-ref instr))))))

(defun reconstruct-program (instructions)
  "Construct a fresh Quilt program from the resolved simple Quilt instructions INSTRUCTIONS."
  (let ((waveform-definitions nil)
        (frame-definitions nil)
        (memory-definitions nil))
    (dolist (instr instructions)
      (a:unionf waveform-definitions (dependent-waveform-definitions instr))
      (a:unionf frame-definitions (dependent-frame-definitions instr))
      (a:unionf memory-definitions (dependent-memory-definitions instr)))
    (resolve-objects
     (make-instance 'quilt::parsed-quilt-program
                    :gate-definitions nil
                    :circuit-definitions nil
                    :calibration-definitions nil
                    :waveform-definitions (nreverse waveform-definitions)
                    :frame-definitions (nreverse frame-definitions)
                    :memory-definitions (nreverse memory-definitions)
                    :executable-code (coerce instructions 'simple-vector)))))

;;; TODO: some of this could be absorbed into EXPAND-DEFCALS
(defun make-hardware-schedule (hw-name instruction-times)
  "Construct a full, explicit schedule for HW-NAME from a list of (instruction . time) pairs INSTRUCTION-TIMES, inserting delays and frame mutations as needed.

This abides by the original ordering of instructions in INSTRUCTION-TIMES."
  ;; NOTE: There are some seemingly idiosyncratic aspects of this. Broadly
  ;; speaking, this is written to produce code akin to what our existing
  ;; translation pipeline produces. In particular
  ;; - we track timing globally, but freqs, scale, phases per frame
  ;; - we elide unneeded frame mutations
  (let ((instruction-times (stable-sort instruction-times
                                        (lambda (a b) (< (cdr a) (cdr b)))))
        (times (make-hash-table :test 'equal))
        (instructions nil)
        (current-time 0.0d0)
        (scales (make-hash-table :test #'frame= :hash-function #'frame-hash))
        (frequencies (make-hash-table :test #'frame= :hash-function #'frame-hash))
        (phases (make-hash-table :test #'frame= :hash-function #'frame-hash)))
    (labels ((add-instr (instr &optional time)
               (setf (gethash instr times) (or time current-time))
               (push instr instructions))
             (initialize-frame-frequency (frame)
               "Emit instructions setting the frame frequency to INITIAL-FREQUENCY, if this has not already been done."
               (unless (gethash frame frequencies)
                 (let ((freq (frame-definition-initial-frequency
                              (frame-name-resolution
                               frame))))
                   (unless freq
                     (error "Unable to resolve frequency of frame ~/quilt::instruction-fmt/." frame))
                   (add-instr (make-instance 'set-frequency
                                             :frame frame
                                             :value freq))
                   (setf (gethash frame frequencies) (constant-value freq)))))
             (initialize-frame-scale (frame)
               "Emit instructions setting the hardware scale to the default 1.0, if this has not already been done."
               (unless (gethash frame scales)
                 (add-instr (make-instance 'set-scale
                                           :frame frame
                                           :value (constant 1.0d0)))
                 (setf (gethash frame scales) 1.0d0)))
             (mutate-frame-state (instr state-table)
               (let ((value (constant-value (frame-mutation-value instr)))
                     (current (gethash (frame-mutation-target-frame instr) state-table)))
                 (unless (and current (= value current))
                   (add-instr (copy-instance instr))
                   (setf (gethash (frame-mutation-target-frame instr) state-table) value)))))
      (loop :for (instr . scheduled-time) :in instruction-times
            :when (and (< current-time scheduled-time) ; we have time to fill
                       (or (zerop current-time) ; special case: start of block
                           (typep instr '(or capture raw-capture pulse))))
              :do (let ((delay (make-instance 'delay-all
                                              :duration (constant (- scheduled-time current-time)))))
                    (add-instr delay)
                    (setf current-time scheduled-time))
            :do (typecase instr
                  (pulse
                   (initialize-frame-frequency (pulse-frame instr))
                   (initialize-frame-scale (pulse-frame instr))
                   (add-instr (copy-instance instr) scheduled-time))
                  (capture
                   (initialize-frame-frequency (capture-frame instr))
                   (initialize-frame-scale (capture-frame instr))
                   (add-instr (copy-instance instr) scheduled-time))
                  (raw-capture
                   (error "RAW-CAPTURE is currently unsupported."))
                  ;; TODO: this is really dumb, but at present
                  ;; i) SET-* operations may be partially evaluated, but SHIFT-* operations are not
                  ;; ii) this will fail for parametric compilation
                  ;; Both of these should have a proper resolution....
                  (set-frequency
                   (mutate-frame-state instr frequencies))
                  (set-scale
                   (mutate-frame-state instr scales))
                  (set-phase
                   (mutate-frame-state instr phases))
                  (otherwise
                   (add-instr (copy-instance instr) scheduled-time)))
            :do (setf current-time
                      (+ scheduled-time (quilt-instruction-duration instr)))))
    (make-instance 'hardware-schedule
                   :hardware-name hw-name
                   :program (reconstruct-program
                             (nreverse instructions))
                   :times times)))


(defun assign-hardware-times (program initial-time align-op)
  "Destructure a simple Quilt program PROGRAM, assigning each instruction to a hardware object for execution at a certain time.

The result is a hash table mapping the name of a hardware object to a (instruction . time) association list."
  (let ((aligned-start (funcall align-op initial-time))
        (hardware-instruction-times (make-hash-table :test 'equal))
        (frame-clocks (make-hash-table :test #'frame= :hash-function #'frame-hash)))
    (flet ((latest (frames)
             (apply #'max (mapcar (lambda (f)
                                    (gethash f frame-clocks aligned-start))
                                  frames))))
      (loop :for instr :across (parsed-program-executable-code program)
            :for obstructed-frames := (properly-obstructed-frames instr program)
            :for target-hw := (resolve-hardware-object instr)
            :unless (typep instr 'simple-quilt-instruction)
              :do (quilt-scheduling-error "Scheduling of ~/quilt::instruction-fmt/ instructions is not implemented." instr)
            :do (let ((op-time (latest obstructed-frames))
                      (duration (quilt-instruction-duration instr)))
                  (when target-hw
                    (push (cons instr op-time)
                          (gethash target-hw hardware-instruction-times)))
                  (loop :for frame :in obstructed-frames
                        :do (setf (gethash frame frame-clocks)
                                  (funcall align-op (+ op-time duration)))))))
    hardware-instruction-times))

(defun schedule-to-hardware (program &key (initial-time 0.0d0) (align-op #'identity))
  "Compute hardware schedules for the instructions in the Quilt program PROGRAM.

The result is a hash table mapping the names of hardware objects to hardware schedules."
  (check-type program parsed-quilt-program)
  ;; This proceeds in two steps. First, we assign every instruction to a
  ;; hardware object at a "suggested" time. Then, for each object we construct
  ;; the actual schedule, which may incorporate additional operations (e.g.
  ;; explicit delays, initializing frame state, etc).
  (let ((hardware-schedules (make-hash-table :test 'equal)))
    (loop :for hw :being :the :hash-keys :of (assign-hardware-times
                                              program
                                              (coerce initial-time 'double-float)
                                              align-op)
            :using (hash-value instr-times)
          ;; instr-times is in the reversed order of the original instructions
          :for ordered-instr-times := (nreverse instr-times)
          :do (setf (gethash hw hardware-schedules)
                    (make-hardware-schedule hw ordered-instr-times)))
    hardware-schedules))
