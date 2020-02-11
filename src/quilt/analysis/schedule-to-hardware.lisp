;;; quilt/analysis/schedule-to-hardware.lisp
;;;
;;; Author: Erik Davis

(in-package #:cl-quil.quilt)

(defun all-defined-frames (program)
  "Get all frames defined in the Quilt program PROGRAM."
  (mapcar #'frame-definition-frame
          (parsed-program-frame-definitions program)))

(defun frames-subsumed (qubits program)
  "Get all frames in PROGRAM on any subset of the specified QUBITS."
  (loop :for frame :in (all-defined-frames program)
        :when (subsetp (frame-qubits frame) qubits :test #'qubit=)
          :collect frame))

(defun frames-on (qubits program)
  "Get all frames in PROGRAM on exactly the specified QUBITS."
  (loop :for frame :in (all-defined-frames program)
        :when (null (set-exclusive-or qubits
                                      (frame-qubits frame)
                                      :test #'qubit=))
          :collect frame))


(defun frame-hardware (frame)
  "Get the name of the hardware object associated with FRAME."
  (unless (frame-name-resolution frame)
    (error "Frame ~/quilt::instruction-fmt/ has not been resolved to a definition." frame))
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
         (error "Instruction ~/quilt::instruction-fmt/ has conflicting hardware objects: ~A and ~A."
                instr
                left
                right))
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
;;; begins at the next time which sychronizes these, and the associated clocks
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

Here a frame is 'properly' obstructed if it is obstructed by instr and the frame
qubits are a subset of the qubits on which INSTR acts.

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



(defun schedule-to-hardware (program &key (initial-time 0.0d0) (align-op #'identity))
  "Compute hardware schedules for the instructions in the Quilt program PROGRAM.

The result is a hash table mapping the names of hardware objects to a list of (instr . time) pairs."
  (check-type program parsed-quilt-program)
  (let ((aligned-start (funcall align-op initial-time))
        (hardware-schedules (make-hash-table :test 'equal))
        (hardware-clocks (make-hash-table :test 'equal)))
    (flet ((latest (hw-objects)
             (apply #'max (mapcar (lambda (hw-obj)
                                    (gethash hw-obj hardware-clocks aligned-start))
                                  hw-objects))))
      (loop :for instr :across (parsed-program-executable-code program)
            :for obstructed-hw := (mapcar #'frame-hardware
                                          (properly-obstructed-frames instr program))
            :for target-hw := (resolve-hardware-object instr)
            :when (typep instr '(or reset reset-qubit))
              :do (error "Scheduling of RESET instructions is not implemented.")
            :do (let ((op-time (latest obstructed-hw))
                      (duration (quilt-instruction-duration instr)))
                  (when target-hw
                    (push (cons op-time instr)
                          (gethash target-hw hardware-schedules)))
                  (loop :for hw :in obstructed-hw
                        :do (setf (gethash hw hardware-clocks)
                                  (funcall align-op (+ op-time duration)))))))
    hardware-schedules))
