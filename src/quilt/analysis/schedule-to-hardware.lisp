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

(defun resolve-hardware-object (instr)
  "Get the name of the hardware object responsible for executing the simple Quilt instruction INSTR.

NOTE: Scheduling instructions (e.g. DELAY, FENCE) will resolve to NIL."
  (flet ((hw-object (frame)
           (frame-definition-hardware-object
            (frame-name-resolution
             frame))))
    (typecase instr
      (pulse
       (hw-object (pulse-frame instr)))
      (capture
       (hw-object (capture-frame instr)))
      (raw-capture
       (hw-object (raw-capture-frame instr)))
      (simple-frame-mutation
       (hw-object (frame-mutation-target-frame instr)))
      (swap-phase
       (let ((left (hw-object (swap-phase-left-frame instr)))
             (right (hw-object (swap-phase-right-frame instr))))
         (unless (string-equal left right)
           (error "Instruction ~/quilt::instruction-fmt/ has conflicting hardware objects: ~A and ~A."
                  instr
                  left
                  right))
         left)))))

(defun properly-obstructed-frames (instr program)
  ;; TODO docstring
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


(defun schedule-to-hardware (program &key (initial-time 0.0) (align-op #'identity))
  (let ((aligned-start (funcall align-op initial-time))
        (hardware-schedules (make-hash-table :test 'equal))
        (frame-clocks (make-hash-table :test #'frame= :hash-function #'frame-hash)))
    (flet ((latest (frames)
             (apply #'max (mapcar (lambda (f)
                                    (gethash f frame-clocks aligned-start))
                                  frames))))
      (loop :for instr :across (parsed-program-executable-code program)
            :for frames := (properly-obstructed-frames instr program)
            :for hw-name := (resolve-hardware-object instr)
            :when (typep instr '(or reset reset-qubit))
              :do (error "Scheduling of RESET instructions is not implemented.")
            :do (let ((op-time (latest frames))
                      (duration (quilt-instruction-duration instr)))
                  (when hw-name
                    (push (cons op-time instr)
                          (gethash hw-name hardware-schedules)))
                  (loop :for frame :in frames
                        :do (setf (gethash frame frame-clocks)
                                  (funcall align-op (+ op-time duration)))))))
    hardware-schedules))
