;;;; outgoing-schedule.lisp
;;;;
;;;; Author: Eric Peterson

;; this file manages two types:
;;     (1) list of physically scheduled events
;;     (2) availability tracker for a given hardware line

;; [a, b] does not overlap with [c, d] iff (b < c) || (d < a).
;; (a, b) does not overlap with (c, d) iff (b <= c) || (d <= a).

(in-package #:cl-quil)

(defclass chip-schedule ()
  ((times
    :initform (make-hash-table :test 'eql)
    :reader chip-schedule-times)
   (chip-spec
    :initarg :chip-spec
    :reader chip-schedule-spec)
   (lschedule
    :type 'logical-scheduler
    :initform (make-lscheduler)
    :reader chip-schedule-data)))

(defun make-chip-schedule (chip-spec)
  (check-type chip-spec chip-specification)
  (make-instance 'chip-schedule :chip-spec chip-spec))

(defun instruction-duration (inst chip-spec &optional (default 1/100) (swaps-special t))
  "Find the duration of INST under the chip specification CHIP-SPEC. If INST is
not native, returns DEFAULT. If SWAPS-SPECIAL and INST is a swap, then uses the
permutation record duration."
  (when (and swaps-special (swap-application-p inst))
    (let* ((qubits (mapcar #'qubit-index (application-arguments inst)))
           (link-index (nth-value 1 (lookup-hardware-address-by-qubits chip-spec qubits))))
      (return-from instruction-duration
        (permutation-record-duration (vnth 0 (hardware-object-permutation-gates
                                              (chip-spec-nth-link chip-spec link-index)))))))
  (flet ((get-it (chip-spec isn)
           (alexandria:if-let ((obj (nth-value 2 (lookup-hardware-address chip-spec isn))))
             (funcall (hardware-object-native-instructions obj) isn)
             nil)))
    (declare (inline get-it))
    (etypecase inst
      (application (or (get-it chip-spec inst) default))
      (measurement (or (get-it chip-spec inst) default))
      (instruction default))))

(defun chip-schedule-start-time (schedule inst)
  "Return the time when the instruction INST begins executing in SCHEDULE."
  (gethash inst (chip-schedule-times schedule)))

(defun (setf chip-schedule-start-time) (time schedule inst)
  (setf (gethash inst (chip-schedule-times schedule)) time))

(defun chip-schedule-end-time (schedule inst)
  "Return the time when the instruction INST finishes executing in SCHEDULE."
  (+ (instruction-duration inst (chip-schedule-spec schedule))
     (chip-schedule-start-time schedule inst)))

(defun chip-schedule-append (schedule inst)
  "Append an instruction INST to the chip SCHEDULE"
  (append-instruction-to-lschedule (chip-schedule-data schedule) inst)
  (setf (chip-schedule-start-time schedule inst)
        (loop
          :for prev :in (gethash inst (lscheduler-earlier-instrs (chip-schedule-data schedule)))
          :maximize (chip-schedule-end-time schedule prev))))

(defun chip-schedule-last-meeting (schedule resource &key before-inst)
  "Return the last instruction to make use of RESOURCE within SCHEDULE.

If BEFORE-INST is provided, finds the last instruction in the predecessors of
BEFORE-INST to make use of RESOURCE."
  (loop
    :with latest-inst := nil
    :for inst
      :in (if before-inst
              (gethash before-inst (lscheduler-earlier-instrs (chip-schedule-data schedule)))
              (lscheduler-last-instrs (chip-schedule-data schedule)))
    :when (and (resources-intersect-p resource (instruction-resources inst))
               (or (not latest-inst)
                   (< (chip-schedule-end-time schedule latest-inst)
                      (chip-schedule-end-time schedule inst))))
      :do (setf latest-inst inst)
    :finally (return latest-inst)))

(defun print-chip-schedule (schedule &optional (stream *standard-output*))
  (dolist (inst (chip-schedule-to-straight-quil schedule))
    (format stream "~10,2f - ~10,2f: " (chip-schedule-start-time schedule inst) (chip-schedule-end-time schedule inst))
    (print-instruction inst stream)
    (format stream "~%")))

(defun chip-schedule-resource-end-time (schedule resource)
  "Return the time at which RESOURCE becomes free permanently within SCHEDULE."
  (alexandria:if-let (inst (chip-schedule-last-meeting schedule resource))
    (chip-schedule-end-time schedule inst)
    0))

(defun chip-schedule-qubit-times (schedule)
  "Find the first time a qubit is available, for each qubit in the schedule."
  (map 'vector
       (lambda (idx) (chip-schedule-resource-end-time schedule (make-qubit-resource idx)))
       (alexandria:iota (chip-spec-n-qubits (chip-schedule-spec schedule)))))

(defun chip-schedule-duration (chip-sched)
  "Find the total duration of a chip schedule."
  (loop
    :for inst :in (lscheduler-last-instrs (chip-schedule-data chip-sched))
    :maximize (chip-schedule-end-time chip-sched inst)))

(defun chip-schedule-to-straight-quil (chip-sched)
  "Converts a chip-schedule object to straight-line Quil code."
  (sort (lscheduler-all-instructions (chip-schedule-data chip-sched))
        #'< :key (lambda (inst) (chip-schedule-start-time chip-sched inst))))

(defun check-chip-schedule (chip-sched)
  (let ((insts (lscheduler-all-instructions (chip-schedule-data chip-sched))))
    (dotimes (qubit (chip-spec-n-qubits (chip-schedule-spec chip-sched)))
      (let ((needed (make-hash-table))
            (resource (make-qubit-resource qubit)))
        (dolist (inst insts)
          (when (resources-intersect-p resource (instruction-resources inst))
            (setf (gethash inst needed) t)))
        (loop
          :for inst := (chip-schedule-last-meeting chip-sched (make-qubit-resource qubit))
            :then (chip-schedule-last-meeting chip-sched (make-qubit-resource qubit) :before-inst inst)
          :while inst
          :do (remhash inst needed))
        (assert (zerop (hash-table-count needed)) ()
                "Qubit ~a did not have a single line in ~a. Missing ~a"
                qubit chip-sched (alexandria:hash-table-keys needed))))))
