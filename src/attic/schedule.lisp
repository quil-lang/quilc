;;;; schedule.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

(defconstant +none+ 0
  "The bit set representing no qubits.")

(defconstant +all+ -1
  "The bit set representing all qubits.")

;;; Parallelization Schedule type

(defstruct (slice (:constructor make-slice ()))
  "A single slice of a parallelization schedule."
  (qubits-used +none+)
  (addresses-used +none+)
  (instructions nil))

(defclass schedule ()
  ((current-slice :accessor current-slice)
   (slices :accessor slices
           :initform nil)))

(defun start-fresh-slice (s)
  "Given a schedule S, start a fresh slice on it, returning the newly created slice."
  (check-type s schedule)
  (let ((new-slice (make-slice)))
    (setf (current-slice s) new-slice)
    (push new-slice (slices s))
    new-slice))

(defmethod initialize-instance :after ((s schedule) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (start-fresh-slice s))

(defun on-fresh-slice-p (sched)
  "Does the schedule SCHED have a fresh slice as its current slice?"
  (check-type sched schedule)
  (eql +none+ (slice-qubits-used (current-slice sched))))

(defun ensure-fresh-slice (sched)
  "Ensure that the current slice of the schedule SCHED is fresh, possibly by making a new one."
  (check-type sched schedule)
  (cond
    ((on-fresh-slice-p sched)
     (current-slice sched))
    (t
     (start-fresh-slice sched))))

(defun find-slice (schedule qubit-set address-set)
  "Find the earliest slice that doesn't conflict with QUBIT-SET."
  (check-type schedule schedule)
  (check-type qubit-set bit-set)
  (check-type address-set bit-set)
  (loop :with found-slice := nil
        :for slice :in (slices schedule)
        :do (if (and (zerop (logand qubit-set (slice-qubits-used slice)))
                     (zerop (logand address-set (slice-addresses-used slice))))
                (setf found-slice slice)
                (return found-slice))
        :finally (return found-slice)))

(defgeneric schedule-instruction (schedule instruction)
  (:documentation "Schedule the instruction INSTRUCTION into the schedule SCHEDULE.

The allowable instructions are ones that can show up in a BASIC-BLOCK. This excludes:

    * HALT
    * JUMP{-WHEN, UNLESS}
"))

;; Side effect only function.
(defmethod schedule-instruction :around (sched instr)
  (declare (ignore sched instr))
  (call-next-method)
  nil)

(defmethod schedule-instruction ((s schedule) (instr no-operation))
  ;; NOP will force a parallelization break. No need to save the
  ;; instruction anywhere.
  (let ((cs (current-slice s)))
    (setf (slice-qubits-used cs) +all+
          (slice-addresses-used cs) +all+))
  (ensure-fresh-slice s))

(defmethod schedule-instruction ((s schedule) (instr wait))
  (let ((slice (ensure-fresh-slice s)))
    (push instr (slice-instructions slice))
    (setf (slice-qubits-used slice) +all+
          (slice-addresses-used slice) +all+)))

(defmethod schedule-instruction ((s schedule) (instr reset))
  (let ((slice (ensure-fresh-slice s)))
    (push instr (slice-instructions slice))
    (setf (slice-qubits-used slice) +all+)))

(defmethod schedule-instruction ((s schedule) (instr unary-classical-instruction))
  (let* ((address-set (dpb 1
                           (byte 1 (address-value (classical-target instr)))
                           +none+))
         (cs (or (find-slice s +none+ address-set)
                 (start-fresh-slice s))))
    (setf (slice-addresses-used cs)
          (logior address-set (slice-addresses-used cs)))
    (push instr (slice-instructions cs))))

(defmethod schedule-instruction ((s schedule) (instr binary-classical-instruction))
  (let* ((address-set (dpb 1
                           (byte 1 (address-value (classical-right-operand instr)))
                           (dpb 1
                                (byte 1 (address-value (classical-left-operand instr)))
                                +none+)))
         (cs (or (find-slice s +none+ address-set)
                 (start-fresh-slice s))))
    (setf (slice-addresses-used cs)
          (logior address-set (slice-addresses-used cs)))
    (push instr (slice-instructions cs))))

(defun parameters-address-set (params)
  (flet ((addr-set (thing)
           (typecase thing
             (address (dpb 1 (byte 1 (address-value thing)) +none+))
             (segment (dpb -1 (byte (1+ (- (segment-end thing)
                                           (segment-start thing)))
                                    (segment-start thing))
                           +none+))
             (t +none+))))
    (reduce #'logior params
            :key #'addr-set
            :initial-value +none+)))

;; TODO: When DEFCIRCUIT is completed, this should only be for gate
;; applications.
(defmethod schedule-instruction ((s schedule) (instr application))
  (let* ((qubit-set (reduce (lambda (bitset q)
                              (dpb 1 (byte 1 q) bitset))
                            (application-arguments instr)
                            :key #'qubit-index
                            :initial-value +none+))
         (address-set (parameters-address-set
                       (application-parameters instr)))
         (cs (or (find-slice s qubit-set address-set)
                 (start-fresh-slice s))))
    (setf (slice-qubits-used cs)
          (logior qubit-set (slice-qubits-used cs))
          (slice-addresses-used cs)
          (logior address-set (slice-qubits-used cs)))
    (push instr (slice-instructions cs))))

(defmethod schedule-instruction ((s schedule) (instr measure))
  (let* ((qubit-set (dpb 1
                         (byte 1 (qubit-index (measurement-qubit instr)))
                         +none+))
         (address-set (dpb 1
                           (byte 1 (address-value (measure-address instr)))
                           +none+))
         (cs (or (find-slice s qubit-set address-set)
                 (start-fresh-slice s))))
    (setf (slice-qubits-used cs)
          (logior qubit-set (slice-qubits-used cs))
          (slice-addresses-used cs)
          (logior address-set (slice-addresses-used cs)))
    (push instr (slice-instructions cs))))

(defmethod schedule-instruction ((s schedule) (instr measure-discard))
  (let* ((qubit-set (dpb 1
                         (byte 1 (qubit-index (measurement-qubit instr)))
                         +none+))
         (cs (or (find-slice s qubit-set +none+)
                 (start-fresh-slice s))))
    (setf (slice-qubits-used cs)
          (logior qubit-set (slice-qubits-used cs)))
    (push instr (slice-instructions cs))))

(defun basic-block-schedule (bb)
  (if (not (null (%basic-block-schedule bb)))
      (%basic-block-schedule bb)
      (setf (%basic-block-schedule bb)
            (let ((s (make-instance 'schedule)))
              (map nil (lambda (instr)
                         (schedule-instruction s instr))
                   (basic-block-code bb))
              (setf (slices s) (nreverse (slices s)))
              s))))

(defun compute-cfg-schedules (cfg)
  "Compute the schedules for each basic block of the CFG."
  (map nil 'basic-block-schedule (cfg-blocks cfg))
  cfg)

(define-transform parallelize (compute-cfg-schedules)
  "Compute the instruction-parallelized version of a control flow graph.")
