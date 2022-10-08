;;; segment-2q.lisp
;;;
;;; Author: Erik Davis
;;;
;;; Many constraint-based addressing schemes are entirely concerned
;;; with the problem of 2Q gate layout, since the placement of 1Q
;;; operators is rather trivial. To support addressing of mixed
;;; programs, we add a 2Q-SEGMENT pseudoinstruction which contains an
;;; ordered sequence of instructions acting on (some subset of) a pair
;;; of qubits. In this way, 1Q gates can go 'along for the ride' with
;;; some neighboring 2Q gate.

(in-package #:cl-quil.smt)

;;; TODO: worth making this NQ-SEGMENT? we won't use it, but perhaps someone else might.
(defclass 2q-segment (instruction)
  ((qubits :initarg :qubits
           :accessor 2q-segment-qubits
           :documentation "The logical qubit indices associated with the 2Q segment.")
   (instrs :initarg :instrs
           :accessor 2q-segment-instrs
           :initform nil
           :documentation "An ordered list of instructions associated with this 2Q segment."))
  (:documentation "A pseudoinstruction representing some contiguous segment of instructions acting on at most the indicated pair of qubits."))

(defmethod cl-quil.frontend::print-instruction-generic ((instr 2q-segment) stream)
  (format stream "2Q-SEGMENT {~{~/cl-quil:instruction-fmt/~^;~^ ~}}" (2q-segment-instrs instr)))

(defun rewire-2q-segment (segment p0 p1)
  "Rewire the instructions associated with SEGMENT to act on qubits P0 and P1.

Returns a new segment."
  (destructuring-bind (q0 q1) (2q-segment-qubits segment)
    (flet ((rewire (qubit)
             (let ((q (qubit-index qubit)))
               (cond ((= q q0) p0)
                     ((= q q1) p1)
                     (t (error "Unexpected qubit ~D" q))))))
      (make-instance '2q-segment
        :qubits (list p0 p1)
        :instrs (loop :for gate :in (2q-segment-instrs segment)
                      :collect (apply #'build-gate
                                      (application-operator gate)
                                      (application-parameters gate)
                                      (mapcar #'rewire (application-arguments gate))))))))

(defun segment-instructions (instrs)
  "Attempt to partition the list of instructions INSTRS into 2Q segments.

For example, H 0; CNOT 0 1; CNOT 1 2; X 0; X 1; Y 3 may get segmented as

  2Q-SEGMENT {H 0; CNOT 0 1; X 0}
  2Q-SEGMENT {CNOT 1 2; X 1}

with gate Y 3 a 'free 1Q gate', since qubit 3 is not touched by any 2Q operations.

Returns three values: a list of segments, a list of free 1Q gates, and the set of logical qubits used."
  (let ((segments nil)
	(qubits nil)
        (1q-free (make-hash-table))
        (neighbors (make-hash-table))
        (2q-queues (make-hash-table)))
    ;; NOTE: we build segments in REVERSE order. pay attention to PUSH & APPEND
    (labels ((key (q0 q1)
               (if (< q0 q1)
                   (complex q0 q1)
                   (complex q1 q0)))
             (ensure-queued (instrs q0 q1 &key prepend)
               (let ((k (key q0 q1)))
                 (alexandria:if-let ((segment (gethash k 2q-queues)))
                   (setf (2q-segment-instrs segment)
                         (if prepend
                             (append (2q-segment-instrs segment) instrs)
                             (append instrs (2q-segment-instrs segment))))
                   (setf (gethash k 2q-queues) (make-instance '2q-segment
                                                 :qubits (list q0 q1)
                                                 :instrs instrs)))))
             (flush-queue (q0 q1)
               (alexandria:when-let ((segment (gethash (key q0 q1) 2q-queues)))
                 (setf (2q-segment-instrs segment) (nreverse (2q-segment-instrs segment)))
                 (push segment segments)
                 (setf (gethash (key q0 q1) 2q-queues) nil)))
             (flush-all-queues ()
               (loop :for k :being :the :hash-key :of 2q-queues
                     :do (flush-queue (realpart k) (imagpart k))))
             (add-1q-instr (instr q0)
               ;; try to stash on an existing segment
               (loop :for q1 :in (gethash q0 neighbors)
                     :for segment := (gethash (key q0 q1) 2q-queues)
                     :when segment
                       :do (push instr (2q-segment-instrs segment))
                           (return-from add-1q-instr nil))
               ;; otherwise, add to free list
               (push instr (gethash q0 1q-free)))
             (add-2q-instr (instr q0 q1)
               ;; ensure in adjacency
               (setf (gethash q0 neighbors) (adjoin q1 (gethash q0 neighbors))
                     (gethash q1 neighbors) (adjoin q0 (gethash q1 neighbors)))
               ;; flush 2q segments if need be
               (dolist (q (list q0 q1))
                 (loop :for q2 :in (gethash q neighbors)
                       :unless (or (= q0 q2) (= q1 q2))
                         :do (flush-queue q q2)))
               ;; push instr
               (ensure-queued (list instr) q0 q1)
               ;; see if we can steal any free 1q instructions
               (dolist (q (list q0 q1))
                 (alexandria:when-let ((free (gethash q 1q-free)))
                   (ensure-queued free q0 q1 :prepend t)
                   (setf (gethash q 1q-free) nil)))))
      (dolist (instr instrs)
        (typecase instr
          (gate-application
           (let ((args (mapcar #'qubit-index (application-arguments instr))))
             (case (length args)
               (1 (add-1q-instr instr (first args)))
               (2 (add-2q-instr instr (first args) (second args)))
               (otherwise
                (addressing-failed "Instruction ~/cl-quil:instruction-fmt/ has unsupported arity" instr)))
	     (dolist (q args)
	       (pushnew q qubits))))
          (otherwise
           (addressing-failed "Unsupported instruction ~/cl-quil:instruction-fmt/" instr))))
      (flush-all-queues)
      ;; remaining 1qs are free
      (values (nreverse segments)
              (loop :for q :being :the :hash-key :of 1q-free
                      :using (hash-value instrs)
                    :append (nreverse instrs))
	      qubits))))
