;;;; compress-qubits.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil/frontend)

;;; This file contains an analysis to compute the qubits used by a
;;; program.

(defgeneric qubits-used (isn)
  (:documentation "Helper function to determine the qubits used by a Quil instruction (classical or quantum), or a PARSED-PROGRAM.")
  (:method ((isn instruction))
    nil)
  (:method ((pseudo-isn jump-target))
    nil)
  (:method ((isn measurement))
    (list (qubit-index (measurement-qubit isn))))
  (:method ((isn application))
    (remove-if #'null
               (map 'list (lambda (arg)
                            (if (qubit-p arg)
                                (qubit-index arg)
                                nil))
                    (application-arguments isn))))
  (:method ((isn reset-qubit))
    (list (qubit-index (reset-qubit-target isn))))
  (:method ((parsed-program parsed-program))
    (reduce #'nunion (parsed-program-executable-code parsed-program)
            :key #'qubits-used
            :initial-value nil)))

(defun qubit-relabeler (mapping)
  "Given a mapping, a sequence of elements q_1 q_2 ... q_n which represents the map q_i -> i, return a function whose lambda list is

    (QUBIT &KEY DONT-CHOKE)

where QUBIT is a QUBIT object which will get mutated according to the mapping, and DONT-CHOKE (default: NIL) specifies whether or not an error should occur should the qubit being relabeled is not specified in the mapping.

Return T if a relabeling happened, and NIL otherwise.

N.B. In this case, a \"relabeling\" *only* means that a mutation of the QUBIT object happened. It does *not* mean that the index of the qubit changed. In other words, it is possible for the return value to be T but the index to remain the same.
"
  (let ((seen nil))
    (lambda (q &key dont-choke)
      (unless (member q seen :test #'eq)
        (push q seen)
        (let ((to (position (qubit-index q) mapping)))
          (cond
            ((null to)
             (unless dont-choke
               (error "The qubit ~A has no mapping, according to the map ~A." q mapping))
             nil)                       ; Return value.
            (t
             (setf (qubit-index q) to)
             t)))))))                   ; Return value.

(defun compute-qubit-mapping (parsed-prog)
  (sort (coerce (qubits-used parsed-prog) 'vector) #'<))

(defun relabel-rewiring (rewiring relabeler)
  "Apply a RELABELER to REWIRING. It may be the case that RELABELER
has no assignments for certain indices; such unassigned indices get
relabeled according to which free assignments remain."
  (let ((l2p-vector (rewiring-l2p rewiring))) ; only need to work with l2p vector
    (let ((num-qubits (length l2p-vector))
          (qubit-list (map 'list #'qubit l2p-vector))
          (indices-used nil)
          (to-relabel nil))
      ;; first, apply the relabeler where possible
      (dolist (q qubit-list)
        (cond ((funcall relabeler q :dont-choke t)
               (push (qubit-index q) indices-used))
              (t
               (push q to-relabel))))
      (setf indices-used (sort indices-used #'<))
      ;; now, relabel stuff we missed in the first pass, but in the
      ;; original order (to make the resulting rewiring trimmable)
      (setf to-relabel (nreverse to-relabel))
      (dotimes (i num-qubits)
        (cond ((eql i (first indices-used))
               (pop indices-used))
              (t
               (setf (qubit-index (pop to-relabel)) i))))
      (make-rewiring-from-l2p
       (map 'vector #'qubit-index qubit-list)))))

(defgeneric %relabel-qubits (isn relabeler)
  (:documentation "Relabel the qubits in the instruction ISN according to the relabeling function RELABELER. RELABELER should be a function that destructively modifies qubits.")

  (:method ((isn jump-target) relabeler)
    (declare (ignore relabeler))
    nil)

  (:method ((isn instruction) relabeler)
    (declare (ignore relabeler))
    nil)

  (:method :after ((isn instruction) relabeler)
    ;; If INSTRUCTION has a rewiring comment attached, update it.
    (a:when-let (comment (comment isn))
      (setf (comment isn)
            (ecase (rewiring-comment-type comment)
              ((:ENTERING)
               (make-rewiring-comment :entering (relabel-rewiring (parse-entering-rewiring comment)
                                                                  relabeler)))
              ((:EXITING)
               (make-rewiring-comment :exiting (relabel-rewiring (parse-exiting-rewiring comment)
                                                                 relabeler)))
              ((:ENTERING/EXITING)
               (multiple-value-bind (entering-rewiring exiting-rewiring)
                   (parse-entering/exiting-rewiring comment)
                 (make-rewiring-comment :entering (relabel-rewiring entering-rewiring relabeler)
                                        :exiting (relabel-rewiring exiting-rewiring relabeler))))))))

  (:method ((isn measurement) relabeler)
    (funcall relabeler (measurement-qubit isn)))

  (:method ((isn application) relabeler)
    (map 'nil (lambda (arg)
                (when (qubit-p arg)
                  (funcall relabeler arg)))
         (application-arguments isn)))

  (:method ((isn pragma) relabeler)
    (unless (typep isn 'specialized-pragma)
      (warn "Unhandled pragma in qubit relabeling: ~S" isn)))

  (:method ((isn pragma-expected-rewiring) relabeler)
    (setf (pragma-rewiring isn)
          (relabel-rewiring (pragma-rewiring isn) relabeler)))

  (:method ((isn pragma-current-rewiring) relabeler)
    (setf (pragma-rewiring isn)
          (relabel-rewiring (pragma-rewiring isn) relabeler)))

  (:method ((isn pragma-add-kraus) relabeler)
    (let ((qubit-list (mapcar #'qubit (pragma-qubit-arguments isn)))
          ;; If we had a relabel fail, then we don't need the PRAGMA.
          (bad-relabel nil))
      (mapc (lambda (q)
              (unless (funcall relabeler q :dont-choke t)
                (setf bad-relabel t)))
            qubit-list)
      (cond
        (bad-relabel (change-class isn 'no-operation))
        (t
         (setf (pragma-qubit-arguments isn)
               (mapcar #'qubit-index qubit-list))))))

  (:method ((isn pragma-readout-povm) relabeler)
    (let ((qubit (qubit (pragma-qubit-index isn)))
          (bad-relabel nil))
      (unless (funcall relabeler qubit :dont-choke t)
        (setf bad-relabel t))
      (cond
        (bad-relabel (change-class isn 'no-operation))
        (t
         (setf (pragma-qubit-index isn) (qubit-index qubit))))))

  (:method ((isn reset-qubit) relabeler)
    (funcall relabeler (reset-qubit-target isn))))

(defun compress-qubits (parsed-prog)
  "Mutate PARSED-PROG so that all qubits get relabeled to a minimum relabeling."
  (let ((relabeler (qubit-relabeler (compute-qubit-mapping parsed-prog))))
    (map 'nil
         (lambda (isn)
           (%relabel-qubits isn relabeler))
         (parsed-program-executable-code parsed-prog)))
  parsed-prog)

(define-transform compress-qubits (compress-qubits)
  "Relabel the qubits so that they are minimally numbered.")
