;;;; print-program
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil.frontend)

(defun print-instruction-sequence (seq
                                   &key
                                     (stream *standard-output*)
                                     (prefix ""))
  (let ((*print-pretty* nil))
    (flet ((print-one-line (instr)
             (write-string prefix stream)
             (print-instruction instr stream)
             (a:when-let ((c (comment instr)))
               (format stream "~40T# ~A" c))
             (terpri stream)))
      (map nil #'print-one-line seq))))

(defgeneric print-parsed-program-generic (parsed-program stream)
  (:documentation "Print the program PARSED-PROGRAM nicely to the stream STREAM.")
  (:method ((pp parsed-program) (stream stream))
    (flet ((print-definitions (defns)
             (dolist (defn defns)
               (print-instruction defn stream)
               (terpri stream)))
           (print-externs (externs)
             (loop :for name :being :the :hash-keys :of externs
                   :do (print-instruction (make-instance 'extern :name name) stream)
                       (fresh-line stream)))
           (print-stubs (stubs)
             (loop :for name :being :the :hash-keys :of stubs
                   :do (print-instruction (make-instance 'stub :name name) stream)
                       (fresh-line stream))))
      
      ;; Ensure that any non-standard gates in the program are defined
      ;; TODO: handle non-simple gates
      (let ((defined-gate-names
              (append (mapcar #'gate-definition-name (parsed-program-gate-definitions pp))
                      (loop :for k :being :the :hash-key :of **default-gate-definitions**
                            :collect k)))
            (defgates (parsed-program-gate-definitions pp))
            (stubs (parsed-program-stub-operations pp))
            (externs (parsed-program-extern-declarations pp))
            (simple-gates (map 'list
                               #'gate-application-gate
                               (remove-if-not (lambda (inst)
                                                (and (typep inst 'gate-application)
                                                     (slot-boundp inst 'name-resolution) ; TODO: this shouldn't be necessary
                                                     (typep (gate-application-gate inst)
                                                            'simple-gate)))
                                              (parsed-program-executable-code pp)))))
        (dolist (gate simple-gates)
          (unless (member (slot-value gate 'name) defined-gate-names)
            (push (make-instance 'static-gate-definition :name (slot-value gate 'name) :entries (coerce (slot-value (simple-gate-matrix gate) 'magicl::storage) 'list)) defgates)))
        
        (print-externs externs)
        (print-stubs stubs)
        (print-definitions (parsed-program-memory-definitions pp))

        ;; instructions and single-line definitions (e.g. DECLARE) do
        ;; not introduce newlines so here we add one if needed
        (unless (endp (parsed-program-memory-definitions pp))
          (terpri stream))
        (print-definitions defgates)
        (print-definitions (parsed-program-circuit-definitions pp))

        (print-instruction-sequence (parsed-program-executable-code pp) :stream stream)))))


