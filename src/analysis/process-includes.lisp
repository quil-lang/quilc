;;;; process-includes.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

(define-transform process-includes (process-includes)
  "This transform expands (and therefore eliminates) INCLUDE directives."
  )

(defun find-include (pp)
  "Given a PARSED-PROGRAM PP, find the first INCLUDE instruction. Return two values: the instruction and its position. If the directive isn't found, return NIL."
  (loop :for i :from 0
        :for isn :across (parsed-program-executable-code pp)
        :when (typep isn 'include)
          :do (return (values isn i))
        :finally (return nil)))

(defun splice-code-at (code i code-to-splice)
  "Replace the Ith element of CODE with the sequence CODE-TO-SPLICE."
  (concatenate 'vector
               (subseq code 0 i)
               code-to-splice
               (subseq code (1+ i))))

(defun handle-include (pp incl pos originating-dir)
  (let ((file (funcall *resolve-include-pathname* (include-pathname incl))))
    (when (uiop:relative-pathname-p file)
      (setf file (merge-pathnames file originating-dir)))
    (unless (uiop:file-exists-p file)
      (error "Could not include ~S because it does not exist." file))
    (let ((incl-pp (parse-quil-into-raw-program (alexandria:read-file-into-string file))))
      (setf (parsed-program-gate-definitions pp)
            (append (parsed-program-gate-definitions pp)
                    (parsed-program-gate-definitions incl-pp)))
      (setf (parsed-program-circuit-definitions pp)
            (append (parsed-program-circuit-definitions pp)
                    (parsed-program-circuit-definitions incl-pp)))
      (setf (parsed-program-executable-code pp)
            (splice-code-at (parsed-program-executable-code pp)
                                        pos
                                        (parsed-program-executable-code incl-pp)))
      pp)))

(defun process-includes (pp originating-file)
  "Process all INCLUDE forms by reading files off the disk. Produces a new PARSED-PROGRAM object."
  (let ((originating-directory (if (null originating-file)
                                   (pathname (uiop:getcwd))
                                   (uiop:pathname-directory-pathname
                                    originating-file))))
    (labels ((expand-all-includes (pp)
               (multiple-value-bind (incl pos) (find-include pp)
                 (if (null incl)
                     pp
                     (expand-all-includes
                      (handle-include pp incl pos originating-directory))))))
      (expand-all-includes pp))))
