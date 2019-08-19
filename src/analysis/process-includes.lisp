;;;; process-includes.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

(defun process-includes (raw-quil &optional originating-file)
  "Recursively process all INCLUDE instructions in the list RAW-QUIL. The
result is a new sequence with the included quil instructions spliced in."
  (let* ((expanded (cons nil nil))
         (expanded-tail expanded)
         (seen-files (make-hash-table :test 'equalp)))
    (labels ((handle-include (incl originating-file)
               ;; load the file to be included, and recursively
               ;; expand includes within it
               (let ((file (funcall *resolve-include-pathname* (include-pathname incl)))
                     (originating-dir (if (null originating-file)
                                          (pathname (uiop:getcwd))
                                          (uiop:pathname-directory-pathname
                                           originating-file))))
                 (when (uiop:relative-pathname-p file)
                   (setf file (merge-pathnames file originating-dir)))
                 (unless (uiop:file-exists-p file)
                   (error "Could not include ~S because it does not exist." file))
                 (when (gethash file seen-files)
                   (error "Cycle detected in INCLUDe graph: ~S has already been processed." file))
                 (setf (gethash file seen-files) t)
                 (let* ((*current-file* file)
                        (body (parse-quil-into-ast (a:read-file-into-string file))))
                   (expand-all-includes body file))))
             (expand-all-includes (instrs originating-file)
               ;; consume an instruction. if it's an include, we handoff to
               ;; HANDLE-INCLUDE before continuing
               (cond ((endp instrs)
                      (cdr expanded))
                     ((typep (car instrs) 'include)
                      (handle-include (car instrs) originating-file)
                      (expand-all-includes (cdr instrs) originating-file))
                     (t
                      (setf (cdr expanded-tail) instrs)
                      (setf expanded-tail (cdr expanded-tail))
                      (expand-all-includes (cdr instrs) originating-file)))))
      (expand-all-includes raw-quil originating-file))))
