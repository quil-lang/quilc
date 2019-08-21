;;;; process-includes.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

(defun process-includes (raw-quil &optional originating-file)
  "Recursively process all INCLUDE instructions in the list RAW-QUIL. The
result is a new sequence with the included quil instructions spliced in."
  (let* (;; We track the expanded list of instructions, and build it from its tail out
         (expanded (cons nil nil))
         (expanded-tail expanded)
         ;; We track an active path of files from the "root" to whatever we are processing
         ;; at the moment, in order to check for cycles.
         (seen-files (when originating-file
                       (list (truename originating-file)))))
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
                   (quil-parse-error "Could not include ~S because it does not exist." file))
                 (setf file (truename file))
                 (when (member file seen-files :test #'uiop:pathname-equal)
                   (quil-parse-error "Cyclic INCLUDE detected: ~S is being INCLUDEd but has already been processed." file))
                 (let* ((*current-file* file)
                        (body (parse-quil-into-raw-program (a:read-file-into-string file))))
                   (push file seen-files)
                   (unwind-protect (expand-all-includes body file)
                     (pop seen-files)))))
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
