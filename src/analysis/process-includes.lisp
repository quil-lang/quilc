;;;; process-includes.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil/frontend)

(defun process-includes (raw-quil &optional originating-file)
  "Recursively process all INCLUDE instructions in the list RAW-QUIL. The result is a new sequence with the included quil instructions spliced in."
  (let* (;; We track the expanded list of instructions, and build it from its tail out
         (expanded (cons nil nil))
         (expanded-tail expanded)
         ;; We track an active path of files from the "root" to whatever we are processing
         ;; at the moment, in order to check for cycles.
         (seen-files (if originating-file
                         (list (truename originating-file))
                         nil)))
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
                   (quil-parse-error "Could not include ~S because it does not exist." (namestring file)))
                 ;; canonicalize the name for the sake of detecting cycles
                 (setf file (truename file))
                 (when (member file seen-files :test #'uiop:pathname-equal)
                   (quil-parse-error "Cyclic INCLUDE detected: ~S is being INCLUDEd but has already been processed." file))
                 (let ((body
                         (let ((*current-file* file))
                           (parse-quil-into-raw-program (a:read-file-into-string file)))))
                   (push file seen-files)
                   (prog1 (expand-all-includes body file)
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
                      (let ((recycled-cons instrs)
                            (remaining-instrs (cdr instrs)))
                        ;; Share the head cons of INSTRS in our result, which
                        ;; also contains a non-INCLUDE as the CAR
                        (rplacd expanded-tail recycled-cons)
                        ;; Set the CDR of this cons to NIL. This isn't
                        ;; necessary, but makes sure later modifications of this
                        ;; code don't mistakenly traverse it.
                        (rplacd recycled-cons nil)
                        ;; Set the tail to our head cons (slightly used, still in good condition)
                        (setf expanded-tail recycled-cons)
                        ;; Proceed with the rest of the journey
                        (expand-all-includes remaining-instrs originating-file))))))
      (expand-all-includes raw-quil originating-file))))
