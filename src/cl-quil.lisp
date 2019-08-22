;;;; src/cl-quil.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

(defvar *standard-post-process-transforms*
  '(expand-circuits type-check)
  "The standard transforms that are applied by PARSE-QUIL.")

(defun parse-quil (string &key originating-file
                            (transforms *standard-post-process-transforms*)
                            (ambiguous-definition-handler #'continue))
  "Parse and process the Quil string STRING, which originated from the file
ORIGINATING-FILE. Transforms in TRANSFORMS are applied in-order to the processed
Quil string. In the presence of multiple definitions with a common signature, a
signal is raised, with the default handler specified by AMBIGUOUS-DEFINITION-HANDLER.
"
  (handler-bind
      ((ambiguous-memory-declaration
         (lambda  (c)
           ;; Within a single file, multiple declarations of the same memory region
           ;; (even if equivalent) are disallowed. We thus extent this to declarations across
           ;; multiple files.
           (let ((name (memory-descriptor-name (car (first (ambiguous-definition-conflicts c)))))
                 (recent-file (cdr (first (ambiguous-definition-conflicts c))))
                 (previous-file (cdr (second (ambiguous-definition-conflicts c)))))
             (quil-parse-error "Memory region ~A~@[ (in ~A)~] has already been DECLAREd~@[ (in ~A)~]."
                               name recent-file previous-file))))
       ;; Note: we could generally allow for more sophisticated handling, but for now the default
       ;; is to continue chugging along.
       (ambiguous-gate-or-circuit-definition ambiguous-definition-handler))
      (let* ((*current-file* originating-file)
             (raw-quil (parse-quil-into-raw-program string))
             (pp (resolve-applications
                  (process-includes raw-quil originating-file))))
        (dolist (xform transforms pp)
          (setf pp (transform xform pp))))))

(defun read-quil-file (filespec)
  "Read the Quil file designated by FILESPEC, and parse it as if by PARSE-QUIL."
  (check-type filespec (or string pathname))
  (assert (and (uiop:file-pathname-p filespec)
               (uiop:file-exists-p filespec))
          (filespec)
          "FILESPEC must be a path to a file that exists")
  (parse-quil (a:read-file-into-string filespec)
              :originating-file (first (directory filespec))))
