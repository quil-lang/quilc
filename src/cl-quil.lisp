;;;; src/cl-quil.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

(defvar *standard-post-process-transforms*
  '(process-includes resolve-applications expand-circuits type-check patch-labels)
  "The standard transforms that are applied by PARSE-QUIL-STRING.")

(defun parse-quil-string (string &optional originating-file)
  "Parse and process the Quil string STRING, which originated from the file ORIGINATING-FILE."
  (let ((pp (parse-quil string)))
    (dolist (xform *standard-post-process-transforms* pp)
      (case xform
        (process-includes (setf pp (transform xform pp originating-file)))
        (otherwise        (setf pp (transform xform pp)))))))

(defun read-quil-file (filespec)
  "Read the Quil file designated by FILESPEC, and parse it as if by PARSE-QUIL-STRING."
  (check-type filespec (or string pathname))
  (assert (and (uiop:file-pathname-p filespec)
               (uiop:file-exists-p filespec))
          (filespec)
          "FILESPEC must be a path to a file that exists")
  (parse-quil-string (alexandria:read-file-into-string filespec)
                     (first (directory filespec))))
