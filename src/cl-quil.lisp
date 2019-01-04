;;;; src/cl-quil.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

(defun parse-quil-string (string &optional originating-file)
  "Parse and process the Quil string STRING, which originated from the file ORIGINATING-FILE."
  (let ((pp (parse-quil string)))
    (setf pp (transform 'process-includes pp originating-file))
    (setf pp (transform 'resolve-applications pp))
    (setf pp (transform 'expand-circuits pp))
    (setf pp (transform 'type-check pp))
    (setf pp (transform 'patch-labels pp))
    pp))

(defun read-quil-file (filespec)
  "Read the Quil file designated by FILESPEC, and parse it as if by PARSE-QUIL-STRING."
  (check-type filespec (or string pathname))
  (assert (uiop:file-pathname-p filespec) (filespec) "FILESPEC must be a path to a file name")
  (parse-quil-string (alexandria:read-file-into-string filespec)
                     (first (directory filespec))))
