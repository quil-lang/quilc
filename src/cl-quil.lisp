;;;; src/cl-quil.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

(defvar *standard-post-process-transforms*
  '(resolve-applications expand-circuits type-check)
  "The standard transforms that are applied by PARSE-QUIL.")

(defun parse-quil (string &key originating-file (transforms *standard-post-process-transforms*))
  "Parse and process the Quil string STRING, which originated from the file ORIGINATING-FILE. Transforms in TRANSFORMS are applied in-order to the processed Quil string."
  (let* ((*current-file* originating-file)
         (pp (parse-quil-into-raw-program string originating-file)))
    (dolist (xform transforms pp)
      (setf pp (transform xform pp)))))

(defun read-quil-file (filespec)
  "Read the Quil file designated by FILESPEC, and parse it as if by PARSE-QUIL."
  (check-type filespec (or string pathname))
  (assert (and (uiop:file-pathname-p filespec)
               (uiop:file-exists-p filespec))
          (filespec)
          "FILESPEC must be a path to a file that exists")
  (parse-quil (a:read-file-into-string filespec)
              :originating-file (first (directory filespec))))
