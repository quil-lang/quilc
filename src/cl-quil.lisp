;;;; src/cl-quil.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

;;; TODO: should quilt calibration expansion be standard?
(defvar *standard-post-process-transforms*
  '(expand-circuits expand-calibrations resolve-waveform-references type-check)
  "The standard transforms that are applied by PARSE-QUIL.")

(defun error-on-ambiguous-memory-declaration  (condition)
  "Handler which signals an error in the presence of an AMBIGUOUS-MEMORY-DEFINITION."
  (when (typep condition 'ambiguous-memory-declaration)
    (destructuring-bind ((mem . file) (other-mem . other-file) &rest cs)
        (ambiguous-definition-conflicts condition)
      (declare (ignore other-mem cs))
      (let* ((name (memory-descriptor-name mem)))
        (quil-parse-error "Memory region ~A~@[ (in ~A)~] has already been DECLAREd~@[ (in ~A)~]."
                          name file other-file)))))

(defun parse-quil (string &key originating-file
                            (transforms *standard-post-process-transforms*)
                            (ambiguous-definition-handler #'continue))
  "Parse and process the Quil string STRING, which originated from the file
ORIGINATING-FILE. Transforms in TRANSFORMS are applied in-order to the processed
Quil string. In the presence of multiple definitions with a common signature, a
signal is raised, with the default handler specified by AMBIGUOUS-DEFINITION-HANDLER.
"
  (handler-bind
      (;; We disallow multiple declarations of the same memory region (even if equivalent).
       (ambiguous-memory-declaration #'error-on-ambiguous-memory-declaration)
       ;; For gate or circuit definitions, the default choice is to "accept the mystery."
       (ambiguous-gate-or-circuit-definition ambiguous-definition-handler)
       ;; TODO change these
       (ambiguous-calibration-definition #'continue)
       (ambiguous-waveform-definition #'continue))
      (let* ((*current-file* originating-file)
             (raw-quil (parse-quil-into-raw-program string))
             (pp (resolve-objects
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
