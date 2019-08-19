;;;; boondoggle/tests/suite.lisp
;;;;
;;;; Author: Chris Osborn

(in-package #:boondoggle-tests)

(defun run-boondoggle-tests ()
  "Run boondoggle tests."
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream *standard-output*))
  (run-package-tests :package ':boondoggle-tests
                     :verbose t
                     :describe-failures t
                     :interactive t))
