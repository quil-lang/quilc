;;;; tests/suite.lisp
;;;;
;;;; Author: Kartik Singh

(in-package #:libquilc-tests)

(defun clean-up ()
  (uiop:with-current-directory ("lib/")
    (uiop:run-program '("make" "clean")))
  (uiop:with-current-directory ("lib/tests/c/")
    (uiop:run-program '("make" "clean"))))

(defun run-libquilc-tests (&key (verbose nil) (headless nil))
  "Run all libquilc tests. If VERBOSE is T, print out lots of test info. If HEADLESS is T, disable interactive debugging and quit on completion."
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream *standard-output*))
  (cond
    ((null headless)
     (run-package-tests :package ':libquilc-tests
                        :verbose verbose
                        :describe-failures t
                        :interactive t)
     (clean-up))
    (t
     (let ((successp (run-package-tests :package ':libquilc-tests
                                        :verbose t
                                        :describe-failures t
                                        :interactive nil)))
       (clean-up)
       (uiop:quit (if successp 0 1))))))
