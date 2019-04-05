;;;; faithfulness-tests.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; Ensures that quilc's output matches its input, according to the -m switch.

(in-package #:quilc-tests)

(defparameter *faithfulness-test-file-directory*
  (asdf:system-relative-pathname
   ':quilc-tests
   "app/tests/faithfulness-test-files/"))

(deftest test-faithfulness ()
  "Test whether compilation preserves semantic equivalence for some test programs."
  (finish-output *debug-io*)
  (let ((test-files (uiop:directory-files *faithfulness-test-file-directory* #P"*.quil")))
    (is (not (null test-files)))
    (fresh-line)
    (dolist (file test-files)
      (format t "    Testing file ~a~%" (pathname-name file))
      (let ((*standard-output* (make-broadcast-stream))
            ;; quilc::process-options sets *protoquil* and *compute-matrix-reps* when passed "-mP"
            ;; flags. Bind them here so that the global bindings are not affected; otherwise,
            ;; subsequent tests that depend on the default values will fail.
            quilc::*protoquil*
            quilc::*compute-matrix-reps*)
        (is (search "#Matrices are equal"
                    (with-open-file (*standard-input* file :direction :input)
                      (with-output-to-string (*error-output*)
                        (locally
                            (declare #+sbcl(sb-ext:muffle-conditions style-warning))
                          (quilc::%entry-point (list "quilc" "-mP")))))
                    :from-end t))))))
