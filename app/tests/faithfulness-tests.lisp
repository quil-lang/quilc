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
  (finish-output)
  (let ((test-files (uiop:directory-files *faithfulness-test-file-directory* #P"*.quil")))
    (is (not (null test-files)))
    (fresh-line)
    (dolist (file test-files)
      (format t "    Testing file ~A~%" (pathname-name file))
      (let ((*standard-output* (make-broadcast-stream)))
        (is (search "#Matrices are equal"
                    (with-open-file (*standard-input* file :direction :input)
                      (with-output-to-string (*error-output*)
                        (locally
                            (declare #+sbcl(sb-ext:muffle-conditions style-warning))
                          (quilc::%entry-point (list "quilc" "-mP" "--check-sdk-version=no")))))
                    :from-end t))))))
