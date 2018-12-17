;;;; faithfulness-tests.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; Ensures that quilc's output matches its input,, according to the -m switch.

(in-package #:quilc-tests)

(defparameter *faithfulness-test-file-directory*
  (asdf:system-relative-pathname
   ':quilc-tests
   "tests/faithfulness-test-files/"))

(deftest test-faithfulness ()
  "Test whether compilation preserves semantic equivalence for some test programs."
  (finish-output *debug-io*)
  (dolist (file (uiop:directory-files *faithfulness-test-file-directory* #P"*.quil"))
    (format *debug-io* "    Testing file ~a~%" (pathname-name file))
    (let ((*error-output* (make-broadcast-stream))
          (*standard-output* (make-broadcast-stream)))
      (with-open-file (*standard-input* file :direction :input)
        (locally
            (declare #+sbcl(sb-ext:muffle-conditions common-lisp:style-warning))
          (quilc::%entry-point (list "quilc" "-mp")))))))
