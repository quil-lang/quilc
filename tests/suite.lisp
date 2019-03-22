;;;; tests/suite.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil-tests)

(defun run-cl-quil-tests (&key (verbose nil) (headless nil) (parallel nil))
  "Run all CL-QUIL tests. If VERBOSE is T, print out lots of test info. If HEADLESS is T, disable interactive debugging and quit on completion. If PARALLEL is non-nil, it is the number of threads to use."
  ;; Bug in Fiasco commit fe89c0e924c22c667cc11c6fc6e79419fc7c1a8b
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream
                                            *standard-output*))
  (let ((quil::*compress-carefully* t))
    (cond
      (parallel
       (setf lparallel:*kernel* (lparallel:make-kernel parallel))
       (let ((fiasco::*debug-on-unexpected-error* nil)
             (fiasco::*debug-on-assertion-failure* nil)
             (fiasco::*pretty-log-stream*
               (make-instance 'fiasco::column-counting-output-stream
                              :understream *standard-output*))
             (fiasco::*run-test-function* #'pretty-run-test)
             (fiasco::*context* nil))
         (let ((tests (fiasco::children-of
                       (fiasco::find-suite-for-package
                        (fiasco::find-package ':cl-quil-tests)))))
           (lparallel:pmapc (lambda (f)
                              (let ((fiasco::*pretty-log-stream* nil)
                                    (fiasco::*print-test-run-progress* nil)
                                    (fiasco::*pretty-log-verbose-p* nil)
                                    (fiasco::*test-run-standard-output* (make-broadcast-stream))
                                    (*debug-io* (make-broadcast-stream))
                                    )
                                (funcall f)
                                (format fiasco::*pretty-log-stream* ".")))
                            (loop :for test :being :the :hash-values :of tests
                                  :collect (fiasco::name-of test))))))
      ((null headless)
       (run-package-tests :package ':cl-quil-tests
                          :verbose verbose
                          :describe-failures t
                          :interactive t))
      (t
       (let ((successp (run-package-tests :package ':cl-quil-tests
                                          :verbose t
                                          :describe-failures t
                                          :interactive nil)))
         (uiop:quit (if successp 0 1)))))))

(defun pretty-run-test (test function)
  (let* ((*error-output* fiasco::*pretty-log-stream*)
         (*standard-output* fiasco::*pretty-log-stream*)
         (fiasco::*pretty-log-stream* nil)
         (fiasco::*test-run-standard-output* (make-broadcast-stream))
         (retval-v-list (multiple-value-list
                         (fiasco::run-test-body-in-handlers test function))))
    (values-list retval-v-list)))
