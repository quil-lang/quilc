;;;; tests/suite.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil-tests)

(defun run-cl-quil-tests (&key (verbose nil) (headless nil) (num-threads 1))
  "Run all CL-QUIL tests. If VERBOSE is T, print out lots of test info. If HEADLESS is T, disable interactive debugging and quit on completion. If NUM-THREADS is non-nil, it is the number of threads to use. "
  (check-type num-threads (integer 1))
  ;; Bug in Fiasco commit fe89c0e924c22c667cc11c6fc6e79419fc7c1a8b
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream
                                            *standard-output*))
  (let ((quil::*compress-carefully* t)
        ;; At least two threads will always be created. Tests, within
        ;; their own threads, may request more threads to run
        ;; sub-tests. If there were only one thread available, the
        ;; sub-test's thread would wait for its parent thread
        ;; (i.e. the parent test) to complete before it could
        ;; start. Thus causing a deadlock.
        (lparallel:*kernel* (lparallel:make-kernel (max 2 num-threads))))
    (cond
      ((< 1 num-threads)
       (let ((tests (fiasco::children-of
                     (fiasco::find-suite-for-package
                      (fiasco::find-package ':cl-quil-tests)))))
         (lparallel:pmapc (lambda (f)
                            (funcall f)
                            (force-output)
                            (format t "."))
                          (loop :for test :being :the :hash-values :of tests
                                :collect (fiasco::name-of test)))
         (terpri)))
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
