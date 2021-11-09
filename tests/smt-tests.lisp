;;; Allegro (and other Lisps) don't support the non-standard "package
;;; local nicknames".
#-(or sbcl ecl ccl)
(rename-package :alexandria :alexandria '(:a))

(fiasco:define-test-package #:cl-quil.smt-tests
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))
  (:use #:cl-quil)

  (:export
   #:run-smt-tests)
  (:shadowing-import-from #:cl-quil #:pi))

(in-package #:cl-quil.smt-tests)

(defun run-smt-tests (&key (verbose nil) (headless nil))
  "Run all SMT tests. If VERBOSE is T, print out lots of test info. If HEADLESS is T, disable interactive debugging and quit on completion."
  ;; Bug in Fiasco commit fe89c0e924c22c667cc11c6fc6e79419fc7c1a8b
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream
                                            *standard-output*))
  (let ((cl-quil::*compress-carefully* t))
    (cond
      ((null headless)
       (run-package-tests :package ':cl-quil.smt-tests
                          :verbose verbose
                          :describe-failures t
                          :interactive t))
      (t
       (let ((successp (run-package-tests :package ':cl-quil.smt-tests
                                          :verbose t
                                          :describe-failures t
                                          :interactive nil)))
         (uiop:quit (if successp 0 1)))))))

(deftest test-segment-instructions ()
  (let ((instrs (mapcar (lambda (args) (apply #'cl-quil:build-gate args))
                        '((H () 0)
                          (Y () 4)
                          (CNOT () 0 1)
                          (CNOT () 1 2)
                          (CZ () 0 1)
                          (CNOT () 2 3)
                          (X () 3)
                          (Z () 4)))))
    (labels ((inst (i)
               (elt instrs i))
             (idx-seg (qubits &rest idxs)
               (make-instance 'cl-quil::2q-segment
                 :qubits qubits
                 :instrs (mapcar #'inst idxs)))
             (segment= (a b)
               (and
                (equalp (cl-quil::2q-segment-qubits a)
                        (cl-quil::2q-segment-qubits b))
                (equalp (cl-quil::2q-segment-instrs a)
                        (cl-quil::2q-segment-instrs b)))))
      (multiple-value-bind (segments free)
          (cl-quil::segment-instructions instrs)
        (let ((expected-segments (list (idx-seg '(0 1) 0 2)
                                       (idx-seg '(1 2) 3)
                                       (idx-seg '(0 1) 4)
                                       (idx-seg '(2 3) 5 6)))
              (expected-free (list (inst 1) (inst 7))))
          (is (cl-quil.frontend::list= segments expected-segments :test #'segment=))
          (is (equalp free expected-free)))))
    t))

