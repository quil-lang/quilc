;; smt-tests.lisp
;;
;; Author: Erik Davis

(fiasco:define-test-package #:cl-quil.smt-tests
  (:use #:cl-quil #:cl-quil.smt)

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
               (make-instance 'cl-quil.smt::2q-segment
                 :qubits qubits
                 :instrs (mapcar #'inst idxs)))
             (segment= (a b)
               (and
                (equalp (cl-quil.smt::2q-segment-qubits a)
                        (cl-quil.smt::2q-segment-qubits b))
                (equalp (cl-quil.smt::2q-segment-instrs a)
                        (cl-quil.smt::2q-segment-instrs b)))))
      (multiple-value-bind (segments free)
          (cl-quil.smt::segment-instructions instrs)
        (let ((expected-segments (list (idx-seg '(0 1) 0 2)
                                       (idx-seg '(1 2) 3)
                                       (idx-seg '(0 1) 4)
                                       (idx-seg '(2 3) 5 6)))
              (expected-free (list (inst 1) (inst 7))))
          (is (cl-quil.frontend::list= segments expected-segments :test #'segment=))
          (is (equalp free expected-free)))))
    t))

;; this is akin to what we do in the addressing tests, but duplicated here so as not to depend on them
(defun address-and-reconstitute-program (pp chip &rest args)
  (multiple-value-bind (instrs initial-l2p final-l2p)
      (apply #'do-constraint-based-addressing
	     (coerce (parsed-program-executable-code pp) 'list)
	     chip
	     args)
    (cond (( = 1 (length instrs))
	   (setf (cl-quil::comment (first instrs))
		 (cl-quil::make-rewiring-comment :entering initial-l2p :exiting final-l2p)))
	  (t
	   (setf (cl-quil::comment (first instrs))
		 (cl-quil::make-rewiring-comment :entering initial-l2p)
		 (cl-quil::comment (first (last instrs)))
		 (cl-quil::make-rewiring-comment :exiting final-l2p))))
    (make-instance 'cl-quil:parsed-program
		   :executable-code (coerce instrs 'vector))))

;; ditto
(defun logical-matrices-agree (pp1 pp2)
  (is
      (cl-quil::operator=
       (cl-quil::parsed-program-to-logical-matrix pp1)
       (cl-quil::parsed-program-to-logical-matrix pp2))))

(defun program-satisfies-connectivity-constraints (pp chip)
  (loop :for instr :across (parsed-program-executable-code pp)
	:always (is (typecase instr
		      ((or application measurement) (cl-quil::lookup-hardware-address chip instr))
		      (t t)))))

(deftest test-tb-olsq-1qs ()
  (let ((chip (cl-quil::build-nq-linear-chip 3)))
    (dolist (pp (list (parse-quil "X 0") (parse-quil "X 0; X 0") (parse-quil "X 0; X 1")))
      (dotimes (num-blocks 3)
	(let ((addressed (address-and-reconstitute-program pp chip :scheme ':tb-olsq :num-blocks num-blocks)))
	  (logical-matrices-agree pp addressed)
	  (program-satisfies-connectivity-constraints addressed chip))))))

(deftest test-tb-olsq-various-num-blocks ()
  (let ((pp (parse-quil "X 0; CZ 0 1; CZ 0 2; X 1; CZ 1 2; CZ 0 1; X 2; CZ 0 2"))
	(chip (cl-quil::build-nq-linear-chip 3)))
    ;; no blocks specified
    (signals addressing-failed
      (address-and-reconstitute-program pp chip :scheme ':tb-olsq))
    ;; not enough blocks
    (signals addressing-failed
      (address-and-reconstitute-program pp chip :scheme ':tb-olsq :num-blocks 1))
    ;; general case: same logical matrices, but chip constraints are met
    (let ((addressed
	    (address-and-reconstitute-program pp chip :scheme ':tb-olsq :num-blocks 3)))
      (is (logical-matrices-agree pp addressed))
      (is (program-satisfies-connectivity-constraints addressed chip)))))
