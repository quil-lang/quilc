;;;; misc.lisp
;;;;
;;;; Author: Mark Skilbeck

(in-package #:quilc-tests)

(deftest test-update-available ()
  (with-mocked-function-definitions
      ((quilc::query-latest-sdk-version (lambda (&rest args)
                                          (declare (ignore args))
                                          "1.0.0")))
    (multiple-value-bind (update-available-p update)
        (quilc::sdk-update-available-p "1.5.0")
      (declare (ignore update))
      (is (not update-available-p)))

    (multiple-value-bind (update-available-p update)
        (quilc::sdk-update-available-p "0.1.0")
      (declare (ignore update))
      (is update-available-p))))

(deftest test-process-program ()
  (let ((progm "H 0")
        (chip (cl-quil::build-nq-fully-connected-chip 2))
        (stats '("topological_swaps" "logical_schedule" "gate_depth" "gate_volume"
                 "program_duration" "program_fidelity" "unused_qubits" "multiqubit_gate_depth")))
    ;; Test that a direct call to PROCESS-PROGRAM respects protoquil
    ;; and returns a statistics dictionary.
    (let ((pp (cl-quil:parse-quil progm)))
      (multiple-value-bind (processed-program statistics)
          (quilc::process-program pp chip :protoquil t)
        (is (cl-quil:protoquil-program-p processed-program))
        (dolist (stat stats)
          (is (nth-value 1 (gethash stat statistics))))))
    ;; Likewise, test that without :PROTOQUIL T there is an empty
    ;; stats dictionary.
    (let ((pp (cl-quil:parse-quil progm)))
      (multiple-value-bind (_ statistics)
          (quilc::process-program pp chip :protoquil nil)
        (declare (ignore _))
        (dolist (stat stats)
          (is (not (nth-value 1 (gethash stat statistics)))))))))

(deftest test-special-bindings-let* ()
  (dolist (thing '(42 "a string" :a-keyword quoted-symbol (a cons)))
    (is (eq thing (quilc::special-bindings-let* ((not-special thing))
                    (bt:join-thread (bt:make-thread (lambda () not-special))))))))

(defun attach-rewirings-at-index (pp index &rest args &key entering exiting)
  ;; Like CL-QUIL-TESTS::ATTACH-REWIRINGS-TO-PROGRAM, but instead of attaching the rewiring on the
  ;; first/last instr of PP, attach the rewiring at the requested INDEX.
  (check-type entering (or null cl-quil::integer-vector))
  (check-type exiting (or null cl-quil::integer-vector))
  (assert (or entering exiting))
  (setf (cl-quil:comment (cl-quil::nth-instr index pp))
        (apply #'cl-quil::make-rewiring-comment args))
  pp)

(deftest test-strip-final-halt-respecting-rewirings ()
  ;; An empty program produces an empty vector
  (is (equalp #() (quilc::strip-final-halt-respecting-rewirings (cl-quil:parse-quil ""))))

  ;; Only a single final HALT is stripped.
  (let* ((pp (cl-quil:parse-quil "X 0; HALT; HALT"))
         (stripped-code (quilc::strip-final-halt-respecting-rewirings pp)))
    (is (= 2 (length stripped-code)))
    (is (cl-quil::haltp (cl-quil::vnth 1 stripped-code))))

  ;; Mid-program HALTs are ignored.
  (let* ((pp (with-output-to-quil
               "JUMP @SKIPHALT"
               "HALT"
               "LABEL @SKIPHALT"
               "HALT"))
         (stripped-code (quilc::strip-final-halt-respecting-rewirings pp)))
    (is (= 3 (length stripped-code)))
    (is (cl-quil::haltp (cl-quil::vnth 1 stripped-code)))
    (is (not (cl-quil::haltp (cl-quil::vnth 2 stripped-code)))))

  ;; single non-halt instr
  (let* ((pp (attach-rewirings-at-index (cl-quil:parse-quil "X 0")
                                        0
                                        :entering #(0 1 2)
                                        :exiting #(2 1 0)))
         (stripped-code (quilc::strip-final-halt-respecting-rewirings pp)))
    (is (= 1 (length stripped-code)))
    (multiple-value-bind (entering-rewiring exiting-rewiring)
        (cl-quil::instruction-rewirings (cl-quil::vnth 0 stripped-code))
      (is (equalp #(0 1 2) (cl-quil::rewiring-l2p entering-rewiring)))
      (is (equalp #(2 1 0) (cl-quil::rewiring-l2p exiting-rewiring)))))

  ;; single halt instr
  (let* ((pp (attach-rewirings-at-index (cl-quil:parse-quil "HALT")
                                        0
                                        :entering #(0 1 2)
                                        :exiting #(2 1 0)))
         (stripped-code (quilc::strip-final-halt-respecting-rewirings pp)))
    (is (equalp #() stripped-code)))

  ;; 2-instr no halts, entering/exiting rewirings untouched
  (let* ((pp (cl-quil:parse-quil "X 0; Y 1"))
         (pp (attach-rewirings-at-index pp 0 :entering #(0 1 2) :exiting #(2 1 0)))
         (pp (attach-rewirings-at-index pp 1 :entering #(1 2 0) :exiting #(0 2 1)))
         (stripped-code (quilc::strip-final-halt-respecting-rewirings pp)))
    (is (= 2 (length stripped-code)))
    (multiple-value-bind (entering-rewiring exiting-rewiring)
        (cl-quil::instruction-rewirings (cl-quil::vnth 0 stripped-code))
      (is (equalp #(0 1 2) (cl-quil::rewiring-l2p entering-rewiring)))
      (is (equalp #(2 1 0) (cl-quil::rewiring-l2p exiting-rewiring))))
    (multiple-value-bind (entering-rewiring exiting-rewiring)
        (cl-quil::instruction-rewirings (cl-quil::vnth 1 stripped-code))
      (is (equalp #(1 2 0) (cl-quil::rewiring-l2p entering-rewiring)))
      (is (equalp #(0 2 1) (cl-quil::rewiring-l2p exiting-rewiring)))))

  ;; {2,3}-instruction terminal halt
  (dolist (quil '("X 0; HALT" "H 0; CNOT 0 1; HALT"))
    (labels ((attach-rewirings (&key last-entering last-exiting
                                     penultimate-entering penultimate-exiting)
               (let* ((pp (cl-quil:parse-quil quil))
                      (last-index (1- (length (cl-quil::parsed-program-executable-code pp))))
                      (penultimate-index (1- last-index)))

                 ;; attach the rewirings
                 (when (or last-entering last-exiting)
                   (setf pp (attach-rewirings-at-index pp last-index
                                                       :entering last-entering
                                                       :exiting last-exiting)))
                 (when (or penultimate-entering penultimate-exiting)
                   (setf pp (attach-rewirings-at-index pp penultimate-index
                                                       :entering penultimate-entering
                                                       :exiting penultimate-exiting)))
                 ;; return the parsed-program
                 pp))
             (test-compatible (&rest args &key last-entering last-exiting
                                               penultimate-entering penultimate-exiting)
               (let* ((pp (apply #'attach-rewirings args))
                      (stripped-code (quilc::strip-final-halt-respecting-rewirings pp)))

                 ;; final HALT was stripped
                 (is (= (length stripped-code) (1- (length (cl-quil::parsed-program-executable-code pp)))))

                 ;; rewirings were correctly copied
                 (multiple-value-bind (stripped-entering stripped-exiting)
                     (cl-quil::instruction-rewirings (cl-quil::vnth (1- (length stripped-code))
                                                              stripped-code))
                   (is (equalp (or last-entering penultimate-entering)
                               (and stripped-entering (cl-quil::rewiring-l2p stripped-entering))))
                   (is (equalp (or last-exiting penultimate-exiting)
                               (and stripped-exiting (cl-quil::rewiring-l2p stripped-exiting)))))))

             (test-incompatible (&rest args &key last-entering last-exiting
                                                 penultimate-entering penultimate-exiting)
               (declare (ignore last-entering last-exiting penultimate-entering penultimate-exiting))
               (signals error (quilc::strip-final-halt-respecting-rewirings
                               (apply #'attach-rewirings args)))))

      ;; various compatible combos of entering/exiting rewirings
      (test-compatible)
      (test-compatible :last-entering #(1 2 0))
      (test-compatible :last-entering #(1 2 0) :penultimate-entering #(1 2 0))
      (test-compatible :last-entering #(1 2 0) :penultimate-exiting #(2 1 0))
      (test-compatible :last-exiting #(0 2 1)) ; common case (probably).
      (test-compatible :last-exiting #(0 2 1) :penultimate-exiting #(0 2 1))
      (test-compatible :last-exiting #(0 2 1) :penultimate-entering #(0 1 2))
      (test-compatible :penultimate-entering #(0 1 2))
      (test-compatible :last-entering #(1 2 0)
                       :last-exiting #(0 2 1))
      (test-compatible :last-entering #(1 2 0)
                       :last-exiting #(0 2 1)
                       :penultimate-exiting #(0 2 1))
      (test-compatible :last-entering #(1 2 0)
                       :last-exiting #(0 2 1)
                       :penultimate-entering #(1 2 0))
      (test-compatible :last-entering #(1 2 0)
                       :last-exiting #(0 2 1)
                       :penultimate-entering #(1 2 0)
                       :penultimate-exiting #(0 2 1))
      (test-compatible :penultimate-entering #(0 1 2)
                       :penultimate-exiting #(2 1 0))
      (test-compatible :last-entering #(0 1 2)
                       :penultimate-entering #(0 1 2)
                       :penultimate-exiting #(2 1 0))
      (test-compatible :last-exiting #(2 1 0)
                       :penultimate-entering #(0 1 2)
                       :penultimate-exiting #(2 1 0))

      ;; various incompatible combos of entering/exiting rewirings
      (test-incompatible :last-entering #(0 1 2) :penultimate-entering #(0 1 3))
      (test-incompatible :last-exiting #(2 1 0) :penultimate-exiting #(3 1 0)))))
