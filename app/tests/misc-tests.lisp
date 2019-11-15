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
        (chip (quil::build-nq-fully-connected-chip 2))
        (stats '("topological_swaps" "logical_schedule" "gate_depth" "gate_volume"
                 "program_duration" "program_fidelity" "unused_qubits" "multiqubit_gate_depth")))
    ;; Test that a direct call to PROCESS-PROGRAM respects protoquil
    ;; and returns a statistics dictionary.
    (let ((pp (quil:parse-quil progm)))
      (multiple-value-bind (processed-program statistics)
          (quilc::process-program pp chip :protoquil t)
        (is (quil:protoquil-program-p processed-program))
        (dolist (stat stats)
          (is (nth-value 1 (gethash stat statistics))))))
    ;; Likewise, test that without :PROTOQUIL T there is an empty
    ;; stats dictionary.
    (let ((pp (quil:parse-quil progm)))
      (multiple-value-bind (_ statistics)
          (quilc::process-program pp chip :protoquil nil)
        (declare (ignore _))
        (dolist (stat stats)
          (is (not (nth-value 1 (gethash stat statistics)))))))))

(deftest test-special-bindings-let* ()
  (dolist (thing '(42 "a string" :a-keyword quoted-symbol (a cons)))
    (is (eq thing (quilc::special-bindings-let* ((not-special thing))
                    (bt:join-thread (bt:make-thread (lambda () not-special))))))))

(deftest test-%strip-halts-respecting-rewirings ()
  ;; empty parsed prog
  (is (equalp #() (quilc::%strip-halts-respecting-rewirings (quil:parse-quil ""))))

  (flet ((attach-rewirings-to-program (pp in-rewiring-vector out-rewiring-vector)
           ;; Stolen from CL-QUIL-TESTS::ATTACH-REWIRINGS-TO-PROGRAM.
           (check-type in-rewiring-vector (or null quil::integer-vector))
           (check-type out-rewiring-vector (or null quil::integer-vector))

           (unless (and (null in-rewiring-vector) (null out-rewiring-vector))
             (let ((code (quil::parsed-program-executable-code pp)))
               (cond
                 ((< (length code) 1)
                  (error "Cannot attach rewirings to program with no instructions"))
                 ((= (length code) 1)
                  (setf (quil::comment (aref code 0))
                        (quil::make-rewiring-comment :entering in-rewiring-vector
                                                     :exiting out-rewiring-vector)))
                 (t
                  (when (not (null in-rewiring-vector))
                    (setf (quil::comment (aref code 0))
                          (quil::make-rewiring-comment :entering in-rewiring-vector)))
                  (when (not (null out-rewiring-vector))
                    (setf (quil::comment (aref code (1- (length code))))
                          (quil::make-rewiring-comment :exiting out-rewiring-vector)))))))
           pp)
         (attach-rewirings-to-halts (pp indices entering-vector exiting-vector)
           ;; Like ATTACH-REWIRINGS-TO-PROGRAM, but instead of attaching the entering/exiting
           ;; rewiring on the first/last instr of PP, attach the exit rewiring at each of the
           ;; requested INDICES and the ENTERING-VECTOR on the previous instr if ENTERING-VECTOR is
           ;; non-NIL and a previous instruction exists.
           (check-type entering-vector (or null quil::integer-vector))
           (check-type exiting-vector quil::integer-vector)

           (dolist (index indices pp)
             (let ((halt-instr (quil::nth-instr index pp)))
               (assert (quil::haltp halt-instr))
               (setf (quil:comment halt-instr)
                     (quil::make-rewiring-comment :exiting exiting-vector)))

             (when (and (plusp index) (not (null entering-vector)))
               (setf (quil:comment (quil::nth-instr (1- index) pp))
                     (quil::make-rewiring-comment :entering entering-vector))))))

    ;; Multiple HALTs in a row are not handled correctly (but should "never" happen).
    (let* ((pp (attach-rewirings-to-program
                (with-output-to-quil
                  "X 0"
                  "HALT"
                  "HALT")
                #(0 1 2)
                #(2 1 0)))
           (stripped-code (quilc::%strip-halts-respecting-rewirings pp)))
      (is (= 1 (length stripped-code)))
      (multiple-value-bind (entering-rewiring exiting-rewiring)
          (quil::instruction-rewirings (quil::vnth 0 stripped-code))
        (is (not (null entering-rewiring)))
        (is (null exiting-rewiring))))

    (loop :for (entering exiting) :in '((nil nil) (#(0 1 2) nil) (nil #(2 1 0)) (#(0 1 2) #(2 1 0))) :do
      (loop :for (quil halt-locations)
              :in `(("HALT" (0))
                    (,(format nil "HALT~%HALT~%HALT") (0 1 2))
                    (,(format nil "X 0") ())
                    (,(format nil "X 0~%HALT") (1))
                    (,(format nil "X 0~%X 1") ())
                    (,(format nil "H 0~%H 1~%CNOT 1 0") ())
                    (,(format nil "H 0~%H 1~%HALT") (2))
                    (,(format nil "H 0~%HALT~%H 1~%HALT") (1 3)))
            :for num-halts := (length halt-locations)
            :for orig-length := (length (quil::parsed-program-executable-code (quil:parse-quil quil)))
            :for have-entering-rewiring-p := (not (null entering))
            :for have-exiting-rewiring-p := (not (null exiting))
            :for have-rewiring-p := (or have-entering-rewiring-p have-exiting-rewiring-p)
            :do (progn
                  ;; test enter and exit rewirings on first/last instr only
                  (let* ((pp (attach-rewirings-to-program (quil:parse-quil quil) entering exiting))
                         (stripped-code (quilc::%strip-halts-respecting-rewirings pp)))

                    ;; The correct number of instrs were removed and no HALTs remain.
                    (is (= (length stripped-code) (- orig-length num-halts)))
                    (is (every (complement #'quil::haltp) stripped-code))

                    (when (and have-exiting-rewiring-p (plusp (length stripped-code)))
                      ;; There exists at least one non-HALT instr. Ensure the final instruction of
                      ;; STRIPPED-CODE now contains the EXITING rewiring, regardless of whether the
                      ;; original program ended in a HALT.
                      (let ((last-stripped-instr (quil::vnth (1- (length stripped-code)) stripped-code)))
                        (is (not (null (quil:comment last-stripped-instr))))
                        (let ((parsed-exiting-rewiring
                                (nth-value 1 (quil::instruction-rewirings last-stripped-instr))))
                          (is (not (null parsed-exiting-rewiring)))
                          (is (equalp exiting (quil::rewiring-l2p parsed-exiting-rewiring)))))))

                  ;; every HALT with exit rewiring has it's rewiring preserved
                  (when (and have-exiting-rewiring-p (plusp num-halts))
                    (let* ((pp (attach-rewirings-to-halts (quil:parse-quil quil)
                                                          halt-locations
                                                          entering
                                                          exiting))
                           (stripped-code (quilc::%strip-halts-respecting-rewirings pp)))

                      ;; The correct number of instrs were removed and no HALTs remain.
                      (is (= (length stripped-code) (- orig-length num-halts)))
                      (is (every (complement #'quil::haltp) stripped-code))

                      (when (plusp (length stripped-code))
                        (loop :for i :in halt-locations
                              :for instr-preceeding-halt := (and (plusp i) (quil::nth-instr (1- i) pp))
                              :when (not (null instr-preceeding-halt)) :do
                                (progn
                                  (is (not (null (quil:comment instr-preceeding-halt))))
                                  (let ((parsed-exiting-rewiring
                                          (nth-value 1 (quil::instruction-rewirings
                                                        instr-preceeding-halt))))
                                    (is (not (null parsed-exiting-rewiring)))
                                    (is (equalp exiting (quil::rewiring-l2p parsed-exiting-rewiring))))))))))))))
