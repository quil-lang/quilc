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


