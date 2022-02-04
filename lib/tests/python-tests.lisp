;;;; python-tests.lisp
;;;;
;;;; Author: Kartik Singh

(in-package #:libquilc-tests)

(deftest test-python-compile-quil ()
  "Test compiling Quil from Python."
  (uiop:with-current-directory ("lib/")
    (let* ((input-source "H 0")
           (parsed-program (cl-quil:safely-parse-quil input-source))
           (chip-spec (cl-quil::build-nq-linear-chip 8))
           (processed-program (cl-quil:compiler-hook parsed-program chip-spec))
           (expected-output (quilc::print-program processed-program nil)))
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program '("python3" "tests/python/compile_quil.py")
                            :env '((:PYTHONPATH . "."))
                            :input `(,input-source)
                            :output :string)
        (declare (ignore error-output exit-code))
        (is (string= output expected-output))))))

(deftest test-python-compile-protoquil ()
  "Test compiling ProtoQuil from Python."
  (uiop:with-current-directory ("lib/")
    (let* ((input-source "DECLARE ro BIT; H 0; MEASURE 0 ro")
           (parsed-program (cl-quil:safely-parse-quil input-source))
           (chip-spec (cl-quil::build-nq-linear-chip 8))
           (processed-program (cl-quil:compiler-hook parsed-program chip-spec :protoquil t))
           (expected-output (quilc::print-program processed-program nil)))
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program '("python3" "tests/python/compile_protoquil.py")
                            :env '((:PYTHONPATH . "."))
                            :input `(,input-source)
                            :output :string)
        (declare (ignore error-output exit-code))
        (is (string= output expected-output))))))

(deftest test-python-compile-protoquil-bad-program ()
  "Test compiling an invalid ProtoQuil program from Python. Should throw an error."
  (uiop:with-current-directory ("lib/")
    (let* ((input-source "DECLARE ro BIT; MEASURE 0 ro; H 0"))
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program '("python3" "tests/python/compile_protoquil.py")
                            :ignore-error-status t
                            :env '((:PYTHONPATH . "."))
                            :input `(,input-source)
                            :output :string)
        (declare (ignore error-output))
        (is (eql exit-code 1)
            (string= output "unable to compile program"))))))

(deftest test-python-print-chip-spec ()
  "Test printing chip specifications from Python."
  (uiop:with-current-directory ("lib/")
    (let ((chip-spec1 (cl-quil::build-nq-linear-chip 8))
          (chip-spec2 (quilc::lookup-isa-descriptor-for-name "8Q")))
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program '("python3" "tests/python/print_chip_spec.py")
                            :env '((:PYTHONPATH . "."))
                            :output :string)
        (declare (ignore error-output exit-code))
        (is (string= output (concatenate 'string
                                         (cl-quil::debug-print-chip-spec chip-spec1 nil)
                                         (cl-quil::debug-print-chip-spec chip-spec2 nil))))))))
