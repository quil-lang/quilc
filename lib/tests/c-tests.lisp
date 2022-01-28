;;;; c-tests.lisp
;;;;
;;;; Author: Kartik Singh

(in-package #:libquilc-tests)

(deftest test-build-lib ()
  "Test that libquilc builds."
  (uiop:with-current-directory ("lib/")
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program "make")
      (is (eql exit-code 0)))))

(deftest test-build-c-tests ()
  "Test that the C tests build."
  (uiop:with-current-directory ("lib/tests/c/")
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program "make")
      (is (eql exit-code 0)))))

(deftest test-compile-quil ()
  "Test compiling Quil from C."
  (uiop:with-current-directory ("lib/")
    (let* ((input-source "H 0")
           (parsed-program (cl-quil:safely-parse-quil input-source))
           (chip-spec (cl-quil::build-nq-linear-chip 8))
           (processed-program (cl-quil:compiler-hook parsed-program chip-spec))
           (expected-output (quilc::print-program processed-program nil)))
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program "tests/c/compile-quil" :input `(,input-source) :output :string)
        (is (string= output expected-output))))))

(deftest test-compile-protoquil ()
  "Test compiling ProtoQuil from C."
  (uiop:with-current-directory ("lib/")
    (let* ((input-source "H 0")
           (parsed-program (cl-quil:safely-parse-quil input-source))
           (chip-spec (cl-quil::build-nq-linear-chip 8))
           (processed-program (cl-quil:compiler-hook parsed-program chip-spec :protoquil t))
           (expected-output (quilc::print-program processed-program nil)))
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program "tests/c/compile-protoquil" :input `(,input-source) :output :string)
        (is (string= output expected-output))))))

(deftest test-print-chip-spec ()
  "Test printing chip specifications from C."
  (uiop:with-current-directory ("lib/")
    (let ((chip-spec1 (cl-quil::build-nq-linear-chip 8))
          (chip-spec2 (quilc::lookup-isa-descriptor-for-name "8Q")))
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program "tests/c/print-chip-spec" :output :string)
        (is (string= output (concatenate 'string
                                         (cl-quil::debug-print-chip-spec chip-spec1 nil)
                                         (cl-quil::debug-print-chip-spec chip-spec2 nil))))))))
