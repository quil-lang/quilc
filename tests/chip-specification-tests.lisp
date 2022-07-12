;;;; chip-specification-tests.lisp
;;;;
;;;; Author: appleby

(in-package #:cl-quil-tests)

(defparameter *qpu-test-file-directory*
  (asdf:system-relative-pathname
   ':cl-quil-tests
   "tests/qpu-test-files/"))

(defun %read-test-chipspec (file-name)
  (cl-quil::read-chip-spec-file (merge-pathnames file-name *qpu-test-file-directory*)))

(deftest test-read-chipspec ()
  "Test that READ-CHIPSPEC can parse some representative Aspen-* qpu files."
  (finish-output)
  (let ((test-files (uiop:directory-files *qpu-test-file-directory* #P"*.qpu")))
    (is (not (null test-files)))
    (dolist (file test-files)
      (format t "~&    Testing file ~A~%" (pathname-name file))
      (not-signals error (cl-quil::read-chip-spec-file file)))))

(deftest test-gh-378-regression ()
  "Regression test for github issue #378."
  ;; https://github.com/rigetti/quilc/issues/378
  (not-signals error
    ;; This Quil program is a minimized version of the one that appears in
    ;; TEST-SOHAIB-GH-361-REGRESSION, which is sufficient to tickle gh-378 when compiling with the
    ;; unmodified Aspen-4-10Q-A.qpu.
    (compiler-hook (cl-quil-tests::with-output-to-quil
                     "CNOT 0 2"
                     "CNOT 0 10"
                     "CNOT 0 15"
                     "CNOT 0 16"
                     "CNOT 1 2")
                   (cl-quil-tests::%read-test-chipspec "Aspen-4-10Q-A.qpu"))))

(deftest test-isa-1q-completion ()
  "Test that the 1Q layer of the chip specification is complete."
  (let* ((isa (yason:parse "{\"isa\":
{\"1Q\": {\"0\": {},
  \"1\": {},
  \"2\": {},
  \"3\": {},
  \"5\": {},
  \"6\": {},
  \"7\": {},
  \"13\": {},
  \"14\": {},
  \"15\": {},
  \"16\": {},
  \"17\": {}},
 \"2Q\": {\"0-1\": {},
  \"0-7\": {},
  \"1-2\": {},
  \"1-16\": {},
  \"2-3\": {},
  \"2-15\": {},
  \"5-6\": {},
  \"6-7\": {},
  \"13-14\": {},
  \"14-15\": {},
  \"15-16\": {},
  \"16-17\": {}}}}"))
         (chip-spec (cl-quil::qpu-hash-table-to-chip-specification isa)))
    (is (cl-quil::chip-specification-p chip-spec))
    (is (= 18 (cl-quil::chip-spec-n-qubits chip-spec)))
    ;; check we got the goods
    (dolist (presumed-dead '(4 8 9 10 11 12))
      (is (cl-quil::chip-spec-qubit-dead? chip-spec presumed-dead))))) ; RIP in piece

(deftest test-bristlecone-chip ()
  "Test construction of Google's Bristlecone 72-qubit chip"
  (let* ((chip (cl-quil::build-bristlecone-chip))
         (prgm (parse-quil
                (with-output-to-string (s)
                  (loop :for i :below (cl-quil::chip-spec-n-qubits chip)
                        :do (format s "H ~D~%" i)))))
         ;; Bit of a kludge here. Since this is a large number of
         ;; qubits, calculating its matrix representation will be a
         ;; terribly long-winded affair.
         (cl-quil::*compress-carefully* nil))
    (is (= 72 (cl-quil::chip-spec-n-qubits chip)))
    (is (= (* 11 11) (cl-quil::chip-spec-n-links chip)))
    (is (plusp (length (parsed-program-executable-code prgm))))
    (is (plusp (length (parsed-program-executable-code (compiler-hook prgm chip)))))))
