;;;; tests/printer-tests.lisp
;;;;
;;;; Author: appleby

(in-package #:cl-quil-tests)

(defparameter *printer-test-files-directory*
  (asdf:system-relative-pathname
   ':cl-quil-tests
   "tests/printer-test-files/"))

(defun parse-and-print-quil-to-string (input)
  (with-output-to-string (s)
    (quil::print-parsed-program (quil:parse-quil input) s)))

(defun update-print-parsed-program-golden-files (&key skip-prompt)
  "Call UPDATE-PRINT-PARSED-PROGRAM-GOLDEN-FILES on all the files in *PRINTER-TEST-FILES-DIRECTORY*.

See the documentation string for UPDATE-PRINT-PARSED-PROGRAM-GOLDEN-FILES for more info and an
admonition against carelessness."
  (update-golden-file-output-sections
   (uiop:directory-files *printer-test-files-directory* #P"*.quil")
   #'parse-and-print-quil-to-string
   :skip-prompt skip-prompt))

(deftest test-print-parsed-program-golden-files ()
  "Ensure that PRINT-PARSED-PROGRAM produces the expected output and that it is parseable by PARSE-QUIL."

  ;; Why hello. Has this test failed due to innocuous changes to the printed representation of the
  ;; parsed program? Then you should consider running UPDATE-PRINT-PARSED-PROGRAM-GOLDEN-FILES,
  ;; above, to update the output sections of the golden files. If you do so, however, be sure to
  ;; examine the diffs of the old vs new output *carefully* to ensure all the changes are intended
  ;; or expected. Golden files are precious, and their sanctity must be preserved. Thank you.

  (let* ((golden-files (uiop:directory-files *printer-test-files-directory* #P"*.quil")))
    (is (not (null golden-files)))
    (dolist (file golden-files)
      (format t "~&    Testing file ~a" (pathname-name file))
      (multiple-value-bind (golden-inputs golden-outputs) (parse-golden-file file)
        (loop :for input :in golden-inputs
              :for expected-output :in golden-outputs :do
                (let* ((input-pp (quil:parse-quil input))
                       (actual-output (with-output-to-string (s)
                                        (quil::print-parsed-program input-pp s))))
                  (is (string= expected-output actual-output))

                  ;; Ensure the output of PRINT-PARSED-PROGRAM can be parsed.
                  (not-signals error (quil:parse-quil actual-output))

                  ;; Ensure expected-output is a fixed point of parse -> print.
                  (is (string= expected-output
                               (parse-and-print-quil-to-string expected-output)))))))))

(deftest test-print-instruction ()
  (is (string= "PRAGMA gate_time CNOT \"50 ns\""
               (with-output-to-string (s)
                 (cl-quil::print-instruction (make-instance 'quil::pragma
                                                   :words '("gate_time" "CNOT")
                                                   :freeform-string "50 ns")
                                             s))))
  ;; try a operand-free instruction
  (is (string= "HALT"
               (with-output-to-string (s)
                 (cl-quil::print-instruction (make-instance 'halt)
                                             s))))
  ;; try a unary instruction
  (is (string= "NEG ro[3]"
               (with-output-to-string (s)
                 (cl-quil::print-instruction (make-instance 'quil::classical-negate
                                                            :target (mref "ro" 3))
                                             s))))
  ;; try a binary instruction
  (is (string= "MEASURE 1 ro[3]"
               (with-output-to-string (s)
                 (cl-quil::print-instruction (make-instance 'quil::measure
                                                            :address (mref "ro" 3)
                                                            :qubit (qubit 1))
                                             s))))
  ;; try something fancy
  (is (string= "CPHASE-AND-MEASURE(%alpha) 1 3 ro[5]"
               (with-output-to-string (s)
                 (cl-quil::print-instruction (make-instance 'cl-quil::circuit-application
                                                            :operator #.(named-operator "CPHASE-AND-MEASURE")
                                                            :parameters `(,(param "alpha"))
                                                            :arguments `(,(qubit 1)
                                                                         ,(qubit 3)
                                                                         ,(mref "ro" 5)))
                                             s)))))

(deftest test-defgate-printing ()
  (let ((befores (list "DEFGATE R(%theta, %beta):
    exp(%beta/3*i), 0
    0, exp(%theta/2*i)

R(pi/2, pi/8) 0"
                       "DEFGATE R:
    exp(2*i), 0
    0, exp(4*i)

R 0")))
    (dolist (before befores)
      (let ((after (with-output-to-string (s)
                     (quil::print-parsed-program
                      (quil::parse-quil before)
                      s))))
        (quil::parse-quil after)))))

(deftest test-circuit-and-declare-printing ()
  (let* ((before "DECLARE theta REAL[16]
DECLARE theta-bits BIT[100] SHARING theta OFFSET 1 REAL

DEFCIRCUIT TEST(%a) b c:
    RZ(%a) b
    RZ(%a) c


TEST(0.5) 0 1
")
         (after (with-output-to-string (s)
                  (cl-quil::print-parsed-program
                   (cl-quil::parse-quil-into-raw-program before) s))))
    (is (string= before after))))

