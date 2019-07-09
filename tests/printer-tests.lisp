;;;; tests/printer-tests.lisp
;;;;
;;;; Author: appleby

(in-package #:cl-quil-tests)

(defparameter *printer-test-files-directory*
  (asdf:system-relative-pathname
   ':cl-quil-tests
   "tests/printer-test-files/")
  "Path to directory containing printer test files. Note that unlike most other test file directories used in this test-suite, all the actual input files are contained in subdirectories of this one. See also *PRINTER-TEST-FILES-GOLD-STANDARD-DIRECTORY* and *PRINTER-TEST-FILES-GOLD-REGEX-DIRECTORY*.")

(defparameter *printer-test-files-gold-standard-directory*
  (merge-pathnames #P"gold-standard/" *printer-test-files-directory*)
  "Path to directory containing golden files whose expected output sections can be compared with STRING=.")

(defparameter *printer-test-files-gold-regex-directory*
  (merge-pathnames #P"gold-regex/" *printer-test-files-directory*)
  "Path to directory containing golden files whose expected output sections must be compared via regex.")

(defun parse-and-print-quil-to-string (input
                                       &key (parser #'quil:parse-quil)
                                            (printer #'quil::print-parsed-program))
  (with-output-to-string (s)
    (funcall printer (funcall parser input) s)))

(defun update-print-parsed-program-golden-files (&key skip-prompt)
  "Call UPDATE-PRINT-PARSED-PROGRAM-GOLDEN-FILES on all the files in the gold-standard subdirectory
of *PRINTER-TEST-FILES-DIRECTORY*.

We limit updating of golden files to the gold-standard directory because they are the only ones that
can be easily updated in an automated fashion. The files in the gold-regex directory make use of
regexes in their output sections, which makes automated update complicated. The number of
regex-enabled tests is small, so updating them by hand shouldn't be too tedious. You can always call
UPDATE-GOLDEN-FILE-OUTPUT-SECTIONS on them manually, then re-instate any required regexes.

See the documentation string for UPDATE-PRINT-PARSED-PROGRAM-GOLDEN-FILES for more info and an
admonition against carelessness."
  (update-golden-file-output-sections
   (uiop:directory-files *printer-test-files-gold-standard-directory* #P"*.quil")
   #'parse-and-print-quil-to-string
   :skip-prompt skip-prompt))


(defun %golden-tester (check-output)
  "Return a function that can be passed to MAP-GOLDEN-FILES-AND-TEST-CASES which uses CHECK-OUTPUT
to compare output sections.

%GOLDEN-TESTER is a helper function for TEST-PRINT-PARSED-PROGRAM-GOLDEN-FILES."
  (check-type check-output function)
  (lambda (test-case)
       (let* ((*always-show-failed-sexp* t)
              (input (golden-test-case-input test-case))
              (expected-output (golden-test-case-output test-case))
              (actual-output nil)
              (skip-fixed-point-check-p (uiop:string-prefix-p "# Disable fixed-point check" input))
              (message (format nil "~&Golden test case at (file:line): ~A:~D"
                               (golden-test-case-file test-case)
                               (golden-test-case-line test-case))))
         ;; This SETF is ugly, but guarding this in a NOT-SIGNALS aids debugging in case
         ;; PARSE-AND-PRINT-QUIL-TO-STRING chokes on INPUT.
         (not-signals error (setf actual-output (parse-and-print-quil-to-string input)))
         (not-signals error (quil:parse-quil actual-output))
         (is (funcall check-output expected-output actual-output) message)

         ;; Ensure expected-output is a fixed point of parse -> print. In rare cases, this check
         ;; might fail, so skip it if we find a magic cookie at the start of the input section
         ;; indicating that we should do so.
         (unless skip-fixed-point-check-p
           (is (funcall check-output expected-output (parse-and-print-quil-to-string actual-output))
               message)))))

(deftest test-print-parsed-program-golden-files ()
  "Ensure that PRINT-PARSED-PROGRAM produces the expected output and that it is parseable by PARSE-QUIL."

  ;; Why hello. Has this test failed due to innocuous changes to the printed representation of the
  ;; parsed program? Then you should consider running UPDATE-PRINT-PARSED-PROGRAM-GOLDEN-FILES,
  ;; above, to update the output sections of the golden files. If you do so, however, be sure to
  ;; examine the diffs of the old vs new output *carefully* to ensure all the changes are intended
  ;; or expected. Golden files are precious, and their sanctity must be preserved. Thank you.

  (let ((golden-files (uiop:directory-files *printer-test-files-gold-standard-directory* #P"*.quil")))
    (format t "~&  Gold-standard tests:")
    (is (not (null golden-files)))
    (map-golden-files-and-test-cases (%golden-tester #'string=) golden-files))

  (let ((golden-files (uiop:directory-files *printer-test-files-gold-regex-directory* #P"*.quil")))
    (format t "~&  Gold-regex tests:")
    (is (not (null golden-files)))
    (map-golden-files-and-test-cases (%golden-tester #'cl-ppcre:scan) golden-files)))

(deftest test-instruction-fmt ()
  (is (string= "PRAGMA gate_time CNOT \"50 ns\"" (format nil "~/cl-quil:instruction-fmt/"
                                                         (make-instance 'quil::pragma
                                                                        :words '("gate_time" "CNOT")
                                                                        :freeform-string "50 ns"))))
  ;; try a operand-free instruction
  (is (string= "HALT" (format nil "~/cl-quil:instruction-fmt/" (make-instance 'halt))))

  ;; try a unary instruction
  (is (string= "NEG ro[3]" (format nil "~/cl-quil:instruction-fmt/"
                                   (make-instance 'quil::classical-negate
                                                  :target (mref "ro" 3)))))
  ;; try a binary instruction
  (is (string= "MEASURE 1 ro[3]" (format nil "~/cl-quil:instruction-fmt/"
                                         (make-instance 'quil::measure
                                                        :address (mref "ro" 3)
                                                        :qubit (qubit 1)))))
  ;; try something fancy
  (is (string= "CPHASE-AND-MEASURE(%alpha) 1 3 ro[5]"
               (format nil "~/cl-quil:instruction-fmt/"
                       (make-instance 'cl-quil::circuit-application
                                      :operator #.(named-operator "CPHASE-AND-MEASURE")
                                      :parameters `(,(param "alpha"))
                                      :arguments `(,(qubit 1)
                                                    ,(qubit 3)
                                                    ,(mref "ro" 5)))))))

(deftest test-defgate-printing ()
  ;; The EXP terms in the below DEFGATE evaluate to floating point values with with lots of digits
  ;; in the printed representation. It seems like a bad idea to have a test depend on the precise
  ;; default printed representation of such a float, so this test is not included in
  ;; TEST-PRINT-PARSED-PROGRAM-GOLDEN-FILES, above. However, a similar test case inspired by this
  ;; one is included in printer-test-files/defgates.quil.
  (let ((before "DEFGATE R:
    exp(2*i), 0
    0, exp(4*i)

R 0"))
    (let ((after (parse-and-print-quil-to-string before)))
      (not-signals error (quil::parse-quil after)))))

(deftest test-circuit-and-declare-printing ()
  ;; This test relies on the fact that PARSE-QUIL-INTO-RAW-PROGRAM doesn't EXPAND-CIRCUITS,
  ;; otherwise it could be included in TEST-PRINT-PARSED-PROGRAM-GOLDEN-FILES, above.
  (let* ((before "DECLARE theta REAL[16]
DECLARE theta-bits BIT[100] SHARING theta OFFSET 1 REAL

DEFCIRCUIT TEST(%a) b c:
    RZ(%a) b
    RZ(%a) c


TEST(0.5) 0 1
")
         (after (parse-and-print-quil-to-string before :parser #'quil::parse-quil-into-raw-program)))
    (is (string= before after))))

(deftest test-jump-to-integer-label-printing ()
  "Ensure that JUMP instructions with integer LABELs are printed correctly."
  (is (string= (quil::print-instruction-to-string
                (quil::make-instance 'quil::unconditional-jump :label 42))
               "JUMP {absolute address 42}"))
  (is (string= (quil::print-instruction-to-string
                (quil::make-instance 'quil::jump-when :label 0 :address (quil::mref "ro" 0)))
               "JUMP-WHEN {absolute address 0} ro[0]"))
  (is (string= (quil::print-instruction-to-string
                (quil::make-instance 'quil::jump-unless :label 1 :address (quil::mref "ro" 2)))
               "JUMP-UNLESS {absolute address 1} ro[2]")))
