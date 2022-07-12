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
                                       &key (parser #'cl-quil:parse-quil)
                                            (printer #'cl-quil::print-parsed-program))
  (with-output-to-string (s)
    (funcall printer (funcall parser input) s)))

(defun update-print-parsed-program-golden-files (&key skip-prompt)
  "Call UPDATE-GOLDEN-FILE-OUTPUT-SECTIONS on all the files in the gold-standard subdirectory of *PRINTER-TEST-FILES-DIRECTORY*.

We limit updating of golden files to the gold-standard directory because they are the only ones that can be easily updated in an automated fashion. The files in the gold-regex directory make use of regexes in their output sections, which makes automated update complicated. The number of regex-enabled tests is small, so updating them by hand shouldn't be too tedious. You can always call UPDATE-GOLDEN-FILE-OUTPUT-SECTIONS on them manually, then re-instate any required regexes.

See the documentation string for UPDATE-GOLDEN-FILE-OUTPUT-SECTIONS for more info and an admonition against carelessness."
  (update-golden-file-output-sections
   (uiop:directory-files *printer-test-files-gold-standard-directory* #P"*.quil")
   #'parse-and-print-quil-to-string
   :skip-prompt skip-prompt))


(defun %golden-parse-print-tester (check-output)
  "Return a function that can be passed to MAP-GOLDEN-FILES-AND-TEST-CASES which uses CHECK-OUTPUT to compare output sections.

%GOLDEN-PARSE-PRINT-TESTER is a helper function for TEST-PRINT-PARSED-PROGRAM-GOLDEN-FILES."
  (check-type check-output function)
  (lambda (test-case golden-error-message)
    (let* ((input (golden-test-case-input test-case))
           (expected-output (golden-test-case-output test-case))
           (actual-output nil)
           (skip-fixed-point-check-p (uiop:string-prefix-p "# Disable fixed-point check" input)))
      ;; This SETF is ugly, but guarding this in a NOT-SIGNALS aids debugging in case
      ;; PARSE-AND-PRINT-QUIL-TO-STRING chokes on INPUT.
      (not-signals error (setf actual-output (parse-and-print-quil-to-string input)))
      (not-signals error (cl-quil:parse-quil actual-output))
      (is (funcall check-output expected-output actual-output) golden-error-message)

      ;; Ensure expected-output is a fixed point of parse -> print. In rare cases, this check
      ;; might fail, so skip it if we find a magic cookie at the start of the input section
      ;; indicating that we should do so.
      (unless skip-fixed-point-check-p
        (is (funcall check-output expected-output (parse-and-print-quil-to-string actual-output))
            golden-error-message)))))

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
    (map-golden-files-and-test-cases (%golden-parse-print-tester #'string=) golden-files))

  (let ((golden-files (uiop:directory-files *printer-test-files-gold-regex-directory* #P"*.quil")))
    (format t "~&  Gold-regex tests:")
    (is (not (null golden-files)))
    (map-golden-files-and-test-cases (%golden-parse-print-tester #'cl-ppcre:scan) golden-files)))

(deftest test-instruction-fmt ()
  (is (string= "PRAGMA gate_time CNOT \"50 ns\"" (format nil "~/cl-quil:instruction-fmt/"
                                                         (make-instance 'cl-quil::pragma
                                                                        :words '("gate_time" "CNOT")
                                                                        :freeform-string "50 ns"))))
  ;; try a operand-free instruction
  (is (string= "HALT" (format nil "~/cl-quil:instruction-fmt/" (make-instance 'halt))))

  ;; try a unary instruction
  (is (string= "NEG ro[3]" (format nil "~/cl-quil:instruction-fmt/"
                                   (make-instance 'cl-quil::classical-negate
                                                  :target (mref "ro" 3)))))
  ;; try a binary instruction
  (is (string= "MEASURE 1 ro[3]" (format nil "~/cl-quil:instruction-fmt/"
                                         (make-instance 'cl-quil::measure
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
      (not-signals error (cl-quil::parse-quil after)))))

(deftest test-circuit-and-declare-printing ()
  ;; This test relies on not applying the EXPAND-CIRCUITS transform,
  ;; otherwise it could be included in TEST-PRINT-PARSED-PROGRAM-GOLDEN-FILES, above.
  (let* ((before "DECLARE theta REAL[16]
DECLARE theta-bits BIT[100] SHARING theta OFFSET 1 REAL

DEFCIRCUIT TEST(%a) b c:
    RZ(%a) b
    RZ(%a) c

TEST(0.5) 0 1
")
         (after (parse-and-print-quil-to-string before :parser (lambda (string)
                                                                 (cl-quil::parse-quil string :transforms nil)))))
    (is (string= before after))))

(deftest test-jump-to-integer-label-printing ()
  "Ensure that JUMP instructions with integer LABELs are printed correctly."
  (is (string= (cl-quil::print-instruction-to-string
                (cl-quil::make-instance 'cl-quil::unconditional-jump :label 42))
               "JUMP {absolute address 42}"))
  (is (string= (cl-quil::print-instruction-to-string
                (cl-quil::make-instance 'cl-quil::jump-when :label 0 :address (cl-quil::mref "ro" 0)))
               "JUMP-WHEN {absolute address 0} ro[0]"))
  (is (string= (cl-quil::print-instruction-to-string
                (cl-quil::make-instance 'cl-quil::jump-unless :label 1 :address (cl-quil::mref "ro" 2)))
               "JUMP-UNLESS {absolute address 1} ro[2]")))


(defun %check-format-single (test-function format-control input expected-output
                             &key print-fractional-radians print-polar-form)
  (let ((cl-quil::*print-fractional-radians* print-fractional-radians)
        (cl-quil::*print-polar-form* print-polar-form))
    (is (funcall test-function expected-output (format nil format-control input)))))

(defun %check-format (test-function format-control testcases
                      &key print-fractional-radians print-polar-form)
  (loop :for (input . expected-output) :in testcases :do
    (%check-format-single test-function format-control input expected-output
                          :print-fractional-radians print-fractional-radians
                          :print-polar-form print-polar-form)))

(defun %pprint-rational-as-pi-multiple (r)
  "Approximate the pretty printing parts of CL-QUIL::FORMAT-REAL.

Used as a helper function in TEST-REAL-FMT."
  (check-type r (rational 0))
  (let ((p (numerator r))
        (q (denominator r)))
    (cond ((= 1 p q)
           (format nil "pi"))
          ((= 1 p)
           (format nil "pi/~D" q))
          ((= 1 q)
           (format nil "~D*pi" p))
          (t
           (format nil "~D*pi/~D" p q)))))

(deftest test-complex-fmt ()
  ;; Test some simple cases which should be unaffected by *PRINT-POLAR-FORM*
  (let ((simple-cases '((0 . "0.0")
                        (0.0 . "0.0")
                        (0.0d0 . "0.0")
                        (#C(0 0) . "0.0")
                        (#C(0.0 0.0) . "0.0")
                        (#C(0.0d0 0.0d0) . "0.0")
                        (42.42 . "42.42")
                        (42.42d0 . "42.42")
                        (#C(4.2 0) . "4.2"))))
    (%check-format #'string= "~/cl-quil:complex-fmt/" simple-cases :print-polar-form t)
    (%check-format #'string= "~/cl-quil:complex-fmt/" simple-cases :print-polar-form nil))

  ;; Test some cases with non-zero IMAGPART with *PRINT-POLAR-FORM* = NIL.
  (let ((complex-cases '((#C(0.0 1.0) . "1.0i")
                         (#C(0.0 -1.0) . "-1.0i")
                         (#C(-1.0 1.0) . "-1.0+1.0i")
                         (#C(-1.0 -1.0) . "-1.0-1.0i")
                         (#C(42.42 123.45) . "42.42+123.45i")
                         (#C(42.42d0 123.45d0) . "42.42+123.45i"))))
    (%check-format #'string= "~/cl-quil:complex-fmt/" complex-cases
                   :print-polar-form nil :print-fractional-radians t)
    (%check-format #'string= "~/cl-quil:complex-fmt/" complex-cases
                   :print-polar-form nil :print-fractional-radians nil))

  ;; Test *PRINT-POLAR-FORM* = T and *PRINT-FRACTIONAL-RADIANS* = T.
  (%check-format #'string=
                 "~/cl-quil:complex-fmt/"
                 `((#C(0.0 1.0) . "1.0∠pi/2")
                   (#C(0.0 -1.0) . "1.0∠3*pi/2")
                   (,(cis cl-quil::pi) . "1.0∠pi")
                   (,(cis (- cl-quil::pi)) . "1.0∠pi")
                   (,(cis (/ cl-quil::pi 2)) . "1.0∠pi/2")
                   (,(cis (/ cl-quil::pi 4)) . "1.0∠pi/4"))
                 :print-polar-form t
                 :print-fractional-radians t))

(deftest test-real-fmt ()
  ;; Test some simple cases which should be unaffected by *PRINT-FRACTIONAL-RADIANS*.
  (let ((simple-cases `(#-allegro (-1e-5 . "-0.00001")
                        (-1.1 . "-1.1")
                        (-1.1d0 . "-1.1")
                        (-1 . "-1.0")
                        (-1.0 . "-1.0")
                        (-4/5 . "-0.8")
                        (-0 . "0.0")
                        (0 . "0.0")
                        (0.0 . "0.0")
                        (0.0d0 . "0.0")
                        (1 . "1.0")
                        (1.0 . "1.0")
                        (3/2 . "1.5")
                        (2.3 . "2.3")
                        (1234.1234 . "1234.1234")
                        #-allegro (2e10 . "20000000000.0")
                        )))
    (%check-format #'string= "~/cl-quil:real-fmt/" simple-cases :print-fractional-radians t)
    (%check-format #'string= "~/cl-quil:real-fmt/" simple-cases :print-fractional-radians nil))

  ;; Test *PRINT-FRACTIONAL-RADIANS* = NIL.
  (%check-format #'uiop:string-prefix-p
                 "~/cl-quil:real-fmt/"
                 `((,(- cl-quil::pi) . "-3.1415")
                   (,cl-quil::pi . "3.1415")
                   (,(* (- 1) (/ cl-quil::pi 2)) . "-1.5707")
                   (,(* (- 3) (/ cl-quil::pi 2)) . "-4.7123")
                   (,(* (- 1) (/ cl-quil::pi 3)) . "-1.0471")
                   (,(* 1 (/ cl-quil::pi 4)) . "0.7853")
                   (,(* 5 (/ cl-quil::pi 6)) . "2.6179"))
                 :print-fractional-radians nil)

  ;; Test *PRINT-FRACTIONAL-RADIANS* = T.
  (let ((loose-threshold cl-quil::+double-comparison-threshold-loose+)
        ;; fudge-factor of 20x because numerical analysis is hard (let's go shopping)
        (epsilon (* 20 double-float-epsilon)))
    (flet ((in-bounds- (r)
             (+ (- r loose-threshold) epsilon))
           (in-bounds+ (r)
             (- (+ r loose-threshold) epsilon))
           (out-of-bounds- (r)
             (- r loose-threshold epsilon))
           (out-of-bounds+ (r)
             (+ r loose-threshold epsilon))
           (check (pred r expected)
             (%check-format-single pred "~/cl-quil:real-fmt/" r expected :print-fractional-radians t)))
      (loop :for rr :across cl-quil/frontend::**reasonable-rationals**
            :for rr-pi := (* rr cl-quil::pi)
            :for minus-rr-pi := (- rr-pi)
            :for rr-pi-expected := (%pprint-rational-as-pi-multiple rr)
            :for minus-rr-pi-expected := (concatenate 'string "-" rr-pi-expected) :do
              (progn
                (check #'string= (in-bounds- rr-pi) rr-pi-expected)
                (check #'string= rr-pi rr-pi-expected)
                (check #'string= (in-bounds+ rr-pi) rr-pi-expected)

                (check #'string= (in-bounds- minus-rr-pi) minus-rr-pi-expected)
                (check #'string= minus-rr-pi minus-rr-pi-expected)
                (check #'string= (in-bounds+ minus-rr-pi) minus-rr-pi-expected)

                (check (complement #'string=) (out-of-bounds- rr-pi) rr-pi-expected)
                (check (complement #'string=) (out-of-bounds+ rr-pi) rr-pi-expected)
                (check (complement #'string=) (out-of-bounds- minus-rr-pi) minus-rr-pi-expected)
                (check (complement #'string=) (out-of-bounds+ minus-rr-pi) minus-rr-pi-expected))))))
