(in-package #:cl-quil/quilt-tests)

(defparameter *good-quilt-test-file-directory*
  (asdf:system-relative-pathname
   ':cl-quil/quilt-tests
   "tests/quilt/good-test-files/"))

(defparameter *bad-quilt-test-file-directory*
  (asdf:system-relative-pathname
   ':cl-quil/quilt-tests
   "tests/quilt/bad-test-files/"))

(deftest test-parsing-good-quilt-test-files ()
  "Test whether all valid test files parse."
  (dolist (file (uiop:directory-files *good-quilt-test-file-directory* #P"*.quil"))
    (format t "~&    Testing good file ~A~%" (pathname-name file))
    (let ((cl-quil:*allow-unresolved-applications* t))
      (not-signals quil-parse-error
        (read-quilt-file file)))))

(deftest test-parsing-bad-quilt-test-files ()
  "Test whether all invalid test files signal a parse error."
  (dolist (file (uiop:directory-files *bad-quilt-test-file-directory* #P"*.quil"))
    (format t "~&    Testing bad file ~A~%" (pathname-name file))
    (let ((cl-quil:*allow-unresolved-applications* t))
      (signals cl-quil:quil-parse-error
        (handler-case (read-quilt-file file)
          ;; Re-signal all of the following errors as
          ;; QUIL-PARSE-ERRORs.
          (cl-quil:quil-type-error (c)
            (declare (ignore c))
            (error 'cl-quil:quil-parse-error))
          (yacc:yacc-parse-error (c)
            (declare (ignore c))
            (error 'cl-quil:quil-parse-error))
          (alexa:lexer-match-error (c)
            (declare (ignore c))
            (error 'cl-quil:quil-parse-error)))))))

(deftest test-parser-fail-without-extensions ()
  "Test that Quilt parsing fails with PARSE-QUIL."
  (signals cl-quil::quil-parse-error
    (parse-quil "DEFFRAME 0 \"foo\"")))

(deftest test-quilt-defwaveform-sample-rate ()
  (signals quil-parse-error
    (parse-quilt "
DEFWAVEFORM foo:
    1.0, 1.0, 1.0, 1.0"))
  (signals quil-parse-error
    (parse-quilt "
DEFWAVEFORM foo 4+2*i:
    1.0, 1.0, 1.0, 1.0"))
  (let ((pp (parse-quilt "
DEFWAVEFORM foo 4.0:
    1.0, 1.0, 1.0, 1.0")))
    (is (= 4.0
           (constant-value
            (waveform-definition-sample-rate
             (first (parsed-program-waveform-definitions pp))))))))

(defun prints-as (expected obj &key (accessor #'cl-quil:parsed-program-executable-code))
  "Checks whether OBJ prints as the EXPECTED string. If OBJ is a string, parses OBJ and then checks that the first instruction prints as EXPECTED."
  (typecase obj
    (string
     (prints-as expected
                (elt (funcall accessor
                              (parse-quilt obj :transforms nil))
                     0)))
    (otherwise
     (is (string= expected
                  (cl-quil::print-instruction-to-string obj))))))

(deftest test-print-quilt-objects ()
  (prints-as "0 \"xy\""
             (frame (list (qubit 0)) "xy"))
  (prints-as "0 1 \"foo\""
             (frame (list (qubit 0) (qubit 1)) "foo"))
  (prints-as "foo(duration: 1.0, t1: 0.000001)"
             (waveform-ref "foo" (list (cons (param "duration") (constant 1.0))
                                       (cons (param "t1") (constant 1e-6))))))

(deftest test-parse-and-print-quilt-instructions ()
  (let ((boilerplate "
DEFFRAME 0 \"rf\"
DEFFRAME 0 \"zz\"
DEFFRAME 0 1 \"foo\"
DECLARE iq REAL[2]
DEFWAVEFORM wf 1.0:
    1.0, 1.0, 1.0
")
        (instrs (list
                 "SET-FREQUENCY 0 \"rf\" 1.0"
                 "SET-FREQUENCY 0 1 \"foo\" 1.0"
                 "SHIFT-FREQUENCY 0 \"rf\" 1.0"
                 "SHIFT-FREQUENCY 0 1 \"foo\" 1.0"
                 "SET-PHASE 0 \"rf\" 1.0"
                 "SET-PHASE 0 1 \"foo\" 1.0"
                 "SHIFT-PHASE 0 \"rf\" 1.0"
                 "SHIFT-PHASE 0 1 \"foo\" 1.0"
                 "SET-SCALE 0 \"rf\" 1.0"
                 "SET-SCALE 0 1 \"foo\" 1.0"
                 "SWAP-PHASE 0 \"rf\" 0 1 \"foo\""
                 "PULSE 0 \"rf\" wf"
                 "PULSE 0 1 \"foo\" wf"
                 "NONBLOCKING PULSE 0 1 \"foo\" wf"
                 "CAPTURE 0 \"rf\" wf iq[0]"
                 "NONBLOCKING CAPTURE 0 \"rf\" wf iq[0]"
                 "RAW-CAPTURE 0 \"rf\" 1.0 iq[0]"
                 "NONBLOCKING RAW-CAPTURE 0 \"rf\" 1.0 iq[0]"
                 "DELAY 0 1.0"          ; delay on qubit
                 "DELAY 0 \"rf\" 1.0"   ; delay on frame
                 "DELAY 0 \"rf\" \"zz\" 1.0" ; delay on frames
                 "FENCE 0 1"
                 "FENCE")))
    (dolist (instr instrs)
      (prints-as instr
                 (concatenate 'string boilerplate instr)))))


;;; NOTE: The convention in PRINT-INSTRUCTION-GENERIC is that definitions print
;;; a newline following an indented body.
(deftest test-parse-and-print-quilt-definitions ()
  (let ((boilerplate "~%DEFFRAME 0 \"rx\"") ; tacked on at end
        (frame-defns (list
                      "DEFFRAME 0 1 \"xy\""
                      "DEFFRAME 0 1 \"xy\":~%    SAMPLE-RATE: 1.0~%"
                      "DEFFRAME 0 1 \"xy\":~%    SAMPLE-RATE: 1.0~%    INITIAL-FREQUENCY: 1.0~%"
                      "DEFFRAME 0 1 \"xy\":~%    INITIAL-FREQUENCY: 1.0~%    DIRECTION: \"rx\"~%"
                      "DEFFRAME 0 1 \"xy\":~%    HARDWARE-OBJECT: \"q0_q1_xy\"~%"
                      "DEFFRAME 0 1 \"xy\":~%    SAMPLE-RATE: 1.0~%    DIRECTION: \"tx\"~%"))
        (waveform-defns (list
                         "DEFWAVEFORM foo 1.0:~%    1.0~%"
                         "DEFWAVEFORM foo 1.0:~%    1.0+1.0i, 1.0+1.0i~%"
                         ;; case sensitivity
                         "DEFWAVEFORM FOO 1.0:~%    1.0~%"
                         ;; parametric waveform def
                         ;; this is a bit too dependent on how arithmetic expressions are printed...
                         "DEFWAVEFORM foo(%theta) 1.0:~%    (%theta*(1.0))~%"
                         ))
        (calibration-defns (list        ; just sticking delays in here to have something nontrivial to print
                            "DEFCAL FOO 0:~%    DELAY 0 1.0~%    NOP~%"
                            "DEFCAL FOO q:~%    DELAY q 1.0~%    NOP~%"
                            "DEFCAL FOO(%d) q:~%    DELAY q %d~%    NOP~%"
                            "DEFCAL MEASURE 0:~%    DELAY 0 1.0~%    NOP~%"
                            "DEFCAL MEASURE q:~%    DELAY q 1.0~%    NOP~%"
                            "DEFCAL MEASURE 0 iq:~%    RAW-CAPTURE 0 \"rx\" 1.0 iq~%")))
    (flet ((verify-definitions (defns accessor)
             (dolist (def defns)
               (prints-as (format nil def)
                          (concatenate 'string (format nil def) (format nil boilerplate))
                          :accessor accessor))))
      (verify-definitions frame-defns #'parsed-program-frame-definitions)
      (verify-definitions waveform-defns #'parsed-program-waveform-definitions)
      (verify-definitions calibration-defns #'parsed-program-calibration-definitions))))

(deftest test-definition-signature ()
  (flet ((signature (raw-quil &rest args)
           (let ((pp (parse-quilt (apply #'format nil raw-quil args))))
             (cl-quil/frontend::definition-signature
              (first (append (parsed-program-gate-definitions pp)
                             (parsed-program-circuit-definitions pp)
                             (parsed-program-waveform-definitions pp)
                             (parsed-program-calibration-definitions pp)
                             (parsed-program-frame-definitions pp)
                             (parsed-program-memory-definitions pp)))))))
    (is (equalp (signature "DEFFRAME 0 \"foo\"")
                (signature "DEFFRAME 0 \"foo\"")))
    (is (not (equalp (signature "DEFFRAME 0 \"foo\"")
                     (signature "DEFFRAME 0 \"Foo\""))))
    (is (equalp (signature "DEFWAVEFORM foo 1.0:~%    1.0, 1.0")
                (signature "DEFWAVEFORM foo 1.0:~%    1.0, 1.0")))
    (is (not (equalp (signature "DEFWAVEFORM foo 1.0:~%    1.0, 1.0")
                     (signature "DEFWAVEFORM Foo 1.0:~%    1.0, 1.0"))))
    (is (equalp (signature "DEFCAL RX(%theta) q:~%    NOP")
                (signature "DEFCAL RX(%theta) q:~%    NOP")))
    (is (equalp (signature "DEFCAL RX(0) 0:~%    NOP")
                (signature "DEFCAL RX(0) 0:~%    NOP")))
    ;; argument names don't matter
    (is (equalp (signature "DEFCAL RX(0) s:~%    NOP")
                (signature "DEFCAL RX(0) t:~%    NOP")))
    ;; parameter names don't matter
    (is (equalp (signature "DEFCAL RX(%theta) q:~%    NOP")
                (signature "DEFCAL RX(%foo) q:~%    NOP")))
    (is (not (equalp (signature "DEFCAL RX(%theta) q:~%    NOP")
                     (signature "DEFCAL RX(%theta) 0:~%    NOP"))))
    ;; measure
    (is (not (equalp (signature "DEFCAL MEASURE q ro:~%    NOP")
                     (signature "DEFCAL MEASURE 0 ro:~%    NOP"))))
    (is (equalp (signature "DEFCAL MEASURE 0 ro:~%    NOP")
                (signature "DEFCAL MEASURE 0 foo:~%    NOP")))
    ;; meaure discard
    (is (equalp (signature "DEFCAL MEASURE q:~%    NOP")
                (signature "DEFCAL MEASURE q:~%    NOP")))
    (is (equalp (signature "DEFCAL MEASURE 0:~%    NOP")
                (signature "DEFCAL MEASURE 0:~%    NOP")))
    (is (not (equalp (signature "DEFCAL MEASURE q:~%    NOP")
                     (signature "DEFCAL MEASURE 0:~%    NOP"))))
    ;; measure vs measure discard
    (is (not (equalp (signature "DEFCAL MEASURE q:~%    NOP")
                     (signature "DEFCAL MEASURE q ro:~%    NOP"))))))
