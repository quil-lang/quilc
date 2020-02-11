(in-package #:cl-quil.quilt-tests)

(deftest test-quilt-circuit-expansion ()
  (let ((pp (parse-quilt "
DECLARE iq REAL[1000]
DEFFRAME 0 \"xy\":
    SAMPLE-RATE: 1.0

DEFFRAME 0 \"ff\"

DEFCIRCUIT FOO(%theta) q:
    SET-PHASE q \"xy\" %theta
    SWAP-PHASE q \"xy\" q \"ff\"
    PULSE q \"xy\" flat(iq: 1.0, duration: %theta)
    CAPTURE q \"xy\" flat(iq: 1.0, duration: %theta) iq[0]
    RAW-CAPTURE q \"xy\" %theta iq[0]
    DELAY q %theta
    DELAY q \"xy\" %theta
    FENCE q

FOO(1.0) 0"
                         :transforms '(quil::expand-circuits)))
        (expected-instrs
          (list
           "SET-PHASE 0 \"xy\" 1.0"
           "SWAP-PHASE 0 \"xy\" 0 \"ff\""
           "PULSE 0 \"xy\" flat(iq: 1.0, duration: 1.0)"
           "CAPTURE 0 \"xy\" flat(iq: 1.0, duration: 1.0) iq[0]"
           "RAW-CAPTURE 0 \"xy\" 1.0 iq[0]"
           "DELAY 0 1.0"
           "DELAY 0 \"xy\" 1.0"
           "FENCE 0")))
    (is (= (length (parsed-program-executable-code pp))
           (length expected-instrs)))
    (loop :for instr :across (parsed-program-executable-code pp)
          :for expected :in expected-instrs
          :do
             (is (string= expected
                          (quil::print-instruction-to-string instr))))))

(deftest test-quilt-name-resolution ()
  (let ((pp (parse-quilt "
DEFFRAME 0 \"xy\"
DEFWAVEFORM foo:
    1.0, 1.0, 1.0

DEFCAL X q:
    PULSE q \"xy\" foo

PULSE 0 \"xy\" foo")))
    (let ((frame-defn (first
                       (parsed-program-frame-definitions pp)))
          (waveform-defn (first
                          (parsed-program-waveform-definitions pp))))
      (loop :for instr :across (parsed-program-executable-code pp)
            :do (progn
                  (is (eq frame-defn
                          (quilt::frame-name-resolution
                           (pulse-frame instr))))
                  (is (eq waveform-defn
                          (quilt::waveform-ref-name-resolution
                           (pulse-waveform instr)))))))))


(deftest test-quilt-duration ()
  (let ((pp (parse-quilt "
DEFFRAME 0 \"rf\"

DEFFRAME 1 \"rf\":
    SAMPLE-RATE: 2.0

DEFWAVEFORM foo:
    1.0, 1.0, 1.0, 1.0

PULSE 0 \"rf\" gaussian(duration: 1.0, fwhm: 0.5, t0: 0.5)
PULSE 0 \"rf\" foo
PULSE 1 \"rf\" foo
")))
    (flet ((instr (i)
             (elt (parsed-program-executable-code pp) i)))
      (is (= 1.0 (quilt::quilt-instruction-duration (instr 0))))
      (signals quilt::quilt-scheduling-error
        (quilt::quilt-instruction-duration (instr 1)))
      (is (= 2.0 (quilt::quilt-instruction-duration (instr 2)))))))

(deftest test-capture-type-safety ()
  (let ((bit-prog "
DECLARE b BIT
DEFFRAME 0 \"xy\"
CAPTURE 0 \"xy\" flat(duration: 1.0, iq: 1.0) b")
        (real-prog "
DECLARE r REAL
DEFFRAME 0 \"xy\"
CAPTURE 0 \"xy\" flat(duration: 1.0, iq: 1.0) r")
        (valid-prog "
DECLARE iqs REAL[4]
DEFFRAME 0 \"xy\"
CAPTURE 0 \"xy\" flat(duration: 1.0, iq: 1.0) iqs[2]")
        (overshoot-prog "
DECLARE iqs REAL[4]
DEFFRAME 0 \"xy\"
CAPTURE 0 \"xy\" flat(duration: 1.0, iq: 1.0) iqs[3]"))
    (signals quil-type-error (parse-quilt bit-prog))
    (signals quil-type-error (parse-quilt real-prog))
    (is (parse-quilt valid-prog))
    (signals quil-type-error (parse-quilt overshoot-prog))))

(deftest test-raw-capture-type-safety ()
  (let ((bad "
DECLARE iqs REAL[10]
DEFFRAME 0 \"xy\":
    SAMPLE-RATE: 4.0
RAW-CAPTURE 0 \"xy\" 2.0 iqs")              ; 8 iq values = 16 reals
        (good "
DECLARE iqs REAL[10]
DEFFRAME 0 \"xy\":
    SAMPLE-RATE: 4.0
RAW-CAPTURE 0 \"xy\" 1.0 iqs"))
    (signals quil-type-error (parse-quilt bad))
    (is (parse-quilt good))))
