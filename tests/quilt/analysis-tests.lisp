(in-package #:cl-quil.quilt-tests)

(deftest test-quilt-circuit-expansion ()
  (let ((pp (parse-quilt "
DECLARE iq REAL[1000]
DEFFRAME 0 \"rf\":
    SAMPLE-RATE: 1.0

DEFFRAME 0 \"ff\"

DEFCIRCUIT FOO(%theta) q:
    SET-PHASE q \"rf\" %theta
    SWAP-PHASE q \"rf\" q \"ff\"
    PULSE q \"rf\" flat(iq: 1.0, duration: 1.0)
    CAPTURE q \"rf\" flat(iq: 1.0, duration: 1.0) iq[0]
    RAW-CAPTURE q \"rf\" %theta iq[0]
    DELAY q %theta
    DELAY q \"rf\" %theta
    FENCE q

FOO(1.0) 0"
                         :transforms '(quil::expand-circuits)))
        (expected-instrs
          (list
           "SET-PHASE 0 \"rf\" 1.0"
           "SWAP-PHASE 0 \"rf\" 0 \"ff\""
           "PULSE 0 \"rf\" flat(iq: 1.0, duration: 1.0)"
           "CAPTURE 0 \"rf\" flat(iq: 1.0, duration: 1.0) iq[0]"
           "RAW-CAPTURE 0 \"rf\" 1.0 iq[0]"
           "DELAY 0 1.0"
           "DELAY 0 \"rf\" 1.0"
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
DEFFRAME 0 \"rf\"
DEFWAVEFORM foo:
    1.0, 1.0, 1.0

DEFCAL X q:
    PULSE q \"rf\" foo

PULSE 0 \"rf\" foo")))
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
DEFFRAME 0 \"rf\"
CAPTURE 0 \"rf\" flat(duration: 1.0, iq: 1.0) b")
        (real-prog "
DECLARE r REAL
DEFFRAME 0 \"rf\"
CAPTURE 0 \"rf\" flat(duration: 1.0, iq: 1.0) r")
        (valid-prog "
DECLARE iqs REAL[4]
DEFFRAME 0 \"rf\"
CAPTURE 0 \"rf\" flat(duration: 1.0, iq: 1.0) iqs[2]")
        (overshoot-prog "
DECLARE iqs REAL[4]
DEFFRAME 0 \"rf\"
CAPTURE 0 \"rf\" flat(duration: 1.0, iq: 1.0) iqs[3]"))
    (signals quil-type-error (parse-quilt bit-prog))
    (signals quil-type-error (parse-quilt real-prog))
    (is (parse-quilt valid-prog))
    (signals quil-type-error (parse-quilt overshoot-prog))))

(deftest test-raw-capture-type-safety ()
  (let ((bad "
DECLARE iqs REAL[10]
DEFFRAME 0 \"rf\":
    SAMPLE-RATE: 4.0
RAW-CAPTURE 0 \"rf\" 2.0 iqs")              ; 8 iq values = 16 reals
        (good "
DECLARE iqs REAL[10]
DEFFRAME 0 \"rf\":
    SAMPLE-RATE: 4.0
RAW-CAPTURE 0 \"rf\" 1.0 iqs"))
    (signals quil-type-error (parse-quilt bad))
    (is (parse-quilt good))))


(deftest test-waveform-ref= ()
  (let ((a (quilt:waveform-ref "foo" `((,(param "a") . ,(constant 1.0))
                                       (,(param "b") . ,(constant 2.0)))))
        (b (quilt:waveform-ref "foo" `((,(param "b") . ,(constant 2.0))
                                       (,(param "a") . ,(constant 1.0)))))
        (c (quilt:waveform-ref "foo" `((,(param "b") . ,(constant 2.0)))))
        (d (quilt:waveform-ref "frob" `((,(param "a") . ,(constant 1.0))
                                        (,(param "b") . ,(constant 2.0))))))
    (is (quilt::waveform-ref= a b))
    (is (not (quilt::waveform-ref= a c)))
    (is (not (quilt::waveform-ref= a d)))))


(deftest test-waveform-expansion ()
  (let* ((pp (parse-quilt (format nil "DEFFRAME 0 \"rf\"~@
                                       DEFWAVEFORM foo:~%    1.0, 1.0, 1.0~@
                                       PULSE 0 \"rf\" foo~@
                                       PULSE 0 \"rf\" flat(duration: 1.0, iq: 2.0*i)~@
                                       PULSE 0 \"rf\" gaussian(duration: 1.0, fwhm: 1.0, t0: 0.5)~@
                                       PULSE 0 \"rf\" drag_gaussian(duration: 1.0, fwhm: 1.0, t0: 0.5, anh: 0.5, alpha: 1.0)~@
                                       PULSE 0 \"rf\" hermite_gaussian(duration: 1.0, fwhm: 1.0, t0: 0.5, anh: 0.5, alpha: 1.0, hrm_coeff: 2.0)~@
                                       PULSE 0 \"rf\" erf_square(duration: 1.0, risetime: 0.1, pad_left: 0.5, pad_right: 1.0)")))
         (expanded (quilt::expand-template-waveforms pp 4)))
    (is (= 6 (length (parsed-program-waveform-definitions expanded))))
    ;; check static waveform entries
    (is (every (lambda (elt)
                 (= elt #C(1d0 0d0)))
               (waveform-definition-entries (nth 0 (parsed-program-waveform-definitions expanded)))))
    ;; check flat waveform entries
    (is (every (lambda (elt)
                 (= elt #C(0d0 2d0)))
               (waveform-definition-entries (nth 1 (parsed-program-waveform-definitions expanded)))))))

(deftest test-waveform-expansion-duplicates ()
  (let* ((pp (parse-quilt (format nil "DEFFRAME 0 \"rf\"~@
                                       PULSE 0 \"rf\" gaussian(duration: 1.0, fwhm: 1.0, t0: 0.5)~@
                                       PULSE 0 \"rf\" gaussian(duration: 1.0, fwhm: 1.0, t0: 0.5)")))
         (expanded (quilt::expand-template-waveforms pp 4)))
    (is (= 1 (length (parsed-program-waveform-definitions expanded))))))
