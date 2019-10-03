(in-package #:cl-quil-tests)

(deftest test-gate-calibration-matching ()
  (let ((pp (parse-quil "
DEFFRAME 0 \"xy\"
DEFFRAME 1 \"xy\"

DEFCAL RZ(pi/2) 0:                      # 0
    SHIFT-PHASE 0 \"xy\" pi/2

DEFCAL RZ(%theta) 0:                    # 1
    SHIFT-PHASE 0 \"xy\" %theta

DEFCAL RZ(%theta) qubit:                # 2
    SHIFT-PHASE qubit \"xy\" %theta

RZ(0) 0                                 # 0
RZ(pi/2) 0                              # 1
RZ(pi/2) 1                              # 2
X 0                                     # 3
MEASURE 1                               # 4
"
                        :transforms nil)))
    ;; We want to check whether definition i matches instruction j
    (let ((matches '((0 (1))            ; def 0 vs instruction 1
                     (1 (0 1))
                     (2 (0 1 2)))))
      (dolist (calib matches)
        (destructuring-bind (defn-index match-indices) calib
          (let ((defn (elt (parsed-program-calibration-definitions pp)
                           defn-index)))
            (loop :for instr :across (parsed-program-executable-code pp)
                  :for i :from 0
                  :for result := (quil::calibration-matches-p defn instr)
                  :do (if (member i match-indices)
                          (is result)
                          (is (not result))))))))))

(deftest test-modified-calibration-matching ()
  (let ((pp (parse-quil "
DEFFRAME 0 \"xy\"

DEFCAL RZ(pi/2) 0:                      # 0
    SHIFT-PHASE 0 \"xy\" pi/2

DEFCAL DAGGER RZ(pi/2) 0:
    SHIFT-PHASE 0 \"xy\" -pi/2          # 1

RZ(pi/2) 0                              # 0
DAGGER RZ(pi/2) 0                       # 1
DAGGER DAGGER RZ(pi/2) 0                # 2
"
                        :transforms nil)))
    (let ((matches '((0 (0))
                     (1 (1)))))
      (dolist (calib matches)
        (destructuring-bind (defn-index match-indices) calib
          (let ((defn (elt (parsed-program-calibration-definitions pp)
                           defn-index)))
            (loop :for instr :across (parsed-program-executable-code pp)
                  :for i :from 0
                  :for result := (quil::calibration-matches-p defn instr)
                  :do (if (member i match-indices)
                          (is result)
                          (is (not result))))))))))

(deftest test-quilt-name-resolution ()
  (let ((pp (parse-quil "
DEFFRAME 0 \"xy\"
DEFWAVEFORM foo 1.0:
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
                          (quil::frame-name-resolution
                           (pulse-frame instr))))
                  (is (eq waveform-defn
                          (quil::waveform-ref-name-resolution
                           (pulse-waveform instr)))))))))

(deftest test-recursive-calibration ()
  (let ((pp (parse-quil "
DEFCAL X 0:
    RX(pi) 0

DEFCAL RX(%theta) q:
    NOP

X 0")))
    (quil::expand-calibrations pp)
    (is (= 1 (length (parsed-program-executable-code pp))))
    (is (typep (elt (parsed-program-executable-code pp) 0)
               'no-operation))))

(deftest test-infinitely-recursive-calibrations ()
  (let ((pp (parse-quil "
DEFCAL X 0:
    RX(pi) 0

DEFCAL RX(pi) 0:
    X 0

X 0" :transforms nil)))
    (signals quil-expansion-error
      (quil::expand-calibrations pp))))


(deftest test-measurement-calibration-matching ()
  (let ((pp (parse-quil "
DECLARE ro BIT

DEFCAL MEASURE 0:                       # 0
    NOP

DEFCAL MEASURE 0 dest:                  # 1
    NOP

DEFCAL MEASURE qubit:                   # 2
    NOP

DEFCAL MEASURE qubit dest:              # 3
    NOP

X 0                                     # 0
MEASURE 0                               # 1
MEASURE 1                               # 2
MEASURE 0 ro                            # 3
MEASURE 1 ro                            # 4
"
                        :transforms nil)))
    ;; We want to check whether definition i matches instruction j
    (let ((matches '((0 (1))            ; def 0 vs instruction 1
                     (1 (3))
                     (2 (1 2))
                     (3 (3 4)))))
      (dolist (calib matches)
        (destructuring-bind (defn-index match-indices) calib
          (let ((defn (elt (parsed-program-calibration-definitions pp)
                           defn-index)))
            (loop :for instr :across (parsed-program-executable-code pp)
                  :for i :from 0
                  :for result := (quil::calibration-matches-p defn instr)
                  :do (if (member i match-indices)
                          (is result)
                          (is (not result))))))))))

(deftest test-strict-calibration-expansion ()
  (let ((pp (parse-quil "
DEFFRAME 0 \"xy\"

DEFCAL X 0:
    PULSE 0 \"xy\" flat(duration: 1, iq: 1)

X 1")))
    (signals quil-expansion-error
      (quil::expand-calibrations pp))))

(deftest test-case-sensitive-calibration-expansion ()
  (let ((pp (parse-quil "
DEFFRAME 0 \"xy\"

DEFCAL x 0:
    PULSE 0 \"xy\" flat(duration: 1, iq: 1)

X 0")))
    (signals quil-expansion-error
      (quil::expand-calibrations pp))))

(deftest test-fence-expansion ()
  (let ((pp
          (parse-quil "
DEFFRAME 0 \"xy\"
DEFFRAME 1 \"xy\"

DECLARE iq REAL[2]

PULSE 0 \"xy\" flat(duration: 1, iq: 1)
FENCE 0 1
CAPTURE 0 \"xy\" flat(duration: 1, iq: 1) iq
CAPTURE 1 \"xy\" flat(duration: 1, iq: 1) iq
")))
    (quil::fill-delays pp)
    ;; no fences
    (map nil
         (lambda (instr)
           (is (not (typep instr 'fence))))
         (parsed-program-executable-code pp))
    (let ((instr (quil::nth-instr 1 pp)))
      (is (and (typep instr 'delay)
               (= 1.0 (constant-value  (delay-duration instr)))
               ;; we can't say which kind of delay is inserted,
               ;; so we just check either
               (typecase instr
                 (delay-on-frames
                  (every (lambda (f)
                           (quil::frame= f
                                         (frame (list (qubit 1)) "xy")))
                         (delay-frames instr)))
                 (delay-on-qubits
                  (every (lambda (q)
                           (= 1 (qubit-index q)))
                         (delay-qubits instr)))))))))

(deftest test-quilt-defwaveform-sample-rate ()
  (signals quil-parse-error
    (parse-quil "
DEFWAVEFORM foo:
    1.0, 1.0, 1.0, 1.0"))
  (signals quil-parse-error
    (parse-quil "
DEFWAVEFORM foo 4+2*i:
    1.0, 1.0, 1.0, 1.0"))
  (let ((pp (parse-quil "
DEFWAVEFORM foo 4.0:
    1.0, 1.0, 1.0, 1.0")))
    (is (= 4.0
           (constant-value
            (waveform-definition-sample-rate
             (first (parsed-program-waveform-definitions pp))))))))

(deftest test-quilt-duration ()
  (let ((pp (parse-quil "
DEFFRAME 0 \"xy\"

DEFWAVEFORM foo 2.0:
    1.0, 1.0, 1.0, 1.0

PULSE 0 \"xy\" gaussian(duration: 1.0, fwhm: 0.5, t0: 0.5)
PULSE 0 \"xy\" foo
")))
    (flet ((instr (i)
             (elt (parsed-program-executable-code pp) i)))
      (is (= 1.0 (quil::quilt-instruction-duration (instr 0))))
      (is (= 2.0 (quil::quilt-instruction-duration (instr 1)))))))

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
    (flet ((parse (raw)
             (parse-quil raw :transforms quil::*standard-quilt-transforms*)))
      (signals quil-type-error (parse bit-prog))
      (signals quil-type-error (parse real-prog))
      (is (parse valid-prog))
      (signals quil-type-error (parse overshoot-prog)))))

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
    (flet ((parse (raw)
             (parse-quil raw :transforms quil::*standard-quilt-transforms*)))
      (signals quil-type-error (parse bad))
      (is (parse good)))))

(deftest test-definition-signature ()
  (flet ((signature (raw-quil &rest args)
           (let ((pp (parse-quil (apply #'format nil raw-quil args))))
             (quil::definition-signature
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
                (signature "DEFCAL RX(0) s:~%    NOP")))
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

