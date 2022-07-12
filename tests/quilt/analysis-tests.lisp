(in-package #:cl-quil/quilt-tests)

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
                         :transforms '(cl-quil::expand-circuits)))
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
                          (cl-quil::print-instruction-to-string instr))))))

(deftest test-quilt-name-resolution ()
  (let ((pp (parse-quilt "
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
                          (quilt::frame-name-resolution
                           (pulse-frame instr))))
                  (is (eq waveform-defn
                          (quilt::waveform-ref-name-resolution
                           (pulse-waveform instr)))))))))


(deftest test-quilt-duration ()
  (let ((pp (parse-quilt "
DEFFRAME 0 \"xy\"

DEFWAVEFORM foo 2.0:
    1.0, 1.0, 1.0, 1.0

PULSE 0 \"xy\" gaussian(duration: 1.0, fwhm: 0.5, t0: 0.5)
PULSE 0 \"xy\" foo
")))
    (flet ((instr (i)
             (elt (parsed-program-executable-code pp) i)))
      (is (= 1.0 (quilt::quilt-instruction-duration (instr 0))))
      (is (= 2.0 (quilt::quilt-instruction-duration (instr 1)))))))

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

(deftest test-frame-set-logic ()
  (let ((f (frame (list (qubit 0) (qubit 2)) "foo")))
    (flet ((intersects-p (&rest qubits)
             (funcall #'frame-intersects-p f (mapcar #'qubit qubits)))
           (on-p (&rest qubits)
             (funcall #'frame-on-p f (mapcar #'qubit qubits))))
      (is (intersects-p 0 5))
      (is (not (intersects-p)))
      (is (not (intersects-p 1 5)))
      (is (on-p 0 2))
      (is (not (on-p 0 2 3)))
      (is (not (on-p 2 0)))
      (is (not (on-p))))))

(deftest test-quilt-instruction-duration ()
  (flet ((duration= (duration instr-str)
           (let ((pp (parse-quilt
                      (format nil "DEFFRAME 0 \"xy\"~%DECLARE iq REAL[2]~%~A"
                              instr-str)
                      :transforms nil)))
             (= duration
                (quilt::quilt-instruction-duration
                 (cl-quil::nth-instr 0 pp))))))
    (is (duration= 1.5 "PULSE 0 \"xy\" flat(duration: 1.5, iq: 1)"))
    (is (duration= 1.5 "CAPTURE 0 \"xy\" flat(duration: 1.5, iq: 1) iq"))
    (is (duration= 1.5 "RAW-CAPTURE 0 \"xy\" 1.5 iq"))
    (is (duration= 1.5 "DELAY 0 1.5"))
    (is (duration= quilt::*quilt-seemingly-instantenous-duration* "SET-FREQUENCY 0 \"xy\" 1.0"))
    (is (duration= quilt::*quilt-seemingly-instantenous-duration* "SET-PHASE 0 \"xy\" 1.0"))
    (is (duration= quilt::*quilt-seemingly-instantenous-duration* "SHIFT-PHASE 0 \"xy\" 1.0"))))

(deftest test-fill-delays-swap-implicit-synchronization ()
  (let ((pp
          (parse-quilt "
DEFFRAME 0 \"xy\"
DEFFRAME 1 \"xy\"

DECLARE is REAL[2]

PULSE 0 \"xy\" flat(duration: 1, iq: 1)
SWAP-PHASE 0 \"xy\" 1 \"xy\"
PULSE 0 \"xy\" flat(duration: 1, iq: 1)
PULSE 1 \"xy\" flat(duration: 1, iq: 1)"))
        (clocks (quilt::make-frame-table)))
    (quilt::fill-delays pp)
    ;; advance clocks until SWAP-PHASE
    (loop :for instr :across (parsed-program-executable-code pp)
          :until (typep instr 'swap-phase)
          :do (loop :for frame :in (quilt::quilt-instruction-frames instr pp)
                    :do (incf (quilt::local-time clocks frame)
                              (quilt::quilt-instruction-duration instr))))
    (is (= (quilt::local-time clocks (frame (list (qubit 0)) "xy"))
           (quilt::local-time clocks (frame (list (qubit 1)) "xy"))))))

(deftest test-fill-delays-fence-expansion ()
  (let ((pp
          (parse-quilt "
DEFFRAME 0 \"xy\"
DEFFRAME 1 \"xy\"

DECLARE iq REAL[2]

PULSE 0 \"xy\" flat(duration: 1, iq: 1)
FENCE 0 1
CAPTURE 0 \"xy\" flat(duration: 1, iq: 1) iq
CAPTURE 1 \"xy\" flat(duration: 1, iq: 1) iq
")))
    (quilt::fill-delays pp)
    ;; no fences
    (map nil
         (lambda (instr)
           (is (not (typep instr 'fence))))
         (parsed-program-executable-code pp))
    (let ((instr (cl-quil::nth-instr 1 pp)))
      (is (and (typep instr 'delay)
               (= 1.0 (constant-value  (delay-duration instr)))
               ;; we can't say which kind of delay is inserted,
               ;; so we just check either
               (typecase instr
                 (delay-on-frames
                  (every (lambda (f)
                           (frame= f
                                   (frame (list (qubit 1)) "xy")))
                         (delay-frames instr)))
                 (delay-on-qubits
                  (every (lambda (q)
                           (= 1 (qubit-index q)))
                         (delay-qubits instr)))))))))
