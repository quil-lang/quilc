;;; TODO deprecate these parsing tests
;;; (they are subsumed by the .quil example files)

(in-package #:cl-quil-tests)

(declaim (optimize (debug 3)))

(deftest test-parsing-waveform-definitions ()
  "Test whether we can parse DEFWAVEFORM."
  (not-signals quil-parse-error
    (cl-quil::parse-quil-into-raw-program
     "
DEFWAVEFORM my_custom_waveform:
    1+2*i, 3+4*i, 5+6*i

DEFWAVEFORM my_custom_parameterized_waveform(%a):
    (1+2*i)*%a, (3+4*i)*%a, (5+6*i)*%a
")))


(deftest test-parsing-simple-frame-mutations ()
  "Test whether we can parse various frame mutations."
  (not-signals quil-parse-error
    (cl-quil::parse-quil-into-raw-program
     "
SET-PHASE 0 \"xy\" pi/2

SHIFT-PHASE 0 \"xy\" -pi
SHIFT-PHASE 0 1 \"cz\" 1e-2

SET-SCALE 0 \"xy\" 0.75
")))

(deftest test-parsing-quilt-capture ()
  (not-signals quil-parse-error
    (cl-quil::parse-quil-into-raw-program
     "
# Simple capture of an IQ point
DECLARE iq REAL[2]
CAPTURE 0 \"out\" flat(1e-6, 2+3*i) iq
")))

(deftest test-parsing-simple-calibration ()
  (not-signals quil-parse-error
    (cl-quil::parse-quil-into-raw-program
     "
DEFCAL X 0:
    PULSE 0 \"xy\" gaussian(1, 2,3)

NOP
")))

(deftest test-parsing-parametric-calibration ()
  (not-signals quil-parse-error
    (cl-quil::parse-quil-into-raw-program
     "
DEFCAL RX(%theta) 0:
    PULSE 0 \"xy\" flat(1e-6, 2+3*i)

NOP
")))

(deftest test-parsing-calibration-any-qubit ()
  (not-signals quil-parse-error
    (cl-quil::parse-quil-into-raw-program
     "
DEFCAL RZ(%theta) qubit:
    SHIFT-PHASE qubit \"xy\" %theta

NOP
")))

(deftest test-parsing-measure-calibration ()
  (not-signals quil-parse-error
    (cl-quil::parse-quil-into-raw-program
     "
DEFCAL MEASURE 0 dest:
    CAPTURE 0 \"out\" flat(1e-6, 2+3*i) iq
    LT dest iq[0] 0.5

NOP
")))

;;;  TODO replace SET-PHASE 0 with something more appropriate
(deftest test-gate-calibration-matching ()
  (let ((pp (parse-quil "
DEFCAL RZ(pi/2) 0:                      # 0
    SHIFT-PHASE 0 \"xy\" pi/2

DEFCAL RZ(%theta) 0:                    # 1
    SHIFT-PHASE 0 \"xy\" %theta

DEFCAL RZ(%theta) qubit:                # 2
    SHIFT-PHASE 0 \"xy\" %theta

RZ(0) 0                                 # 0
RZ(pi/2) 0                              # 1
RZ(pi/2) 1                              # 2
X 0                                     # 3
MEASURE 1                               # 4
")))
    ;; We want to check whether definition i matches instruction j
    (let ((matches '((0 (1))            ; def 0 vs instruction 1
                     (1 (0 1))
                     (2 (0 1 2)))))
      (dolist (calib matches)
        (destructuring-bind (defn-index match-indices) calib
          (let ((defn (elt (quil::parsed-program-calibration-definitions pp)
                           defn-index)))
            (loop :for instr :across (parsed-program-executable-code pp)
                  :for i :from 0
                  :for result := (quil::calibration-matches-p defn instr)
                  :do (if (member i match-indices)
                          (is result)
                          (is (not result))))))))))


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
")))
    ;; We want to check whether definition i matches instruction j
    (let ((matches '((0 (1))            ; def 0 vs instruction 1
                     (1 (3))
                     (2 (1 2))
                     (3 (3 4)))))
      (dolist (calib matches)
        (destructuring-bind (defn-index match-indices) calib
          (let ((defn (elt (quil::parsed-program-calibration-definitions pp)
                           defn-index)))
            (loop :for instr :across (parsed-program-executable-code pp)
                  :for i :from 0
                  :for result := (quil::calibration-matches-p defn instr)
                  :do (if (member i match-indices)
                          (is result)
                          (is (not result))))))))))
