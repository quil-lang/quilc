;;; TODO deprecate these parsing tests
;;; (they are subsumed by the .quil example files)

(in-package #:cl-quil-tests)

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


(deftest test-parsing-frame-mutations ()
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

;; # Raw capture
;; DECLARE iqs REAL[400] # length needs to be determined based on the sample rate
;; CAPTURE 0 "out" 200e-6 iqs
