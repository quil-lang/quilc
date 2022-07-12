(in-package #:cl-quil/quilt-tests)

(deftest test-gate-calibration-matching ()
  (let ((pp (parse-quilt "
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
                  :for result := (quilt::calibration-matches-p defn instr)
                  :do (if (member i match-indices)
                          (is result)
                          (is (not result))))))))))

(deftest test-modified-calibration-matching ()
  (let ((pp (parse-quilt "
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
                  :for result := (quilt::calibration-matches-p defn instr)
                  :do (if (member i match-indices)
                          (is result)
                          (is (not result))))))))))


(deftest test-recursive-calibration ()
  (let ((pp (parse-quilt "
DEFCAL X 0:
    RX(pi) 0

DEFCAL RX(%theta) q:
    NOP

X 0")))
    (quilt::expand-calibrations pp)
    (is (= 1 (length (parsed-program-executable-code pp))))
    (is (typep (elt (parsed-program-executable-code pp) 0) 'cl-quil::no-operation))))

(deftest test-infinitely-recursive-calibrations ()
  (let ((pp (parse-quilt "
DEFCAL X 0:
    RX(pi) 0

DEFCAL RX(pi) 0:
    X 0

X 0" :transforms nil)))
    (signals quil-expansion-error
      (quilt::expand-calibrations pp))))


(deftest test-measurement-calibration-matching ()
  (let ((pp (parse-quilt "
DECLARE ro BIT
DECLARE foo BIT[2]

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
MEASURE 0 ro[0]                         # 5
MEASURE 0 foo[1]                        # 6
"
                        :transforms nil)))
    ;; We want to check whether definition i matches instruction j
    (let ((matches '((0 (1))            ; def 0 vs instruction 1
                     (1 (3 5 6))
                     (2 (1 2))
                     (3 (3 4 5 6)))))
      (dolist (calib matches)
        (destructuring-bind (defn-index match-indices) calib
          (let ((defn (elt (parsed-program-calibration-definitions pp)
                           defn-index)))
            (loop :for instr :across (parsed-program-executable-code pp)
                  :for i :from 0
                  :for result := (quilt::calibration-matches-p defn instr)
                  :do (if (member i match-indices)
                          (is result)
                          (is (not result))))))))))

(deftest test-strict-calibration-expansion ()
  (let ((pp (parse-quilt "
DEFFRAME 0 \"xy\"

DEFCAL X 0:
    PULSE 0 \"xy\" flat(duration: 1, iq: 1)

X 1" :transforms nil)))
    (signals quil-expansion-error
      (quilt::expand-calibrations pp))))

(deftest test-case-sensitive-calibration-expansion ()
  (let ((pp (parse-quilt "
DEFFRAME 0 \"xy\"

DEFCAL x 0:
    PULSE 0 \"xy\" flat(duration: 1, iq: 1)

X 0" :transforms nil)))
    (signals quil-expansion-error
      (quilt::expand-calibrations pp))))
