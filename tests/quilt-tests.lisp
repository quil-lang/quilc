;;; TODO deprecate these parsing tests
;;; (they are subsumed by the .quil example files)

(in-package #:cl-quil-tests)

(deftest test-gate-calibration-matching ()
  (let ((pp (parse-quil "
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
