;;;; tests/defcircuit-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil-tests)

(defun verify-h-cnot-code (code)
  (is (= 3 (length code)))
  (destructuring-bind (h cnot reset) (coerce code 'list)
    (is (string= "H" (quil::application-operator-name h)))
    (is (= 0 (quil:qubit-index (elt (quil:application-arguments h) 0))))

    (is (string= "CNOT" (quil::application-operator-name cnot)))
    (is (= 0 (quil:qubit-index (elt (quil:application-arguments cnot) 0))))
    (is (= 1 (quil:qubit-index (elt (quil:application-arguments cnot) 1))))
    
    (is (= 0 (quil::qubit-index (quil::reset-qubit-target reset))))))

(defun verify-rx-code (code)
  (is (= 1 (length code)))
  (let ((rx (elt code 0)))
    (is (string= "RX" (quil::application-operator-name rx)))
    (is (= 1 (length (quil:application-parameters rx))))
    (is (= 1 (length (quil:application-arguments rx))))
    (is (= 0 (quil:qubit-index (elt (quil:application-arguments rx) 0))))
    (is (zerop (quil:constant-value (elt (quil:application-parameters rx) 0))))))

(deftest test-alias-defcircuit ()
  "Test a simple parameter-less DEFCIRCUIT expansion."
  (let ((p (with-output-to-quil
             "DEFCIRCUIT BELL:"
             "    H 0"
             "    CNOT 0 1"
             "    RESET 0"
             "BELL"
             )))
    (verify-h-cnot-code (quil:parsed-program-executable-code p))))

(deftest test-simple-defcircuit-with-arguments ()
  "Test that arguments can be passed to DEFCIRCUIT."
  (let ((p (with-output-to-quil
             "DEFCIRCUIT BELL p q:"
             "    H p"
             "    CNOT p q"
             "    RESET p"
             "BELL 0 1"
             )))
    (verify-h-cnot-code (quil:parsed-program-executable-code p))))

(deftest test-simple-defcircuit-with-parameter ()
  "Test that a parameter can be passed to a DEFCIRCUIT."
  (let ((p (with-output-to-quil
             "DEFCIRCUIT ROT(%a) p:"
             "    RX(%a) p"
             "ROT(0.0) 0"
             )))
    (verify-rx-code (quil:parsed-program-executable-code p))))

(deftest test-simple-defcircuit-with-simple-indirection ()
  "Test that arguments can be passed from a DEFCIRCUIT to an inner one.."
  (let ((p (with-output-to-quil
             "DEFCIRCUIT BELL p q:"
             "    H p"
             "    CNOT p q"
             "    RESET p"
             "DEFCIRCUIT INDIRECTION:"
             "    BELL 0 1"
             "INDIRECTION")))
    (verify-h-cnot-code (quil:parsed-program-executable-code p))))

(deftest test-simple-defcircuit-with-parameter-and-indirection ()
  "Test that a parameter and argument can be passed from a parameter-less DEFCIRCUIT."
  (let ((p (with-output-to-quil
             "DEFCIRCUIT ROT(%a) p:"
             "    RX(%a) p"
             "DEFCIRCUIT INDIRECTION:"
             "    ROT(0.0) 0"
             "INDIRECTION")))
    (verify-rx-code (quil:parsed-program-executable-code p))))

(deftest test-simple-defcircuit-with-argument-passing ()
  "Test that arguments get passed from outer DEFCIRCUITs to inner ones."
  (let ((p (with-output-to-quil
             "DEFCIRCUIT BELL p q:"
             "    H p"
             "    CNOT p q"
             "    RESET p"
             "DEFCIRCUIT INDIRECTION r s:"
             "    BELL s r"
             "INDIRECTION 1 0")))
    (verify-h-cnot-code (quil:parsed-program-executable-code p))))

(deftest test-simple-defcircuit-with-parameter-passing ()
  "Test that parameters get passed from outer DEFCIRCUITs to inner ones."
  (let ((p (with-output-to-quil
             "DEFCIRCUIT ROT(%a) p:"
             "    RX(%a) p"
             "DEFCIRCUIT INDIRECTION(%b):"
             "    ROT(%b) 0"
             "INDIRECTION(0.0)")))
    (verify-rx-code (quil:parsed-program-executable-code p))))

(deftest test-simple-defcircuit-with-mixed-parameter-passing ()
  "Test that some parameters in a DEFCIRCUIT can be filled while others not."
  (let ((p (with-output-to-quil
             "DEFCIRCUIT ROT(%a, %b) p:"
             "    FOO(%a, %b) p"
             "DEFCIRCUIT INDIRECTION(%b):"
             "    FOO(0.0, %b) 0"
             "INDIRECTION(1.0)")))
    (let ((code (quil:parsed-program-executable-code p)))
      (is (= 1 (length code)))
      (let ((rx (elt code 0)))
        (is (string= "FOO" (quil::application-operator-name rx)))
        (is (= 2 (length (quil:application-parameters rx))))
        (is (= 1 (length (quil:application-arguments rx))))
        (is (= 0 (quil:qubit-index (elt (quil:application-arguments rx) 0))))
        (is (zerop (quil:constant-value (elt (quil:application-parameters rx) 0))))
        (is (= 1 (quil:constant-value (elt (quil:application-parameters rx) 1))))))))

(deftest test-defcircuit-recursion-limit ()
  "Test that unbounded recursion is detected."
  (signals simple-error
    (with-output-to-quil
      "DEFCIRCUIT RECUR:"
      "    RECUR"
      "RECUR")))

(deftest test-chained-arithmetic-in-defcircuit ()
  "Test nested arithmetic in a sequence of DEFCIRCUITs."
  (let ((p (with-output-to-quil
             "DEFCIRCUIT LEVEL-1(%a):"
             "    RX(4 + %a) 0"
             "DEFCIRCUIT LEVEL-2(%b):"
             "    LEVEL-1(-2 * %b)"
             "DEFCIRCUIT LEVEL-3(%c):"
             "    LEVEL-2(%c - 1)"
             ;; 4 + (-2 * (3 - 1)) = 0
             "LEVEL-3(3)")))
    (verify-rx-code (quil:parsed-program-executable-code p))))

(deftest test-chained-defcircuit-other-instructions ()
  "Test recursive DEFCIRCUIT expansion with MEASUREs and JUMPs."
  (let ((p (with-output-to-quil
             "DECLARE ro BIT"
             "DEFCIRCUIT INNER(%p) qubit addr:"
             "    LABEL @start1"
             "    MEASURE qubit"
             "    MEASURE qubit addr"
             "    JUMP-WHEN @start1 addr"
             "    JUMP-UNLESS @start1 addr"
             "DEFCIRCUIT OUTER(%p) qubit addr:"
             "    LABEL @start2"
             "    MEASURE qubit"
             "    MEASURE qubit addr"
             "    JUMP-WHEN @start2 addr"
             "    JUMP-UNLESS @start2 addr"
             "    INNER(%p) qubit addr"
             "OUTER(0.0) 0 ro[0]")))
    (let ((code (quil:parsed-program-executable-code p)))
      (is (= 8 (length code)))
      (destructuring-bind (meas-dis1 meas1 jw1 ju1 meas-dis2 meas2 jw2 ju2) (coerce code 'list)
        (is (= 0 (qubit-index (measurement-qubit meas-dis1))))
        (is (= 0 (qubit-index (measurement-qubit meas1))))
        (is (= 0 (memory-ref-position (measure-address meas1))))
        (is (= 0 (memory-ref-position (conditional-jump-address jw1))))
        (is (= 0 (memory-ref-position (conditional-jump-address ju1))))

        (is (= 0 (qubit-index (measurement-qubit meas-dis2))))
        (is (= 0 (qubit-index (measurement-qubit meas2))))
        (is (= 0 (memory-ref-position (measure-address meas2))))
        (is (= 0 (memory-ref-position (conditional-jump-address jw2))))
        (is (= 0 (memory-ref-position (conditional-jump-address ju2))))))))

(deftest test-defcircuit-with-classical-instructions ()
  "Test DEFCIRCUIT expansion with classical instructions."
  (let ((p (with-output-to-quil
             "DECLARE ro BIT[2]"
             "DEFCIRCUIT CLASSICAL a b:"
             "    NOT a"
             "    AND a b"
             "    IOR b a"
             "    MOVE a b"
             "    EXCHANGE b a"
             "CLASSICAL ro[0] ro[1]")))
    (let ((code (quil:parsed-program-executable-code p)))
      (is (= 5 (length code)))
      (destructuring-bind (not and ior move exchange) (coerce code 'list)
        (is (typep not 'quil:classical-not))
        (is (typep and 'quil:classical-and))
        (is (typep ior 'quil:classical-inclusive-or))
        (is (typep move 'quil:classical-move))
        (is (typep exchange 'quil:classical-exchange))

        (is (= 0 (memory-ref-position (classical-target not))))

        (is (= 0 (memory-ref-position (classical-left-operand and))))
        (is (= 1 (memory-ref-position (classical-right-operand and))))

        (is (= 1 (memory-ref-position (classical-left-operand ior))))
        (is (= 0 (memory-ref-position (classical-right-operand ior))))

        (is (= 0 (memory-ref-position (classical-left-operand move))))
        (is (= 1 (memory-ref-position (classical-right-operand move))))

        (is (= 1 (memory-ref-position (classical-left-operand exchange))))
        (is (= 0 (memory-ref-position (classical-right-operand exchange))))))))

(deftest test-defcircuit-unique-labels ()
  "Test that DEFCIRCUIT gets unique labels."
  (let* ((p (not-signals simple-error
              (with-output-to-quil
                "DECLARE ro BIT"
                "DEFCIRCUIT FOO:"
                "    LABEL @INNER"
                "    JUMP @INNER"
                "    JUMP-WHEN @INNER ro[0]"
                "    JUMP-UNLESS @INNER ro[0]"
                "FOO"
                "FOO")))
         (code (quil:parsed-program-executable-code p)))
    (is (= 6 (length code)))
    (destructuring-bind (j1 jw1 ju1 j2 jw2 ju2) (coerce code 'list)
      (let ((label1 (jump-label j1))
            (label2 (jump-label j2)))
        (is (/= label1 label2))
        (is (= label1 (jump-label jw1)))
        (is (= label1 (jump-label ju1)))
        (is (= label2 (jump-label jw2)))
        (is (= label2 (jump-label ju2)))))))

(deftest test-defcircuit-dagger ()
  "Test application DAGGER modifier on a circuit application."
  (let* ((p (with-output-to-quil
              "DEFCIRCUIT BELL:"
              "    H 0"
              "    CNOT 0 1"
              "DAGGER BELL"))
         (code (quil:parsed-program-executable-code p)))
    (destructuring-bind (instr-cnot instr-h)
        (mapcar (alexandria:compose #'quil::operator-description-string
                                    #'quil:application-operator)
                (coerce code 'list))
      (is (string= "DAGGER CNOT" instr-cnot))
      (is (string= "DAGGER H" instr-h)))))

(deftest test-defcircuit-dagger-nested ()
  "Test application of DAGGER modifier on a circuit application that itself contains a DAGGER."
  (let* ((p (with-output-to-quil
              "DEFCIRCUIT A:"
              "    H 0"
              "    CNOT 0 1"
              "DEFCIRCUIT B:"
              "    DAGGER A"
              "DAGGER B"))
         (code (quil:parsed-program-executable-code p)))
    ;; Note that here the order of operations is reversed yet again,
    ;; so that the H 0 instruction is back on top.
    (destructuring-bind (instr-h instr-cnot)
        (mapcar (alexandria:compose #'quil::operator-description-string
                                    #'quil:application-operator)
                (coerce code 'list))
      (is (string= "H" instr-h))
      (is (string= "CNOT" instr-cnot)))))
