(in-package #:cl-quil.quilt-tests)

(defvar *dummy-frame-definitions*
  "
DEFFRAME 0 1 \"cz\":
    SAMPLE-RATE: 1000000000.0
    INITIAL-FREQUENCY: 364250000.0
    DIRECTION: \"tx\"
    HARDWARE-OBJECT: \"q0_ff\"

DEFFRAME 0 \"rf\":
    SAMPLE-RATE: 1000000000.0
    INITIAL-FREQUENCY: 4807537342.41533
    DIRECTION: \"tx\"
    HARDWARE-OBJECT: \"q0_rf\"

DEFFRAME 1 \"rf\":
    SAMPLE-RATE: 1000000000.0
    INITIAL-FREQUENCY: 3357341778.52477
    DIRECTION: \"tx\"
    HARDWARE-OBJECT: \"q1_rf\"

DEFFRAME 0 \"ro_rx\":
    SAMPLE-RATE: 2000000000.0
    INITIAL-FREQUENCY: 7139370148.40106
    DIRECTION: \"rx\"
    HARDWARE-OBJECT: \"q0_ro_rx\"

DEFFRAME 1 \"ro_rx\":
    SAMPLE-RATE: 2000000000.0
    INITIAL-FREQUENCY: 7007052561.50131
    DIRECTION: \"rx\"
    HARDWARE-OBJECT: \"q1_ro_rx\"

DEFFRAME 0 \"ro_tx\":
    SAMPLE-RATE: 1000000000.0
    INITIAL-FREQUENCY: 7139370148.40106
    DIRECTION: \"tx\"
    HARDWARE-OBJECT: \"q0_ro_tx\"

DEFFRAME 1 \"ro_tx\":
    SAMPLE-RATE: 1000000000.0
    INITIAL-FREQUENCY: 7007052561.50131
    DIRECTION: \"tx\"
    HARDWARE-OBJECT: \"q1_ro_tx\"
")

(defun calibrate (text)
  (parse-quilt
   (concatenate 'string *dummy-frame-definitions* text)))

(deftest test-schedule-basic-counts ()
  (let ((schedule
          (quilt::schedule-to-hardware
           (calibrate "
DECLARE iq REAL[2]
PULSE 0 \"ro_tx\" flat(duration: 1.0, iq: 1.0)
SHIFT-PHASE 0 \"rf\" 0.2
SET-FREQUENCY 0 \"rf\" 10
CAPTURE 0 \"rf\" flat(duration: 1.0, iq: 1.0) iq
CAPTURE 1 \"rf\" flat(duration: 1.0, iq: 1.0) iq
DELAY 0 1 \"cz\" 1.0
PULSE 0 1 \"cz\" flat(duration: 1.0, iq: 1.0)
FENCE 0 1
FENCE
DELAY 0 1.0
"))))
    (flet ((num-instrs (hw)
             (length (gethash hw schedule))))
      (assert (= 4 (hash-table-count schedule)))
      (assert (= 1 (num-instrs "q0_ro_tx")))
      (assert (= 3 (num-instrs "q0_rf")))
      (assert (= 1 (num-instrs "q1_rf")))
      (assert (= 1 (num-instrs "q0_ff"))))))

;;; TODO: test swap-phase error

(defun instruction-start-time (instr schedule)
  (let ((hw-name (quilt::resolve-hardware-object instr)))
    (loop :for (time . op) :in (gethash hw-name schedule)
          :when (eql op instr)
            :do (return-from instruction-start-time time))))

(deftest test-schedule-nonblocking-pulses ()
  (let* ((pp (calibrate "
NONBLOCKING PULSE 0 \"ro_tx\" flat(duration: 1.0, iq: 1.0)
NONBLOCKING PULSE 0 \"ro_rx\" flat(duration: 1.0, iq: 1.0)
"))
        (schedule
          (quilt::schedule-to-hardware pp)))

    (assert (quil::double= 0.0d0 (instruction-start-time (quil::nth-instr 0 pp) schedule)))
    (assert (quil::double= 0.0d0 (instruction-start-time (quil::nth-instr 1 pp) schedule)))))

(deftest test-schedule-mixed-pulses ()
  (let* ((pp (calibrate "
NONBLOCKING PULSE 0 \"ro_tx\" flat(duration: 1.0, iq: 1.0)
PULSE 0 \"ro_rx\" flat(duration: 1.0, iq: 1.0)
"))
         (schedule
           (quilt::schedule-to-hardware pp)))

    (assert (quil::double= 0.0d0 (instruction-start-time (quil::nth-instr 0 pp) schedule)))
    ;; the second pulse obstructs qubit 0, and so cannot occur simultaneously
    ;; with the first pulse
    (assert (quil::double= 1.0d0 (instruction-start-time (quil::nth-instr 1 pp) schedule)))))

(deftest test-schedule-fenced-pulses ()
  (let* ((pp (calibrate "
PULSE 0 \"rf\" flat(duration: 1.0, iq: 1.0)
FENCE 0
PULSE 1 \"rf\" flat(duration: 1.5, iq: 1.0)
FENCE 0 1
PULSE 0 \"rf\" flat(duration: 1.0, iq: 1.0)
"))
         (schedule
           (quilt::schedule-to-hardware pp)))

    (assert (quil::double= 0.0d0 (instruction-start-time (quil::nth-instr 0 pp) schedule)))
    ;; the first FENCE does nto delay the pulse on 1 "rf"
    (assert (quil::double= 0.0d0 (instruction-start-time (quil::nth-instr 2 pp) schedule)))
    ;; the second fence does delay the following pulse
    (assert (quil::double= 1.5d0 (instruction-start-time (quil::nth-instr 4 pp) schedule)))))

(deftest test-schedule-global-fence ()
  (let* ((pp (calibrate "
DECLARE iq REAL[2]
PULSE 0 \"rf\" flat(duration: 0.1, iq: 1.0)
CAPTURE 1 \"ro_rx\" flat(duration: 0.5, iq: 1.0) iq
FENCE
PULSE 0 \"rf\" flat(duration: 1.0, iq: 1.0)
CAPTURE 1 \"ro_rx\" flat(duration: 1.0, iq: 1.0) iq
"))
         (schedule
           (quilt::schedule-to-hardware pp)))

    (assert (quil::double= 0.0d0 (instruction-start-time (quil::nth-instr 0 pp) schedule)))
    (assert (quil::double= 0.0d0 (instruction-start-time (quil::nth-instr 1 pp) schedule)))
    ;; the second fence does delay the following pulse
    (assert (quil::double= 0.5d0 (instruction-start-time (quil::nth-instr 3 pp) schedule)))
    (assert (quil::double= 0.5d0 (instruction-start-time (quil::nth-instr 4 pp) schedule)))))

(deftest test-rigid-pulse-pair ()
  (let* ((pp (calibrate "
PULSE 0 \"rf\" flat(duration: 0.123, iq: 1.0)
PULSE 0 \"rf\" flat(duration: 1.0, iq: 1.0)
"))
         (schedule (quilt::schedule-to-hardware pp)))
    (assert (quil::double= 0.0d0 (instruction-start-time (quil::nth-instr 0 pp) schedule)))
    (assert (quil::double= 0.123d0 (instruction-start-time (quil::nth-instr 1 pp) schedule)))))

(deftest test-delay-qubit ()
  (let* ((pp (calibrate "
PULSE 0 \"rf\" flat(duration: 0.123, iq: 1.0)
DELAY 0 0.456
PULSE 0 \"ro_tx\" flat(duration: 1.0, iq: 1.0)
"))
         (schedule (quilt::schedule-to-hardware pp)))
    (assert (quil::double= 0.0d0 (instruction-start-time (quil::nth-instr 0 pp) schedule)))
    ;; 0 "rf" pulse obstructs all frames on qubit 0
    ;; DELAY on 0 then obstructs 0 "ro_tx" pulse
    (assert (quil::double= 0.579d0
                           (instruction-start-time (quil::nth-instr 2 pp) schedule)))))

(deftest test-delay-frame ()
  (let* ((pp (calibrate "
PULSE 0 \"rf\" flat(duration: 0.123, iq: 1.0)
DELAY 0 \"ro_tx\" 0.456
PULSE 0 \"ro_tx\" flat(duration: 1.0, iq: 1.0)
"))
         (schedule (quilt::schedule-to-hardware pp)))
    (assert (quil::double= 0.0d0 (instruction-start-time (quil::nth-instr 0 pp) schedule)))
    ;; PULSE 0 obstructs all frames on qubit 0
    ;; DELAY on 0 "rf" occurs after this, and obstructs PULSE 0 "ro_tx"
    (assert (quil::double= 0.579d0
                           (instruction-start-time (quil::nth-instr 2 pp) schedule)))))

(deftest test-delay-frame-after-nonblocking ()
  (let* ((pp (calibrate "
NONBLOCKING PULSE 0 \"rf\" flat(duration: 0.123, iq: 1.0)
DELAY 0 \"ro_tx\" 0.456
PULSE 0 \"ro_tx\" flat(duration: 1.0, iq: 1.0)
"))
         (schedule (quilt::schedule-to-hardware pp)))
    (assert (quil::double= 0.0d0 (instruction-start-time (quil::nth-instr 0 pp) schedule)))
    ;; NONBLOCKING PULSE only obstructs its own frame
    ;; DELAY obstructs the other one
    (assert (quil::double= 0.456d0
                           (instruction-start-time (quil::nth-instr 2 pp) schedule)))))

(deftest test-swap-forces-synchronization ()
  (let* ((pp (calibrate "
PULSE 0 \"rf\" flat(duration: 1.0, iq: 1.0)
SWAP-PHASE 0 \"rf\" 1 \"rf\"
PULSE 0 \"rf\" flat(duration: 1.0, iq: 1.0)
PULSE 1 \"rf\" flat(duration: 1.0, iq: 1.0)
"))
         (schedule (quilt::schedule-to-hardware pp)))
    (assert (quil::double= 0.0d0 (instruction-start-time (quil::nth-instr 0 pp) schedule)))
    (assert (quil::double= 1.0d0 (instruction-start-time (quil::nth-instr 1 pp) schedule)))
    (assert (quil::double= 1.0d0 (instruction-start-time (quil::nth-instr 2 pp) schedule)))
    (assert (quil::double= 1.0d0 (instruction-start-time (quil::nth-instr 2 pp) schedule)))))

(deftest test-quilt-instruction-duration ()
  (flet ((duration= (duration instr-str)
           (let ((pp (parse-quilt
                      (format nil "DEFFRAME 0 \"xy\"~%DECLARE iq REAL[2]~%~A"
                              instr-str)
                      :transforms nil)))
             (= duration
                (quilt::quilt-instruction-duration
                 (quil::nth-instr 0 pp))))))
    (is (duration= 1.5 "PULSE 0 \"xy\" flat(duration: 1.5, iq: 1)"))
    (is (duration= 1.5 "CAPTURE 0 \"xy\" flat(duration: 1.5, iq: 1) iq"))
    (is (duration= 1.5 "RAW-CAPTURE 0 \"xy\" 1.5 iq"))
    (is (duration= 1.5 "DELAY 0 1.5"))
    (is (duration= quilt::*quilt-seemingly-instantenous-duration* "SET-FREQUENCY 0 \"xy\" 1.0"))
    (is (duration= quilt::*quilt-seemingly-instantenous-duration* "SET-PHASE 0 \"xy\" 1.0"))
    (is (duration= quilt::*quilt-seemingly-instantenous-duration* "SHIFT-PHASE 0 \"xy\" 1.0"))))
