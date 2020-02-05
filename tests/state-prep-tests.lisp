;;;; state-prep-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-tests)

(defun wf-to-matrix (wf)
  "Convert a sequence WF to a corresponding column vector."
  (magicl:make-matrix :rows (length wf)
                      :cols 1
                      :data (copy-seq wf)))

(defun check-state-prep (source-wf target-wf matrix)
  "Checks whether SOURCE-WF maps to TARGET-WF under the specified MATRIX."
  (let* ((result (magicl:multiply-complex-matrices matrix
                                                   (wf-to-matrix source-wf)))
         (prefactor (/ (aref target-wf 0) (magicl:ref result 0 0))))
    (is (quil::matrix-equality (magicl:scale prefactor result)
                               (wf-to-matrix target-wf)))))

(deftest test-state-prep-formation ()
  "Checks that STATE-PREP-APPLICATION (with SOURCE-WF in the ground state) correctly compiles into native instructions."
  (let* ((qubits (mapcar #'quil:qubit (list 0 1 2 3)))
         (target-wf (quil::random-wavefunction (length qubits)))
         (source-wf (quil::build-ground-state (length qubits)))
         (instr (make-instance 'quil::state-prep-application
                               :arguments qubits
                               :target-wf target-wf
                               :source-wf source-wf)))
    (let* ((output-matrix (quil::make-matrix-from-quil
                           (quil::expand-to-native-instructions
                            (list instr)
                            (quil::build-8Q-chip)))))
      (check-state-prep source-wf target-wf output-matrix))))

(deftest test-aqvm-unlink-refuses-large-GHZ-state ()
  "Checks that an AQVM correctly assembles a GHZ state and then correctly disables itself."
  (let ((aqvm (quil::build-aqvm 8))
        (quil (quil::parse-quil "
H 0
CNOT 0 1
CNOT 1 2
CNOT 2 3
")))
    (dolist (instr (coerce (quil::parsed-program-executable-code quil) 'list))
      (quil::aqvm-apply-instruction aqvm instr))
    ;; check that the correct state was constructed
    (destructuring-bind (wf qc-complex)
        (quil::aqvm-extract-single-wf-component aqvm 0)
      (declare (ignore qc-complex))
      (is (and (quil::double~ (/ (sqrt 2)) (aref wf 0))
               (quil::double~ (/ (sqrt 2)) (aref wf 15)))))
    ;; now check that unlinking the AQVM kills this state, since it is too entangled
    (quil::aqvm-unlink aqvm)
    (loop :for i :below 4
          :do (destructuring-bind (wf qc-complex)
                  (quil::aqvm-extract-single-wf-component aqvm i)
                (declare (ignore qc-complex))
                (is (eql ':not-simulated wf))))
    (destructuring-bind (wf qc-complex)
        (quil::aqvm-extract-single-wf-component aqvm 4)
      (declare (ignore qc-complex))
      (every #'quil::double~ wf (quil::build-ground-state 1)))))

(deftest test-state-prep-2Q-source-and-target ()
  "Checks that STATE-PREP-APPLICATION (both with arbitrary and with adversarial SOURCE-WF and TARGET-WF) correctly compiles into native instructions."
  (flet ((grab-row (m j)
           (make-array 4
                       :element-type '(complex double-float)
                       :initial-contents (loop :for i :below 4
                                               :collect (magicl:ref m i j))))
         (build-state-prep (source-wf target-wf)
           (make-instance 'quil::state-prep-application
                          :operator (named-operator "STATE-PREP")
                          :arguments (list (qubit 1) (qubit 0))
                          :source-wf source-wf
                          :target-wf target-wf)))
    (let ((chip (quil::build-8q-chip)))
      (dotimes (j 10)
        (let* ((unentangled-matrix (quil::make-matrix-from-quil
                                    (list (quil::anon-gate "U0" (quil::random-special-unitary 2) 0)
                                          (quil::anon-gate "U1" (quil::random-special-unitary 2) 1))))
               (entangled-matrix (quil::random-special-unitary 4)))
          (loop :for (source-wf target-wf) :in (list ;; entangled-entangled
                                                     (list (grab-row entangled-matrix 0)
                                                           (grab-row entangled-matrix 1))
                                                     ;; unentangled-entangled
                                                     (list (grab-row unentangled-matrix 0)
                                                           (grab-row entangled-matrix 2))
                                                     ;; entangled-unentangled
                                                     (list (grab-row entangled-matrix 3)
                                                           (grab-row unentangled-matrix 1))
                                                     ;; unentangled-unentangled
                                                     (list (grab-row unentangled-matrix 2)
                                                           (grab-row unentangled-matrix 3)))
                :for state-instr := (build-state-prep source-wf target-wf)
                :for state-circuit := (quil::expand-to-native-instructions
                                       (quil::state-prep-2Q-compiler state-instr) chip)
                :for circuit-result := (quil::nondestructively-apply-instrs-to-wf state-circuit source-wf
                                                                                  (list 0 1))
                :do (is (quil::collinearp target-wf circuit-result))))))))

(deftest test-state-prep-1q-source-and-target ()
  "Checks that STATE-PREP-APPLICATION (with arbitrary SOURCE-WF and TARGET-WF) correctly compiles into native instructions."
  (let* ((source-wf (quil::random-wavefunction 1))
         (target-wf (quil::random-wavefunction 1))
         (instr (make-instance 'quil::state-prep-application
                               :arguments (mapcar #'quil::qubit (list 0))
                               :target-wf target-wf
                               :source-wf source-wf)))
    (let* ((output-matrix (quil::make-matrix-from-quil
                           (quil::expand-to-native-instructions
                            (list instr)
                            (quil::build-8Q-chip)))))
      (check-state-prep source-wf target-wf output-matrix))))

(deftest test-state-prep-4q-compiler ()
  "Check that STATE-PREP-4Q-COMPILER (with arbitrary SOURCE-WF and TARGET-WF) correctly compiles into native instructions."
  (let* ((qubits (mapcar #'quil::qubit (list 0 1 2 3)))
         (source-wf (quil::random-wavefunction 4))
         (target-wf (quil::random-wavefunction 4))
         (instr (make-instance 'quil::state-prep-application
                               :arguments qubits
                               :target-wf target-wf
                               :source-wf source-wf))
         (output-matrix (quil::make-matrix-from-quil
                         (quil::expand-to-native-instructions
                          (quil::state-prep-4q-compiler instr)
                          (quil::build-8Q-chip)))))
    (check-state-prep source-wf target-wf output-matrix)))

(deftest test-schmidt-decomposition ()
  "Check that a random wavefunction can be reconstructed from its SCHMIDT-DECOMPOSITION."
  (flet ((matrix-column (m i)
           (magicl::slice m
                          0 (magicl:matrix-rows m)
                          i (1+ i))))
    (let* ((random-wf (quil::random-wavefunction 4)))
      (multiple-value-bind (c U V) (quil::schmidt-decomposition random-wf 2 2)
        (let* ((schmidt-terms (loop :for i :from 0 :below 4
                                    :collect (magicl:scale (aref c i)
                                                           (magicl::kron
                                                            (matrix-column U i)
                                                            (matrix-column V i)))))
               (reconstructed-wf (apply #'magicl:add-matrix schmidt-terms)))
          ;; adjust for column major nonsense
          (is (quil::matrix-equality reconstructed-wf
                                     (wf-to-matrix random-wf))))))))

(deftest test-aqvm-unlink-on-10Q ()
  (let ((quil::*aqvm-correlation-threshold* 4)
        (aqvm (quil::build-aqvm 11))
        (pp (quil::parse-quil "
# set up wf
RX(1.0) 3
RX(1.3) 1
RX(1.4) 0
RX(-0.2) 6
RX(-0.4) 7
RX(-0.6) 8
RX(-0.8) 9
RX(1.2) 2
RX(0.5) 5
RX(0.7) 4
CNOT 5 2
CNOT 6 7
CNOT 7 8
CNOT 8 9
CNOT 5 1
CNOT 0 4
# formally entangle qubits
CNOT 1 4
CNOT 1 4
CNOT 6 5
CNOT 6 5
CNOT 3 5
CNOT 3 5
")))
    (dolist (instr (coerce (parsed-program-executable-code pp) 'list))
      (quil::aqvm-apply-instruction aqvm instr))
    (quil::aqvm-stop-simulating aqvm 10)
    (destructuring-bind (wf qubit-list)
        (quil::aqvm-extract-state aqvm (list 0 1 2 3 4 5 6 7 8 9)
                                  :destructive-update nil)
      (quil::aqvm-unlink aqvm)
      (destructuring-bind (new-wf new-qubit-list)
          (quil::aqvm-extract-state aqvm (list 0 1 2 3 4 5 6 7 8 9))
        ;; check wf against new-wf
        (is (loop :for j :below (ash 1 10)
                  :always (let ((wf-index (loop :for i :from 0
                                                :for idx :in qubit-list
                                                :sum (ash (ldb (byte 1 idx) j) (- 9 i))))
                                (new-wf-index (loop :for i :from 0
                                                    :for idx :in new-qubit-list
                                                    :sum (ash (ldb (byte 1 idx) j) (- 9 i)))))
                            (quil::double= (aref wf wf-index)
                                           (aref new-wf new-wf-index)))))
        ;; check new-wf has small components
        (is (loop :for wf :across (quil::antisocial-qvm-wfs aqvm)
                  :for expected-size :in (list 4 8 8 2 4 8 16 16 16 16 ':not-simulated)
                  :always (if (eql ':not-simulated wf)
                              (eql ':not-simulated expected-size)
                              (= expected-size (array-total-size wf)))))))))


(deftest test-canonicalize-wf ()
  (dotimes (n 100)
    (let ((wf (quil::random-wavefunction 2)))
      (multiple-value-bind (m v)
          (quil::canonicalize-wf wf)
        (is (every #'quil::double=
                   v
                   (quil::nondestructively-apply-matrix-to-vector m wf)))
        (is (quil::double= 0d0 (imagpart (aref v 1))))
        (is (quil::double= 0d0 (aref v 2)))
        (is (quil::double= 0d0 (aref v 3)))))))
