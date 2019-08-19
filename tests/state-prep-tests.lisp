;;;; state-prep-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-tests)

(deftest test-state-prep-formation ()
  "Checks that STATE-PREP-APPLICATION (with SOURCE-WF in the ground state) correctly compiles into native instructions."
  (let* ((random-matrix (quil::random-special-unitary 16))
         (column (loop :for j :below 16 :collect (magicl:ref random-matrix j 0)))
         (instr (make-instance 'quil::state-prep-application
                               :arguments (mapcar #'quil::qubit (list 0 1 2 3))
                               :target-wf (make-array 16 :initial-contents column :element-type '(complex double-float))
                               :source-wf (make-array 16 :initial-element #C(0d0 0d0) :element-type '(complex double-float)))))
    (setf (aref (quil::state-prep-application-source-wf instr) 0) #C(1d0 0d0))
    (let* ((output-matrix (quil::make-matrix-from-quil
                           (quil::expand-to-native-instructions
                            (list instr)
                            (quil::build-8Q-chip))))
           (prefactor (/ (magicl:ref random-matrix 0 0) (magicl:ref output-matrix 0 0))))
      (is (loop :for j :below 16
                :always (quil::double= (magicl:ref random-matrix j 0)
                                       (* prefactor (magicl:ref output-matrix j 0))))))))

(deftest test-aqvm-unlink-refuses-large-GHZ-state ()
  "Checks that an AQVM correctly assembles a GHZ state and then correctly disables itself."
  (let ((aqvm (quil::build-aqvm 8))
        (quil (quil::parse-quil "
H 0
CNOT 0 1
CNOT 1 2
CNOT 2 3
")))
    (quil::transform 'quil::resolve-applications quil)
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

(deftest test-state-prep-1q-source-and-target ()
  "Checks that STATE-PREP-APPLICATION (with arbitrary SOURCE-WF and TARGET-WF) correctly compiles into native instructions."
  (let* ((random-matrix (quil::random-special-unitary 2))
         (column (loop :for j :below 2 :collect (magicl:ref random-matrix j 0)))
         (instr (make-instance 'quil::state-prep-application
                               :arguments (mapcar #'quil::qubit (list 0))
                               :target-wf (make-array 2 :initial-contents (list 1d0 0d0))
                               :source-wf (make-array 2 :initial-contents column))))
    (let* ((output-matrix (quil::make-matrix-from-quil
                           (quil::expand-to-native-instructions
                            (list instr)
                            (quil::build-8Q-chip)))))
      (is (quil::matrix-equality
           (magicl:multiply-complex-matrices output-matrix random-matrix)
           (magicl:diag 2 2 (list 1d0 1d0)))))))

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
    (quil::transform 'quil::resolve-applications pp)
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
