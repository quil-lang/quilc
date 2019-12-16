(in-package #:cl-quil-tests)

;;; Tests covering the OpenQASM spec
;;;
;;; For what is ostensibly the latest version of the spec, see
;;; https://github.com/Qiskit/openqasm/blob/master/spec/qasm2.rst
;;;
;;; Some of these tests only test that parsing proceeds without error,
;;; others test that a particular error is raised, and the bulk should
;;; test that parsing produces an equivalent program (up to supported
;;; features).

(defparameter *qasm-qelib.inc-path*
  (asdf:system-relative-pathname
   ':cl-quil-tests
   "tests/qasm-files/qelib1.inc"))

(deftest test-qasm-openqasm ()
  (is (quil.qasm::parse-qasm "OPENQASM 2.0")))

(deftest test-qasm-creg/qreg-declaration ()
  (let* ((qasm "creg c1[1]; creg c2[1]; qreg q[1]; measure q[0] -> c1[0]; measure q[0] -> c2[0];")
         (quil (quil.qasm::parse-qasm qasm))
         (mems (quil:parsed-program-memory-definitions quil))
         (code (quil:parsed-program-executable-code quil)))
    (is (= 2 (length mems)))
    (is (= 2 (length code)))

    ;; Can't guarantee the ordering of the memory regions, so gotta
    ;; look up the region by name.
    (let* ((mem (find "c1" mems :key #'quil:memory-descriptor-name :test #'equalp))
           (cod (elt code 0))
           (des (quil::memory-ref-descriptor (quil:measure-address cod))))
      (is (not (null mem)))
      (is (eql quil::quil-bit (quil:memory-descriptor-type mem)))
      (is (= 1 (quil:memory-descriptor-length mem)))
      (is (equalp mem des)))
    ;; c2
    (let* ((mem (find "c2" mems :key #'quil:memory-descriptor-name :test #'equalp))
           (cod (elt code 1))
           (des (quil::memory-ref-descriptor (quil:measure-address cod))))
      (is (not (null mem)))
      (is (eql quil::quil-bit (quil:memory-descriptor-type mem)))
      (is (= 1 (quil:memory-descriptor-length mem)))
      (is (equalp mem des)))))

(deftest test-qasm-include ()
  (uiop:with-temporary-file (:stream stream :pathname path)
    (format stream "creg c[1]; qreg q[2]; measure q[1] -> c[0];")
    (force-output stream)
    (let* ((qasm (format nil "include \"~A\";" path))
           (quil (quil.qasm::parse-qasm qasm))
           (mems (quil:parsed-program-memory-definitions quil))
           (code (quil:parsed-program-executable-code quil)))
      (is (= 1 (length mems)))
      (is (find "c" mems :key #'quil:memory-descriptor-name :test #'equalp))
      (is (typep (elt code 0) 'quil:measure)))))

(deftest test-qasm-gate-definition-and-application ()
  (let* ((qasm (format nil "include ~S; qreg a[2]; qreg b[2]; gate bell q, r { h q; cx q, r; }; bell a[0], b[1];"
                       (namestring *qasm-qelib.inc-path*)))
         (quil (quil.qasm::parse-qasm qasm))
         (circ (quil:parsed-program-circuit-definitions quil))
         (bell (find (quil.qasm::%qasm-gate-name "bell")
                     circ
                     :key #'quil:circuit-definition-name
                     :test #'string=)))
    (is bell)
    (is (= 2 (length (quil:circuit-definition-body bell))))
    (is (quil::matrix-equals-dwim
         (quil:parsed-program-to-logical-matrix quil)
         (quil:parsed-program-to-logical-matrix
          (quil:parse-quil "H 0; CNOT 0 3;"))))))

(deftest test-qasm-gate-definition-and-application-on-qreg ()
  (let* ((qasm (format nil "include ~S; qreg a[2]; qreg b[2]; gate bell q, r { h q; cx q, r; }; bell a, b;"
                       (namestring *qasm-qelib.inc-path*)))
         (quil (quil.qasm::parse-qasm qasm))
         (circ (quil:parsed-program-circuit-definitions quil))
         (bell (find (quil.qasm::%qasm-gate-name "bell")
                     circ
                     :key #'quil:circuit-definition-name
                     :test #'string=)))
    (is bell)
    (is (= 2 (length (quil:circuit-definition-body bell))))
    (is (quil::matrix-equals-dwim
         (quil:parsed-program-to-logical-matrix quil)
         (quil:parsed-program-to-logical-matrix
          (quil:parse-quil "H 0; CNOT 0 2; H 1; CNOT 1 3;"))))))

(deftest test-qasm-opaque-gate-definition-and-application ()
  ;; opaque definitions and applications produce pragmas, rather than
  ;; a circuit definition + application.
  (let* ((qasm "qreg q[2]; opaque gate bell q, r; bell q[0], q[1];")
         (quil (quil.qasm::parse-qasm qasm))
         (circ (quil:parsed-program-circuit-definitions quil))
         (code (quil:parsed-program-executable-code quil)))
    (is (= 0 (length circ)))
    (is (= 2 (length code)))
    (map nil (lambda (a b)
               (is (typep a 'quil:pragma))
               (is (equal (quil:pragma-words a)
                          (quil:pragma-words b)))
               (is (equal (quil:pragma-freeform-string a)
                          (quil:pragma-freeform-string a))))
         code
         (list (quil:make-pragma '("QASM_OPAQUE_DEFINITION"  "bell") "() q, r")
               (quil:make-pragma '("QASM_OPAQUE_APPLICATION" "bell") "() 0, 1")))))

(deftest test-qasm-comment ()
  ;; Comments are stripped.
  (let* ((qasm "// hi
qreg q[2]; CX q[0], q[1]; // bye")
         (quil (quil.qasm::parse-qasm qasm))
         (code (quil:parsed-program-executable-code quil)))
    (is (= 1 (length code)))))

(deftest test-qasm-single-qubit-u ()
  ;; U is compiled into the appropriate identity

  ;; u1(lambda) = u3(0, 0, lambda)
  (let* ((qasm (format nil "include ~S; qreg q[1]; u1(1.0) q[0];"
                       (namestring *qasm-qelib.inc-path*)))
         (quil (quil.qasm::parse-qasm qasm))
         (code (quil:parsed-program-executable-code quil)))
    (is (= 3 (length code)))
    
    (is (equal "RZ" (quil::application-operator-root-name (elt code 0))))
    (is (= 1 (quil:constant-value (first (quil:application-parameters (elt code 0))))))
    (is (= 0 (quil:qubit-index (first (quil:application-arguments (elt code 0))))))
    
    (is (equal "RY" (quil::application-operator-root-name (elt code 1))))
    (is (= 0 (quil:constant-value (first (quil:application-parameters (elt code 1))))))
    (is (= 0 (quil:qubit-index (first (quil:application-arguments (elt code 1))))))

    (is (equal "RZ" (quil::application-operator-root-name (elt code 2))))
    (is (= 0 (quil:constant-value (first (quil:application-parameters (elt code 2))))))
    (is (= 0 (quil:qubit-index (first (quil:application-arguments (elt code 2)))))))

  ;; u2(phi, lambda) = u3(pi/2, phi, lambda)
  (let* ((qasm (format nil "include ~S; qreg q[1]; u2(0.5, 1) q[0];"
                       (namestring *qasm-qelib.inc-path*)))
         (quil (quil.qasm::parse-qasm qasm))
         (code (quil:parsed-program-executable-code quil)))
    (is (= 3 (length code)))
    
    (is (equal "RZ" (quil::application-operator-root-name (elt code 0))))
    (is (= 1 (quil:constant-value (first (quil:application-parameters (elt code 0))))))
    (is (= 0 (quil:qubit-index (first (quil:application-arguments (elt code 0))))))
    
    (is (equal "RY" (quil::application-operator-root-name (elt code 1))))
    (is (= quil:pi/2 (quil:constant-value (first (quil:application-parameters (elt code 1))))))
    (is (= 0 (quil:qubit-index (first (quil:application-arguments (elt code 1))))))

    (is (equal "RZ" (quil::application-operator-root-name (elt code 2))))
    (is (= 0.5 (quil:constant-value (first (quil:application-parameters (elt code 2))))))
    (is (= 0 (quil:qubit-index (first (quil:application-arguments (elt code 2)))))))

  ;; u3(theta, phi, lambda) = RZ(phi) RY(theta) RZ(lambda)
  (let* ((qasm (format nil "include ~S; qreg q[1]; u3(-0.5, 0.5, 1) q[0];"
                       (namestring *qasm-qelib.inc-path*)))
         (quil (quil.qasm::parse-qasm qasm))
         (code (quil:parsed-program-executable-code quil)))
    (is (= 3 (length code)))
    
    (is (equal "RZ" (quil::application-operator-root-name (elt code 0))))
    (is (= 1 (quil:constant-value (first (quil:application-parameters (elt code 0))))))
    (is (= 0 (quil:qubit-index (first (quil:application-arguments (elt code 0))))))
    
    (is (equal "RY" (quil::application-operator-root-name (elt code 1))))
    (is (= -0.5 (quil:constant-value (first (quil:application-parameters (elt code 1))))))
    (is (= 0 (quil:qubit-index (first (quil:application-arguments (elt code 1))))))

    (is (equal "RZ" (quil::application-operator-root-name (elt code 2))))
    (is (= 0.5 (quil:constant-value (first (quil:application-parameters (elt code 2))))))
    (is (= 0 (quil:qubit-index (first (quil:application-arguments (elt code 2))))))))

(deftest test-qasm-measure ()
  (let* ((qasm "qreg q[1]; creg c[1]; measure q[0] -> c[0];")
         (quil (quil.qasm::parse-qasm qasm))
         (cmem (quil:parsed-program-memory-definitions quil))
         (code (quil:parsed-program-executable-code quil)))
    (is (= 1 (length cmem)))
    (is (= 1 (length code)))
    (is (typep (elt code 0) 'quil:measure))
    (is (= 0 (quil:qubit-index (quil:measurement-qubit (elt code 0)))))))

(deftest test-qasm-measure-on-qreg ()
  (let* ((qasm "qreg q[2]; creg c[1]; measure q -> c[0];")
         (quil (quil.qasm::parse-qasm qasm))
         (cmem (quil:parsed-program-memory-definitions quil))
         (code (quil:parsed-program-executable-code quil)))
    (is (= 1 (length cmem)))
    (is (= 2 (length code)))
    (is (typep (elt code 0) 'quil:measure))
    (is (typep (elt code 1) 'quil:measure))
    (is (= 0 (quil:qubit-index (quil:measurement-qubit (elt code 0)))))
    (is (= 1 (quil:qubit-index (quil:measurement-qubit (elt code 1)))))
    ;; Should expand into same memory destination
    (is (= 0 (quil:memory-ref-position (quil:measure-address (elt code 0)))))
    (is (= 0 (quil:memory-ref-position (quil:measure-address (elt code 1)))))
    (is (= 1 (quil:memory-descriptor-length (first cmem)))))
  (let* ((qasm "qreg q[2]; creg c[2]; measure q -> c;")
         (quil (quil.qasm::parse-qasm qasm))
         (cmem (quil:parsed-program-memory-definitions quil))
         (code (quil:parsed-program-executable-code quil)))
    (is (= 1 (length cmem)))
    (is (= 2 (length code)))
    (is (typep (elt code 0) 'quil:measure))
    (is (typep (elt code 1) 'quil:measure))
    (is (= 0 (quil:qubit-index (quil:measurement-qubit (elt code 0)))))
    (is (= 1 (quil:qubit-index (quil:measurement-qubit (elt code 1)))))
    ;; Should expand into distinct memory destinations
    (is (= 0 (quil:memory-ref-position (quil:measure-address (elt code 0)))))
    (is (= 1 (quil:memory-ref-position (quil:measure-address (elt code 1)))))
    (is (= 2 (quil:memory-descriptor-length (first cmem))))))

(deftest test-qasm-reset ()
  (let* ((qasm "qreg q[1]; reset q[0];")
         (quil (quil.qasm::parse-qasm qasm))
         (code (quil:parsed-program-executable-code quil)))
    (is (= 1 (length code)))
    (is (typep (elt code 0) 'quil:reset-qubit))
    (is (= 0 (quil:qubit-index (quil:reset-qubit-target (elt code 0)))))))

(deftest test-qasm-reset-on-qreg ()
  (let* ((qasm "qreg q[2]; reset q;")
         (quil (quil.qasm::parse-qasm qasm))
         (code (quil:parsed-program-executable-code quil)))
    (is (= 2 (length code)))
    (is (typep (elt code 0) 'quil:reset-qubit))
    (is (typep (elt code 1) 'quil:reset-qubit))
    (is (= 0 (quil:qubit-index (quil:reset-qubit-target (elt code 0)))))
    (is (= 1 (quil:qubit-index (quil:reset-qubit-target (elt code 1)))))))

(defun %probs (qvm)
  (map 'vector 'qvm:probability (qvm::amplitudes qvm)))

(defun %probs= (probs-a probs-b)
  (every #'quil::double= probs-a probs-b))

(deftest test-qasm-if ()
  (let* ((qasm (format nil "include ~S; qreg q[1]; creg c[1]; x q[0]; measure q[0] -> c[0]; if(c==1) x q[0];"
                       (namestring *qasm-qelib.inc-path*)))
         (quil (quil.qasm::parse-qasm qasm))
         (qvm (qvm:run-program 1 quil))
         (mem (qvm::classical-memories (qvm:classical-memory-subsystem qvm))))
    (is (%probs= '(1 0) (%probs qvm)))
    (is (gethash "c" mem))
    (is (= 1 (qvm::memory-view-ref (gethash "c" mem) 0))))
  
  (let* ((qasm (format nil "include ~S; qreg q[2]; creg c[3]; x q; measure q -> c; if(c==3) x q;"
                       (namestring *qasm-qelib.inc-path*)))
         (quil (quil.qasm::parse-qasm qasm))
         (qvm (qvm:run-program 2 quil))
         (mem (qvm::classical-memories (qvm:classical-memory-subsystem qvm))))
    (is (%probs= '(1 0 0 0) (%probs qvm)))
    (is (gethash "c" mem))
    (is (= 1 (qvm::memory-view-ref (gethash "c" mem) 0))))

  ;; Test that the if branch is not executed (and we're left in |11>).
  (let* ((qasm (format nil "include ~S; qreg q[2]; creg c[3]; x q; measure q -> c; if(c==4) x q;"
                       (namestring *qasm-qelib.inc-path*)))
         (quil (quil.qasm::parse-qasm qasm))
         (qvm (qvm:run-program 2 quil))
         (mem (qvm::classical-memories (qvm:classical-memory-subsystem qvm))))
    (is (%probs= '(0 0 0 1) (%probs qvm)))
    (is (gethash "c" mem))
    (is (= 1 (qvm::memory-view-ref (gethash "c" mem) 0)))))

;; Barrier is currently uninteresting as Quil has no equivalent.
