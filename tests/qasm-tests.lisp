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
  (is (cl-quil/qasm::parse-qasm "OPENQASM 2.0")))

(deftest test-qasm-creg/qreg-declaration ()
  (let* ((qasm "creg c1[1]; creg c2[1]; qreg q[1]; measure q[0] -> c1[0]; measure q[0] -> c2[0];")
         (quil (cl-quil/qasm::parse-qasm qasm))
         (mems (cl-quil:parsed-program-memory-definitions quil))
         (code (cl-quil:parsed-program-executable-code quil)))
    (is (= 2 (length mems)))
    (is (= 2 (length code)))

    ;; Can't guarantee the ordering of the memory regions, so gotta
    ;; look up the region by name.
    (let* ((mem (find "c1" mems :key #'cl-quil:memory-descriptor-name :test #'equalp))
           (cod (elt code 0))
           (des (cl-quil::memory-ref-descriptor (cl-quil:measure-address cod))))
      (is (not (null mem)))
      (is (eql cl-quil::quil-bit (cl-quil:memory-descriptor-type mem)))
      (is (= 1 (cl-quil:memory-descriptor-length mem)))
      (is (equalp mem des)))
    ;; c2
    (let* ((mem (find "c2" mems :key #'cl-quil:memory-descriptor-name :test #'equalp))
           (cod (elt code 1))
           (des (cl-quil::memory-ref-descriptor (cl-quil:measure-address cod))))
      (is (not (null mem)))
      (is (eql cl-quil::quil-bit (cl-quil:memory-descriptor-type mem)))
      (is (= 1 (cl-quil:memory-descriptor-length mem)))
      (is (equalp mem des)))))

(deftest test-qasm-include ()
  (uiop:with-temporary-file (:stream stream :pathname path)
    (format stream "creg c[1]; qreg q[2]; measure q[1] -> c[0];")
    (force-output stream)
    (let* ((qasm (format nil "include \"~A\";" path))
           (quil (cl-quil/qasm::parse-qasm qasm))
           (mems (cl-quil:parsed-program-memory-definitions quil))
           (code (cl-quil:parsed-program-executable-code quil)))
      (is (= 1 (length mems)))
      (is (find "c" mems :key #'cl-quil:memory-descriptor-name :test #'equalp))
      (is (typep (elt code 0) 'cl-quil:measure)))))

(deftest test-qasm-gate-definition-and-application ()
  (let* ((qasm (format nil "include ~S; qreg a[2]; qreg b[2]; gate bell q, r { h q; cx q, r; }; bell a[0], b[1];"
                       (namestring *qasm-qelib.inc-path*)))
         (quil (cl-quil/qasm::parse-qasm qasm))
         (circ (cl-quil:parsed-program-circuit-definitions quil))
         (bell (find (cl-quil/qasm::%qasm-gate-name "bell")
                     circ
                     :key #'cl-quil:circuit-definition-name
                     :test #'string=)))
    (is bell)
    (is (= 2 (length (cl-quil:circuit-definition-body bell))))
    (is (cl-quil::matrix-equals-dwim
         (cl-quil:parsed-program-to-logical-matrix quil)
         (cl-quil:parsed-program-to-logical-matrix
          (cl-quil:parse-quil "H 0; CNOT 0 3;"))))))

(deftest test-qasm-gate-definition-and-application-on-qreg ()
  (let* ((qasm (format nil "include ~S; qreg a[2]; qreg b[2]; gate bell q, r { h q; cx q, r; }; bell a, b;"
                       (namestring *qasm-qelib.inc-path*)))
         (quil (cl-quil/qasm::parse-qasm qasm))
         (circ (cl-quil:parsed-program-circuit-definitions quil))
         (bell (find (cl-quil/qasm::%qasm-gate-name "bell")
                     circ
                     :key #'cl-quil:circuit-definition-name
                     :test #'string=)))
    (is bell)
    (is (= 2 (length (cl-quil:circuit-definition-body bell))))
    (is (cl-quil::matrix-equals-dwim
         (cl-quil:parsed-program-to-logical-matrix quil)
         (cl-quil:parsed-program-to-logical-matrix
          (cl-quil:parse-quil "H 0; CNOT 0 2; H 1; CNOT 1 3;"))))))

(deftest test-qasm-opaque-gate-definition-and-application ()
  ;; opaque definitions and applications produce pragmas, rather than
  ;; a circuit definition + application.
  (let* ((qasm "qreg q[2]; opaque gate bell q, r; bell q[0], q[1];")
         (quil (cl-quil/qasm::parse-qasm qasm))
         (circ (cl-quil:parsed-program-circuit-definitions quil))
         (code (cl-quil:parsed-program-executable-code quil)))
    (is (= 0 (length circ)))
    (is (= 2 (length code)))
    (map nil (lambda (a b)
               (is (typep a 'cl-quil:pragma))
               (is (equal (cl-quil:pragma-words a)
                          (cl-quil:pragma-words b)))
               (is (equal (cl-quil:pragma-freeform-string a)
                          (cl-quil:pragma-freeform-string a))))
         code
         (list (cl-quil:make-pragma '("QASM_OPAQUE_DEFINITION"  "bell") "() q, r")
               (cl-quil:make-pragma '("QASM_OPAQUE_APPLICATION" "bell") "() 0, 1")))))

(deftest test-qasm-comment ()
  ;; Comments are stripped.
  (let* ((qasm "// hi
qreg q[2]; CX q[0], q[1]; // bye")
         (quil (cl-quil/qasm::parse-qasm qasm))
         (code (cl-quil:parsed-program-executable-code quil)))
    (is (= 1 (length code)))))

(deftest test-qasm-single-qubit-u ()
  ;; u1(lambda) = U(0, 0, lambda)
  (dolist (qasm (list "qreg q[1]; U(0, 0, 1.0) q[0];"
                      (format nil "include ~S; qreg q[1]; u1(1.0) q[0];"
                              (namestring *qasm-qelib.inc-path*))))
    (let* ((quil (cl-quil/qasm::parse-qasm qasm))
           (code (cl-quil:parsed-program-executable-code quil)))
      (is (= 3 (length code)))

      (is (equal "RZ" (cl-quil::application-operator-root-name (elt code 0))))
      (is (= 1 (cl-quil:constant-value (first (cl-quil:application-parameters (elt code 0))))))
      (is (= 0 (cl-quil:qubit-index (first (cl-quil:application-arguments (elt code 0))))))

      (is (equal "RY" (cl-quil::application-operator-root-name (elt code 1))))
      (is (= 0 (cl-quil:constant-value (first (cl-quil:application-parameters (elt code 1))))))
      (is (= 0 (cl-quil:qubit-index (first (cl-quil:application-arguments (elt code 1))))))

      (is (equal "RZ" (cl-quil::application-operator-root-name (elt code 2))))
      (is (= 0 (cl-quil:constant-value (first (cl-quil:application-parameters (elt code 2))))))
      (is (= 0 (cl-quil:qubit-index (first (cl-quil:application-arguments (elt code 2))))))))

  ;; u2(phi, lambda) = U(pi/2, phi, lambda)
  (dolist (qasm (list "qreg q[1]; U(pi/2, 0.5, 1) q[0];"
                      (format nil "include ~S; qreg q[1]; u2(0.5, 1) q[0];"
                              (namestring *qasm-qelib.inc-path*))))
    (let* ((quil (cl-quil/qasm::parse-qasm qasm))
           (code (cl-quil:parsed-program-executable-code quil)))
      (is (= 3 (length code)))

      (is (equal "RZ" (cl-quil::application-operator-root-name (elt code 0))))
      (is (= 1 (cl-quil:constant-value (first (cl-quil:application-parameters (elt code 0))))))
      (is (= 0 (cl-quil:qubit-index (first (cl-quil:application-arguments (elt code 0))))))

      (is (equal "RY" (cl-quil::application-operator-root-name (elt code 1))))
      (is (= cl-quil:pi/2 (cl-quil:constant-value (first (cl-quil:application-parameters (elt code 1))))))
      (is (= 0 (cl-quil:qubit-index (first (cl-quil:application-arguments (elt code 1))))))

      (is (equal "RZ" (cl-quil::application-operator-root-name (elt code 2))))
      (is (= 0.5 (cl-quil:constant-value (first (cl-quil:application-parameters (elt code 2))))))
      (is (= 0 (cl-quil:qubit-index (first (cl-quil:application-arguments (elt code 2))))))))

  ;; u3(theta, phi, lambda) = U(theta, phi, lambda) = RZ(phi) RY(theta) RZ(lambda)
  (dolist (qasm (list "qreg q[1]; U(-0.5, 0.5, 1) q[0];"
                      (format nil "include ~S; qreg q[1]; u3(-0.5, 0.5, 1) q[0];"
                              (namestring *qasm-qelib.inc-path*))))
    (let* ((quil (cl-quil/qasm::parse-qasm qasm))
           (code (cl-quil:parsed-program-executable-code quil)))
      (is (= 3 (length code)))

      (is (equal "RZ" (cl-quil::application-operator-root-name (elt code 0))))
      (is (= 1 (cl-quil:constant-value (first (cl-quil:application-parameters (elt code 0))))))
      (is (= 0 (cl-quil:qubit-index (first (cl-quil:application-arguments (elt code 0))))))

      (is (equal "RY" (cl-quil::application-operator-root-name (elt code 1))))
      (is (= -0.5 (cl-quil:constant-value (first (cl-quil:application-parameters (elt code 1))))))
      (is (= 0 (cl-quil:qubit-index (first (cl-quil:application-arguments (elt code 1))))))

      (is (equal "RZ" (cl-quil::application-operator-root-name (elt code 2))))
      (is (= 0.5 (cl-quil:constant-value (first (cl-quil:application-parameters (elt code 2))))))
      (is (= 0 (cl-quil:qubit-index (first (cl-quil:application-arguments (elt code 2))))))))

  ;; U(0,0,0) is compiled into the appropriate sequence
  (let* ((qasm "qreg q[1]; U(0, 0, 0) q[0];")
         (quil (cl-quil/qasm::parse-qasm qasm))
         (code (cl-quil:parsed-program-executable-code quil)))
    (is (= 3 (length code)))

    (is (equal "RZ" (cl-quil::application-operator-root-name (elt code 0))))
    (is (= 0 (cl-quil:constant-value (first (cl-quil:application-parameters (elt code 0))))))
    (is (= 0 (cl-quil:qubit-index (first (cl-quil:application-arguments (elt code 0))))))

    (is (equal "RY" (cl-quil::application-operator-root-name (elt code 1))))
    (is (= 0 (cl-quil:constant-value (first (cl-quil:application-parameters (elt code 1))))))
    (is (= 0 (cl-quil:qubit-index (first (cl-quil:application-arguments (elt code 1))))))

    (is (equal "RZ" (cl-quil::application-operator-root-name (elt code 2))))
    (is (= 0 (cl-quil:constant-value (first (cl-quil:application-parameters (elt code 2))))))
    (is (= 0 (cl-quil:qubit-index (first (cl-quil:application-arguments (elt code 2))))))))

(deftest test-qasm-measure ()
  (let* ((qasm "qreg q[1]; creg c[1]; measure q[0] -> c[0];")
         (quil (cl-quil/qasm::parse-qasm qasm))
         (cmem (cl-quil:parsed-program-memory-definitions quil))
         (code (cl-quil:parsed-program-executable-code quil)))
    (is (= 1 (length cmem)))
    (is (= 1 (length code)))
    (is (typep (elt code 0) 'cl-quil:measure))
    (is (= 0 (cl-quil:qubit-index (cl-quil:measurement-qubit (elt code 0)))))))

(deftest test-qasm-measure-on-qreg ()
  (let* ((qasm "qreg q[2]; creg c[1]; measure q -> c[0];")
         (quil (cl-quil/qasm::parse-qasm qasm))
         (cmem (cl-quil:parsed-program-memory-definitions quil))
         (code (cl-quil:parsed-program-executable-code quil)))
    (is (= 1 (length cmem)))
    (is (= 2 (length code)))
    (is (typep (elt code 0) 'cl-quil:measure))
    (is (typep (elt code 1) 'cl-quil:measure))
    (is (= 0 (cl-quil:qubit-index (cl-quil:measurement-qubit (elt code 0)))))
    (is (= 1 (cl-quil:qubit-index (cl-quil:measurement-qubit (elt code 1)))))
    ;; Should expand into same memory destination
    (is (= 0 (cl-quil:memory-ref-position (cl-quil:measure-address (elt code 0)))))
    (is (= 0 (cl-quil:memory-ref-position (cl-quil:measure-address (elt code 1)))))
    (is (= 1 (cl-quil:memory-descriptor-length (first cmem)))))
  (let* ((qasm "qreg q[2]; creg c[2]; measure q -> c;")
         (quil (cl-quil/qasm::parse-qasm qasm))
         (cmem (cl-quil:parsed-program-memory-definitions quil))
         (code (cl-quil:parsed-program-executable-code quil)))
    (is (= 1 (length cmem)))
    (is (= 2 (length code)))
    (is (typep (elt code 0) 'cl-quil:measure))
    (is (typep (elt code 1) 'cl-quil:measure))
    (is (= 0 (cl-quil:qubit-index (cl-quil:measurement-qubit (elt code 0)))))
    (is (= 1 (cl-quil:qubit-index (cl-quil:measurement-qubit (elt code 1)))))
    ;; Should expand into distinct memory destinations
    (is (= 0 (cl-quil:memory-ref-position (cl-quil:measure-address (elt code 0)))))
    (is (= 1 (cl-quil:memory-ref-position (cl-quil:measure-address (elt code 1)))))
    (is (= 2 (cl-quil:memory-descriptor-length (first cmem))))))

(deftest test-qasm-reset ()
  (let* ((qasm "qreg q[1]; reset q[0];")
         (quil (cl-quil/qasm::parse-qasm qasm))
         (code (cl-quil:parsed-program-executable-code quil)))
    (is (= 1 (length code)))
    (is (typep (elt code 0) 'cl-quil:reset-qubit))
    (is (= 0 (cl-quil:qubit-index (cl-quil:reset-qubit-target (elt code 0)))))))

(deftest test-qasm-reset-on-qreg ()
  (let* ((qasm "qreg q[2]; reset q;")
         (quil (cl-quil/qasm::parse-qasm qasm))
         (code (cl-quil:parsed-program-executable-code quil)))
    (is (= 2 (length code)))
    (is (typep (elt code 0) 'cl-quil:reset-qubit))
    (is (typep (elt code 1) 'cl-quil:reset-qubit))
    (is (= 0 (cl-quil:qubit-index (cl-quil:reset-qubit-target (elt code 0)))))
    (is (= 1 (cl-quil:qubit-index (cl-quil:reset-qubit-target (elt code 1)))))))

(defun %probs (qvm)
  (map 'vector 'qvm:probability (qvm::amplitudes qvm)))

(defun %probs= (probs-a probs-b)
  (every #'cl-quil::double= probs-a probs-b))

(deftest test-qasm-if ()
  (let* ((qasm (format nil "include ~S; qreg q[1]; creg c[1]; x q[0]; measure q[0] -> c[0]; if(c==1) x q[0];"
                       (namestring *qasm-qelib.inc-path*)))
         (quil (cl-quil/qasm::parse-qasm qasm))
         (qvm (qvm:run-program 1 quil))
         (mem (qvm::classical-memories (qvm:classical-memory-subsystem qvm))))
    (is (%probs= '(1 0) (%probs qvm)))
    (is (gethash "c" mem))
    (is (= 1 (qvm::memory-view-ref (gethash "c" mem) 0))))

  (let* ((qasm (format nil "include ~S; qreg q[2]; creg c[3]; x q; measure q -> c; if(c==3) x q;"
                       (namestring *qasm-qelib.inc-path*)))
         (quil (cl-quil/qasm::parse-qasm qasm))
         (qvm (qvm:run-program 2 quil))
         (mem (qvm::classical-memories (qvm:classical-memory-subsystem qvm))))
    (is (%probs= '(1 0 0 0) (%probs qvm)))
    (is (gethash "c" mem))
    (is (= 1 (qvm::memory-view-ref (gethash "c" mem) 0))))

  ;; Test that the if branch is not executed (and we're left in |11>).
  (let* ((qasm (format nil "include ~S; qreg q[2]; creg c[3]; x q; measure q -> c; if(c==4) x q;"
                       (namestring *qasm-qelib.inc-path*)))
         (quil (cl-quil/qasm::parse-qasm qasm))
         (qvm (qvm:run-program 2 quil))
         (mem (qvm::classical-memories (qvm:classical-memory-subsystem qvm))))
    (is (%probs= '(0 0 0 1) (%probs qvm)))
    (is (gethash "c" mem))
    (is (= 1 (qvm::memory-view-ref (gethash "c" mem) 0)))))

(deftest test-qasm-header ()
  (is (cl-quil/frontend::%check-for-qasm-header "OPENQASM 2.0"))
  (is (cl-quil/frontend::%check-for-qasm-header "OPENQASM 2.0; creg c[0];"))
  (is (cl-quil/frontend::%check-for-qasm-header "// this is a comment
OPENQASM 2.0;"))
  (is (cl-quil/frontend::%check-for-qasm-header "// this is a comment
// this is also a comment, and the next line is indented lightly
// but that should be ok.
  OPENQASM 2.0;"))
  (is (cl-quil/frontend::%check-for-qasm-header "// this is a comment
// this is also a comment, and the next line is empty

// and this a comment, and that should be ok
// and the next line is only whitespace


  OPENQASM 2.0;"))
  (is (not (cl-quil/frontend::%check-for-qasm-header "")))
  (is (not (cl-quil/frontend::%check-for-qasm-header " ")))
  (is (not (cl-quil/frontend::%check-for-qasm-header "
")))
  (is (not (cl-quil/frontend::%check-for-qasm-header "	"))) ; tab char
  (is (not (cl-quil/frontend::%check-for-qasm-header "X 0; OPENQASM 2.0")))
  (is (not (cl-quil/frontend::%check-for-qasm-header "# This is a Quil comment,
# and thus cannot be a qasm program

OPENQASM 2.0"))))


(deftest test-qasm-pragmas ()
  "Require that OpenQASM #pragma directives are respected

In particular, this test requires that a CZ 0 1 be compiled to CZ 0 1 when a naive strategy is used, and CZ 1 2 when a partial strategy is used."
  (let ((program-naive (format nil "
OPENQASM 2.0;
include ~S;
#pragma INITIAL_REWIRING \"NAIVE\"
qreg q[3];
cz q[0], q[1];
" (namestring *qasm-qelib.inc-path*)))
        (program-partial (format nil "
OPENQASM 2.0;
include ~S;
#pragma INITIAL_REWIRING \"PARTIAL\"
qreg q[3];
cz q[0], q[1];
" (namestring *qasm-qelib.inc-path*)))
        (chip (%read-test-chipspec "3q.qpu")))
    (let* ((compiled-program-naive (compiler-hook (parse program-naive) chip))
           (naive-2q (program-2q-instructions compiled-program-naive))
           (compiled-program-partial (compiler-hook (parse program-partial) chip))
           (partial-2q (program-2q-instructions compiled-program-partial)))
      (is (= 1 (length naive-2q)))
      (let ((2q (elt naive-2q 0)))
        (is (string= "CZ" (cl-quil::application-operator-root-name 2q)))
        (is (subsetp (mapcar #'qubit-index (application-arguments 2q))
                     '(0 1))))

      (is (= 1 (length partial-2q)))
      (let ((2q (elt partial-2q 0)))
        (is (string= "CZ" (cl-quil::application-operator-root-name 2q)))
        (is (subsetp (mapcar #'qubit-index (application-arguments 2q))
                     '(1 2)))))))
