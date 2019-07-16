;;;; compilation-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-tests)


(deftest test-su4-to-su2x2 ()
  "Ensures that Optimal 2Q Compilation decomposes SU(2)xSU(2) matrices correctly."
  (let* ((a1 (quil::random-special-unitary 2))
         (a0 (quil::random-special-unitary 2))
         (a (magicl:multiply-complex-matrices (cl-quil::su2-on-line 1 a1)
                                              (cl-quil::su2-on-line 0 a0))))
        (multiple-value-bind (b1 b0) (cl-quil::convert-su4-to-su2x2 a)
          (fiasco-assert-matrices-are-equal a1 b1)
          (fiasco-assert-matrices-are-equal a0 b0))))

(deftest test-optimal-2q-on-su2x2 ()
  "Tests that optimal 2Q compilation can handle a gate of the form SU(2) x SU(2)."
  (let* ((m (quil::make-matrix-from-quil
             (list (quil::anon-gate "U0" (quil::random-special-unitary 2) 0)
                   (quil::anon-gate "U1" (quil::random-special-unitary 2) 1))))
         (compiled-list (cl-quil::approximate-2q-compiler
                         (list #'quil::nearest-circuit-of-depth-0)
                         (build-anonymous-gate m 1 0)
                         :context (quil::make-compilation-context :chip-specification (build-8Q-chip))))
         (u (quil::make-matrix-from-quil compiled-list)))
    (fiasco-assert-matrices-are-equal m u)))

(deftest test-QSD-on-4Q ()
  "Tests Quantum Shannon Compilation on a random 4Q gate."
  (let* ((m (quil::random-special-unitary 16))
         (compiled-list (quil::qs-compiler (build-anonymous-gate m 3 2 1 0)))
         (u (quil::make-matrix-from-quil compiled-list)))
    (check-type u magicl:matrix)
    (quil::scale-out-matrix-phases u m)
    (fiasco-assert-matrices-are-equal m u)))

(deftest test-cnot->cnot ()
  (let ((progm (parse-quil "CNOT 1 0"))
        (chip (quil::build-ibm-qx5)))
    (let* ((comp (compiler-hook progm chip))
           (code (remove-if-not (lambda (isn) (typep isn 'application))
                                (parsed-program-executable-code comp))))
      (is (= 1 (length code)))
      (is (string= "CNOT 1 0" (print-instruction (aref code 0) nil))))))

(defun application-argument-indicies (app)
  (mapcar #'qubit-index (application-arguments app)))

(defun program-applications (parsed-prog)
  (remove-if-not (lambda (isn) (typep isn 'application))
                 (parsed-program-executable-code parsed-prog)))

(defun program-2q-instructions (parsed-prog)
  (remove-if-not (lambda (isn) (= 2 (length (application-arguments isn))))
                 (program-applications parsed-prog)))

(deftest test-cnot-flipped-edge ()
  (let ((progm (parse-quil "CNOT 0 1"))
        (chip (quil::build-ibm-qx5)))
    (let* ((comp (compiler-hook progm chip))
           (code (program-applications comp))
           (2q-code (program-2q-instructions comp)))
      (is (plusp (length code)))
      (is (= 1 (length 2q-code)))
      (is (string= "CNOT 1 0" (print-instruction (aref 2q-code 0) nil))))))

(deftest test-cnot-to-native-cnots ()
  (let* ((cnot-gate (quil::build-gate "CNOT" () 1 15))
         (chip (quil::build-ibm-qx5))
         (possible-paths '(((1 2) (2 15) (1 2) (2 15))
                           ((1 0) (0 15) (1 0) (0 15))))
         (path (quil::cnot-to-native-cnots cnot-gate
                                           :context (quil::make-compilation-context
                                                     :chip-specification chip))))
    (is (find (mapcar #'application-argument-indicies path) possible-paths
              :test #'equalp))))

(defun link-nativep (chip-spec)
  (reduce #'a:disjoin
          (quil::chip-spec-links chip-spec)
          :initial-value (constantly t)
          :key (lambda (x) (lambda (y) (quil::hardware-object-native-instruction-p x y)))))

(defun test-rewiring-in-cnot-for (gate-name i j)
  (let* ((chip (quil::build-ibm-qx5))
         (sssppp (quil::parse-quil (format nil "~A ~D ~D" gate-name i j)))
         (code (program-2q-instructions (quil::compiler-hook sssppp chip))))
    (is (= 1 (length code)))
    (is (funcall (link-nativep chip) (aref code 0)))))

(deftest test-cnot-rewiring-in-cnot-architecture ()
  "Test that all CNOTs on each pair of qubits compile into a single CNOT rewired appropriately."
  (format t "~&    [Test output: ~%")
  (finish-output)
  ;; don't let nobody bully you into allocating (2^16)^2 elements
  (let ((fiasco:*print-test-run-progress* nil)
        (quil::*compress-carefully* nil))
    (dotimes (i 16)
      (format t "              ")
      (dotimes (j 16)
        (if (/= i j)
            (progn (format t " ~X~X" i j)
                   (test-rewiring-in-cnot-for "CNOT" i j))
            (format t " --"))
        (finish-output))
      (format t "~%")))
  (format t "]"))

(deftest test-cz-compilation-and-rewiring-in-cnot-architecture ()
  "Test that all CZs on all qubit combinations compile to a single CNOT that's native."
  (format t "~&    [Test output: ~%")
  (finish-output)
  (let ((fiasco:*print-test-run-progress* nil)
        (quil::*compress-carefully* nil))
    (dotimes (i 16)
      (format t "              ")
      (dotimes (j 16)
        (if (/= i j)
            (progn (format t " ~X~X" i j)
                   (test-rewiring-in-cnot-for "CZ" i j))
            (format t " --"))
        (finish-output))
      (format t "~%")))
  (format t "]"))

(deftest test-absolute-unit-cnot-compilation ()
  (let* ((chip (quil::build-ibm-qx5))
         (pp (parse-quil "
# in awe at the size of this lad
CCNOT 8 9 2
CNOT 15 4
CZ 5 7
CPHASE(pi/8) 1 3
ISWAP 5 2"))
         (cp (let ((quil::*compress-carefully* nil))
               (compiler-hook pp chip)))
         (2q-code (program-2q-instructions cp)))
    (is (every (link-nativep chip) 2q-code))))

(deftest test-cnot-triangle ()
  (let* ((chip (quil::build-nq-linear-chip 3 :architecture ':cnot))
         (orig-prog (quil::parse-quil "
CNOT 1 0
CNOT 2 1
CNOT 0 2"))
         (orig-matrix (quil::parsed-program-to-logical-matrix orig-prog))
         (proc-prog (quil::compiler-hook orig-prog chip))
         (proc-matrix (quil::parsed-program-to-logical-matrix proc-prog))
         (2q-code (program-2q-instructions proc-prog)))
    (is (quil::matrix-equals-dwim orig-matrix proc-matrix))
    (is (every (link-nativep chip) 2q-code))))

(deftest test-ccnot-compilation-on-cphase-iswap ()
  "Test that CCNOT compiles nicely on a line having the (:CPHASE ISWAP) architecture."
  (let* ((chip (quil::build-nq-linear-chip 3 :architecture '(:cphase :cz :iswap)))
         (orig-prog (quil::parse-quil "CCNOT 0 1 2"))
         (orig-matrix (quil::parsed-program-to-logical-matrix orig-prog))
         (proc-prog (quil::compiler-hook orig-prog chip))
         (proc-matrix (quil::parsed-program-to-logical-matrix proc-prog))
         (2q-code (program-2q-instructions proc-prog)))
    (is (quil::matrix-equals-dwim orig-matrix proc-matrix))
    (is (every (link-nativep chip) 2q-code))
    ;; NOTE: Decomposing into fewer 2q gates is more of a regression
    ;; test on quality of compilation, and not on correctness.
    (is (>= 6 (length 2q-code)))))
