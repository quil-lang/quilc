;;;; compilation-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-tests)


(deftest test-su4-to-su2x2 ()
  "Ensures that Optimal 2Q Compilation decomposes SU(2)xSU(2) matrices correctly."
  (let* ((a1 (cl-quil::random-special-unitary 2))
         (a0 (cl-quil::random-special-unitary 2))
         (a (magicl:@ (cl-quil::su2-on-line 1 a1)
                      (cl-quil::su2-on-line 0 a0))))
    (multiple-value-bind (b1 b0) (cl-quil::convert-su4-to-su2x2 a)
      (fiasco-assert-matrices-are-equal a1 b1)
      (fiasco-assert-matrices-are-equal a0 b0))))

(deftest test-optimal-2q-on-su2x2 ()
  "Tests that optimal 2Q compilation can handle a gate of the form SU(2) x SU(2)."
  (let* ((m (cl-quil::make-matrix-from-quil
             (list (cl-quil::anon-gate "U0" (cl-quil::random-special-unitary 2) 0)
                   (cl-quil::anon-gate "U1" (cl-quil::random-special-unitary 2) 1))))
         (compiled-list (cl-quil::approximate-2q-compiler
                         (list #'cl-quil::nearest-circuit-of-depth-0)
                         (build-anonymous-gate m 1 0)
                         :context (cl-quil::make-compilation-context :chip-specification (build-8Q-chip))))
         (u (cl-quil::make-matrix-from-quil compiled-list)))
    (fiasco-assert-matrices-are-equal m u)))

(deftest test-QSD-on-4Q ()
  "Tests Quantum Shannon Compilation on a random 4Q gate."
  (let* ((m (cl-quil::random-special-unitary 16))
         (compiled-list (cl-quil::qs-compiler (build-anonymous-gate m 3 2 1 0)))
         (u (cl-quil::make-matrix-from-quil compiled-list)))
    (check-type u magicl:matrix)
    (cl-quil::scale-out-matrix-phases u m)
    (fiasco-assert-matrices-are-equal m u)))

(deftest test-cnot->cnot ()
  (let ((progm (parse-quil "CNOT 1 0"))
        (chip (cl-quil::build-ibm-qx5)))
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
        (chip (cl-quil::build-ibm-qx5)))
    (let* ((comp (compiler-hook progm chip))
           (code (program-applications comp))
           (2q-code (program-2q-instructions comp)))
      (is (plusp (length code)))
      (is (= 1 (length 2q-code)))
      (is (string= "CNOT 1 0" (print-instruction (aref 2q-code 0) nil))))))

(deftest test-cnot-to-native-cnots ()
  (let* ((cnot-gate (cl-quil::build-gate "CNOT" () 1 15))
         (chip (cl-quil::build-ibm-qx5))
         (possible-paths '(((1 2) (2 15) (1 2) (2 15))
                           ((1 0) (0 15) (1 0) (0 15))))
         (path (cl-quil::cnot-to-native-cnots cnot-gate
                                           :context (cl-quil::make-compilation-context
                                                     :chip-specification chip))))
    (is (find (mapcar #'application-argument-indicies path) possible-paths
              :test #'equalp))))

(defun link-nativep (chip-spec)
  (reduce #'a:disjoin
          (cl-quil::chip-spec-links chip-spec)
          :initial-value (constantly t)
          :key (lambda (x) (lambda (y) (cl-quil::hardware-object-native-instruction-p x y)))))

(defun test-rewiring-in-cnot-for (chip gate-name i j)
  (let* ((sssppp (cl-quil::parse-quil (format nil "~A ~D ~D" gate-name i j)))
         (code (program-2q-instructions (cl-quil::compiler-hook sssppp chip))))
    (is (= 1 (length code)))
    (is (funcall (link-nativep chip) (aref code 0)))))

(deftest test-cnot-rewiring-in-cnot-architecture ()
  "Test that all CNOTs on each pair of qubits compile into a single CNOT rewired appropriately."
  (format t "~&    [Test output: ~%")
  (finish-output)
  ;; don't let nobody bully you into allocating (2^16)^2 elements
  (let ((fiasco:*print-test-run-progress* nil)
        (cl-quil::*compress-carefully* nil)
        (chip (cl-quil::build-ibm-qx5)))
    (dotimes (i 16)
      (format t "              ")
      (dotimes (j 16)
        (if (/= i j)
            (progn (format t " ~X~X" i j)
                   (test-rewiring-in-cnot-for chip "CNOT" i j))
            (format t " --"))
        (finish-output))
      (format t "~%")))
  (format t "]"))

(deftest test-cz-compilation-and-rewiring-in-cnot-architecture ()
  "Test that all CZs on all qubit combinations compile to a single CNOT that's native."
  (format t "~&    [Test output: ~%")
  (finish-output)
  (let ((fiasco:*print-test-run-progress* nil)
        (cl-quil::*compress-carefully* nil)
        (chip (cl-quil::build-ibm-qx5)))
    (dotimes (i 16)
      (format t "              ")
      (dotimes (j 16)
        (if (/= i j)
            (progn (format t " ~X~X" i j)
                   (test-rewiring-in-cnot-for chip "CZ" i j))
            (format t " --"))
        (finish-output))
      (format t "~%")))
  (format t "]"))

(deftest test-absolute-unit-cnot-compilation ()
  (let* ((chip (cl-quil::build-ibm-qx5))
         (pp (parse-quil "
# in awe at the size of this lad
CCNOT 8 9 2
CNOT 15 4
CZ 5 7
CPHASE(pi/8) 1 3
ISWAP 5 2"))
         (cp (let ((cl-quil::*compress-carefully* nil))
               (compiler-hook pp chip)))
         (2q-code (program-2q-instructions cp)))
    (is (every (link-nativep chip) 2q-code))))

(deftest test-bloch-gate-compilation ()
  (let* ((chip (cl-quil::build-ibm-qx5))
         (pp (parse-quil "
BLOCH(0,0,0) 0
BLOCH(0.1, 0.2, 0.3) 1
CONTROLLED BLOCH(-pi, pi/2, 0.1) 2 3
DAGGER BLOCH(-0.1, -0.2, -0.3) 4
"))
         (cp (let ((cl-quil::*compress-carefully* nil))
               (compiler-hook pp chip)))
         (2q-code (program-2q-instructions cp)))
    (is (every (link-nativep chip) 2q-code))))

(deftest test-cnot-triangle ()
  (let* ((chip (cl-quil::build-nq-linear-chip 3 :architecture ':cnot))
         (orig-prog (cl-quil::parse-quil "
CNOT 1 0
CNOT 2 1
CNOT 0 2"))
         (orig-matrix (cl-quil:parsed-program-to-logical-matrix orig-prog))
         (proc-prog (cl-quil::compiler-hook orig-prog chip))
         (proc-matrix (cl-quil:parsed-program-to-logical-matrix proc-prog))
         (2q-code (program-2q-instructions proc-prog)))
    (is (cl-quil::matrix-equals-dwim orig-matrix proc-matrix))
    (is (every (link-nativep chip) 2q-code))))

(deftest test-ccnot-compilation-well-behaved ()
  "Test that CCNOT on a line compiles into the best possible outcome of 7 CZ's."
  (let* ((chip (cl-quil::build-nq-linear-chip 3))
         (ccnot-prog (parse "CCNOT 0 1 2"))
         (ccnot-comp (compiler-hook ccnot-prog chip))
         (ccnot-code (program-2q-instructions ccnot-comp))
         (2q-code (program-2q-instructions ccnot-comp)))
    (is (cl-quil::matrix-equals-dwim
         (cl-quil:parsed-program-to-logical-matrix ccnot-prog)
         (cl-quil:parsed-program-to-logical-matrix ccnot-comp)))
    (is (every (link-nativep chip) 2q-code))
    (is (= 7 (length ccnot-code)))))

(deftest test-ccnot-compilation-on-cphase-iswap ()
  "Test that CCNOT compiles nicely on a line having the (:CPHASE ISWAP) architecture."
  (let* ((cl-quil::*default-addresser-state-class* 'cl-quil::temporal-addresser-state)
         (cl-quil::*addresser-use-1q-queues* t)
         (chip (cl-quil::build-nq-linear-chip 3 :architecture '(:cphase :cz :iswap)))
         (orig-prog (cl-quil::parse-quil "CCNOT 0 1 2"))
         (orig-matrix (cl-quil:parsed-program-to-logical-matrix orig-prog))
         (proc-prog (cl-quil::compiler-hook orig-prog chip))
         (proc-matrix (cl-quil:parsed-program-to-logical-matrix proc-prog))
         (2q-code (program-2q-instructions proc-prog)))
    (is (cl-quil::matrix-equals-dwim orig-matrix proc-matrix))
    (is (every (link-nativep chip) 2q-code))
    ;; NOTE: Decomposing into fewer 2q gates is more of a regression
    ;; test on quality of compilation, and not on correctness.
    (is (>= 6 (length 2q-code)))))

(deftest test-cswap-compiles-with-qs ()
  "Test that CSWAP compiles with QS-COMPILER. (Don't test the output's validity.)"
  (let ((result (cl-quil::qs-compiler (cl-quil::build-gate "CSWAP" () 0 1 2))))
    ;; Just check we get compilation output.
    (is (plusp (length result)))))

(deftest test-anons-compile-with-qs ()
  "Test that a few anonymous gates compile with QS-COMPILER. (Don't test the output's validity.)"
  (let ((result3 (cl-quil::qs-compiler (cl-quil::anon-gate "ANON" (cl-quil::random-special-unitary (expt 2 3)) 0 1 2)))
        (result4 (cl-quil::qs-compiler (cl-quil::anon-gate "ANON" (cl-quil::random-special-unitary (expt 2 4)) 0 1 2 3)))
        (result5 (cl-quil::qs-compiler (cl-quil::anon-gate "ANON" (cl-quil::random-special-unitary (expt 2 5)) 0 1 2 3 4))))
    ;; Just check we get compilation output.
    (is (plusp (length result3)))
    (is (plusp (length result4)))
    (is (plusp (length result5)))))

(deftest test-qs-dont-compile-nonsense ()
  "Test that QS-COMPILER doesn't compile an anonymous 1Q or 2Q gate."
  (signals CL-QUIL::COMPILER-DOES-NOT-APPLY
    (cl-quil::qs-compiler (cl-quil::anon-gate "ANON" (cl-quil::random-special-unitary 2) 0)))
  (signals CL-QUIL::COMPILER-DOES-NOT-APPLY
    (cl-quil::qs-compiler (cl-quil::anon-gate "ANON" (cl-quil::random-special-unitary 4) 0 1))))

(deftest test-sohaib-fidelity-rewiring-regression ()
  (not-signals bt:timeout
    (bt:with-timeout (1)
      (compiler-hook (parse "CZ 0 1")
                     (cl-quil::build-chip-from-digraph '((2 3) (3 2)))))))

(deftest test-aspen-28q-no-swap-bug ()
  (let* ((pp (parse "H 5
H 7
H 8

CNOT 4 5
CNOT 4 6
CNOT 4 6
CNOT 4 7
CNOT 4 7
CNOT 4 8
"))
         (chip (cl-quil::read-chip-spec-file
                (merge-pathnames *qpu-test-file-directory*
                                 "Aspen-7-28Q-A.qpu"))))
    ;; An absolute unit of a matrix. Better compress those qubits.
    (is (cl-quil::matrix-equals-dwim
         (cl-quil:parsed-program-to-logical-matrix pp :compress-qubits t)
         (cl-quil:parsed-program-to-logical-matrix (compiler-hook pp chip)
                                                :compress-qubits t)))))

(deftest test-rx-agglutination-with-wildcard-chip ()
  ;; See #567
  (let* ((chip (cl-quil::read-chip-spec-file
                (merge-pathnames *qpu-test-file-directory*
                                 "1q-wildcard-arguments.qpu")))
         (progm (parse "RX(pi/2) 0; RX(pi/2) 0"))
         (comp (compiler-hook progm chip))
         (code (%filter-halt comp)))
    (is (cl-quil::matrix-equals-dwim
         (cl-quil:parsed-program-to-logical-matrix
          (parse "RX(pi) 0"))
         (cl-quil:parsed-program-to-logical-matrix comp)))
    (is (= 1 (length code)))
    (is (string= "RX" (cl-quil::application-operator-root-name (elt code 0))))
    (is (cl-quil::double= pi
                       (abs (constant-value (first (application-parameters (elt code 0)))))))))

(defun %random-rz-rx-program (length)
  (make-instance 'parsed-program
    :executable-code
    (coerce
     (loop :for i :from 0 :below length
           :collect (if (evenp i)
                        (build-gate "RZ" (list (* (random 1.0) pi)) 0)
                        (build-gate "RX" (list (* (random 1.0) pi)) 0)))
     'vector)))

(defun %test-reduction-with-chip (reduced-bound chip)
  (loop :for length :from 1 :to 100
        :for progm := (%random-rz-rx-program length)
        :for comp := (%filter-halt (compiler-hook progm chip))
        :always (is (<= (length comp) reduced-bound))))

(deftest test-rx-rz-strings-reduce ()
  ;; Any string of RXs and RYs should reduce to a string of at most 3
  ;; RZs and 2 RXs.
  (let ((chips (list (cl-quil::build-nq-linear-chip 1)
                     (%read-test-chipspec "1q-wildcard-arguments.qpu")
                     (%read-test-chipspec "Aspen-4-2Q-A.qpu")
                     (%read-test-chipspec "Aspen-6-2Q-A.qpu")
                     (%read-test-chipspec "Aspen-7-28Q-A.qpu"))))
    (dolist (chip chips)
      (%test-reduction-with-chip 5 chip))))

(deftest test-free-rx-rz-strings-reduce ()
  (%test-reduction-with-chip 3 (%read-test-chipspec "1q-free-rx.qpu")))

(deftest test-symbolic-parameter-compiles-in-examples ()
  "There have been bugs where symbolic compilers aren't found because non-symbolic ones are lower cost. (Bug #667)"
  ;; Check we can compute this fact right.
  (is (cl-quil::compiler-allows-symbolic-parameters-p #'cl-quil::RX-to-ZXZXZ))
  ;; If this happens to change in the future... let me know!
  (is (not (cl-quil::compiler-allows-symbolic-parameters-p #'cl-quil::EULER-ZYZ-COMPILER)))
  ;; Some full stack test cases that arose out of bug reports.
  (not-signals error
    (cl-quil:compiler-hook (cl-quil:parse-quil "
DECLARE theta REAL
RX(theta) 0")
                        (cl-quil::build-nq-linear-chip 2)))
  (not-signals error
    (cl-quil:compiler-hook (cl-quil:parse-quil "
PRAGMA INITIAL_REWIRING \"NAIVE\"
DECLARE ro BIT[5]
DECLARE theta REAL[2]
H 1
H 2
H 3
H 4
H 5
CPHASE(theta[0]) 1 2
CPHASE(theta[0]) 2 3
CPHASE(theta[0]) 3 4
CPHASE(theta[0]) 4 5
RX(theta[1]) 1
RX(theta[1]) 2
RX(theta[1]) 3
RX(theta[1]) 4
RX(theta[1]) 5
MEASURE 1 ro[0]
MEASURE 2 ro[1]
MEASURE 3 ro[2]
MEASURE 4 ro[3]
MEASURE 5 ro[4]
")
                        (cl-quil::build-nq-linear-chip 6))))

(deftest test-swap-native-compile ()
  "Test that SWAP can be compiled natively."
  (let* ((chip (cl-quil::build-8q-chip :architecture '(:cz :swap)))
         (compiled-prog-code
           (cl-quil::parsed-program-executable-code
            (cl-quil::compiler-hook (cl-quil::parse-quil "SWAP 0 1")
                                 chip))))
    (is (cl-quil::swap-application-p (aref compiled-prog-code 0)))
    (is (cl-quil::haltp (aref compiled-prog-code 1)))))

(deftest test-swap-native-compile-chip-reader ()
  "Test that SWAP is recognized as a type in a chip file."
  (let* ((chip (cl-quil::read-chip-spec-file
                (merge-pathnames *qpu-test-file-directory*
                                 "swap.qpu")))
         (compiled-prog-code
           (cl-quil::parsed-program-executable-code
            (cl-quil::compiler-hook (cl-quil::parse-quil "SWAP 0 1")
                                 chip))))
    (is (cl-quil::swap-application-p (aref compiled-prog-code 0)))
    (is (cl-quil::haltp (aref compiled-prog-code 1)))))
