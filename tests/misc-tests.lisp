;;;; misc-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil-tests)

(deftest test-partition-sequence-into-segments ()
  (flet ((test-it (expected-first expected-segments input-sequence)
           (multiple-value-bind (segments first?)
               (cl-quil::partition-sequence-into-segments #'evenp input-sequence)
             (is (eq first? expected-first))
             (is (equalp segments expected-segments)))))
    (test-it nil nil nil)
    (test-it nil nil #())
    (test-it nil nil "")
    (test-it nil '((1)) '(1))
    (test-it nil '((1) (2)) '(1 2))
    (test-it nil '((1 1)) '(1 1))
    (test-it t '((2)) '(2))
    (test-it t '((2) (1)) '(2 1))
    (test-it t '((2 2)) '(2 2))
    (test-it t '((2) (1 1) (2 2 2) (1 1 1 1)) '(2 1 1 2 2 2 1 1 1 1))))

(deftest test-code-splicing ()
  (is (equalp (cl-quil::splice-code-at #(1 x 2 3) 1 #(a b c))
              #(1 A B C 2 3)))
  (is (equalp (cl-quil::splice-code-at #(x 1 2 3) 0 #(a b c))
              #(A B C 1 2 3)))
  (is (equalp (cl-quil::splice-code-at #(0 1 2 x) 3 #(a b c))
              #(0 1 2 A B C))))

(deftest test-append-reduce ()
  (is (equal nil (quil::reduce-append nil)))
  (is (equal nil (quil::reduce-append '(nil))))
  (is (equal nil (quil::reduce-append '(nil nil))))
  (is (equal '(a) (quil::reduce-append '(nil (a)))))
  (is (equal '(a) (quil::reduce-append '((a) nil))))
  (is (equal '(a) (quil::reduce-append '((a) nil nil))))
  (is (equal '(a) (quil::reduce-append '(nil (a) nil))))
  (is (equal '(a) (quil::reduce-append '(nil nil (a)))))
  (is (equal '(a b c) (quil::reduce-append '((a) nil (b) nil (c)))))
  (is (equal '(a b c d) (quil::reduce-append '((a) (b c) nil (d)))))
  (is (equal '(a b c d e f) (quil::reduce-append '((a) (b c) nil (d) (e f))))))

(deftest test-big-defgate ()
  (let* ((qubit-count 8)
         (program-string
           (with-output-to-string (s)
             (format s "DEFGATE TEST:~%")
             (dotimes (i (expt 2 qubit-count))
               (format s "    ")
               (dotimes (j (expt 2 qubit-count))
                 (format s "~d" (if (= i j)
                                    1.0
                                    0.0))
                 (unless (= j (1- (expt 2 qubit-count)))
                   (format s ", ")))
               (format s "~%"))
             (format s "TEST ~{~d ~}" (a:iota qubit-count))))
         (parsed-prog (quil::parse-quil-into-raw-program program-string)))
    (setf parsed-prog (quil::transform 'quil::resolve-applications parsed-prog))
    (is (quil::matrix-equality (magicl:make-identity-matrix (expt 2 qubit-count))
                               (quil::make-matrix-from-quil (coerce (parsed-program-executable-code parsed-prog) 'list))))))

(defclass transformable-thing (quil::transformable)
  ((data
    :initarg :data
    :accessor data
    :initform (list 1 2 3))))

(defun double-data (thing)
  (setf (data thing) (mapcar (lambda (x) (* x x)) (data thing)))
  thing)

(quil::define-transform identity (identity))

(quil::define-transform double-data (double-data)
  "Double the data values in a thing."
  identity)

(deftest test-transform-predecessor-checking ()
  "Test that omitting a predecessor tranform signals an error."
  (let ((transform (quil::find-transform 'double-data)))
    (is (not (null transform)))
    (is (member 'identity (quil::transform-description-predecessors transform))))
  (signals unsatisfied-transform-dependency
    (let ((thing (make-instance 'transformable-thing)))
      ;; IDENTITY is a prerequisite transform but not
      ;; performed.
      (setf thing (quil::transform 'double-data thing))
      (values thing (quil::transforms-performed thing)))))

(deftest test-peephole-splicing ()
  (let ((code '(a b c d e))
        (new '(x y)))
    (is (equalp '(x y c d e)
                (quil::splice-instructions code '(a b) new 0)))
    ;; Position is relative to the original code, not the output
    (is (equalp '(x y c d e)
                (quil::splice-instructions code '(a b) new 1)))
    (is (equalp '(x y c d e)
                (quil::splice-instructions code '(a b) new 2)))
    (is (equalp '(c x y d e)
                (quil::splice-instructions code '(a b) new 3)))
    (is (equalp '(c d x y e)
                (quil::splice-instructions code '(a b) new 4)))
    (is (equalp '(c d e x y)
                (quil::splice-instructions code '(a b) new 5)))
    ;; Position 6 is off the end of the original
    (signals simple-error
      (quil::splice-instructions code '(a b) new 6))
    ;; (B A) is out of order
    (signals simple-error
      (quil::splice-instructions code '(b a) new 0))
    ;; (J K) aren't in the original
    (signals simple-error
      (quil::splice-instructions code '(j k) new 0))))

(deftest test-mref-equality ()
  "Test MEMORY-REF equality and hash table creation."
  (let ((a0  (mref "a" 0))
        (a0* (mref "a" 0))
        (a0! (mref "a" 0 (quil::make-memory-descriptor :name "hello" :type quil::quil-real)))
        (b0  (mref "b" 0))
        (a1  (mref "a" 1)))
    (is (quil::memory-ref= a0 a0))
    (is (quil::memory-ref= a0 a0*))
    (is (quil::memory-ref= a0 a0!))
    (is (not (quil::memory-ref= a0 a1)))
    (is (not (quil::memory-ref= a0 b0)))
    (let ((T-A-B-L-E (make-hash-table :test 'quil::memory-ref=
                                      :hash-function 'quil::memory-ref-hash)))
      (setf (gethash a0  T-A-B-L-E) t
            (gethash a0* T-A-B-L-E) t
            (gethash a0! T-A-B-L-E) t
            (gethash b0  T-A-B-L-E) t
            (gethash a1  T-A-B-L-E) t)
      (is (= 3 (hash-table-count T-A-B-L-E))))))

(defun gethash-chain (chain obj)
  (cond
    ((null chain) obj)
    ((hash-table-p obj) (gethash-chain (rest chain) (gethash (first chain) obj)))
    (t (error "Invalid call to GETHASH-CHAIN."))))

(deftest test-isa-1q-completion ()
  "Test that the 1Q layer of the chip specification is complete."
  (let* ((isa (yason:parse "{\"isa\":
{\"1Q\": {\"0\": {},
  \"1\": {},
  \"2\": {},
  \"3\": {},
  \"5\": {},
  \"6\": {},
  \"7\": {},
  \"13\": {},
  \"14\": {},
  \"15\": {},
  \"16\": {},
  \"17\": {}},
 \"2Q\": {\"0-1\": {},
  \"0-7\": {},
  \"1-2\": {},
  \"1-16\": {},
  \"2-3\": {},
  \"2-15\": {},
  \"5-6\": {},
  \"6-7\": {},
  \"13-14\": {},
  \"14-15\": {},
  \"15-16\": {},
  \"16-17\": {}}}}"))
         (chip-spec (quil::qpu-hash-table-to-chip-specification isa)))
    (is (quil::chip-specification-p chip-spec))
    (is (= 18 (quil::chip-spec-n-qubits chip-spec)))
    ;; check we got the goods
    (dolist (presumed-dead '(4 8 9 10 11 12))
      (is (quil::chip-spec-qubit-dead? chip-spec presumed-dead))))) ; RIP in piece

(deftest test-bristlecone-chip ()
  "Test construction of Google's Bristlecone 72-qubit chip"
  (let* ((chip (quil::build-bristlecone-chip))
         (prgm (parse-quil
                (with-output-to-string (s)
                  (loop :for i :below (quil::chip-spec-n-qubits chip)
                        :do (format s "H ~D~%" i)))))
         ;; Bit of a kludge here. Since this is a large number of
         ;; qubits, calculating its matrix representation will be a
         ;; terribly long-winded affair.
         (quil::*compress-carefully* nil))
    (is (= 72 (quil::chip-spec-n-qubits chip)))
    (is (= (* 11 11) (quil::chip-spec-n-links chip)))
    (is (plusp (length (parsed-program-executable-code prgm))))
    (is (plusp (length (parsed-program-executable-code (compiler-hook prgm chip)))))))

(deftest test-power-of-two-p ()
  "Test that POWER-OF-TWO-P and POSITIVE-POWER-OF-TWO-P do what they say on the tin."
  (is (not (quil::power-of-two-p -2)))
  (is (not (quil::power-of-two-p -1)))
  (is (not (quil::power-of-two-p 0)))
  (is (not (quil::positive-power-of-two-p -2)))
  (is (not (quil::positive-power-of-two-p -1)))
  (is (not (quil::positive-power-of-two-p 0)))

  (is (quil::power-of-two-p 1))
  (is (not (quil::positive-power-of-two-p 1)))

  (loop :for power-of-two = 2 :then (* 2 power-of-two)
        :while (<= power-of-two 1024)
        :do (progn
              (is (quil::power-of-two-p power-of-two))
              (is (not (quil::power-of-two-p (1+ power-of-two))))
              (is (quil::positive-power-of-two-p power-of-two))
              (is (not (quil::positive-power-of-two-p (1+ power-of-two)))))))

(deftest test-check-permutation ()
  "Test that CHECK-PERMUTATION signals error iff input is not valid."
  ;; Duplicates
  (signals simple-error (quil::check-permutation '(0 0)))
  (signals simple-error (quil::check-permutation '(0 0 1)))
  (signals simple-error (quil::check-permutation '(0 1 0)))
  (signals simple-error (quil::check-permutation '(1 0 0)))
  (signals simple-error (quil::check-permutation '(0 1 2 3 4 5 2)))
  ;; Out of range values
  (signals simple-error (quil::check-permutation '(1)))
  (signals simple-error (quil::check-permutation '(-1)))
  (signals simple-error (quil::check-permutation '(0 2)))
  (signals simple-error (quil::check-permutation '(2 0)))
  (signals simple-error (quil::check-permutation '(0 1 3)))
  (signals simple-error (quil::check-permutation '(0 3 1)))
  (signals simple-error (quil::check-permutation '(3 1 0)))
  (signals simple-error (quil::check-permutation '(0 1 2 5 3)))
  ;; Valid permutations. Grows as n!, so don't get too crazy here.
  (dotimes (n 6)
    (a:map-permutations
     (lambda (permutation)
       (not-signals simple-error (quil::check-permutation permutation)))
     (a:iota n))))

(deftest test-quil<->lisp-bridge ()
  "Test that the functions for mapping between quil<->lisp work."
  (loop :for (quil-string . lisp-symbol) :in quil::+quil<->lisp-prefix-arithmetic-operators+ :do
    (progn
      (is (quil::valid-quil-function-or-operator-p lisp-symbol))
      (is (eq lisp-symbol (quil::quil-prefix-operator->lisp-symbol quil-string)))
      (is (string= quil-string (quil::lisp-symbol->quil-prefix-operator lisp-symbol)))
      (is (string= quil-string (quil::lisp-symbol->quil-function-or-prefix-operator lisp-symbol)))))

  (loop :for (quil-string . lisp-symbol) :in quil::+quil<->lisp-infix-arithmetic-operators+ :do
    (progn
      (is (quil::valid-quil-function-or-operator-p lisp-symbol))
      (is (eq lisp-symbol (quil::quil-infix-operator->lisp-symbol quil-string)))
      (is (string= quil-string (quil::lisp-symbol->quil-infix-operator lisp-symbol)))))

  (loop :for (quil-string . lisp-symbol) :in quil::+quil<->lisp-functions+ :do
    (progn
      (is (quil::valid-quil-function-or-operator-p lisp-symbol))
      (is (eq lisp-symbol (quil::quil-function->lisp-symbol quil-string)))
      (is (string= quil-string (quil::lisp-symbol->quil-function lisp-symbol)))
      (is (string= quil-string (quil::lisp-symbol->quil-function-or-prefix-operator lisp-symbol))))))

(defun %extract-trivial-exit-rewiring (pp)
  "Extract the exit rewiring comment from parsed program PP. Trivial here means PP is expected to have a single exit rewiring. A more complicated CFG could produce multiple exit rewirings in a program, but that is outside our scope of interest."
  (declare (type parsed-program pp))
  (loop :with code := (parsed-program-executable-code pp)
        :for i :below (length code)
        :for comment := (quil::comment (elt code i))
        :when (and comment
                   (uiop:string-prefix-p "Exiting rewiring: " comment))
          :return (quil::rewiring-l2p
                   (quil::make-rewiring-from-string
                    (subseq comment (length "Exiting rewiring: "))))))

(defun %make-density-qvm-initialized-in-basis (num-qubits basis-index)
  "Make a DENSITY-QVM that is initialized in the basis state described by BASIS-INDEX.

To put the density matrix into the basis state, e.g., |01><11|, we would choose BASIS-INDEX = 7. In general, the basis state |a><b| is prepared by choosing BASIS-INDEX = (2^N * a + b)."
  (let ((amps (make-array (expt 2 (* 2 num-qubits))
                          :element-type 'qvm:cflonum
                          :initial-element (qvm:cflonum 0))))
    (setf (aref amps basis-index) (qvm:cflonum 1))
    (qvm:make-density-qvm num-qubits :amplitudes amps)))

;; Our test below is built on the following idea: rather than directly
;; calculating the entire superoperator encoding the pre- and post-compilation
;; programs and comparing the results, we instead merely calculate the behavior
;; of the superoperator (itself necessarily a linear operator) on a maximal
;; linearly independent set of inputs and check that they match individually.
;;
;; However, we cheat a bit while doing this: we select a maximal linearly
;; independent set drawn from __all matrices__, without regards to the three
;; defining characteristics of density operators (Hermitian, unit trace, and
;; positive semi-definite), and so we are actually feeding invalid inputs to the
;; QVM. Provided the QVM does not closely inspect its input, this is fine---but
;; it would matter when simulating, e.g., a measure-and-store.
;;
;; Should this test begin to misbehave, it might be resolved by enforcing these
;; constraints on our generated probe operators.  I believe the constraints to
;; be of descending order of severity: it is easy to imagine two distinct
;; extensions to the space of all matrices of the "same" superoperator, in the
;; sense that they give the same behavior on density operators but differ on the
;; complement of Hermitian matrices.  I don't believe that this is possible for
;; the subclasses of unit-trace or positive-definite operators, but it could
;; still be that (for something other than pure operations and MEASURE-DISCARDs)
;; the QVM makes a calculation based on these assumptions and gives garbage
;; output if they're violated.  Caveat programmer.
;;
;; - ecp
(defun %test-measure-semantics (p-str)
  (let* ((p (parse-quil p-str))
         (p-comp (quil:compiler-hook (parse-quil p-str) (quil::build-nq-linear-chip 3) :protoquil nil))
         (rewiring (quil::qubit-relabeler (%extract-trivial-exit-rewiring p-comp))))
    (loop :for i :below (expt 2 6) :do
      (let* ((qvm (%make-density-qvm-initialized-in-basis 3 i))
             (qvm-comp (%make-density-qvm-initialized-in-basis 3 i)))
        (qvm:load-program qvm p :supersede-memory-subsystem t)
        ;; relabeling is a side-effect
        (map nil (a:rcurry #'quil::%relabel-qubits rewiring)
             (parsed-program-executable-code p-comp))
        (qvm:load-program qvm-comp p-comp :supersede-memory-subsystem t)
        (qvm:run qvm)
        (qvm:run qvm-comp)
        (is (every #'quil::double=
                   (qvm::amplitudes qvm)
                   (qvm::amplitudes qvm-comp)))))))

(defun map-measure-combinations (f p n)
  "Apply function F to a permutation of program P with N measures inserted."
  (let* ((len (length (parsed-program-executable-code p)))
         (spec (perm:vector-to-word-spec
                (concatenate 'vector
                             (make-array len :initial-element 0)
                             (a:iota n :start 1)))))
    (labels
        ((word->program (word)
           (let ((code (make-array (+ len n) :fill-pointer 0))
                 (orig-code (coerce (parsed-program-executable-code p) 'list))
                 (new-prog (quil:copy-instance p)))
             (loop :for char :across word :do
               (vector-push (if (zerop char)
                                (pop orig-code)
                                (make-instance 'quil:measure-discard :qubit (quil:qubit (1- char))))
                            code))
             (setf (parsed-program-executable-code new-prog) code)
             new-prog)))
      (perm:map-spec (lambda (rank word)
                       (declare (ignore rank))
                       (funcall f (word->program word)))
                     spec))))

(deftest test-measure-semantics ()
  "Test that artifacts of compilation (namely moving and rewiring MEASUREs) does not change the semantics of the program."
  (map-measure-combinations (lambda (p)
                              (%test-measure-semantics
                               (with-output-to-string (s)
                                 (quil::print-parsed-program p s))))
                            (parse-quil "RX(pi/3) 0
CNOT 0 2
RX(7*pi/3) 2")
                            3))
