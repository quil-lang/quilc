;;;; misc-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil-tests)

(deftest test-partition-sequence-into-segments ()
  (flet ((test-it (expected-first expected-segments input-sequence)
           (multiple-value-bind (segments first?)
               (cl-quil/frontend::partition-sequence-into-segments #'evenp input-sequence)
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

(deftest test-append-reduce ()
  (is (equal nil (cl-quil::reduce-append nil)))
  (is (equal nil (cl-quil::reduce-append '(nil))))
  (is (equal nil (cl-quil::reduce-append '(nil nil))))
  (is (equal '(a) (cl-quil::reduce-append '(nil (a)))))
  (is (equal '(a) (cl-quil::reduce-append '((a) nil))))
  (is (equal '(a) (cl-quil::reduce-append '((a) nil nil))))
  (is (equal '(a) (cl-quil::reduce-append '(nil (a) nil))))
  (is (equal '(a) (cl-quil::reduce-append '(nil nil (a)))))
  (is (equal '(a b c) (cl-quil::reduce-append '((a) nil (b) nil (c)))))
  (is (equal '(a b c d) (cl-quil::reduce-append '((a) (b c) nil (d)))))
  (is (equal '(a b c d e f) (cl-quil::reduce-append '((a) (b c) nil (d) (e f))))))

(deftest test-big-defgate ()
  (let* ((qubit-count 8)
         (program-string
           (with-output-to-string (s)
             (format s "DEFGATE TEST:~%")
             (dotimes (i (expt 2 qubit-count))
               (format s "    ")
               (dotimes (j (expt 2 qubit-count))
                 (format s "~D" (if (= i j)
                                    1.0
                                    0.0))
                 (unless (= j (1- (expt 2 qubit-count)))
                   (format s ", ")))
               (format s "~%"))
             (format s "TEST ~{~D ~}" (a:iota qubit-count))))
         (parsed-prog (cl-quil::parse-quil program-string)))
    (is (cl-quil::matrix-equality (cl-quil::eye (expt 2 qubit-count))
                               (cl-quil::make-matrix-from-quil (coerce (parsed-program-executable-code parsed-prog) 'list))))))

(defclass transformable-thing (cl-quil::transformable)
  ((data
    :initarg :data
    :accessor data
    :initform (list 1 2 3))))

(defun double-data (thing)
  (setf (data thing) (mapcar (lambda (x) (* x x)) (data thing)))
  thing)

(cl-quil::define-transform identity (identity))

(cl-quil::define-transform double-data (double-data)
  "Double the data values in a thing."
  identity)

(deftest test-transform-predecessor-checking ()
  "Test that omitting a predecessor tranform signals an error."
  (let ((transform (cl-quil::find-transform 'double-data)))
    (is (not (null transform)))
    (is (member 'identity (cl-quil::transform-description-predecessors transform))))
  (signals unsatisfied-transform-dependency
    (let ((thing (make-instance 'transformable-thing)))
      ;; IDENTITY is a prerequisite transform but not
      ;; performed.
      (setf thing (cl-quil::transform 'double-data thing))
      (values thing (cl-quil::transforms-performed thing)))))

(deftest test-peephole-splicing ()
  (let ((code '(a b c d e))
        (new '(x y)))
    (is (equalp '(x y c d e)
                (cl-quil::splice-instructions code '(a b) new 0)))
    ;; Position is relative to the original code, not the output
    (is (equalp '(x y c d e)
                (cl-quil::splice-instructions code '(a b) new 1)))
    (is (equalp '(x y c d e)
                (cl-quil::splice-instructions code '(a b) new 2)))
    (is (equalp '(c x y d e)
                (cl-quil::splice-instructions code '(a b) new 3)))
    (is (equalp '(c d x y e)
                (cl-quil::splice-instructions code '(a b) new 4)))
    (is (equalp '(c d e x y)
                (cl-quil::splice-instructions code '(a b) new 5)))
    ;; Position 6 is off the end of the original
    (signals simple-error
      (cl-quil::splice-instructions code '(a b) new 6))
    ;; (B A) is out of order
    (signals simple-error
      (cl-quil::splice-instructions code '(b a) new 0))
    ;; (J K) aren't in the original
    (signals simple-error
      (cl-quil::splice-instructions code '(j k) new 0))))

(deftest test-mref-equality ()
  "Test MEMORY-REF equality and hash table creation."
  (let ((a0  (mref "a" 0))
        (a0* (mref "a" 0))
        (a0! (mref "a" 0 (cl-quil::make-memory-descriptor :name "hello" :type cl-quil::quil-real)))
        (b0  (mref "b" 0))
        (a1  (mref "a" 1)))
    (is (cl-quil::memory-ref= a0 a0))
    (is (cl-quil::memory-ref= a0 a0*))
    (is (cl-quil::memory-ref= a0 a0!))
    (is (not (cl-quil::memory-ref= a0 a1)))
    (is (not (cl-quil::memory-ref= a0 b0)))
    (let ((T-A-B-L-E (make-hash-table :test 'cl-quil::memory-ref=
                                      :hash-function 'cl-quil::memory-ref-hash)))
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

(deftest test-power-of-two-p ()
  "Test that POWER-OF-TWO-P and POSITIVE-POWER-OF-TWO-P do what they say on the tin."
  (is (not (cl-quil::power-of-two-p -2)))
  (is (not (cl-quil::power-of-two-p -1)))
  (is (not (cl-quil::power-of-two-p 0)))
  (is (not (cl-quil::positive-power-of-two-p -2)))
  (is (not (cl-quil::positive-power-of-two-p -1)))
  (is (not (cl-quil::positive-power-of-two-p 0)))

  (is (cl-quil::power-of-two-p 1))
  (is (not (cl-quil::positive-power-of-two-p 1)))

  (loop :for power-of-two = 2 :then (* 2 power-of-two)
        :while (<= power-of-two 1024)
        :do (progn
              (is (cl-quil::power-of-two-p power-of-two))
              (is (not (cl-quil::power-of-two-p (1+ power-of-two))))
              (is (cl-quil::positive-power-of-two-p power-of-two))
              (is (not (cl-quil::positive-power-of-two-p (1+ power-of-two)))))))

(deftest test-check-permutation ()
  "Test that CHECK-PERMUTATION signals error iff input is not valid."
  ;; Duplicates
  (signals simple-error (cl-quil/frontend::check-permutation '(0 0)))
  (signals simple-error (cl-quil/frontend::check-permutation '(0 0 1)))
  (signals simple-error (cl-quil/frontend::check-permutation '(0 1 0)))
  (signals simple-error (cl-quil/frontend::check-permutation '(1 0 0)))
  (signals simple-error (cl-quil/frontend::check-permutation '(0 1 2 3 4 5 2)))
  ;; Out of range values
  (signals simple-error (cl-quil/frontend::check-permutation '(1)))
  (signals simple-error (cl-quil/frontend::check-permutation '(-1)))
  (signals simple-error (cl-quil/frontend::check-permutation '(0 2)))
  (signals simple-error (cl-quil/frontend::check-permutation '(2 0)))
  (signals simple-error (cl-quil/frontend::check-permutation '(0 1 3)))
  (signals simple-error (cl-quil/frontend::check-permutation '(0 3 1)))
  (signals simple-error (cl-quil/frontend::check-permutation '(3 1 0)))
  (signals simple-error (cl-quil/frontend::check-permutation '(0 1 2 5 3)))
  ;; Valid permutations. Grows as n!, so don't get too crazy here.
  (dotimes (n 6)
    (a:map-permutations
     (lambda (permutation)
       (not-signals simple-error (cl-quil/frontend::check-permutation permutation)))
     (a:iota n))))

(deftest test-quil<->lisp-bridge ()
  "Test that the functions for mapping between quil<->lisp work."
  (loop :for (quil-string . lisp-symbol) :in cl-quil/frontend::+quil<->lisp-prefix-arithmetic-operators+ :do
    (progn
      (is (cl-quil/frontend::valid-quil-function-or-operator-p lisp-symbol))
      (is (eq lisp-symbol (cl-quil/frontend::quil-prefix-operator->lisp-symbol quil-string)))
      (is (string= quil-string (cl-quil/frontend::lisp-symbol->quil-prefix-operator lisp-symbol)))
      (is (string= quil-string (cl-quil/frontend::lisp-symbol->quil-function-or-prefix-operator lisp-symbol)))))

  (loop :for (quil-string . lisp-symbol) :in cl-quil/frontend::+quil<->lisp-infix-arithmetic-operators+ :do
    (progn
      (is (cl-quil/frontend::valid-quil-function-or-operator-p lisp-symbol))
      (is (eq lisp-symbol (cl-quil/frontend::quil-infix-operator->lisp-symbol quil-string)))
      (is (string= quil-string (cl-quil/frontend::lisp-symbol->quil-infix-operator lisp-symbol)))))

  (loop :for (quil-string . lisp-symbol) :in cl-quil/frontend::+quil<->lisp-functions+ :do
    (progn
      (is (cl-quil/frontend::valid-quil-function-or-operator-p lisp-symbol))
      (is (eq lisp-symbol (cl-quil/frontend::quil-function->lisp-symbol quil-string)))
      (is (string= quil-string (cl-quil/frontend::lisp-symbol->quil-function lisp-symbol)))
      (is (string= quil-string (cl-quil/frontend::lisp-symbol->quil-function-or-prefix-operator lisp-symbol))))))

(deftest test-nth-instr ()
  (dolist (pp (list (cl-quil:parse-quil "")
                    (with-output-to-quil
                      "RESET")
                    (with-output-to-quil
                      "RESET"
                      "MEASURE 0")
                    (with-output-to-quil
                      "RESET"
                      "MEASURE 0"
                      "HALT")))
    (let* ((code (parsed-program-executable-code pp))
           (length (length code)))
      ;; Index out-of-bounds checks
      (signals error (cl-quil::nth-instr length pp :from-end nil))
      (signals error (cl-quil::nth-instr length pp :from-end t))
      (signals error (cl-quil::nth-instr -1 pp :from-end nil))
      (signals error (cl-quil::nth-instr -1 pp :from-end t))

      ;; Test all valid indices
      (dotimes (i length)
        (is (eq (cl-quil::nth-instr i pp) (aref code i)))
        (is (eq (cl-quil::nth-instr (- length i 1) pp :from-end t) (aref code i)))
        (let ((no-op1 (make-instance 'cl-quil::no-operation))
              (no-op2 (make-instance 'cl-quil::no-operation)))
          ;; These two SETFs set the same location, hence the need for two distinct no-op
          ;; instructions to compare against.
          (setf (cl-quil::nth-instr i pp) no-op1)
          (is (eq no-op1 (aref code i)))
          (setf (cl-quil::nth-instr (- length i 1) pp :from-end t) no-op2)
          (is (eq no-op2 (aref code i))))))))

(deftest test-make-rewiring-from-string ()
  (signals error (cl-quil::make-rewiring-from-string ""))
  (signals error (cl-quil::make-rewiring-from-string "foobar"))
  (signals error (cl-quil::make-rewiring-from-string "(0 1 2)"))
  (signals error (cl-quil::make-rewiring-from-string "#(0 1 2"))
  (signals error (cl-quil::make-rewiring-from-string "#(1)"))
  (signals error (cl-quil::make-rewiring-from-string "#(0 3 2)"))
  (signals error (cl-quil::make-rewiring-pair-from-string ""))
  (signals error (cl-quil::make-rewiring-pair-from-string "()"))
  (signals error (cl-quil::make-rewiring-pair-from-string "(1 . 2)"))
  (signals error (cl-quil::make-rewiring-pair-from-string "(#(0 . #())"))
  (signals error (cl-quil::make-rewiring-pair-from-string "(#(0) . #())"))
  (signals error (cl-quil::make-rewiring-pair-from-string "( #(0) . #(0) )"))
  (signals error (cl-quil::make-rewiring-pair-from-string "(#(0) #(0))"))
  (signals error (cl-quil::make-rewiring-pair-from-string "#(0) #(0)"))

  (dolist (input '("#()" "#(0)" "#(0 1 2)" "#(2 1 0)" "#(1 0 3 4 2 5 6)"))
    (is (equalp (cl-quil::rewiring-l2p (cl-quil::make-rewiring-from-string input))
                (read-from-string input))))

  (dolist (input '("(#() . #())" "(#(0) . #(0))" "(#(0 2 1 4 3) . #(3 2 0 1 4))"))
    (multiple-value-bind (actual-first-rewiring actual-second-rewiring)
        (cl-quil::make-rewiring-pair-from-string input)
      (destructuring-bind (expected-first-rewiring . expected-second-rewiring)
          (read-from-string input)
        (is (equalp (cl-quil::rewiring-l2p actual-first-rewiring) expected-first-rewiring))
        (is (equalp (cl-quil::rewiring-l2p actual-second-rewiring) expected-second-rewiring))))))

(deftest test-extract-final-exit-rewiring-vector ()
  (is (null (cl-quil::extract-final-exit-rewiring-vector (cl-quil:parse-quil ""))))

  ;; test that the *final* rewiring is extracted
  (let ((pp (with-output-to-quil
              "RESET"
              "MEASURE 0")))
    (setf (cl-quil::comment (cl-quil::nth-instr 0 pp))
          (cl-quil::make-rewiring-comment :exiting #(0 1 2)))
    (setf (cl-quil::comment (cl-quil::nth-instr 1 pp))
          (cl-quil::make-rewiring-comment :exiting #(1 0 2)))
    (is (equalp #(1 0 2) (cl-quil::extract-final-exit-rewiring-vector pp))))

  ;; Each test in the loop should allocate to a fresh PARSED-PROGRAM; otherwise, comments attached
  ;; earlier in the loop persist for later tests.
  (dolist (quil (list "RESET"
                      (format nil "RESET~%MEASURE 0")
                      (format nil "RESET~%MEASURE 0~%HALT")))
    ;; no rewirings
    (is (null (cl-quil::extract-final-exit-rewiring-vector (cl-quil:parse-quil quil))))

    ;; only enter rewiring
    (is (null (cl-quil::extract-final-exit-rewiring-vector
               (attach-rewirings-to-program (cl-quil:parse-quil quil) #(0 1 2) nil))))

    ;; only exit rewiring
    (is (equalp #(2 1 0) (cl-quil::extract-final-exit-rewiring-vector
                          (attach-rewirings-to-program (cl-quil:parse-quil quil) nil #(2 1 0)))))

    ;; both enter and exiting rewirings
    (is (equalp #(2 1 0) (cl-quil::extract-final-exit-rewiring-vector
                          (attach-rewirings-to-program (cl-quil:parse-quil quil) #(0 1 2) #(2 1 0)))))))

(defun %make-density-qvm-initialized-in-basis (num-qubits basis-index)
  "Make a DENSITY-QVM that is initialized in the basis state described by BASIS-INDEX.

To put the density matrix into the basis state, e.g., |01><11|, we would choose BASIS-INDEX = 7. In general, the basis state |a><b| is prepared by choosing BASIS-INDEX = (2^N * a + b)."
  (let ((qvm (qvm:make-density-qvm num-qubits)))
    (setf (aref (qvm::amplitudes qvm) 0) (qvm:cflonum 0)
          (aref (qvm::amplitudes qvm) basis-index) (qvm:cflonum 1))
    qvm))

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
         (p-comp (cl-quil:compiler-hook (parse-quil p-str) (cl-quil::build-nq-linear-chip 3) :protoquil nil))
         (rewiring (cl-quil/frontend::qubit-relabeler (cl-quil::extract-final-exit-rewiring-vector p-comp))))
    (loop :for i :below (expt 2 6) :do
      (let* ((qvm (%make-density-qvm-initialized-in-basis 3 i))
             (qvm-comp (%make-density-qvm-initialized-in-basis 3 i)))
        (qvm:load-program qvm p :supersede-memory-subsystem t)
        ;; relabeling is a side-effect
        (map nil (a:rcurry #'cl-quil/frontend::%relabel-qubits rewiring)
             (parsed-program-executable-code p-comp))
        (qvm:load-program qvm-comp p-comp :supersede-memory-subsystem t)
        (qvm:run qvm)
        (qvm:run qvm-comp)
        (is (every #'cl-quil::double=
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
                 (new-prog (cl-quil:copy-instance p)))
             (loop :for char :across word :do
               (vector-push (if (zerop char)
                                (pop orig-code)
                                (make-instance 'cl-quil:measure-discard :qubit (cl-quil:qubit (1- char))))
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
                                 (cl-quil::print-parsed-program p s))))
                            (parse-quil "RX(pi/3) 0
CNOT 0 2
RX(7*pi/3) 2")
                            3))
