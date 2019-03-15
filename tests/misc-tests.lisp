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

(deftest test-print-instruction ()
  (is (string= "PRAGMA gate_time CNOT \"50 ns\""
               (with-output-to-string (s)
                 (cl-quil::print-instruction (make-instance 'quil::pragma
                                                   :words '("gate_time" "CNOT")
                                                   :freeform-string "50 ns")
                                             s))))
  ;; try a operand-free instruction
  (is (string= "HALT"
               (with-output-to-string (s)
                 (cl-quil::print-instruction (make-instance 'halt)
                                             s))))
  ;; try a unary instruction
  (is (string= "NEG ro[3]"
               (with-output-to-string (s)
                 (cl-quil::print-instruction (make-instance 'quil::classical-negate
                                                            :target (mref "ro" 3))
                                             s))))
  ;; try a binary instruction
  (is (string= "MEASURE 1 ro[3]"
               (with-output-to-string (s)
                 (cl-quil::print-instruction (make-instance 'quil::measure
                                                            :address (mref "ro" 3)
                                                            :qubit (qubit 1))
                                             s))))
  ;; try something fancy
  (is (string= "CPHASE-AND-MEASURE(%alpha) 1 3 ro[5]"
               (with-output-to-string (s)
                 (cl-quil::print-instruction (make-instance 'cl-quil::circuit-application
                                                            :operator #.(named-operator "CPHASE-AND-MEASURE")
                                                            :parameters `(,(param "alpha"))
                                                            :arguments `(,(qubit 1)
                                                                         ,(qubit 3)
                                                                         ,(mref "ro" 5)))
                                             s)))))

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
             (format s "TEST ~{~d ~}" (alexandria:iota qubit-count))))
         (parsed-prog (quil::parse-quil program-string)))
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

(deftest transform-predecessor-checking ()
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
    (let ((T-A-B-L-E (make-hash-table :test 'quil::memory-ref=)))
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
    ;; re-load the chip spec
    (quil::load-isa-layer chip-spec (gethash "isa" isa))
    ;; check we got the goods
    (dolist (presumed-dead '("4" "8" "9" "10" "11" "12"))
      (let ((goods (gethash-chain (list "isa" "1Q" presumed-dead) isa)))
        (is (and (not (null goods))
                 (hash-table-p goods)
                 (gethash "dead" goods))))))) ; RIP in piece

(deftest test-bristlecone-chip ()
  "Test construction of Google's Bristlecone 72-qubit chip"
  (let* ((chip (quil::build-bristlecone-chip))
         (prgm (parse-quil-string
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
