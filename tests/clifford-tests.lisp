;;;; clifford-tests.lisp
;;;;
;;;; Author: Nik Tezak
;;;;         Robert Smith

(in-package #:cl-quil-tests)

(defun hash-keys (ht)
  (loop :for k :being :the :hash-keys :of ht
        :collect k))

(deftest test-clifford-utilities ()
  (is (= 10 (list-length (quil.clifford::sample-from '(a b c) 10))))

  (is (every (lambda (x) (member x '(a b c))) (quil.clifford::sample-from '(a b c) 10)))

  (let ((q (quil.clifford::make-queue)))
    (quil.clifford::enqueue q 'A)
    (quil.clifford::enqueue q 'B)
    (is (equalp (quil.clifford::dequeue q) 'A))
    (is (equalp (quil.clifford::dequeue q) 'B))
    (is (equalp (quil.clifford::dequeue q) nil))))

(deftest test-paulis ()
  (is (quil.clifford::pauli-sym-p 'quil.clifford::X))
  (is (quil.clifford::pauli-sym-p 'quil.clifford::Y))
  (is (quil.clifford::pauli-sym-p 'quil.clifford::Z))
  (is (quil.clifford::pauli-sym-p 'quil.clifford::I))
  (is (not (quil.clifford::pauli-sym-p 'quil.clifford::A)))

  (is (quil.clifford::pauli-sym-p 'X))
  (is (quil.clifford::pauli-sym-p 'Y))
  (is (quil.clifford::pauli-sym-p 'Z))
  (is (quil.clifford::pauli-sym-p 'I))
  (is (not (quil.clifford::pauli-sym-p 'A)))

  (is (quil.clifford::base4-p 0))
  (is (quil.clifford::base4-p 1))
  (is (quil.clifford::base4-p 2))
  (is (quil.clifford::base4-p 3))
  (is (not (quil.clifford::base4-p -1)))
  (is (not (quil.clifford::base4-p 4)))

  (is (equalp (quil.clifford::base4-to-sym 0) 'quil.clifford::I))
  (is (equalp (quil.clifford::base4-to-sym 1) 'quil.clifford::X))
  (is (equalp (quil.clifford::base4-to-sym 2) 'quil.clifford::Z))
  (is (equalp (quil.clifford::base4-to-sym 3) 'quil.clifford::Y))

  (is (equalp (quil.clifford::pack-base4 3 2) 14))

  (loop :for sym :in '(quil.clifford::I quil.clifford::X quil.clifford::Y quil.clifford::Z)
        :do (is (equalp (quil.clifford::base4-to-sym (quil.clifford::sym-to-base4 sym)) sym)))


  (is (quil.clifford:pauli= (quil.clifford:pauli-identity 3) (quil.clifford:pauli-from-symbols '(I I I))))
  (is (not (quil.clifford:pauli= (quil.clifford:pauli-identity 3) (quil.clifford:pauli-from-symbols '(I X I)))))

  (is (quil.clifford:pauli= (quil.clifford:group-mul (quil.clifford:pauli-from-symbols '(I X Z))
                                                     (quil.clifford:pauli-from-symbols '(I Y X)))
                            (quil.clifford:pauli-from-symbols '(I Z Y) 2)))

  (is (quil.clifford:pauli= (quil.clifford:embed (quil.clifford:pauli-from-symbols '(X Y) 2) 4 '(1 2))
                            (quil.clifford:pauli-from-symbols '(I Y X I) 2)))

  (is (quil.clifford:pauli= (quil.clifford:tensor-mul quil.clifford::+X+ quil.clifford::+Z+)
                            (quil.clifford:pauli-from-symbols '(X Z)))))

(deftest test-pauli-integer ()
  (flet ((int (s)
           (cl-quil.clifford::pauli-integer
            (cl-quil.clifford:pauli-from-string s))))
    (loop :for i :from 1 :to 10 :do
      (is (= 0 (int (make-string i :initial-element #\I)))))
    (is (= 1 (int "iI")))
    (is (= 2 (int "-I")))
    (is (= 3 (int "-iI")))
    (is (= 4 (int "X")))
    (is (= 4 (int "IIIIX")))
    (loop :for n :from 1 :to 9 :do
      (is (loop :for i :below (expt 4 (1+ n))
                :always (= i (cl-quil.clifford::pauli-integer
                              (cl-quil.clifford::integer-pauli i n))))))))

(deftest test-clifford-identity ()
  (loop :for i :from 1 :to 10 :do
    (is (cl-quil.clifford::clifford-identity-p
         (cl-quil.clifford::clifford-identity i)))))

(deftest test-clifford-element ()
  ;; TODO: should test more here
  (is (cl-quil.clifford::clifford-identity-p
       (cl-quil.clifford::clifford-element
         X -> X
         Z -> Z))))

(deftest test-cliffords ()
  (let (
        (xyz (quil.clifford:pauli-from-symbols '(X Y Z)))
        (xyx (quil.clifford:pauli-from-symbols '(X Y X)))
        (gt1 (quil.clifford:make-god-table (quil.clifford:default-gateset 1)))
        (gt2 (quil.clifford:make-god-table (quil.clifford:default-gateset 2)))
        )
    (is (quil.clifford:pauli= (quil.clifford:apply-clifford (quil.clifford:clifford-identity 3) xyz)
                              xyz))
    (is (quil.clifford:pauli= (quil.clifford:apply-clifford (quil.clifford:hadamard 3 0) xyz)
                              xyx))
    (is (quil.clifford:pauli= (quil.clifford:apply-clifford (quil.clifford::clifford-from-pauli (quil.clifford:pauli-from-symbols '(Z Y Z))) xyz)
                              (quil.clifford:pauli-from-symbols '(X Y Z) 2)))
    (is (quil.clifford:clifford= (quil.clifford:group-mul (quil.clifford:cnot 3 2 1) (quil.clifford:cnot 3 2 1))
                                 (quil.clifford:clifford-identity 3)))
    (is (= (hash-table-count (quil.clifford::mapping gt1)) 24))
    (is (= (hash-table-count (quil.clifford::mapping gt2)) 11520))

    (let ((gt1-10 (nth 10 (hash-keys (quil.clifford::mapping gt1))))
          (gt2-1231 (nth 1231 (hash-keys (quil.clifford::mapping gt2)))))
      (is (typep gt1-10 'quil.clifford::clifford))
      (is (typep gt2-1231 'quil.clifford::clifford))

      (is (quil.clifford:clifford= gt1-10 (reduce #'quil.clifford:group-mul (quil.clifford:reconstruct gt1-10 gt1))))
      (is (quil.clifford:clifford= gt2-1231 (reduce  #'quil.clifford:group-mul (quil.clifford:reconstruct gt2-1231 gt2)))))))

(deftest test-sample ()
  (let ((gt (make-god-table (default-gateset 1))))
    (dotimes (num-samples 5) (is (= (length (quil.clifford:sample num-samples gt)) num-samples)))
    (let ((sample (quil.clifford::sample 1 gt)))
      (is (gethash (car sample) (quil.clifford::mapping gt))))))

(deftest test-count-things ()
  ;;   |Sp(2n, 2)|*|P*(n)|
  ;; = |Sp(2n, 2)|*2^(2n)
  ;; = |C(n)|
  (is (= 6
         (cl-quil.clifford::count-symplectic 1)
         (/ (count-clifford 1) (expt 2 (* 2 1)))))
  (is (= 720
         (cl-quil.clifford::count-symplectic 2)
         (/ (count-clifford 2) (expt 2 (* 2 2)))))
  (is (= 1451520
         (cl-quil.clifford::count-symplectic 3)
         (/ (count-clifford 3) (expt 2 (* 2 3)))))
  (is (= 47377612800
         (cl-quil.clifford::count-symplectic 4)
         (/ (count-clifford 4) (expt 2 (* 2 4))))))

(deftest test-direct-sum ()
  (loop :for n :from 1 :to 10
        :for i := (* 2 n)               ; Sp(2n, 2)
        :for s := (make-array (list i i) :element-type 'bit :initial-element 0)
        :do (progn
              ;; make identity
              (dotimes (j i)
                (setf (aref s j j) 1))
              ;; the test
              (let ((result (cl-quil.clifford::direct-sum s s)))
                (is (loop :for row :below (* 2 i)
                          :always (loop :for col :below (* 2 i)
                                        :always (if (= row col)
                                                    (= 1 (aref result row col))
                                                    (= 0 (aref result row col))))))))))

(deftest test-integer-bits ()
  (flet ((test-both-ways (i x n)
           (is (equal x (cl-quil.clifford::integer-bits i n)))
           (is (equal i (cl-quil.clifford::bits-integer x)))
           (is (= i (cl-quil.clifford::bits-integer (cl-quil.clifford::integer-bits i n))))))
    (is (test-both-ways #b0 #*0 1))
    (is (test-both-ways #b0 #*000 3))
    (is (test-both-ways #b1 #*1 1))
    (is (test-both-ways #b1 #*100 3))
    (is (test-both-ways #b101 #*101 3))
    (is (test-both-ways #b1111 #*1111 4))))

(deftest test-random-clifford ()
  ;; There is a very tiny probability this test won't pass in the
  ;; worst case that out of 10k runs we never reach all 24. This is
  ;; extremely, extremely rare.
  (let ((tab (cl-quil.clifford::make-clifford-hash-table)))
    (loop :repeat 10000 :do
      (let ((r (random-clifford 1)))
        (assert (= 1 (num-qubits r)))
        (setf (gethash r tab) t)))
    ;; Make sure we only have 24 Cliffords
    (is (= (count-clifford 1) (hash-table-count tab)))))

(deftest test-symplectic-conversion ()
  "Test that we can go back and forth between symplectic and Clifford representations."
  (loop :for n :from 1 :to 10 :do
    (is (loop :repeat 1000
              :always (let ((c (random-clifford n)))
                        (multiple-value-bind (sp r s)
                            (cl-quil.clifford::clifford-symplectic c)
                          (clifford= c (cl-quil.clifford::symplectic-clifford sp r s))))))))

(deftest test-canonical-swap-representative ()
  ;;Verify that this map canonizes SWAP to identity
  ;; (is (quil.clifford:clifford= (canonical-swap-representative (quil.clifford:SWAP 2 (cl-permutation:make-perm 2 1))) (SWAP 2 (cl-permutation:perm-identity 2))))
  (let ((gt (make-god-table (default-gateset 2)))
        (equivalence-classes (quil.clifford::make-clifford-hash-table)))
    (loop :for key :being :the :hash-keys :of (quil.clifford::mapping gt) :do
      ;;Check that each element canonizes to the same representative
      (is (clifford= (canonical-swap-representative key)
                     (canonical-swap-representative
                      (quil.clifford:group-mul
                       (quil.clifford:SWAP 2 (cl-permutation:make-perm 2 1))
                       key))))
      (let ((rep (canonical-swap-representative key)))
        (if (not (gethash rep equivalence-classes))
            (push (list key) (gethash rep equivalence-classes))
            (setf (gethash rep equivalence-classes)
                  (cons key (gethash rep equivalence-classes))))))
    ;;Check that all 2Q classes contain two elements
    (dolist (class (a:hash-table-values equivalence-classes))
      (is (= 2 (length class))))))

(defparameter *chp-test-files-directory*
  (asdf:system-relative-pathname ':cl-quil-tests "tests/chp-test-files/"))

(deftest test-chp-files ()
  "Check that basic CHP interpretation works."
  (fresh-line)
  (dolist (file (uiop:directory-files *chp-test-files-directory* "*.chp"))
    (format t "    Interpreting CHP file ~A..." (pathname-name file))
    (finish-output)
    (let ((start-time (get-internal-real-time)))
      (not-signals error
        (let ((*standard-output* (make-broadcast-stream)))
          (cl-quil.clifford::interpret-chp-file file)))
      (format t " ~D ms~%" (round (* 1000 (- (get-internal-real-time) start-time))
                                 internal-time-units-per-second)))))

(deftest test-chp-cnot-hadamard-measure ()
  "Simple statistical tests for CNOT, H, and MEASURE for the stabilizer formalism."
  (loop :with results := (vector 0 0)
        :repeat 500
        :do
           (let ((tab (cl-quil.clifford::make-tableau-zero-state 6))
                 (qubits (a:iota 6) #+igh(a:shuffle '(0 1 2 3 4 5))))
             (cl-quil.clifford::tableau-apply-h    tab 0)
             (cl-quil.clifford::tableau-apply-cnot tab 0 1)
             (cl-quil.clifford::tableau-apply-cnot tab 1 2)
             (cl-quil.clifford::tableau-apply-cnot tab 2 3)
             (cl-quil.clifford::tableau-apply-cnot tab 3 4)
             (cl-quil.clifford::tableau-apply-cnot tab 4 5)
             (multiple-value-bind (v d?)
                 (cl-quil.clifford::tableau-measure tab (pop qubits))
               ;; First measurement is *not* deterministic! It's a GHZ
               ;; state!
               (is (not d?))
               (is (typep v 'bit))
               (incf (aref results v))
               (loop :repeat 5 :do
                 (multiple-value-bind (v1 d?)
                     (cl-quil.clifford::tableau-measure tab (pop qubits))
                   ;; This measurement *is* deterministic, and the
                   ;; same as V.
                   (is d?)
                   (is (typep v1 'bit))
                   (is (= v1 v))))))
           ;; We should have approx 50/50 distribution of results,
           ;; which means there should be 250 of each. We just check
           ;; that their difference doesn't exceed 100, which is
           ;; *VERY* forgiving.
        :finally (is (< (abs (- (aref results 0)
                                (aref results 1)))
                        100))))

(defun blank-wf (n)
  (make-array (expt 2 n) :initial-element #C(0.0d0 0.0d0)))

(deftest test-tableau-wavefunction ()
  "Some simple tests to see if wavefunctions are correctly generated from tableaus. There are many, many more tests in qvm/tests/test-stabilizer-qvm.lisp"
  (let ((zero-wf (blank-wf 2))
        (bell-wf (blank-wf 2))
        (x-wf (blank-wf 2))
        (phased-bell-wf (blank-wf 2))
        (zero-tab (cl-quil.clifford::make-tableau-zero-state 2))
        (bell-tab (cl-quil.clifford::make-tableau-zero-state 2))
        (x-tab (cl-quil.clifford::make-tableau-zero-state 2))
        (phased-bell-tab (cl-quil.clifford::make-tableau-zero-state 2)))
    ;; zero state wavefunction
    (setf (aref zero-wf 0) 1)
    (is (cl-quil.clifford::global-phase~ 
         zero-wf
         (cl-quil.clifford::tableau-wavefunction zero-tab)))
    ;; bell state wavefunction
    (setf (aref bell-wf 0) (/ (sqrt 2)))
    (setf (aref bell-wf 3) (/ (sqrt 2)))
    (cl-quil.clifford::tableau-apply-h bell-tab 0)
    (cl-quil.clifford::tableau-apply-cnot bell-tab 0 1)
    (is (cl-quil.clifford::global-phase~
         bell-wf
         (cl-quil.clifford::tableau-wavefunction bell-tab)))
    ;; simple x gate test
    (setf (aref x-wf 1) 1)
    (cl-quil.clifford::tableau-apply-h x-tab 0)
    (cl-quil.clifford::tableau-apply-phase x-tab 0)
    (cl-quil.clifford::tableau-apply-phase x-tab 0)
    (cl-quil.clifford::tableau-apply-h x-tab 0)
    (is (cl-quil.clifford::global-phase~
         x-wf
         (cl-quil.clifford::tableau-wavefunction x-tab)))
    ;; wavefunction with complex phase
    (setf (aref phased-bell-wf 0) (/ (sqrt 2)))
    (setf (aref phased-bell-wf 3) (/ #C(0 -1) (sqrt 2)))
    (cl-quil.clifford::tableau-apply-h phased-bell-tab 0)
    (cl-quil.clifford::tableau-apply-cnot phased-bell-tab 0 1)
    (cl-quil.clifford::tableau-apply-phase phased-bell-tab 1)
    (cl-quil.clifford::tableau-apply-phase phased-bell-tab 1)
    (cl-quil.clifford::tableau-apply-phase phased-bell-tab 1)
    (is (cl-quil.clifford::global-phase~
         phased-bell-wf
         (cl-quil.clifford::tableau-wavefunction phased-bell-tab)))))

(deftest test-clifford-as-perm ()
  (loop :for i :from 1 :to 4 :do
    (format t "~&    Testing n=~D..." i)
    (is (= (cl-quil.clifford:count-clifford i)
           (perm:group-order (cl-quil.clifford::clifford-group-as-perm-group i))))
    (finish-output)))
