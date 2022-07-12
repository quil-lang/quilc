
;;;; benchmarking-procedures-tests.lisp
;;;;
;;;; Author: Anthony Polloreno
(in-package #:cl-quil-tests)

;; Do we seed the RNG somewhere?
(deftest test-rb-sequence ()
  (let* ((sequence-length 10)
         (num-qubits1 1)
         (num-qubits2 2)
         (gateset1 (cl-quil/clifford:default-gateset num-qubits1))
         (gateset2 (cl-quil/clifford:default-gateset num-qubits2))
         (cliffords1 (cl-quil/clifford::cliffords gateset1))
         (cliffords2 (cl-quil/clifford::cliffords gateset2))
         (sequence1 (rb-sequence sequence-length num-qubits1 cliffords1))
         (sequence2 (rb-sequence sequence-length num-qubits2 cliffords2)))
    ;; Check that they compose to the identity.
    (is (cl-quil/clifford:clifford-identity-p (reduce #'cl-quil/clifford:group-mul (apply #'append sequence1))))
    (is (cl-quil/clifford:clifford-identity-p (reduce #'cl-quil/clifford:group-mul (apply #'append sequence2))))
    ;; Check they are of the right length.
    (is (= (length sequence1) (length sequence2) sequence-length))
    ;; Check that the clifford elements consist of element from the gateset.
    (is (loop :for clifford :in (apply #'append sequence1)
              :always (member clifford cliffords1 :test #'cl-quil/clifford:clifford=)))
    (is (loop :for clifford :in (apply #'append sequence2)
              :always (member clifford cliffords2 :test #'cl-quil/clifford:clifford=)))
    ;; Ensure that decompositions are always non-empty.
    (is (loop :for clifford :in sequence1
              :always (plusp (length clifford))))))

(deftest test-interleaved-rb-sequence ()
  (let* ((sequence-length 10)
         (gateset (cl-quil/clifford:default-gateset 1))
         (cliffords (cl-quil/clifford::cliffords gateset))
         (sequence (rb-sequence sequence-length 1 cliffords (first cliffords))))
    ;; Check that they compose to the identity.
    (is (cl-quil/clifford:clifford-identity-p (reduce #'cl-quil/clifford:group-mul (apply #'append sequence))))
    ;; Check they are of the right length.
    (is (= (length sequence) (1- (* 2 sequence-length))))
    (loop :for s :in sequence
          :for j :from 0
          :when (oddp j)
            :do (is (clifford= (first s) (first cliffords))))
    ;; Check that the clifford elements consist of element from the gateset.
    (is (loop :for clifford :in (apply #'append sequence)
              :always (member clifford cliffords :test #'cl-quil/clifford:clifford=)))
    ;; Ensure that decompositions are always non-empty.
    (is (loop :for clifford :in sequence
              :always (plusp (length clifford))))))

(deftest test-serialize-clifford-sequence ()
  (let ((cliff-id1 (list (cl-quil/clifford:clifford-identity 1)))
        (cliff-id2 (list (cl-quil/clifford:clifford-identity 2))))
    (is (equalp (serialize-clifford-sequence cliff-id1) (list (list "X" "Z"))))
    (is (equalp (serialize-clifford-sequence cliff-id2) (list (list "IX" "IZ" "XI" "ZI"))))))

(defun matrix-equalp (a b)
  (let ((ma (magicl:nrows a))
        (na (magicl:ncols a))
        (mb (magicl:nrows b))
        (nb (magicl:ncols b)))
    (and (= ma mb)
         (= na nb)
         (loop :for i :below ma
               :always (loop :for j :below na
                             :always (cl-quil::double= (magicl:tref a i j) (magicl:tref b i j)))))))

(deftest test-n-qubit-pauli-basis-matrices ()
  (let* ((x (cl-quil::from-list '(0 1 1 0) '(2 2)))
         (z (cl-quil::from-list '(1 0 0 -1) '(2 2)))
         (paulis1 (list x z))
         (generated-paulis1 (n-qubit-pauli-basis-matrices 1))
         (generated-paulis2 (n-qubit-pauli-basis-matrices 2)))
    (is (loop :for pauli :in paulis1
              :always
              (member pauli generated-paulis1 :test #'matrix-equalp)))
    (is (= 2 (length paulis1) (length generated-paulis1)))
    (is (= 4 (length generated-paulis2)))))


(deftest test-pauli-matrix-p ()
  (let* ((eye (cl-quil::eye 2))
         (x (cl-quil::from-list '(0 1 1 0) '(2 2)))
         (y (cl-quil::from-list '(0 #C(0 1) #C(0 -1) 0) '(2 2)))
         (z (cl-quil::from-list '(1 0 0 -1) '(2 2)))
         (paulis (list eye x y z))
         (phases (loop :for i :below 4 :collect (expt #C(0 1) i))))
    ;; Make sure the 1 and 2 qubit pauli groups are accepted.
    (is (loop :for pauli :in paulis
              :always (loop :for phase :in phases
                            :always (pauli-matrix-p (magicl:scale pauli phase)))))
    (is (loop :for pauli1 :in paulis
              :always (loop :for pauli2 :in paulis
                            :always (loop :for phase :in phases
                                          :always (pauli-matrix-p (magicl:scale (magicl:kron pauli1 pauli2) phase))))))
    (is (not (pauli-matrix-p (cl-quil::from-list '(0 1 1 1) '(2 2)))))
    (is (not (loop :for pauli :in paulis
                   :always (loop :for phase :in phases
                                 :always (pauli-matrix-p (magicl:scale pauli (* 2 phase)))))))))

(defmacro time-it (&body body)
  ;; Time evaluation of BODY, and return two values: (1) the first
  ;; value returned by body, and (2) elapsed internal runtime.
  (let ((start (gensym "START"))
        (end (gensym "END"))
        (body-value (gensym "BODY-VALUE")))
    `(let ((,start (get-internal-run-time))
           (,end nil)
           (,body-value nil))
       (setf ,body-value (progn ,@body)
             ,end (get-internal-run-time))
       (values ,body-value
               (- ,end ,start)))))

(deftest test-memoize-pauli-basis-matrices ()
  ;; Test that making many calls to the memoized function is cheaper than making many unmemoized calls.
  (let* ((num-calls 100)
         (basis-size 4)
         (non-memo-times
           (loop :for i :from 0 :upto num-calls
                 :do (cl-quil/clifford::%clear-memo-table)
                 :sum (nth-value 1 (time-it (n-qubit-pauli-basis-matrices basis-size)))))
         (memo-times
           (loop :for i :from 0 :upto num-calls
                 :sum (nth-value 1 (time-it (n-qubit-pauli-basis-matrices basis-size)))))
         (diff (- non-memo-times memo-times))
         ;; Require at least 8x faster, reasonable considering a 2020
         ;; Macbook pro experimentally resulted in around 100X.
         (min-slow-to-fast-ratio 8))
    (is (plusp diff))
    (is (>= non-memo-times (* memo-times min-slow-to-fast-ratio)))))

(deftest test-clifford-from-quil ()
  (let ((clifford-quil (format nil "~{~A~%~}" (list "CNOT 0 1" "H 5" "CNOT 0 5" "X 3" "Y 1"))))
    (is (not (null (cl-quil/clifford:clifford-from-quil clifford-quil)))))
  ;; Check to make sure the indices are being parsed correctly (big endian)
  (let ((clifford-quil "CZ 0 5"))
    (is (cl-quil/clifford:clifford=
	 (cl-quil/clifford:clifford-from-quil clifford-quil)
	 (cl-quil/clifford::make-clifford :num-qubits 2
					  :basis-map (make-array 4 :initial-contents (mapcar 'pauli-from-string '("ZX" "IZ" "XZ" "ZI")))))))
  ;; Check to make sure that the program is being parsed the correct way (The top is applied first)
  (let* ((ZI (cl-quil/clifford:pauli-from-string "ZI"))
	(IZ (cl-quil/clifford:pauli-from-string "IZ"))
	(ZZ (cl-quil/clifford:pauli-from-string "ZZ"))
	(IX (cl-quil/clifford:pauli-from-string "IX"))
	(ZX (cl-quil/clifford:pauli-from-string "ZX"))
	(-YY (cl-quil/clifford:pauli-from-string "-YY"))
	(XX (cl-quil/clifford:pauli-from-string "XX"))
	(CN0T01H0-quil (format nil "~{~A~%~}" (list "CNOT 0 1" "H 0")))
	(CN0T01H0 (cl-quil/clifford:clifford-from-quil CN0T01H0-quil))
	(H0CNOT01-quil (format nil "~{~A~%~}" (list "H 0" "CNOT 0 1")))
	(H0CNOT01 (cl-quil/clifford:clifford-from-quil H0CNOT01-quil)))
    (loop
       :for pauli-in :in `(,ZI ,IZ ,ZZ)
       :for pauli-out :in `(,ZX ,IX ,ZI)
       :do (is (cl-quil/clifford:pauli= pauli-out (cl-quil/clifford:apply-clifford CN0T01H0 pauli-in))))
        (loop
       :for pauli-in :in `(,ZI ,IZ ,ZZ)
       :for pauli-out :in `(,ZZ ,XX ,-YY)
       :do (is (cl-quil/clifford:pauli= pauli-out (cl-quil/clifford:apply-clifford H0CNOT01 pauli-in))))))
    

