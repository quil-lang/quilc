;;;; src/fault-tolerance/cleve-gottesman.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:cl-quil.quilec)

(defun cleve-gottesman (code)
  "Apply the Cleve-Gottesman algorithm to CODE and return a new code object containing the results."
  (declare (type code code))
  (multiple-value-bind (xg zg)
      (code-to-matrices code)
    (cleve-gottesman-from-matrices xg zg)))

(defun cleve-gottesman-from-matrices (xg zg)
  "Apply the Cleve-Gottesman algorithm to the X- and Z-matrices given in XG and ZG. Return a new object of type CODE containing the results."
  ;; This implements the algorithm of:
  ;;
  ;; R. Cleve and D. Gottesman, “Efficient computations of encodings for
  ;; quantum error correction,” Phys. Rev. A, vol. 56, no. 1,pp. 76–82,
  ;; Jul. 1997, doi: 10.1103/PhysRevA.56.76.
  ;;
  ;; Warning: the arXiv version of the above paper contains serious
  ;; errors. See also:
  ;;
  ;; D. Gottesman, “Class of quantum error-correcting codes saturating the
  ;; quantum Hamming bound,” Phys. Rev. A, vol. 54, no. 3, pp. 1862–1868,
  ;; Sep. 1996, doi: 10.1103/PhysRevA.54.1862.
  (declare (type tall-and-thin-matrix xg zg)
           (values processed-code))
  ;; TODO It would be nice (but not necessary) to enforce some kind of
  ;; lexicographic ordering and/or *minimum* entropy criterion on the
  ;; resulting codeword vectors.
  (flet ((get-x*-and-z* (xg zg r b k)
           (let* ((n (matrix-rows xg))
                  (eye (matrix-eye k))
                  (b1t (matrix-transpose (matrix-submatrix zg 0 0 k r)))
                  (zeros1 (make-matrix b k))
                  (zeros2 (make-matrix n k)))
             (values (matrix-horzcat (matrix-vertcat eye b1t zeros1) xg)
                     (matrix-horzcat zeros2 zg)))))
    (multiple-value-bind (xg zg r b k)
        (canonicalize xg zg)
      (assert (plusp k))
      (multiple-value-bind (zg xg r2 r1 k*)
          (canonicalize zg xg :rows (+ r k) :cols r)
        (declare (ignore k*))
        (assert (and (= r r1) (zerop r2)))
        (multiple-value-bind (x* z*)
            (get-x*-and-z* xg zg r b k)
          (let* ((new-code (make-processed-code x* z* b r k))
                 (codeword-circuit (make-codeword-circuit new-code)))
            (values new-code codeword-circuit)))))))

(defun commutes-p (code)
  "Return T if the generators of CODE commute and NIL otherwise."
  (declare (type code code)
           (values boolean))
  (flet ((binary-product (a b)
           (loop :with value := 0
                 :for al :across a
                 :for bl :across b :do
                   (setf value (logxor value (logand al bl)))
                 :finally (return value))))
    (multiple-value-bind (x z)
        (code-to-matrices code)
      (loop :with generators := (generators code)
            :with d := (length generators)
            :for i :below d
            :for xi := (matrix-col x i)
            :for zi := (matrix-col z i) :do
              (loop :for j :from (1+ i) :below d
                    :for xj := (matrix-col x j)
                    :for zj := (matrix-col z j) :do
                      (unless (zerop (logxor (binary-product xi zj)
                                             (binary-product zi xj)))
                        (warn "Generators ~D (~A) and ~D (~A) do not commute!"
                              i (elt generators i) j (elt generators j))
                        (return-from commutes-p nil)))
            :finally (return t)))))

(defun canonicalize (matrix1 matrix2 &key rows cols)
  "Canonicalize the X- and Z-matrices MATRIX1 and MATRIX2. The keyword arguments ROWS and COLS specify the submatrix on which to act.

                                        ⎡ 0  A ⎤       ⎡ B  C ⎤
The resulting matrices are of the form  ⎢      ⎥  and  ⎢      ⎥.
                                        ⎣ 0  I ⎦       ⎣ D  E ⎦

In addition to the canonicalized matrices, this function returns the number of secondary and primary generators, and the number of data qubits."

  (declare (type tall-and-thin-matrix matrix1 matrix2)
           (type (or null non-negative-fixnum) rows cols)
           (values matrix matrix))
  (flet ((get-pivot-index-within-col (matrix col-index row-index)
           "Find non-zero entry in the column vector of MATRIX with index COL-INDEX starting from ROW-INDEX and ending at zero. Return the index or NIL if none was found."
           (declare (type matrix matrix)
                    (type non-negative-fixnum col-index row-index)
                    (values (or null non-negative-fixnum)))
           (position 1 (matrix-col matrix col-index) :from-end t :end (1+ row-index))))

    (multiple-value-bind (m n)
        (matrix-shape matrix1)
      (assert (equalp (list m n) (multiple-value-list (matrix-shape matrix2))))

      (setf rows (or rows m)
            cols (or cols n))
      (assert (and (<= rows m) (<= cols n)))

      (loop :with m :of-type non-negative-fixnum := rows
            :with n :of-type non-negative-fixnum := cols
            :with i :of-type fixnum := (1- m)
            :with j :of-type fixnum := (1- n)
            :with b :of-type non-negative-fixnum := 0
            :repeat n
            :for pivot-index := (get-pivot-index-within-col matrix1 j i)
            :if pivot-index :do
              (exchange-rows matrix1 pivot-index i)
              (exchange-rows matrix2 pivot-index i)
              (loop :for k :of-type non-negative-fixnum :below n
                    :unless (or (= j k) (zerop (matrix-ref matrix1 i k))) :do
                      (matrix-xor-cols matrix1 j k)
                      (matrix-xor-cols matrix2 j k))
              (assert (commutes-p (code-from-matrices matrix1 matrix2)))
              (decf i)
              (decf j)
              (incf b)
            :else :do
              ;; No pivot found implies zero column vector.
              (move-to-first-col matrix1 j)
              (move-to-first-col matrix2 j)
            :finally (let* ((r (- n b))
                            (r+k (- m b))
                            (k (- r+k r)))
                       (return (values matrix1 matrix2 r b k)))))))

(defun make-processed-code (x* z*
                            number-of-primary-generators
                            number-of-secondary-generators
                            number-of-data-qubits)
  "Return a new code based on the entries of the X* and Z* matrices and the partitioning of its columns into seed, secondary, and primary generators."
  (declare (type square-matrix x* z*)
           (type (integer 1) number-of-primary-generators number-of-data-qubits)
           (type (integer 0) number-of-secondary-generators)
           (values processed-code))

  (let* ((n (matrix-rows x*))
         (b number-of-primary-generators)
         (r number-of-secondary-generators)
         (k number-of-data-qubits)
         (all-generators (generators-from-matrices x* z*))
         (code (make-instance 'code :n n :k k)))

    (assert (= (+ b r k) n))

    (setf (slot-value code 'generators) (subseq all-generators k)
          (slot-value code 'primary-generators) (subseq all-generators (+ r k))
          (slot-value code 'secondary-generators) (subseq all-generators k (+ r k))
          (slot-value code 'seed-generators) (subseq all-generators 0 k))

    code))

(defun make-codeword-circuit (code)
  "Return a parsed program to obtain codewords in CODE.

Note that the resulting program requires that the data qubits be set by the user prior to its execution."
  ;; Important: we depart from the original algorithm in that we omit the Z
  ;; gates at the controlled qubits and we place the Hadamard gates right
  ;; before the application of the generator.
  (declare (type processed-code code))
  (with-output-to-quil
    (let ((count 0)
          (n (number-of-physical-qubits code)))

      (loop :for g :across (seed-generators code) :do
        (loop :for qubit :below n
              :for gate :across (subseq (pauli-components g) 1)
              :when (and (/= qubit count)
                         (= gate 1)) :do
                           (format t "CNOT ~D ~D~%" count qubit)
              :finally (incf count))
            :finally (incf count)) ; This last INCF here is for the |0> in
                                   ; the input |c> ⊗ |0> ⊗ |a> of the
                                   ; encoding circuit.

      (loop :for g :across (primary-generators code) :do
        (loop :initially (format t "H ~D~%" count)
              :for qubit :below n
              :for gate :across (subseq (pauli-components g) 1)
              :unless (= qubit count) :do
                (case gate
                  (1 (format t "CNOT ~D ~D~%" count qubit))
                  (2 (format t "CZ ~D ~D~%" count qubit))
                  (3 (format t "CONTROLLED YY ~D ~D~%" count qubit)))
              :finally (incf count))))))
