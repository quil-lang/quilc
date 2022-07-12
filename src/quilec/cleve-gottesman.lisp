;;;; src/quilec/cleve-gottesman.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:cl-quil/quilec)

;;; This file contains an implementation of the algorithm introduced in:
;;;
;;; R. Cleve and D. Gottesman, “Efficient computations of encodings for
;;; quantum error correction,” Phys. Rev. A, vol. 56, no. 1, pp. 76–82,
;;; Jul. 1997, doi: 10.1103/PhysRevA.56.76.
;;;
;;; Both the arXiv and published version of the above paper contain
;;; significant errors which are not present in this
;;; implementation. D. Gottesman has published an erratum for his
;;; Ph.D. thesis at:
;;; https://www.perimeterinstitute.ca/personal/dgottesman/thesis-errata.html
;;; Specifically, the comment about Chapter 4 addresses the most important
;;; issue. Note that the notation in the thesis differs from the notation
;;; used in the paper, which is the one we have adopted here.
;;;
;;; See also:
;;;
;;; D. Gottesman, “Class of quantum error-correcting codes saturating the
;;; quantum Hamming bound,” Phys. Rev. A, vol. 54, no. 3, pp. 1862–1868,
;;; Sep. 1996, doi: 10.1103/PhysRevA.54.1862.

(defun cleve-gottesman (group)
  "Apply the Cleve-Gottesman algorithm to GROUP and return a STABILIZER-GROUP to be used for quantum error correction."
  (declare (type group group))
  (multiple-value-bind (xg zg)
      (group-to-matrices group)
    (cleve-gottesman-from-matrices xg zg)))

(defun cleve-gottesman-from-matrices (xg zg)
  "Apply the Cleve-Gottesman algorithm to the X- and Z-matrices given in XG and ZG. Return a STABILIZER-GROUP object containing the results."
  (declare (type tall-and-thin-matrix xg zg)
           (values stabilizer-group))
  (let ((n (matrix-shape xg)))
    (flet ((get-x*-and-z* (xg zg r b k)
             "Calculate seed generators and prepend them to the columns of the X- and Z-matrices."
             (unless (plusp r)
               (error "No secondary generators present."))
             (let ((eye (matrix-eye k))
                   (b1t (matrix-transpose (matrix-submatrix zg 0 0 k r)))
                   (zeros1 (make-matrix b k))
                   (zeros2 (make-matrix n k)))
               (values (matrix-horzcat (matrix-vertcat eye b1t zeros1) xg)
                       (matrix-horzcat zeros2 zg)))))
      (multiple-value-bind (xg zg r b k)
          (canonicalize xg zg)
        (multiple-value-bind (zg xg r2 r1 k*)
            (canonicalize zg xg :rows (+ r k) :cols r)
          (declare (ignore k*))
          (when (plusp r2)
            (setf xg (matrix-submatrix xg 0 r2 n (+ r b))
                  zg (matrix-submatrix zg 0 r2 n (+ r b))
                  r r1
                  k (- n (+ r b)))
            (warn "Redundant generators have been discarded."))
          (multiple-value-bind (x* z*)
              (get-x*-and-z* xg zg r b k)
            (let* ((stabilizer-group (make-stabilizer-group-from-matrices x* z* b r k))
                   (codeword-circuit (make-codeword-circuit stabilizer-group)))
              (setf (slot-value stabilizer-group 'codeword-circuit) codeword-circuit)
              stabilizer-group)))))))

(defun commutes-p (group)
  "Return T if the generators of GROUP commute, NIL otherwise."
  (declare (type group group)
           (values boolean))
  (flet ((binary-product (a b)
           (loop :with value := 0
                 :for al :across a
                 :for bl :across b :do
                   (setf value (logxor value (logand al bl)))
                 :finally (return value))))
    (multiple-value-bind (x z)
        (group-to-matrices group)
      (loop :with generators := (generators group)
            :with d := (length generators)
            :for i :below d
            :for xi := (matrix-col x i)
            :for zi := (matrix-col z i) :do
              (loop :for j :from (1+ i) :below d
                    :for xj := (matrix-col x j)
                    :for zj := (matrix-col z j) :do
                      (unless (zerop (logxor (binary-product xi zj)
                                             (binary-product zi xj)))
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
              (assert (commutes-p (group-from-matrices matrix1 matrix2)))
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

(defun make-stabilizer-group-from-matrices (x* z*
                                            number-of-primary-generators
                                            number-of-secondary-generators
                                            number-of-data-qubits)
  "Return a new stabilizer group based on the entries of the X* and Z* matrices and the partitioning of its columns into seed, secondary, and primary generators."
  (declare (type square-matrix x* z*)
           (type (integer 1) number-of-primary-generators number-of-data-qubits)
           (type (integer 0) number-of-secondary-generators)
           (values stabilizer-group))

  (let* ((n (matrix-rows x*))
         (b number-of-primary-generators)
         (r number-of-secondary-generators)
         (k number-of-data-qubits)
         (all-generators (generators-from-matrices x* z*))
         (stabilizer-group (make-instance 'stabilizer-group :n n :k k)))

    (assert (= (+ b r k) n))

    (setf (slot-value stabilizer-group 'number-of-logical-qubits) k
          (slot-value stabilizer-group 'generators) (subseq all-generators k)
          (slot-value stabilizer-group 'primary-generators) (subseq all-generators (+ r k))
          (slot-value stabilizer-group 'secondary-generators) (subseq all-generators k (+ r k))
          (slot-value stabilizer-group 'seed-generators) (subseq all-generators 0 k))

    stabilizer-group))

(defun print-as-gate (1-base4 control-qubit target-qubit)
  (format t "~[CNOT~;CZ~;CONTROLLED RY(pi)~] ~D ~D~%"
          1-base4 control-qubit target-qubit))

(defun make-codeword-circuit (stabilizer-group)
  "Return a parsed program to obtain codewords in the code determined by STABILIZER-GROUP.

Note that the resulting program requires that the data qubits be set by the user prior to its execution."
  ;; Important: we depart from the original algorithm in that we omit the Z
  ;; gates at the controlled qubits and we place the Hadamard gates right
  ;; before the application of the generator.
  (declare (type stabilizer-group stabilizer-group))
  (with-output-to-quil
    (let ((n (number-of-physical-qubits stabilizer-group))
          (b (length (primary-generators stabilizer-group))))

      (loop :for l :from 0
            :for g :across (seed-generators stabilizer-group) :do
              (loop :for qubit :below n
                    :for gate :across (subseq (pauli-components g) 1)
                    :when (and (/= qubit l) (= gate 1)) :do
                      (format t "CNOT ~D ~D~%" l qubit)))

      (loop :for l :from (- n b)
            :for g :across (primary-generators stabilizer-group) :do
              (loop :initially (format t "H ~D~%" l)
                    :for qubit :below n
                    :for base4-term :across (subseq (pauli-components g) 1)
                    :unless (or (= qubit l) (zerop base4-term)) :do
                      (print-as-gate (1- base4-term) l qubit))))))

(defun make-syndrome-measurement-circuit (stabilizer-group &optional (ancillae-name "ancillae"))
  "Return a parsed program to evaluate the syndromes in STABILIZER-GROUP. The resulting program stores the results in the classical memory named ANCILLAE-NAME."
  (declare (type stabilizer-group stabilizer-group)
           (type string ancillae-name))
  (with-output-to-quil
    (loop :with generators := (generators stabilizer-group)
          :with d := (length generators)
          :initially (format t "DECLARE ~A BIT[~D]~%" ancillae-name d)
          :for i :from 0
          :for qubit :from (number-of-physical-qubits stabilizer-group)
          :for generator :across generators :do
            (loop :initially (format t "H ~D~%" qubit)
                  :with base4-terms := (pauli-components generator)
                  :for q :from 0
                  :for base4-term :across (subseq base4-terms 1) :do
                    (when (plusp base4-term)
                      (print-as-gate (1- base4-term) qubit q))
                  :finally (format t "H ~D~%~
                                      MEASURE ~D ~A[~D]~%"
                                   qubit qubit ancillae-name i)))))
