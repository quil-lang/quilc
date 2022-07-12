;;;; src/quilec/matrix.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:cl-quil/quilec)

;;; Linear algebra routines in GF(2).

;;; TODO Merge into latest MAGICL.

(defstruct (matrix (:constructor %make-matrix))
  (rows 0 :type (integer 0) :read-only t)
  (cols 0 :type (integer 0) :read-only t)
  (entries nil :type (or null simple-bit-vector)))

(defun make-matrix (rows cols &optional entries)
  (declare (type non-negative-fixnum rows cols)
           (type (or null sequence) entries))
  (%make-matrix :rows rows :cols cols
                :entries (if (null entries)
                             (make-array (* rows cols) :element-type 'bit :initial-element 0)
                             (copy-seq (coerce entries 'simple-bit-vector)))))

(defun deep-copy-matrix (matrix)
  (make-matrix (matrix-rows matrix) (matrix-cols matrix) (matrix-entries matrix)))

(defun matrix-shape (matrix)
  (values (matrix-rows matrix)
          (matrix-cols matrix)))

(defun make-zero-matrix-like (matrix)
  (let ((m (matrix-rows matrix))
        (n (matrix-cols matrix)))
    (make-matrix m n)))

(declaim (inline get-row-offset))
(defun row-endpoints (matrix row-index)
  (declare (type matrix matrix)
           (type non-negative-fixnum row-index)
           (values non-negative-fixnum non-negative-fixnum))
  (values (* row-index (matrix-cols matrix))
          (* (1+ row-index) (matrix-cols matrix))))

(defun matrix-row (matrix row-index)
  ;; (policy-cond:policy-when (<= speed safety)
  ;;     (unless (<= 0 row-index (1- (matrix-rows matrix)))
  ;;       (error "Invalid array index")))
  (multiple-value-bind (start end)
      (row-endpoints matrix row-index)
    (copy-seq (subseq (matrix-entries matrix) start end))))

(defun (setf matrix-row) (value matrix row-index)
  (multiple-value-bind (start end)
      (row-endpoints matrix row-index)
    (replace (matrix-entries matrix) value :start1 start :end1 end)))

(define-condition invalid-array-index (simple-error) ())

(defun matrix-ref (matrix row col)
  (let ((cols (matrix-cols matrix)))
    ;; (policy-cond:policy-when (<= speed safety)
    ;;   (unless (and (<= 0 row (1- (matrix-rows matrix)))
    ;;                (<= 0 col (1- (matrix-cols matrix))))
    ;;     (error "Invalid array index")))
                                        ; XXX use the condition.
    (aref (matrix-entries matrix) (+ (* row cols) col))))

(defun (setf matrix-ref) (value matrix row col)
  (declare (type bit value)
           (type matrix matrix)
           (type non-negative-fixnum row col))
  (let ((cols (matrix-cols matrix)))
    ;; (policy-cond:policy-when (<= speed safety)
    ;;   (unless (and (<= 0 row (1- (matrix-rows matrix)))
    ;;                (<= 0 col (1- (matrix-cols matrix))))
    ;;     (error "Invalid array index")))
    (setf (aref (matrix-entries matrix) (+ (* row cols) col)) value)))

(defun matrix-col (matrix col-index)
  (loop :with rows := (matrix-rows matrix)
        :with col := (make-array rows :element-type 'bit)
        :for i :below rows :do
          (setf (aref col i) (matrix-ref matrix i col-index))
        :finally (return col)))

(defun (setf matrix-col) (value matrix col-index)
  (loop :with rows := (matrix-rows matrix)
        :for i :below rows :do
          (setf (matrix-ref matrix i col-index) (aref value i))
        :finally (return matrix)))

(defun pprint-matrix (stream matrix &optional colon-p at-sign-p)
  "Pretty-print a matrix MATRIX to the stream STREAM."
  (declare (ignorable colon-p at-sign-p))
  (let* ((rows (matrix-rows matrix))
         (cols (matrix-cols matrix)))
    (print-unreadable-object (matrix stream :type t :identity t)
      (loop :initially (terpri stream)
            :for i :below rows
            :do (loop :for j :below cols
                      :do (format stream "~2D" (matrix-ref matrix i j))
                      :finally (terpri stream))))))

(set-pprint-dispatch 'matrix 'pprint-matrix)

(defun matrix-xor-rows (matrix i j)
  "Apply an exclusive-or operation to the I-th and J-th rows of MATRIX and store the result in its J-th row."
  (loop :with ri := (matrix-row matrix i)
        :with rj := (matrix-row matrix j)
        :for k :below (matrix-cols matrix) :do
          (setf (aref ri k) (logxor (aref ri k) (aref rj k)))
        :finally (setf (matrix-row matrix j) ri)
                 (return matrix)))

(defun matrix-xor-cols (matrix i j)
  "Apply an exclusive-or operation to the I-th and J-th columns of MATRIX and store the result in its J-th column."
  (loop :with ci := (matrix-col matrix i)
        :with cj := (matrix-col matrix j)
        :for k :below (matrix-rows matrix) :do
          (setf (aref ci k) (logxor (aref ci k) (aref cj k)))
        :finally (setf (matrix-col matrix j) ci)
                 (return matrix)))

(defun matrix-transpose (matrix)
  (let* ((rows (matrix-rows matrix))
         (cols (matrix-cols matrix))
         (transpose (make-matrix cols rows)))
    (dotimes (i rows transpose)
      (dotimes (j cols)
        (setf (matrix-ref transpose j i) (matrix-ref matrix i j))))))

(defun matrix-vertcat (&rest matrices)
  (flet ((vertcat (matrix1 matrix2)
           (multiple-value-bind (m1 n1) (matrix-shape matrix1)
             (multiple-value-bind (m2 n2) (matrix-shape matrix2)
               (assert (= n1 n2))
               (let ((matrix (make-matrix (+ m1 m2) n1)))
                 (loop :for i :below m1 :do
                   (setf (matrix-row matrix i) (matrix-row matrix1 i)))
                 (loop :for k :from 0
                       :for i :from m1 :below (+ m1 m2) :do
                         (setf (matrix-row matrix i) (matrix-row matrix2 k))
                       :finally (return matrix)))))))
    (when matrices
      (reduce #'vertcat matrices))))

(defun matrix-horzcat (&rest matrices)
  (flet ((horzcat (matrix1 matrix2)
           (multiple-value-bind (m1 n1) (matrix-shape matrix1)
             (multiple-value-bind (m2 n2) (matrix-shape matrix2)
               (assert (= m1 m2))
               (let ((matrix (make-matrix m1 (+ n1 n2))))
                 (loop :for j :below n1 :do
                   (setf (matrix-col matrix j) (matrix-col matrix1 j)))
                 (loop :for k :from 0
                       :for j :from n1 :below (+ n1 n2) :do
                         (setf (matrix-col matrix j) (matrix-col matrix2 k))
                       :finally (return matrix)))))))
    (when matrices
      (reduce #'horzcat matrices))))

(defun matrix-eye (n)
  (loop :with matrix := (make-matrix n n)
        :for i :below n :do
          (setf (matrix-ref matrix i i) 1)
        :finally (return matrix)))

(defun matrix-submatrix (matrix i0 j0 &optional i1 j1)
  ;; XXX rewrite matrix-row and matrix-col to use this function.
  (let* ((i1 (or i1 (matrix-rows matrix)))
         (j1 (or j1 (matrix-cols matrix)))
         (submatrix (make-matrix (- i1 i0) (- j1 j0))))
    (unless (and (and (not (minusp i0)) (< i0 i1))
                 (and (not (minusp j1)) (< j0 j1)))
      (error "Invalid submatrix."))
    (loop :for i :from 0
          :for r :from i0 :below i1 :do
            (loop :for j :from 0
                  :for s :from j0 :below j1 :do
                    (setf (matrix-ref submatrix i j) (matrix-ref matrix r s)))
          :finally (return submatrix))))

(defun square-matrix-p (matrix)
  (= (matrix-rows matrix)
     (matrix-cols matrix)))

(deftype square-matrix ()
  `(and matrix (satisfies square-matrix-p)))

(defun tall-and-thin-matrix-p (matrix)
  (< (matrix-cols matrix)
     (matrix-rows matrix)))

(deftype tall-and-thin-matrix ()
  `(and matrix (satisfies tall-and-thin-matrix-p)))

(defun exchange-rows (matrix from-index to-index)
  "Exchange rows of MATRIX numbered by FROM-INDEX and TO-INDEX in-place."
  (when (/= from-index to-index)
    (rotatef (matrix-row matrix from-index) (matrix-row matrix to-index))))

(defun move-to-first-col (matrix col-index)
  (loop :with aux := (matrix-col matrix col-index)
        :for i :upto col-index :do
          (rotatef aux (matrix-col matrix i))
        :finally (return matrix)))
