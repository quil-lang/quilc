;;;; symplectic.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil/clifford)

;;; In this file we write some routines to deal with elements of
;;; the symplectic group Sp(2n, F_2).
;;;
;;; Helpful references:
;;;
;;;   - http://www-math.mit.edu/~dav/sympgen.pdf
;;;
;;;   - "How to efficiently select an arbitrary Clifford group element" (https://arxiv.org/pdf/1406.2170.pdf)

;;; We use SIMPLE-BIT-VECTOR and (SIMPLE-ARRAY BIT (* *)) to represent
;;; vectors and symplectic operators respectively.

(defun direct-sum (a b)
  "The direct sum of two elements A, B in Sp(2n)."
  (uiop:nest
   (destructuring-bind (ra ca) (array-dimensions a))
   (destructuring-bind (rb cb) (array-dimensions b))
   (let ((out (make-array (list (+ ra rb)
                                (+ ca cb))
                          :element-type 'bit
                          :initial-element 0)))

     (dotimes (r ra)
       (dotimes (c ca)
         (setf (aref out r c) (aref a r c))))
     (dotimes (r rb out)
       (dotimes (c cb)
         (setf (aref out (+ r ra) (+ c ca)) (aref b r c)))))))

(defun symplectic-inner-product (u v)
  "The symplectic inner product of vectors U, V in F_2^(2n)."
  (check-type u simple-bit-vector)
  (check-type v simple-bit-vector)
  (assert (and (evenp (length u))
               (= (length u) (length v))))
  (loop :with r := 0
        :for i :below (length u) :by 2
        :do (setf r (logxor r
                            (logand (aref u i) (aref v (1+ i)))
                            (logand (aref v i) (aref u (1+ i)))))
        :finally (return r)))

;;; Operators over F_2^(2n).
(defun .+ (u v)
  (check-type u simple-bit-vector)
  (check-type v simple-bit-vector)
  (map 'simple-bit-vector #'logxor u v))
(defun s.* (c v)
  (check-type c bit)
  (check-type v simple-bit-vector)
  (map 'simple-bit-vector (lambda (vi) (logand c vi)) v))

(defun transvection (k v)
  "Apply the symplectic transvection

    τ(k) := x ↦ x + <x, k>*k

to v."
  (check-type k simple-bit-vector)
  (check-type v simple-bit-vector)
  (.+ v (s.* (symplectic-inner-product k v) k)))

(defun transvections (v &rest ks)
  "Let τ(k) = x + <x, k>*k. Compute the composition of maps

   τ(kn) ∘ ⋯ ∘ τ(k2) ∘ τ(k1)

and apply it to V."
  (loop :with vprime := v
        :while ks
        :do (setf vprime (transvection (pop ks) vprime))
        :finally (return vprime)))

(defun integer-bits (i &optional (n (integer-length i)))
  "Extract the N least-significant bits of I into a SIMPLE-BIT-VECTOR."
  (check-type i unsigned-byte)
  (let ((bits (make-array n :element-type 'bit :initial-element 0)))
    (loop :for j :below n
          :until (zerop i)
          :do (psetf (aref bits j) (logand i 1)
                     i (ash i -1))
          :finally (return bits))))

(defun bits-integer (v)
  "Take the bits defined in the bit-vector V listed from LSB to MSB and recover the integer represented."
  (check-type v simple-bit-vector)
  (reduce (lambda (vi r) (+ vi (* 2 r))) v :from-end t :initial-value 0))

(defun find-transvection (x y)
  "Given the bit-vectors X and Y, compute two transvections S and T such that

    S(T(x)) = y
"
  (check-type x simple-bit-vector)
  (check-type y simple-bit-vector)
  (assert (= (length x) (length y)))
  (let* ((nx (length x))
         (t0 (make-array nx :element-type 'bit :initial-element 0))
         (t1 (make-array nx :element-type 'bit :initial-element 0)))
    (cond
      ;; No transformation needed.
      ((every #'= x y)
       (values t0 t1))

      ;; Transformation is just x + y
      ((= 1 (symplectic-inner-product x y))
       (map-into t0 #'logxor x y)
       (values t0 t1))

      ;; General case.
      (t
       (let* ((nx/2 (floor nx 2))
              (z (make-array nx :element-type 'bit :initial-element 0)))
         ;; This loop can return early!
         (dotimes (i nx/2)
           (let* ((ii (* 2 i))
                  (ii+1 (1+ ii)))
             (unless (or (zerop (+ (aref x ii)
                                   (aref x ii+1)))
                         (zerop (+ (aref y ii)
                                   (aref y ii+1))))
               (setf (aref z ii)   (logxor (aref x ii) (aref y ii))
                     (aref z ii+1) (logxor (aref x ii+1) (aref y ii+1)))
               (when (zerop (+ (aref z ii) (aref z ii+1)))
                 (setf (aref z ii+1) 1)
                 (when (/= (aref x ii) (aref x ii+1))
                   (setf (aref z ii) 1)))
               (map-into t0 #'logxor x z)
               (map-into t1 #'logxor y z)
               (return-from find-transvection (values t0 t1)))))

         ;; Didn't find a pair.
         (dotimes (i nx/2)
           (let* ((ii (* 2 i))
                  (ii+1 (1+ ii)))
             (when (and (not (zerop (+ (aref x ii)
                                       (aref x ii+1))))
                        (zerop (+ (aref y ii)
                                  (aref y ii+1))))

               (if (= (aref x ii) (aref x ii+1))
                   (setf (aref z ii+1) 1)
                   (setf (aref z ii+1) (aref x ii)
                         (aref z ii)   (aref x ii+1)))
               (return))))

         (dotimes (i nx/2)
           (let* ((ii (* 2 i))
                  (ii+1 (1+ ii)))
             (when (and (zerop (+ (aref x ii)
                                  (aref x ii+1)))
                        (not (zerop (+ (aref y ii)
                                       (aref y ii+1)))))

               (if (= (aref y ii) (aref y ii+1))
                   (setf (aref z ii+1) 1)
                   (setf (aref z ii+1) (aref y ii)
                         (aref z ii)   (aref y ii+1)))
               (return))))
         (map-into t0 #'logxor x z)
         (map-into t1 #'logxor y z)
         (values t0 t1))))))

(defun random-bit-string (n)
  "Choose a random bit string of length N."
  (let ((v (make-array n :element-type 'bit :initial-element 0)))
    (dotimes (i n v)
      (setf (aref v i) (random 2)))))

(defun test-trans (&optional (trials 1000))
  (loop :for n :from 1 :to 100 :do
    (loop :repeat trials
          :for x := (random-bit-string (* 2 n))
          :for y := (random-bit-string (* 2 n))
          :do (unless (or (every #'zerop x)
                          (every #'zerop y))
                (multiple-value-bind (T0 T1) (find-transvection x y)
                  (assert (every #'= y (transvections x T1 T0)))))))
  t)

(defun row (m r)
  (let* ((cols (array-dimension m 1))
         (row (make-array cols :element-type 'bit :initial-element 0)))
    (dotimes (c cols row)
      (setf (aref row c) (aref m r c)))))

(defun (setf row) (new-row m r)
  (let ((cols (array-dimension m 1)))
    (dotimes (c cols new-row)
      (setf (aref m r c) (aref new-row c)))))

(defun symplectic (i n)
  (check-type i unsigned-byte)
  (check-type n unsigned-byte)
  (assert (<= 0 i (1- (count-symplectic n))))
  (let (nn s k f1 e1 t0 t1 b ep h0)
    (setf nn (* 2 n))

    ;; Step 1
    (setf s (1- (ash 1 nn)))

    (setf k (1+ (mod i s)))

    (setf i (floor i s))

    ;; Step 2
    (setf f1 (integer-bits k nn))

    ;; Step 3
    (setf e1 (make-array nn :element-type 'bit :initial-element 0))
    (setf (aref e1 0) 1)

    (multiple-value-setq (t0 t1) (find-transvection e1 f1))

    ;; Step 4
    (let ((nb (mod i (ash 1 (1- nn)))))
      (setf b (integer-bits nb (1- nn))))

    ;; Step 5
    (setf ep (copy-seq e1))
    (loop :for j :from 2 :below nn :do
      (setf (aref ep j) (aref b (1- j))))

    (setf h0 (transvections ep t0 t1))

    ;; Step 6
    (when (= 1 (aref b 0))
      (fill f1 0))

    ;; Step 7
    (let* ((id2 (make-array '(2 2) :element-type 'bit
                                   :initial-contents '((1 0) (0 1))))
           (g (if (= n 1)
                  id2
                  (direct-sum id2 (symplectic (ash i (- (1- nn))) (1- n))))))
      (dotimes (j nn g)
        (setf (row g j) (transvections (row g j) t0 t1 h0 f1))))))

(defun count-cosets (n)
  "Count the cosets of the Sp(2n)/Sp(2n-2)."
  (* (expt 2 (1- (* 2 n)))
     (1- (expt 2 (* 2 n)))))

(defun count-symplectic (n)
  "Compute the size of Sp(2n)."
  (loop :with p := 1
        :for i :from 1 :to n :do (setf p (* p (count-cosets i)))
        :finally (return p)))

(defun pauli-from-row (sp j &optional (phase 0))
  "Construct the representative Pauli element from row J of the symplectic matrix SP. (Give the resulting Pauli the phase PHASE.)"
  (loop :for i :below (floor (array-dimension sp 1) 2)
        :for cx := (aref sp j (* 2 i))
        :for cz := (aref sp j (1+ (* 2 i)))
        :collect (logior cx (ash cz 1)) :into b
        :finally (return
                   (let ((c (make-components (list-length b))))
                     (setf (aref c 0) phase)
                     (loop :for i :from 1
                           :while b
                           :do (setf (aref c i) (pop b)))
                     (%make-pauli :components c)))))

(declaim (inline boolean-bit)
         (ftype (function (t) bit) boolean-bit))
(defun boolean-bit (boolean)
  (if boolean 1 0))

(defun pauli-to-row (p)
  "Convert a Pauli P into a symplectic row. Also return the phase represented as a bit."
  (let* ((n (num-qubits p))
         (row (make-array (* 2 n) :element-type 'bit :initial-element 0))
         (phase-bit  (ecase (phase-factor p)
                       (0 0)
                       (2 1))))
    (loop :for i :below n
          :for term := (aref (pauli-components p) (1+ i))
          :for cx_i := (* 2 i)
          :for cx := (boolean-bit (logbitp 0 term))
          :for cz_i := (1+ (* 2 i))
          :for cz := (boolean-bit (logbitp 1 term))
          :do (setf (aref row cx_i) cx
                    (aref row cz_i) cz)
          :finally (return (values row phase-bit)))))
