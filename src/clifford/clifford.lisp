;;;; src/clifford/clifford.lisp
;;;;
;;;; Author: Nik Tezak

(in-package #:cl-quil/clifford)

;;; This file contains the Pauli basis representation of Clifford
;;; operators.

(defun count-clifford (n)
  "The size of C_N."
  (* (expt 2 (+ (* n n) n n))
     (loop :with p := 1
           :for k :from 1 :to n
           :do (setf p (* p (1- (expt 4 k))))
           :finally (return p))))

(defun pauli-to-index (p)
  "Convert a Pauli basis element to its canonical index."
  (assert (zerop (phase-factor p))
          (p)
          "The Pauli ~A must have a phase of 1 (phase-factor 0)."
          p)
  (let ((X?       nil)
        (position nil))
    (loop :for i :from 0
          :for sigma :in (base4-list p) :do
            (case sigma
              (1
               (when position
                 (error "~A is not a valid Pauli basis element; it has more than ~
                         one X or Z." p))
               (setf position i
                     X?       t))
              (2
               (when position
                 (error "~A is not a valid Pauli basis element; it has more than ~
                         one X or Z." p))
               (setf position i
                     X?       nil))
              (3 (error "~A is not a valid Pauli basis element; it has a Y." p))))
    (cond
      ((null position) (error "The identity is not a Pauli basis element."))
      (X? (* 2 position))
      (t (1+ (* 2 position))))))

(defun pauli-from-index (n i)
  "Compute the Pauli from P_N indexed by I."
  (if (evenp i)
      (embed +X+ n (list (/ i 2)))
      (embed +Z+ n (list (/ (1- i) 2)))))

(defun map-pauli-basis (n f)
  "Iterate of the single qubit X and Z pauli operators represented on
N qubits and call F on their index and each operator.

For qubit i, X_i will have index 2i and Z_i will have index 2i+1, for 0 <= i < 2n."
  (dotimes (idx n)
    (funcall f (* 2 idx)      (embed +X+ n (list idx)))
    (funcall f (1+ (* 2 idx)) (embed +Z+ n (list idx)))))

(defun map-all-paulis (n f)
  "Iterate over the single qubit X, Y, and Z pauli operators represented on
N qubits and call F on each operator and the bitwise representation."
  (dotimes (idx n)
    (funcall f (ash #.(sym-to-base4 'X) (* 2 idx)) (embed +X+ n (list idx)))
    (funcall f (ash #.(sym-to-base4 'Z) (* 2 idx)) (embed +Z+ n (list idx)))
    (funcall f (ash #.(sym-to-base4 'Y) (* 2 idx)) (embed +Y+ n (list idx)))))

(defun enumerate-pauli-basis (n)
  "Enumerate the single qubit X and Z pauli operators represented on N
qubits."
  (let ((list '()))
    (flet ((collect-element (j p)
             (declare (ignore j))
             (push p list)))
      (map-pauli-basis n #'collect-element)
      (nreverse list))))

(declaim (ftype (function (t) simple-vector) basis-map))
(defstruct (clifford (:include qubit-algebra))
  "An element of the Clifford group on NUM-QUBITS qubits."
  (num-qubits 0 :type cl-quil::unsigned-fixnum)
  (basis-map nil :type simple-vector))

(defmethod num-qubits ((c clifford))
  (clifford-num-qubits c))

(declaim (inline basis-map))
(defun basis-map (c)
  (clifford-basis-map c))

(defun apply-clifford (c p)
  "Apply a clifford C to a pauli P."
  (multiple-value-bind (b ph) (pauli-basis-decompose p)
    (reduce #'group-mul b
            :initial-value (pauli-identity (num-qubits p) ph)
            :key (lambda (e) (aref (basis-map c) (pauli-to-index e))))))

(defun clifford-identity (n)
  "Create the identity clifford map on N qubits."
  (let ((bm (make-array (* 2 n))))
    (map-pauli-basis n (lambda (j p) (setf (aref bm j) p)))
    (make-clifford
     :num-qubits n
     :basis-map bm)))

(defun clifford-identity-p (c)
  (map-pauli-basis (num-qubits c)
                   (lambda (i p)
                     (unless (pauli= p (aref (basis-map c) i))
                       (return-from clifford-identity-p nil))))
  t)

(defmethod group-mul ((c1 clifford) (c2 clifford))
  (let ((c1c2 (clifford-identity (num-qubits c1))))
    (map-into (basis-map c1c2)
              (lambda (c2p) (apply-clifford c1 c2p))
              (basis-map c2))
    c1c2))

(defmethod group-inv ((g clifford))
  (labels ((find-inverse (x next-x)
             (if (clifford-identity-p next-x)
                 x
                 (find-inverse next-x (group-mul g next-x)))))
    (if (clifford-identity-p g)
        g
        (find-inverse g (group-mul g g)))))

(defun clifford-from-pauli (p)
  "Promote a Pauli P (acting via conjugation) to a Clifford."
  (let* ((n (num-qubits p))
         (result (make-clifford
                  :num-qubits n
                  :basis-map (make-array (* 2 n)))))
    (map-pauli-basis n (lambda (j b)
                         (setf (aref (basis-map result) j) (group-conj p b))))
    result))

(defmethod embed ((a clifford) n idxs)
  (let ((result (clifford-identity n)))
    (loop :for i :from 0
          :for ap :across (basis-map a)
          :for p := (pauli-from-index (num-qubits a) i)
          :for raised-i := (pauli-to-index (embed p n idxs))
          :do (setf (aref (basis-map result) raised-i) (embed ap n idxs))
          :finally (return result))))

(defmacro clifford-element (&body body)
  "Construct a clifford element from the mappings defined by BODY. The mappings should be of the form

    {Pauli basis element} -> {Valid image}

The maps may be specified by any string designator (symbols, strings, and characters).

For example, one can construct a Hadamard gate by doing

    (clifford-element
      X -> Z
      Z -> X)

The Clifford element produced will have the number of qubits equal to the maximal dimension of any of the specified Paulis.

NOTE: THERE IS NO CHECKING OF THE VALIDITY OF THE MAP. ANTICOMMUTATIVITY IS NOT VERIFIED AND REAL EIGENVALUES OF THE IMAGE ARE NOT VERIFIED."
  (let ((clifford (gensym "CLIFFORD-"))
        (table (gensym "TABLE-"))
        (maps nil)
        (num-qubits 0))
    (labels ((pauli-string-p (string)
               ;; Is STRING a valid Pauli string?
               (let ((n (length string)))
                 (and (or (= 1 (count #\X string))
                          (= 1 (count #\Z string)))
                      (= (1- n) (count #\I string)))))
             (dimension (string)
               ;; What is the dimension of the Pauli STRING?
               (if (char= #\- (char string 0))
                   (1- (length string))
                   (length string)))
             (embed-pauli-string (string)
               ;; Embed the Pauli string into a NUM-QUBITS space.
               (let* ((p (pauli-from-string string))
                      (p-num-qubits (num-qubits p)))
                 (with-output-to-string (s)
                   (print-pauli
                    (embed p num-qubits (loop :for i :from (1- p-num-qubits) :downto 0 :collect i))
                    s)))))
      ;; Parse out all of the map data, including the number of qubits
      ;; of the entire Clifford element.
      (loop :for (from arrow to) :on body :by #'cdddr
            :for from-name := (string from)
            :for to-name := (string to)
            :do (progn
                  (assert (string= "->" arrow))
                  (assert (pauli-string-p from-name)
                          ()
                          "The symbol ~S is not a Pauli basis element." from)
                  ;; Get the max dimension
                  (a:maxf num-qubits (dimension from-name) (dimension to-name))
                  ;; Record the map.
                  (push (cons from-name to-name)
                        maps)))

      ;; Embed into proper dimension.
      (mapc (lambda (m)
              (rplaca m (embed-pauli-string (car m)))
              (rplacd m (embed-pauli-string (cdr m))))
            maps)

      ;; Set the mappings.
      `(let* ((,clifford (clifford-identity ,num-qubits))
              (,table (basis-map ,clifford)))
         ,@(loop :for (from . to) :in maps
                 :collect
                 `(setf (aref ,table (pauli-to-index (pauli-from-string ,from)))
                        (pauli-from-string ,to)))
         ;; Return the table.
         ,clifford))))

(declaim (ftype (function (clifford clifford) boolean) clifford=))
(defun clifford= (a b)
  "Equality test for Cliffords A and B."
  (declare (optimize speed) (inline pauli=))
  (and
   (= (clifford-num-qubits a) (clifford-num-qubits b))
   (loop :for ai :across (basis-map a)
         :for bi :across (basis-map b)
         :always (pauli= ai bi))
   #+#:slightly-slower
   (every #'pauli= (basis-map a) (basis-map b))))

(defun clifford-hash (c)
  "Hash function for CLIFFORD objects."
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (declare (type clifford c))
  (declare (inline pauli-hash))
  (sxhash
   (loop :with h :of-type cl-quil::unsigned-fixnum := 0
         :for p :of-type pauli :across (basis-map c)
         :do (setf h (hash-mix h (pauli-hash p)))
         :finally (return h))))

(defun make-clifford-hash-table (&key pre-allocate synchronized)
  "Make a hash-table that has CLIFFORD objects as keys."
  (declare (ignorable synchronized))
  (check-type pre-allocate (or null (integer 1)))
  ;; LispWorks and ClozureCL hash tables support concurrent writers by
  ;; default. SBCL requires the extra :SYNCHRONIZED option.
  (if pre-allocate
      (make-hash-table :test 'clifford=
                       :hash-function 'clifford-hash
                       #+(or sbcl ecl) :synchronized #+(or sbcl ecl) synchronized
                       :size (count-clifford pre-allocate))
      (make-hash-table :test 'clifford=
                       #+(or sbcl ecl) :synchronized #+(or sbcl ecl) synchronized
                       :hash-function 'clifford-hash)))

(defmethod print-object ((c clifford) stream)
  (print-unreadable-object (c stream :type t)
    (let ((*print-circle* nil))
      (format stream "~Dq" (num-qubits c))
      (map-pauli-basis (num-qubits c)
                       (lambda (i input-pauli)
                         (let ((output-pauli (aref (basis-map c) i)))
                           (unless (pauli= input-pauli output-pauli)
                             (format stream "~%  ~A -> ~A"
                                     (print-pauli input-pauli nil)
                                     (print-pauli output-pauli nil)))))))))

(defun symplectic-clifford (sp &optional r s)
  "Return the Clifford corresponding to the symplectic matrix SP."
  (let* ((n (floor (array-dimension sp 0) 2))
         (bm (make-array (* 2 n)))
         (r (or r (make-array n :element-type 'bit :initial-element 0)))
         (s (or s (make-array n :element-type 'bit :initial-element 0))))
    ;; We could use MAP-PAULI-BASIS, but we don't actually need the
    ;; Pauli elements that get constructed.
    (dotimes (j (* 2 n))
      (let* ((ph (if (evenp j) r s))
             (ph_i (aref ph (floor j 2))))
        (setf (aref bm j) (pauli-from-row
                           sp
                           j
                           (ecase ph_i
                             (0 0)
                             (1 2))))))
    (make-clifford
     :num-qubits n
     :basis-map bm)))

(defun clifford-symplectic (c)
  "Return the symplectic representation of the Clifford C."
  (let* ((n (num-qubits c))
         (r (make-array n :element-type 'bit :initial-element 0))
         (s (make-array n :element-type 'bit :initial-element 0))
         (sp (make-array (list (* 2 n) (* 2 n)) :element-type 'bit :initial-element 0)))
    ;; We could use MAP-PAULI-BASIS, but we don't actually need the
    ;; Pauli elements that get constructed.
    (dotimes (j (* 2 n))
      (let* ((ph-vector (if (evenp j) r s))
             (idx (floor j 2)))
        (multiple-value-bind (sp-row phase-bit)
            (pauli-to-row (aref (basis-map c) j))
          (setf (aref ph-vector idx) phase-bit)
          (setf (row sp j) sp-row))))
    (values sp r s)))

(defun random-clifford (n)
  "Pick a random element from C_n."
  (let ((r (random-bit-string n))
        (s (random-bit-string n))
        (sp (symplectic (random (count-symplectic n)) n)))
    (symplectic-clifford sp r s)))
