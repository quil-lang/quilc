;;;; src/quilec/stabilizer-group.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:cl-quil/quilec)

;;; Data structures for representing and using stabilizer groups.

(deftype generators ()
  "A simple array of Pauli terms."
  `(simple-array pauli (*)))

;;; The GROUP class acts as a precursor to the STABILIZER-GROUP class. Its
;;; main goal is to represent (finitely generated) subgroups of the group of
;;; tensor products of n Pauli matrices.
(defclass group ()
  ((number-of-physical-qubits
    :type non-negative-fixnum
    :initarg :n
    :reader number-of-physical-qubits
    :documentation "Number of physical qubits")
   (generators
    :type generators
    :initarg :generators
    :reader generators
    :documentation "Array of generators of the stabilizer group associated with this code."))
  (:documentation "Subgroup of a group of N tensor products of Pauli operators."))

(defun get-generators (generators)
  "Pre-process the sequence GENERATORS, which is expected to contain valid inputs to PAULI-FROM-SYMBOLS and return the number of physical qubits as well as the corresponding array of PAULI objects."
  (declare (type sequence generators)
           (values non-negative-fixnum generators))
  (let (number-of-physical-qubits
        effective-generators)
    (dolist (term
             (coerce generators 'list)
             (values number-of-physical-qubits
                     (coerce (nreverse effective-generators) 'generators)))
      (let ((n (length term))
            (generator (pauli-from-symbols term)))
        (if number-of-physical-qubits
            (unless (= number-of-physical-qubits n)
              (error "The number of qubits in generator ~A is not ~D as expected."
                     generator number-of-physical-qubits))
            (setf number-of-physical-qubits n))
        (pushnew generator effective-generators :test #'equalp)))))

(defun make-group (generators)
  "Helper function to instantiate GROUP classes."
  (multiple-value-bind (number-of-physical-qubits generators)
      (get-generators generators)
    (make-instance 'group :n number-of-physical-qubits :generators generators)))

(defmethod print-object ((group group) stream)
  (print-unreadable-object (group stream :type t :identity t)
    (format stream ":n ~D :generators ~{~A~^ ~}"
            (number-of-physical-qubits group)
            (map 'list #'print-pauli (generators group)))))

(defun group-to-matrices (group)
  "Return the X- and Z-matrices corresponding to GROUP."
  (declare (type group group)
           (values matrix matrix))
  (loop :with generators := (generators group)
        :with n := (number-of-physical-qubits group)
        :with d := (length generators)
        :with xg := (make-matrix n d)
        :with zg := (make-matrix n d)
        :for j :below d
        :for generator :across generators :do
          (loop :with base4-components := (subseq (pauli-components generator) 1)
                :for i :below n
                :for pauli-term :across base4-components :do
                  (case pauli-term
                    (0 (setf (matrix-ref xg i j) 0 (matrix-ref zg i j) 0))
                    (1 (setf (matrix-ref xg i j) 1 (matrix-ref zg i j) 0))
                    (2 (setf (matrix-ref xg i j) 0 (matrix-ref zg i j) 1))
                    (3 (setf (matrix-ref xg i j) 1 (matrix-ref zg i j) 1))))
        :finally (return (values xg zg))))

(defun generators-from-matrices (xg zg)
  "Return the generators of the stabilizer code corresponding to the columns of the X- and Z-matrices given by XG and ZG."
  (declare (type matrix xg zg)
           (values generators))
  ;; TODO ensure XG and ZG have the same dimensions.
  (loop :with n := (matrix-rows xg)
        :with d := (matrix-cols xg)
        :with generators := (make-array d :element-type '(or null pauli)
                                          :initial-element nil :fill-pointer 0)
        :for j :below d :do
          (loop :with base-4-list := nil
                :for i :from (1- n) :downto 0
                :for px := (matrix-ref xg i j)
                :for pz := (matrix-ref zg i j) :do
                  (cond
                    ((and (zerop px) (zerop pz))
                     (push 0 base-4-list))
                    ((and (plusp px) (zerop pz))
                     (push 1 base-4-list))
                    ((and (zerop px) (plusp pz))
                     (push 2 base-4-list))
                    ((and (plusp px) (plusp pz))
                     (push 3 base-4-list)))
                :finally (vector-push (make-pauli base-4-list) generators))
        :finally (return (coerce generators 'generators))))

(defun group-from-matrices (xg zg)
  "Return a new instance of GROUP whose generators are dictated by the columns of XG and ZG."
  (declare (type matrix xg zg))

  (multiple-value-bind (n d) (matrix-shape xg)
    (assert (and (equalp (list n d) (multiple-value-list (matrix-shape zg)))))
    (make-instance 'group :n (matrix-rows xg) :generators (generators-from-matrices xg zg))))

(defclass stabilizer-group (group)
  ((number-of-logical-qubits
    :type non-negative-fixnum
    :initarg :k
    :reader number-of-logical-qubits
    :documentation "Number of logical qubits")
   (primary-generators ;; TODO: replace this by an array of indices into the GENERATORS slot.
    :type generators
    :initarg :primary-generators
    :reader primary-generators
    :documentation "Array of primary generators.")
   (secondary-generators
    :type (simple-array non-negative-fixnum (*))
    :initarg :secondary-generators
    :reader secondary-generators
    :documentation "Array of secondary generators.")
   (seed-generators
    :type generators
    :initarg :seed-generators
    :reader seed-generators
    :documentation "Array of seed generators (i.e., logical X gates) for the code.")
   (codeword-circuit
    :type cl-quil:parsed-program
    :reader codeword-circuit
    :documentation "Circuit for encoding bitstrings using the code."))
  (:documentation "Stabilizer group. The naming of primary, secondary, and seed generators follows D. Gottesman's nomenclature."))

(defun make-stabilizer-group (generators)
  "Helper function to instantiate STABILIZER-GROUP classes."
  (multiple-value-bind (number-of-physical-qubits generators)
      (get-generators generators)
    (cleve-gottesman (make-instance 'stabilizer-group
                                    :n number-of-physical-qubits
                                    :generators generators))))

(defmethod print-object ((stabilizer-group stabilizer-group) stream)
  (print-unreadable-object (stabilizer-group stream :type t :identity t)
    (format stream ":n ~D :k ~D :generators ~{~A~^ ~}"
            (number-of-physical-qubits stabilizer-group)
            (number-of-logical-qubits stabilizer-group)
            (map 'list #'print-pauli (generators stabilizer-group)))))

(defun encode (stabilizer-group bits)
  "Encode BITS using the encoder circuit of STABILIZER-GROUP. Return the PARSED-PROGRAM object that produces the desired result."
  (declare (type stabilizer-group stabilizer-group)
           (type non-negative-fixnum bits)
           (values parsed-program))
  (let ((k (number-of-logical-qubits stabilizer-group)))
    (unless (<= (integer-length bits) k)
      (error "Unable to encode the bitstring ~B using a code with ~D data qubits." bits k)))

  (flet ((make-bit-toggler-program (bits)
           "Returns a program that takes the |0...0> state to the |BITS> state by flipping the appropriate qubits with X gates."
           (declare (type non-negative-fixnum bits))
           (with-output-to-quil
             (format t "RESET~%")
             (when (plusp bits)
               (loop :for i :below (integer-length bits)
                     :when (logbitp i bits) :do
                       (format t "X ~D~%" i))))))

    (let ((toggler-executable-code (parsed-program-executable-code (make-bit-toggler-program bits)))
          (codeword-circuit (make-codeword-circuit stabilizer-group)))
      (setf (parsed-program-executable-code codeword-circuit)
            (concatenate '(vector instruction)
                         toggler-executable-code
                         (parsed-program-executable-code codeword-circuit)))
      codeword-circuit)))
