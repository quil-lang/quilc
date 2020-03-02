;;;; src/quilec/code.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:cl-quil.quilec)

;;; Data structure for representing and using stabilizer codes.

(deftype generators ()
  "A simple array of Pauli terms."
  `(simple-array pauli (*)))

(defun error-missing-initform (symbol)
  (error "You must specify ~S." symbol))

(defclass code ()
  ((number-of-physical-qubits
    :type non-negative-fixnum
    :initarg :n
    :initform (error-missing-initform :n)
    :reader number-of-physical-qubits
    :documentation "Number of physical qubits")
   (number-of-logical-qubits
    :type non-negative-fixnum
    :initarg :k
    :initform (error-missing-initform :k)
    :reader number-of-logical-qubits
    :documentation "Number of logical qubits")
   (distance
    :type non-negative-fixnum
    :initarg :d
    :reader distance
    :documentation "Number of Pauli operators required to create an undetectable error.")
   (generators
    :type generators
    :initarg :generators
    ;; :initform (error-missing-initform :generators)
    :reader generators
    :documentation "Array of generators of the stabilizer group associated with this code.")
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
    :documentation "Array of seed generators (i.e., logical X gates) for the code."))
  (:documentation "Stabilizer code."))

(deftype processed-code ()
  "A processed code is a CODE structure that is the result of applying the Cleve-Gottesman algorithm."
  `(satisfies seed-generators))

(defun make-code (generators)
  "Helper function to instantiate CODE classes."
  (flet ((get-generators (generators)
           "Pre-process the sequence GENERATORS, which is expected to contain valid inputs to PAULI-FROM-SYMBOLS and return the corresponding array of PAULI objects."
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
                 (pushnew generator effective-generators))))))

    (multiple-value-bind (number-of-physical-qubits generators)
        (get-generators generators)
      (make-instance 'code
                     :n number-of-physical-qubits
                     :k (- number-of-physical-qubits (length generators))
                     :generators generators))))

(defmethod print-object ((code code) stream)
  (print-unreadable-object (code stream :type t :identity t)
    (format stream "[[~D, ~D, ~A]] ~{~A~^ ~}"
            (number-of-physical-qubits code)
            (number-of-logical-qubits code)
            (if (slot-boundp code 'distance)
                (distance code)
                "?")
            (map 'list #'cl-quil.clifford::print-pauli (generators code)))))

(defun code-to-matrices (code)
  "Return the X- and Z-matrices corresponding to CODE."
  (declare (type code code)
           (values matrix matrix))
  (loop :with generators := (generators code)
        :with n := (number-of-physical-qubits code)
        :with d := (length generators)
        :with xg := (make-matrix n d)
        :with zg := (make-matrix n d)
        :for j :below d
        :for generator :across generators :do
          (loop :with base4-components := (subseq (cl-quil.clifford::pauli-components generator) 1)
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

(defun code-from-matrices (xg zg)
  "Return a new instance of CODE whose generators are dictated by the columns of XG and ZG."
  (declare (type matrix xg zg))

  (multiple-value-bind (n d) (matrix-shape xg)
    (assert (and (equalp (list n d) (multiple-value-list (matrix-shape zg)))))
    (make-instance 'code :n (matrix-rows xg)
                         :k (- n d)
                         :generators (generators-from-matrices xg zg))))
