;;;; utilities.lisp
;;;;
;;;; Initial author: Eric Peterson

(in-package #:cl-quil)

(defun required-slot (slot-name)
  (check-type slot-name symbol)
  (error "The slot named ~S is required." slot-name))


(defmacro postpend (obj place)
  `(if ,place
       (push ,obj (cdr (last ,place)))
       (setf ,place (list ,obj))))

(defun make-adjustable-vector ()
  (make-array 4 :element-type t
                :initial-element nil
                :adjustable t
                :fill-pointer 0))

(defun vnth (index vector)
  "Like NTH, but for VECTORs."
  (aref vector index))

(defun (setf vnth) (val index vector)
  (setf (aref vector index) val))

(defun vector-argmax (arr)
  "Finds the position of the largest (using #'<) item in a nonempty vector."
  (loop :with pos := 0
        :with current-max := (aref arr 0)
        :for j :from 0
        :for item :across arr
        :when (< current-max item)
          :do (setf current-max item
                    pos j)
        :finally (return pos)))

(defun findhash (item hash &rest args &key (key nil key-p) (test nil test-p))
  (declare (ignore args))
  (dohash ((hash-key hash-val) hash)
    (let* ((val  (if key-p (funcall key hash-val) hash-val))
           (bool (if test-p
                     (funcall test item val)
                     (eql val item))))
      (when bool
        (return (values hash-key hash-val))))))

(defun first-column-operator= (mat1 mat2)
  (multiple-value-bind (mat1 mat2) (matrix-rescale mat1 mat2)
    (setf mat1 (scale-out-matrix-phases mat1 mat2))
    (matrix-first-column-equality mat1 mat2)))

(defun operator= (mat1 mat2)
  (multiple-value-bind (mat1 mat2) (matrix-rescale mat1 mat2)
    (setf mat1 (scale-out-matrix-phases mat1 mat2))
    (matrix-equality mat1 mat2)))

(declaim (special *enable-state-prep-compression*)) ; forward declare
(defun matrix-equals-dwim (mat1 mat2)
  "Returns true if mat1 is equal to mat2. DWIM (Do What I Mean) means take into
account whether *ENABLE-STATE-PREP-COMPRESSION* is enabled, and use the
appropriate method of comparison."
  (if *enable-state-prep-compression*
      (first-column-operator= mat1 mat2)
      (operator= mat1 mat2)))

(defun reduce-append (lists)
  "Append all of the lists of LISTS together. Called 'concat' in some other languages. Equivalent to (REDUCE #'APPEND LISTS)."
  ;; We could LOAD-TIME-VALUE this CONS but then the function wouldn't
  ;; be re-entrant.
  (let* ((leash (cons nil nil))
         (last-cons leash))
    (declare (type cons leash last-cons))
    (loop :for list :of-type list :in lists
          :do (loop :for x :in list
                    :for new-cons :of-type cons := (cons x nil)
                    :do (rplacd last-cons new-cons)
                        (setf last-cons new-cons)))
    (cdr leash)))

(defun print-hash (hash &optional (stream *standard-output*))
  (fresh-line stream)
  (dohash ((key val) hash)
    (format stream "~A -> ~A~%" key val)))

(defun program-fidelity (program chip)
  "Compute the expected fidelity of PROGRAM if executed on CHIP."
  (check-type program parsed-program)
  (check-type chip chip-specification)
  (calculate-instructions-fidelity
   (coerce (parsed-program-executable-code program) 'list)
   chip))
