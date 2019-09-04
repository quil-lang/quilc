;;;; utilities.lisp
;;;;
;;;; Initial author: Eric Peterson

(in-package #:cl-quil)

(defun required-slot (slot-name)
  (check-type slot-name symbol)
  (error "The slot named ~S is required." slot-name))

(defgeneric copy-instance (instance)
  (:documentation
   "Create a shallow copy of the object INSTANCE.
WARNING: The default will work for instances of \"idiomatic\" classes that aren't doing too many crazy things.")
  (:method ((instance t))
    (let* ((class (class-of instance))
           (copy (allocate-instance class)))
      (dolist (slot (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots class)))
        (when (slot-boundp instance slot)
          (setf (slot-value copy slot)
                (slot-value instance slot))))
      copy)))

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

(defmacro dohash (((key val) hash &optional ret) &body body)
  `(loop :for ,key :being :the :hash-keys :of ,hash
           :using (hash-value ,val)
         :do ,@body
         ,@(when ret `(:finally (return ,ret)))))

(defun findhash (item hash &rest args &key (key nil key-p) (test nil test-p))
  (declare (ignore args))
  (dohash ((hash-key hash-val) hash)
    (let* ((val  (if key-p (funcall key hash-val) hash-val))
           (bool (if test-p
                     (funcall test item val)
                     (eql val item))))
      (when bool
        (return (values hash-key hash-val))))))

(defmacro define-global-counter (counter-name incf-name)
  `(progn
     (declaim (type fixnum ,counter-name))
     (global-vars:define-global-var ,counter-name 0)
     (defun ,incf-name ()
       #+sbcl
       (sb-ext:atomic-incf ,counter-name)
       #+lispworks
       (system:atomic-incf ,counter-name)
       #+ecl
       (mp:atomic-incf ,counter-name)
       #+ccl
       (ccl::atomic-incf ,counter-name)
       #-(or ccl ecl sbcl lispworks)
       (incf ,counter-name))))

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

(defun partition-sequence-into-segments (predicate sequence)
  "Partition a sequence SEQUENCE into segments S1, S2, ..., Sn such that

    1. SEQUENCE == (concatenate (type-of sequence) S1 S2 ... Sn),

    2. Each segment has elements that all satisfy or dissatisfy PREDICATE, and

    3. Adjacent segments' elements have opposite satisfaction.

Return two values:

    1. The list (S1 S2 ... Sn).

    2. Whether the elements of S1 satisfy PREDICATE."
  (let ((segments nil)
        (num-segments 0)
        (current-segment nil)
        (current-satisfaction ':unknown))
    (labels ((finish-segment ()
               (unless (null current-segment)
                 (push (nreverse current-segment) segments)
                 (incf num-segments)
                 (setf current-segment nil)))
             (process-item (item)
               (cond
                 ((eq current-satisfaction ':unknown)
                  (setf current-satisfaction (and (funcall predicate item) t))
                  (push item current-segment))
                 (t
                  (let ((satisfied (funcall predicate item)))
                    (when (a:xor satisfied current-satisfaction)
                      ;; We have differing satisfactions.
                      (finish-segment)
                      (setf current-satisfaction satisfied))
                    (push item current-segment))))))
      (declare (dynamic-extent #'finish-segment #'process-item))
      ;; Collect the elements in the segments.
      (map nil #'process-item sequence)
      ;; Check if we need to unload the last segment, which happens if
      ;; we were provided an empty sequence.
      (cond
        ((eq current-satisfaction ':unknown)
         (values nil nil))
        (t
         (finish-segment)
         (values (nreverse segments)
                 ;; I'm not even gonna comment this.
                 (a:xor current-satisfaction (evenp num-segments))))))))

(defun ilog2 (x)
  "Compute integer logarithm of X to the base 2."
  (1- (integer-length x)))

(defun power-of-two-p (n)
  "Given an INTEGER N, return true if N is a power of 2."
  (and (plusp n) (= 1 (logcount n))))

(defun positive-power-of-two-p (n)
  "Given an INTEGER N, return true if N is a power of 2, greater than 1."
  (and (> n 1) (power-of-two-p n)))

(defun perfect-square-p (n)
  "Given a non-negative INTEGER N, return true if N is a perfect square."
  (= n (expt (isqrt n) 2)))

;;; Rotate-byte function courtesy of cl-utilities, from
;;; https://github.com/Publitechs/cl-utilities/blob/master/rotate-byte.lisp
#+sbcl (declaim (inline rotate-byte))
(defun rotate-byte (count bytespec integer)
  "Rotates a field of bits within INTEGER; specifically, returns an
integer that contains the bits of INTEGER rotated COUNT times
leftwards within the byte specified by BYTESPEC, and elsewhere
contains the bits of INTEGER. See http://www.cliki.net/ROTATE-BYTE"
  #-sbcl
  (let ((size (byte-size bytespec)))
    (when (= size 0)
      (return-from rotate-byte integer))
    (let ((count (mod count size)))
      (labels ((rotate-byte-from-0 (count size integer)
                 (let ((bytespec (byte size 0)))
                   (if (> count 0)
                       (logior (ldb bytespec (ash integer count))
                               (ldb bytespec (ash integer (- count size))))
                       (logior (ldb bytespec (ash integer count))
                               (ldb bytespec (ash integer (+ count size))))))))
        (dpb (rotate-byte-from-0 count size (ldb bytespec integer))
             bytespec
             integer))))
  ;; On SBCL, we use the SB-ROTATE-BYTE extension.
  #+sbcl (sb-rotate-byte:rotate-byte count bytespec integer))

(a:define-constant double-float-positive-infinity
    #+ccl ccl::double-float-positive-infinity
    #+ecl ext:double-float-positive-infinity
    #+sbcl sb-ext:double-float-positive-infinity
    #+allegro excl:*infinity-double*
    #-(or ccl ecl sbcl allegro) (error "double-float-positive-infinity not available."))

(a:define-constant pi #.(coerce cl:pi 'double-float))

(defun print-hash (hash &optional (stream *standard-output*))
  (fresh-line stream)
  (dohash ((key val) hash)
    (format stream "~a -> ~a~%" key val)))

;;; Cribbed from QVM-TESTS
(defmacro with-output-to-quil (&body body)
  `(let ((quil:*allow-unresolved-applications* t))
     (quil:parse-quil
      (with-output-to-string (*standard-output*)
        ,@(loop :for form :in body
                :if (stringp form)
                  :collect `(write-line ,form)
                :else
                  :collect form)))))
