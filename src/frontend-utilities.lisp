;;;; frontend-utilities.lisp
;;;;
;;;; Initial author: Eric Peterson
;;;; Revised by: Erik Davis

(in-package #:cl-quil/frontend)

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

(defmacro dohash (((key val) hash &optional ret) &body body)
  `(loop :for ,key :being :the :hash-keys :of ,hash
           :using (hash-value ,val)
         :do ,@body
         ,@(when ret `(:finally (return ,ret)))))

(defmacro define-global-counter (counter-name incf-name)
  `(progn
     (declaim (type fixnum ,counter-name))
     (global-vars:define-global-var ,counter-name 0)
     (declaim (inline ,incf-name))
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

(defun list= (xs ys &key (key #'identity) (test #'equal))
  "Checks whether lists XS and YS are equal, element-by-element."
  (if (or (endp xs) (endp ys))
      (and (endp xs) (endp ys))         ; if one is empty, both should be empty
      (and (funcall test
                    (funcall key (car xs))
                    (funcall key (car ys)))
           (list= (cdr xs) (cdr ys) :key key :test test))))

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

(macrolet ((define-self-documenting-constant (symbol value)
             `(defconstant ,symbol ,value ,(string-downcase symbol))))
  (define-self-documenting-constant pi    (coerce cl:pi 'double-float))
  (define-self-documenting-constant -pi   (- pi))
  (define-self-documenting-constant pi/2  (/ pi 2))
  (define-self-documenting-constant -pi/2 (/ pi -2))
  (define-self-documenting-constant 2pi   (* 2 pi))
  (define-self-documenting-constant 4pi   (* 4 pi)))

;;; some of the analysis code uses this, otherwise this might belong in cl-quil proper
(defconstant +double-comparison-threshold-loose+  1d-5)
(defconstant +double-comparison-threshold-strict+ 5d-11)
(defun double~ (x y)
  "Loose equality of complex double floats, using the absolute threshold stored in +DOUBLE-COMPARISON-THRESHOLD-LOOSE+.  Use this comparison operator when testing for output correctness."
  (let ((diff (abs (- x y))))
    (< diff +double-comparison-threshold-loose+)))
(defun double= (x y)
  "Stringent equality of complex double floats, using the absolute threshold stored in +DOUBLE-COMPARISON-THRESHOLD-STRICT+.  Use this comparison operator when testing for substitution viability."
  (let ((diff (abs (- x y))))
    (< diff +double-comparison-threshold-strict+)))

(defun double>= (&rest args)
  (loop :for (x y) :on args
        :while y
        :always (>= (+ x +double-comparison-threshold-strict+)
                    (- y +double-comparison-threshold-strict+))))

(defmacro format-noise (str &rest args)
  "FORMAT-NOISE checks to see whether *COMPILER-NOISE* has been set and, if so, formats it according to the passed format string and arguments."
  `(progn
     (when *compiler-noise*
       (format *compiler-noise* ,str ,@args)
       (fresh-line *compiler-noise*))
     (values)))

;;; Cribbed from QVM-TESTS
(defmacro with-output-to-quil (&body body)
  "Collect all data sent to *STANDARD-OUTPUT* and return it parsed as as a Quil program."
  `(let ((cl-quil:*allow-unresolved-applications* t))
     (cl-quil:parse-quil
      (with-output-to-string (*standard-output*)
        ,@(loop :for form :in body
                :if (stringp form)
                  :collect `(write-line ,form)
                :else
                  :collect form)))))
