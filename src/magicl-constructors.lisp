;;;; magicl-constructors.lisp
;;;;
;;;; Author: Cole Scott
;;;;
;;;; Collaborators: Robert Smith

(in-package #:cl-quil/frontend)

;;; This file aims to provide shims for magicl constructors with
;;; '(COMPLEX DOUBLE-FLOAT) as the default tensor type.

(alexandria:define-constant +default-magicl-type+ '(complex double-float) :test #'equal)

(defun empty (shape &key (type +default-magicl-type+) layout)
  "CL-QUIL version of MAGICL:EMPTY"
  (magicl:empty shape :type type :layout layout))

(defun const (const shape &key (type +default-magicl-type+) layout)
  "CL-QUIL version of MAGICL:CONST"
  (magicl:const const shape :type type :layout layout))

(defun rand (shape &key (type +default-magicl-type+) layout distribution)
  "CL-QUIL version of MAGICL:RAND"
  (magicl:rand shape :type type :layout layout :distribution distribution))

(defun eye (shape &key value (type +default-magicl-type+) layout)
  "CL-QUIL version of MAGICL:EYE"
  (magicl:eye shape :value value :type type :layout layout))

(defun arange (range &key (type +default-magicl-type+))
  "CL-QUIL version of MAGICL:ARANGE"
  (magicl:arange range :type type))

(defun from-array (array shape &key (type +default-magicl-type+) (layout :row-major))
  "CL-QUIL version of MAGICL:FROM-ARRAY

NOTE: When TYPE is not specified the elemnets in ARRAY are coerced to +DEFAULT-MAGICL-TYPE+"
  (magicl:from-array array shape :type type :layout layout))

(defun from-list (list shape &key (type +default-magicl-type+) layout (input-layout :row-major))
  "CL-QUIL version of MAGICL:FROM-LIST

NOTE: When TYPE is not specified the elemnets in LIST are coerced to +DEFAULT-MAGICL-TYPE+"
  (magicl:from-list list shape :type type :layout layout :input-layout input-layout))

(defun from-diag (list &key (order 2) (type +default-magicl-type+) layout)
  "CL-QUIL version of MAGICL:FROM-DIAG

NOTE: When TYPE is not specified the elemnets in LIST are coerced to +DEFAULT-MAGICL-TYPE+"
  (magicl:from-diag list :order order :type type :layout layout))

(defun zeros (shape &key (type +default-magicl-type+) layout)
  "CL-QUIL version of MAGICL:ZEROS"
  (magicl:zeros shape :type type :layout layout))

(defun ones (shape &key (type +default-magicl-type+) layout)
  "CL-QUIL version of MAGICL:ONES"
  (magicl:ones shape :type type :layout layout))

(defun random-unitary (shape &key (type +default-magicl-type+))
  "CL-QUIL version of MAGICL:RANDOM-UNITARY"
  (magicl:random-unitary shape :type type))

(defun matrix-equality (x y)
  (magicl:= x y +double-comparison-threshold-loose+))

(defun pauli-term->matrix (term arguments parameters parameter-names)
  (let* ((prefactor
           (typecase (pauli-term-prefactor term)
             (number
              (pauli-term-prefactor term))
             (delayed-expression
              (apply
               (compile nil `(lambda ,parameter-names
                               (declare (ignorable ,@parameter-names))
                               ,(delayed-expression-expression (pauli-term-prefactor term))))
               parameters))
             ((or symbol cons)
              (apply
               (compile nil `(lambda ,parameter-names
                               (declare (ignorable ,@parameter-names))
                               ,(pauli-term-prefactor term)))
               parameters))))
         (arg-count (length arguments))
         (size (expt 2 arg-count))
         (m (zeros (list size size))))
    (dotimes (col size)
      (let ((row col)
            (entry prefactor))
        (loop :for letter :across (pauli-term-pauli-word term)
              :for arg :in (pauli-term-arguments term)
              :for arg-position := (- arg-count 1 (position arg arguments :test #'equalp))
              :for row-toggle := (ldb (byte 1 arg-position) col)
              :do (ecase letter
                    (#\X
                     (setf row (dpb (- 1 row-toggle) (byte 1 arg-position) row)))
                    (#\Y
                     (setf row (dpb (- 1 row-toggle) (byte 1 arg-position) row))
                     (setf entry (* entry (if (zerop row-toggle) #C(0 1) #C(0 -1)))))
                    (#\Z
                     (setf entry (* entry (if (zerop row-toggle) 1 -1))))
                    (#\I
                     nil)))
        (incf (magicl:tref m row col) entry)))
    m))
