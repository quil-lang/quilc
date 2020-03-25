;;;; magicl-constructors.lisp
;;;;
;;;; Author: Cole Scott
;;;;
;;;; Collaborators: Robert Smith

(in-package #:cl-quil)

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

(defun eye (shape &key (value #C(1d0 0d0)) (type +default-magicl-type+) layout)
  "CL-QUIL version of MAGICL:EYE"
  (magicl:eye shape :value value :type type :layout layout))

(defun arange (range &key (type +default-magicl-type+))
  "CL-QUIL version of MAGICL:ARANGE"
  (magicl:arange range :type type))

(defun from-array (array shape &key (type +default-magicl-type+) (layout :row-major))
  "CL-QUIL version of MAGICL:FROM-ARRAY"
  (magicl:from-array array shape :type type :layout layout))

(defun from-list (list shape &key (type +default-magicl-type+) layout (input-layout :row-major))
  "CL-QUIL version of MAGICL:FROM-LIST

NOTE: This _always_ coerces the input to +DEFAULT-MAGICL-TYPE+"
  (magicl:from-list list shape :type type :layout layout :input-layout input-layout))

(defun from-diag (list &key (order 2) (type +default-magicl-type+) layout)
  "CL-QUIL version of MAGICL:FROM-DIAG"
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
