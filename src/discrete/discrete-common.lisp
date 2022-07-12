;;;; src/discrete/discrete-chip.lisp
;;;;
;;;; Author: A.J. Nyquist

(in-package #:cl-quil/discrete)

(defvar *tolerance* cl-quil/frontend::+double-comparison-threshold-loose+
  "Holds the value of the TOLERANCE pragma.")

(deftype tolerance-type ()
  '(double-float (0d0) (1.0d0)))

(cl-quil/frontend::define-pragma "TOLERANCE" pragma-tolerance
  (:documentation "PRAGMA denoting the target precision for discrete
compilations that take an operator U and return an approximate U'. That is, for
any qubit v, the upper limit of the magnitude of (U - U')v. Depending on the
simplification rules available this target may be exceeded.

  - PRAGMA TOLERANCE must only occur once and before non-pragma instructions
  - EPSILON must be greater than 0 and less than 1

Expected syntax: PRAGMA TOLERANCE \"EPSILON\"")
  (:freeform-string epsilon)
  (:slots (tolerance tolerance-type))
  (:initialization
   ;; XXX: It would be nice to not have this be a global variable
   (setf *tolerance* (parse-float:parse-float epsilon :type 'double-float))
   (setf tolerance *tolerance*))
  (:display-string
   (format nil "~E" tolerance)))
