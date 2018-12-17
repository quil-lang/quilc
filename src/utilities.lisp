;;;; utilities.lisp
;;;;
;;;; Initial author: Eric Peterson

(in-package #:cl-quil)

(defun required-slot (slot-name)
  (check-type slot-name symbol)
  (error "The slot named ~S is required." slot-name))
