;;;; simplification-grab-bag.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil.frontend)

;;;; This file contains a grab bag of INDIVIDUAL INSTRUCTION
;;;; SIMPLIFICATIONS. The reason for the "grab bag" nature is to avoid
;;;; having lots of passes over the same program, and instead have one
;;;; pass that may simplify a lot in one go.
;;;;
;;;; Current simplifications include:

(defparameter *grab-bag-simplifiers*
  '(simplify-instruction-modifiers
    simplify-arithmetic))

;;; Arithmetic Simplifier

;;; See simplify-arithmetic.lisp

;;; Dagger Simplifier

(defun simplify-daggers (od)
  "Take an operator description and simplify the DAGGERs on it."
  (labels ((rec (od c)
             ;; c = # of daggers counted so far
             (adt:match operator-description od
               ((named-operator _)
                (if (evenp c)
                    od
                    (dagger-operator od)))
               ((controlled-operator x)
                (controlled-operator (rec x c)))
               ((dagger-operator x)
                (rec x (1+ c)))
               ((forked-operator x)
                (forked-operator (rec x c))))))
    (rec od 0)))

(defgeneric simplify-instruction-modifiers (isn)
  (:method ((isn t))
    isn)

  (:method ((isn application))
    (let ((new-app (copy-instance isn)))
      (setf (application-operator new-app)
            (simplify-daggers (application-operator new-app)))
      new-app)))


;;; Instruction Rewriter

(defun grab-bag-simplify-instruction (isn)
  (dolist (simp *grab-bag-simplifiers* isn)
    (setf isn (funcall simp isn))))

;;; Transform

(defun simplify-individual-instructions (parsed-prog)
  (map-into (parsed-program-executable-code parsed-prog)
            #'grab-bag-simplify-instruction
            (parsed-program-executable-code parsed-prog))
  parsed-prog)

(define-transform simplify-individual-instructions (simplify-individual-instructions)
  "Simplify individual instructions with a variety of straightforward techniques.")
