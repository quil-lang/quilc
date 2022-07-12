;;;; modifiers.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; In certain cases, we know how to compile down gate modifiers in an
;;;; algebraic way.  This file contains those methods, except for
;;;; FORKED rotation expansion, which lives in ucr-explode.lisp .

(in-package #:cl-quil)

(define-compiler undagger-rotation
    ((rotation-gate :where (and (member (operator-description-root-name (application-operator rotation-gate))
                                        (list "RX" "RY" "RZ")
                                        :test #'string=)
                                (typep (application-operator rotation-gate) 'dagger-operator))))
  (adt:with-data (dagger-operator underlying-operator) (application-operator rotation-gate)
    (inst* underlying-operator
           (mapcar (lambda (p) (param-* -1d0 p)) (application-parameters rotation-gate))
           (application-arguments rotation-gate))))

(define-compiler uncontrol-rotation
    ((rotation-gate :where (and (member (operator-description-root-name (application-operator rotation-gate))
                                        (list "RX" "RY" "RZ")
                                        :test #'string=)
                                (typep (application-operator rotation-gate) 'controlled-operator))))
  (adt:with-data (controlled-operator underlying-operator) (application-operator rotation-gate)
    (inst* (forked-operator underlying-operator)
           (append (make-list (length (application-parameters rotation-gate))
                              :initial-element (constant 0d0))
                   (application-parameters rotation-gate))
           (application-arguments rotation-gate))))
