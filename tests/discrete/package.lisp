;;;; tests/discrete/package.lisp
;;;;
;;;; Author: Robert Smith, A.J. Nyquist

(fiasco:define-test-package #:cl-quil.discrete-tests
  (:use #:cl
        #:cl-quil.discrete
        )
  (:local-nicknames
   (#:q #:cl-quil)
   )
  ;; suite.lisp
  (:export
   #:run-discrete-tests))

(defpackage #:cl-quil.discrete-coalton-tests
  (:documentation "Tests for the Coalton componenents of clifford-t approx.")
  (:use #:coalton
        #:coalton-prelude
        #:coalton-testing
        #:coalton-library/math
        #:cl-quil.discrete/numeric
        #:cl-quil.discrete/operators
        #:cl-quil.discrete/rz-approx
        )
  (:local-nicknames
   (#:list #:coalton-library/list)
   (#:numeric #:cl-quil.discrete/numeric)
   (#:operator #:cl-quil.discrete/operators)
   (#:rz #:cl-quil.discrete/rz-approx)
   ))
