;;;; tests/quilec/package.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(fiasco:define-test-package #:cl-quil.quilec-tests
  (:nicknames #:qec-tests)
  (:local-nicknames (#:a    #:alexandria)
                    (#:quil #:cl-quil)
                    (#:qec  #:cl-quil.quilec))
  (:use #:common-lisp #:cl-quil.quilec)

  ;; suite.lisp
  (:export
   #:run-quilec-tests))
