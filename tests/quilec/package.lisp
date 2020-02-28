;;;; tests/package.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

;;; Allegro (and other Lisps) don't support the non-standard "package
;;; local nicknames".
#-(or sbcl ecl ccl)
(rename-package :alexandria :alexandria '(:a))

(fiasco:define-test-package #:cl-quil.quilec-tests
  (:nicknames #:qec-tests)
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))
  (:use #:common-lisp #:cl-quil.quilec)

  ;; suite.lisp
  (:export
   #:run-quilec-tests))
