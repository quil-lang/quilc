;;;; package.lisp
;;;;
;;;; Author: Chris Osborn

(fiasco:define-test-package #:boondoggle-tests
  (:local-nicknames (:a :alexandria))
  (:use #:boondoggle)
  
  ;; suite.lisp
  (:export #:run-boondoggle-tests)
  (:shadowing-import-from #:fiasco)
  )
