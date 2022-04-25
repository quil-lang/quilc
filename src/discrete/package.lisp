;;;; src/discrete/package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:cl-quil.discrete
  (:use #:cl)
  (:local-nicknames (:q :cl-quil))
  (:export
   #:+discrete-gate-names+              ; CONSTANT
   #:build-discrete-qubit               ; FUNCTION
   #:install-discrete-link-onto-chip    ; FUNCTION
   #:build-discrete-linear-chip         ; FUNCTION
   ))
