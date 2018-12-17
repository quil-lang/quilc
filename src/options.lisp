;;;; options.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

(defparameter *allow-unresolved-applications* nil
  "Allow unresolved applications to exist in the finally parsed code.")

(defparameter *recognize-swap-specially* t
  "Make SWAP-APPLICATION nodes specially for SWAP instructions.")

(defparameter *resolve-include-pathname* 'identity
  "The function used to handle/resolve pathnames passed to INCLUDE directives in Quil. Specifically, it should be a function designator that takes one argument (a pathname designator) and returns the absolute pathname to include, or signals an error.")

(defparameter *compiler-noise-stream* (make-broadcast-stream)
  "The stream used to emit compiler debug output to.  By default, this variable is bound to a stream that suppresses display.")
