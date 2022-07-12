;;;; frontend-options.lisp
;;;;
;;;; Author: Robert Smith, Erik Davis

(in-package #:cl-quil/frontend)

;;; The variables below allow for general configuration of the
;;; compiler frontend.

(defvar *allow-unresolved-applications* nil
  "Allow unresolved applications to exist in the finally parsed code.")

(defvar *resolve-include-pathname* 'identity
  "The function used to handle/resolve pathnames passed to INCLUDE directives in Quil. Specifically, it should be a function designator that takes one argument (a pathname designator) and returns the absolute pathname to include, or signals an error.")

;;; See FORMAT-NOISE for making compiler noise.
(defvar *compiler-noise* nil
  "The stream on which to emit compiler debug output or NIL to suppress compiler noise.")

;;; These are more internal options for debugging (and forward
;;; declaration).

;;; See **REASONABLE-RATIONALS** for what rational numbers get
;;; printed.
(defvar *print-fractional-radians* t
  "When true, FORMAT-COMPLEX pretty-prints some common fractions of pi in a more human-readable form.

N.B., The fractions of pi will be printed up to a certain precision!")

(defvar *print-polar-form* nil
  "When true, FORMAT-COMPLEX prints out complex numbers in polar form with syntax AMPLITUDE∠PHASE.")
