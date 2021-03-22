;;;; options.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

;;; The variables below allow for general configuration of the
;;; compiler. More configuration of the compressor specifically can be
;;; found in compressor-configuration.lisp.

(defvar *allow-unresolved-applications* nil
  "Allow unresolved applications to exist in the finally parsed code.")

(defvar *recognize-swap-specially* t
  "Make SWAP-APPLICATION nodes specially for SWAP instructions.")

(defvar *resolve-include-pathname* 'identity
  "The function used to handle/resolve pathnames passed to INCLUDE directives in Quil. Specifically, it should be a function designator that takes one argument (a pathname designator) and returns the absolute pathname to include, or signals an error.")

;;; See FORMAT-NOISE for making compiler noise.
(defvar *compiler-noise* nil
  "The stream on which to emit compiler debug output or NIL to suppress compiler noise.")

(defvar *compress-carefully* nil
  "Flag that turns on/off a bunch of intermediate correctness checks during compression.  WARNING: this can be *very* costly, since it involves computing explicit matrix presentations.")

(defvar *enable-approximate-compilation* nil
  "When NIL, compression by replacing instructions sequences with approximate sequences is disabled.

NOTE: When T, this permits the approximate compilation templates to emit inexact results, but does not actually enable any alternative code paths.  When NIL, these results are still generated, but they are discarded.")


;;; These are more internal options for debugging (and forward
;;; declaration).

;;; See **REASONABLE-RATIONALS** for what rational numbers get
;;; printed.
(defvar *print-fractional-radians* t
  "When true, FORMAT-COMPLEX pretty-prints some common fractions of pi in a more human-readable form.

N.B., The fractions of pi will be printed up to a certain precision!")

(defvar *print-polar-form* nil
  "When true, FORMAT-COMPLEX prints out complex numbers in polar form with syntax AMPLITUDE∠PHASE.")
