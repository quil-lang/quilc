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

(defvar *compiler-noise-stream* (make-broadcast-stream)
  "The stream used to emit compiler debug output to.  By default, this variable is bound to a stream that suppresses display.")

(defvar *compress-carefully* nil
  "Flag that turns on/off a bunch of intermediate correctness checks during compression.  WARNING: this can be *very* costly, since it involves computing explicit matrix presentations.")

(defvar *enable-approximate-compilation* nil
  "When NIL, compression by replacing instructions sequences with approximate sequences is disabled.

NOTE: When T, this permits the approximate compilation templates to emit inexact results, but does not actually enable any alternative code paths.  When NIL, these results are still generated, but they are discarded.")
