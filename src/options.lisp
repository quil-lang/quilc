;;;; options.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

;;; The variables below allow for general configuration of the
;;; compiler. For frontend specific configuration, see
;;; frontend-options.lisp. More configuration of the compressor
;;; specifically can be found in compressor-configuration.lisp.

(defvar *recognize-swap-specially* t
  "Make SWAP-APPLICATION nodes specially for SWAP instructions.")

(defvar *compress-carefully* nil
  "Flag that turns on/off a bunch of intermediate correctness checks during compression.  WARNING: this can be *very* costly, since it involves computing explicit matrix presentations.")

(defvar *enable-approximate-compilation* nil
  "When NIL, compression by replacing instructions sequences with approximate sequences is disabled.

NOTE: When T, this permits the approximate compilation templates to emit inexact results, but does not actually enable any alternative code paths.  When NIL, these results are still generated, but they are discarded.")
