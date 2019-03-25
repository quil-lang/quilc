;;;; compressor-configuration.lisp
;;;;
;;;; Author: Eric Peterson
;;;;         Robert Smith

(in-package #:cl-quil)

;;; This file contains configuration variables for the compressor. In
;;; general, it's expected that these are dynamically bound, instead
;;; of being outright set.

(defvar *global-queue-tolerance-threshold* 3
  "Number of link queues that are allowed to feed into the pseudo-object queue before we perform a flush.")

(defvar *rewriting-peephole-size* 4
  "Maximum number of instructions to collect for rewriting inspection.")

(defvar *compress-carefully* nil
  "Flag that turns on/off a bunch of intermediate correctness checks during compression.  WARNING: this can be *very* costly, since it involves computing explicit matrix presentations.")

(defvar *compressor-passes* 1
  "The number of times to run the compressor within #'COMPILER-HOOK.")

(defvar *enable-state-prep-compression* nil
  "When NIL, compression using state preparation methods is disabled.")

(defvar *enable-approximate-compilation* nil
  "When NIL, compression by replacing instructions sequences with approximate sequences is disabled.

NOTE: When T, this permits the approximate compilation templates to emit inexact results, but does not actually enable any alternative code paths.  When NIL, these results are still generated, but they are discarded.")
