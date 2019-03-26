;;;; compressor-configuration.lisp
;;;;
;;;; Author: Eric Peterson
;;;;         Robert Smith

(in-package #:cl-quil)

;;; This file contains configuration variables for the compressor. In
;;; general, it's expected that these are dynamically bound, instead
;;; of being outright set.
;;;
;;; See also:
;;;
;;;  From config.lisp:
;;;
;;;     - *COMPRESS-CAREFULLY*
;;;     - *ENABLE-APPROXIMATE-COMPILATION*



(defvar *global-queue-tolerance-threshold* 3
  "Number of link queues that are allowed to feed into the pseudo-object queue before we perform a flush.")

(defvar *rewriting-peephole-size* 4
  "Maximum number of instructions to collect for rewriting inspection.")

(defvar *compressor-passes* 1
  "The number of times to run the compressor within #'COMPILER-HOOK.")

(defvar *enable-state-prep-compression* nil
  "When NIL, compression using state preparation methods is disabled.")

