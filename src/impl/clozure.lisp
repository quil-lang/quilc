;;;; clozure.lisp

(in-package #:quilc)

(defun disable-debugger ()
  (setf ccl::*batch-flag* t))

(defun enable-debugger ()
  (setf ccl::*batch-flag* nil))

(deftype interactive-interrupt ()
  'ccl:interrupt-signal-condition)

