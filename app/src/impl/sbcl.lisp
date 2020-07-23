;;;; sbcl.lisp

(in-package #:quilc)

(defun disable-debugger ()
  (sb-ext:disable-debugger))

(defun enable-debugger ()
  (sb-ext:enable-debugger))

(deftype interactive-interrupt ()
  'sb-sys:interactive-interrupt)
