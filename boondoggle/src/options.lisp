;;;; options.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:boondoggle)

(defvar *debug-noise-stream* (make-broadcast-stream)
  "Stream that debug output is written to.")
