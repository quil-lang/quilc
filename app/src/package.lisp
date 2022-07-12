;;;; package.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-user)

(defpackage #:quilc
  (:use #:cl #:cl-quil #:cl-quil/chip-library)
  (:local-nicknames (#:a    #:alexandria)
                    (#:quil #:cl-quil)))
