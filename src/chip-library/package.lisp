;;;; src/chip-library/package.lisp
;;;;
;;;; Author: A.J. Nyquist

(defpackage #:cl-quil/chip-library
  (:use #:cl)
  (:local-nicknames (:q :cl-quil)
                    (:a :alexandria))
  (:export
   #:available-chips                    ; FUNCTION
   #:install-chip-builder               ; FUNCTION
   #:get-chip-builder                   ; FUNCTION
   #:call-chip-builder                  ; FUNCTION
   ))
