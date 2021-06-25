;;;; src/tools/package.lisp
;;;;
;;;; Author: Mark David

;;; Allegro (and other Lisps) don't support the non-standard "package
;;; local nicknames".
#-(or sbcl ecl ccl)
(rename-package :alexandria :alexandria '(:a))

(defpackage #:cl-quil.tools
  (:nicknames #:tools)
  (:use #:cl
        #:cl-quil)
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))

  (:export
   )
  )
