;;;; src/tools/package.lisp
;;;;
;;;; Author: Mark David

;;; Allegro (and other Lisps) don't support the non-standard "package
;;; local nicknames".
#-(or sbcl ecl ccl)
(rename-package :alexandria :alexandria '(:a))

(defpackage #:cl-quil.tools
  (:nicknames #:tools)
  (:use #:cl)
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))

  ;; hasse-schedule.lisp
  (:export
   #:write-hasse-for-quil
   #:write-hasse-for-logical-scheduler))
