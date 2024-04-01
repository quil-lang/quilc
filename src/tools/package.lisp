;;;; src/tools/package.lisp
;;;;
;;;; Authors: Mark David
;;;           Erik Davis

;;; Allegro (and other Lisps) don't support the non-standard "package
;;; local nicknames".
#-(or sbcl ecl ccl)
(rename-package :alexandria :alexandria '(:a))

(defpackage #:cl-quil.tools
  (:nicknames #:tools)
  (:use #:cl)
  (:import-from #:cl-quil.tools.unicode-diagram
                #:print-program-diagram
                #:make-charmap
                #:+ascii-charmap+
                #:+box-drawings-charmap+)
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))

  ;; hasse-schedule.lisp
  (:export
   #:write-hasse-for-quil
   #:write-hasse-for-logical-schedule)

  ;; circuit-diagram.lisp
  (:export
   #:plot-circuit)

  ;; unicode-digram.lisp
  (:export
   #:print-program-diagram
   #:make-charmap
   #:+ascii-charmap+
   #:+box-drawings-charmap+))
