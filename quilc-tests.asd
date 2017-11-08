;;;; quilc-tests.asd
;;;;
;;;; Author: Eric Peterson

(asdf:defsystem #:quilc-tests
  :description "Regression tests for quilc."
  :author "Eric Peterson <eric@rigetti.com>"
  :depends-on (#:quilc
               #:fiasco
               #:uiop
               )
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':quilc-tests
                                           '#:run-quilc-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "faithfulness-tests")
               )
  )
