;;;; quilc-tests.asd
;;;;
;;;; Author: Eric Peterson

(asdf:defsystem #:quilc-tests
  :description "Regression tests for quilc."
  :author "Eric Peterson <eric@rigetti.com>"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:quilc
               #:fiasco
               #:uiop
               )
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':quilc-tests
                                           '#:run-quilc-tests))
  :pathname "app/tests/"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "suite")
               (:file "faithfulness-tests")
               (:file "rpcq-tests")
               (:file "misc-tests")))
