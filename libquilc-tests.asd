;;;; libquilc-tests.asd
;;;;
;;;; Author: Kartik Singh

(asdf:defsystem #:libquilc-tests
  :description "Regression tests for libquilc."
  :author "Kartik Singh <kssingh@hrl.com"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:quilc
               #:libquilc
               #:fiasco
               #:uiop
               )
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':libquilc-tests
                                           '#:run-libquilc-tests))
  :pathname "lib/tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "c-tests")
               (:file "python-tests")))
