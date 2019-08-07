;;;; boondoggle-tests.asd
;;;;
;;;; Author: Chris Osborn

(asdf:defsystem #:boondoggle-tests
  :description "Boondoggle tests."
  :author "Chris Osborn <chris@rigetti.com>"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:cl-quil
               #:boondoggle
               #:fiasco
               #:uiop)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':boondoggle-tests
                                           '#:run-boondoggle-tests))
  :pathname "boondoggle/tests"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "tests")))
