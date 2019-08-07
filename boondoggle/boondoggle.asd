;;;; boondoggle.asd
;;;;
;;;; Author: Eric Peterson

(asdf:defsystem #:boondoggle
  :description "A quilc/QVM/QPU integration tester."
  :author "Eric Peterson <eric@rigetti.com>"
  :version (:read-file-form "VERSION.txt")
  :depends-on (#:cl-quil
               #:command-line-arguments
               #:drakma
               #:uiop
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:boondoggle-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl(sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "options")
               (:file "producers")
               (:file "consumers")
               (:file "processors")
               (:file "pipeline")))
