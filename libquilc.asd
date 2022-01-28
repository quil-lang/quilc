;;;; libquilc.asd
;;;;
;;;; Author: Kartik Singh

(asdf:defsystem #:libquilc
  :description "A shared library interface for the Quil compiler"
  :author "Kartik Singh <kssingh@hrl.com>"
  :version (:read-file-form "VERSION.txt")
  :license "Apache License 2.0 (See LICENSE.txt)"
  :pathname "lib/src/"
  :depends-on (#:quilc
               #:sbcl-librarian
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:libquilc-tests)))
  :serial t
  :components ((:file "package")))
