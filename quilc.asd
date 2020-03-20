;;;; quilc.asd
;;;;
;;;; Author: Eric Peterson

(asdf:defsystem #:quilc
  :description "A CLI front-end for the Quil compiler"
  :author "Eric Peterson <eric@rigetti.com>"
  :version (:read-file-form "VERSION.txt")
  :license "Apache License 2.0 (See LICENSE.txt)"
  :pathname "app/src/"
  :depends-on (#:cl-ppcre
               #:split-sequence
               #:command-line-arguments
               #:yason
               (:version #:magicl "0.7.0")
               #:cl-quil
               #:cl-quil-benchmarking
               #:uiop
               #:bordeaux-threads
               #:cl-syslog
               #:rpcq
               #:drakma
               #:trivial-features       ; for portable *features*
               #:alexandria
               #:swank
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:quilc-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl(sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :serial t
  :components ((:file "package")
               (:file "globals")
               (:file "versions")
               #+sbcl
               (:file "impl/sbcl")
               #+clozure
               (:file "impl/clozure")
               (:file "rpc-server")
               (:file "printers")
               (:file "entry-point")))
