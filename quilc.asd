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
               (:version #:magicl "0.6.2")
               #:cl-quil
               #:cl-quil-benchmarking
               #:uiop
               #:hunchentoot          ; deprecated
               #:bordeaux-threads     ; deprecated
               #:cl-syslog
               #:rpcq                 ; to replace HUNCHENTOOT and B-T
               #:drakma
               #:trivial-features     ; for portable *features*
               #:alexandria
               #:cl-qcs
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:quilc-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl(sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :serial t
  :components ((:file "package")
               (:file "versions")
               #+sbcl
               (:file "impl/sbcl")
               #+clozure
               (:file "impl/clozure")
               (:file "web-server")
               (:file "rpc-server")
               (:file "printers")
               (:file "entry-point")))
