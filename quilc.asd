;;;; quilc.asd
;;;;
;;;; Author: Eric Peterson

(asdf:defsystem #:quilc
  :description "A CLI compiler for the Quantum Instruction Language (Quil)."
  :author "Eric Peterson <eric@rigetti.com>"
  :version (:read-file-form "VERSION.txt")
  :pathname "src/"
  :depends-on (#:cl-ppcre
               #:split-sequence
               #:command-line-arguments
               #:yason
               (:version #:magicl "0.4.0")
               (:version #:cl-quil "0.18.0")
               #:uiop
               #:hunchentoot
               #:bordeaux-threads
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:quilc-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl(sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :serial t
  :components ((:file "package")
               #+sbcl
               (:file "impl/sbcl")
               #+clozure
               (:file "impl/clozure")
               (:file "server")
               (:file "printers")
               (:file "entry-point")))
