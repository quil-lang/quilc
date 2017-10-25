;;;; quilc.asd
;;;;
;;;; Author: Eric Peterson

(asdf:defsystem #:quilc
  :description "A CLI compiler for the Quantum Instruction Language (Quil)."
  :author "Eric Peterson <eric@rigetti.com>"
  :version (:read-file-form "VERSION.txt")
  :pathname "src/"
  :depends-on (#:cl-ppcre
               #:command-line-arguments
               #:magicl
               #:cl-quil
              )
  :around-compile (lambda (compile)
                    (let (#+sbcl(sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :serial t
  :components ((:file "package")
               (:file "entry-point")
              )
)
