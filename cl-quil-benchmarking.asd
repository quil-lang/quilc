(asdf:defsystem #:cl-quil-benchmarking
  :depends-on (#:cl-quil #:trivial-benchmark #:bordeaux-threads)
  :license "Apache License 2.0 (See LICENSE.txt)"
  :pathname "benchmarking/"
  :serial t
  :components ((:file "package")
               (:file "rewiring-analysis")
               (:file "program-size")
               (:file "suite")))
