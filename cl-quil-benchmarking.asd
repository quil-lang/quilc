(asdf:defsystem #:cl-quil-benchmarking
  :depends-on (#:cl-quil #:trivial-benchmark)
  :pathname "benchmarking/"
  :serial t
  :components ((:file "package")
               (:file "rewiring-analysis")
               (:file "suite")))
