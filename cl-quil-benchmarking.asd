(asdf:defsystem #:cl-quil-benchmarking
  :depends-on (#:cl-quil #:trivial-benchmark)
  :license "Apache License 2.0 (See LICENSE.txt)"
  :pathname "benchmarking/"
  :depends-on (#:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "rewiring-analysis")
               (:file "qasm-benchmarks")
               (:file "suite")))
