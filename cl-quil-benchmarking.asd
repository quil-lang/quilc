(asdf:defsystem #:cl-quil-benchmarking
  :depends-on (#:cl-quil
               #:trivial-benchmark
               #:bordeaux-threads
               #:trivial-garbage)
  :license "Apache License 2.0 (See LICENSE.txt)"
  :pathname "benchmarking/"
  :serial t
  :components ((:file "package")
               (:file "rewiring-analysis")
               (:file "qasm-benchmarks")
               (:file "suite")))
