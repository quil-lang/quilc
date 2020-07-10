(asdf:defsystem #:cl-quil-benchmarking
  :depends-on (#:cl-quil
               #:trivial-benchmark
               #:bordeaux-threads
               #:trivial-garbage)
  :license "Apache License 2.0 (See LICENSE.txt)"
  :pathname "benchmarking/"
  :serial t
  :components ((:file "package")
               (:module "qaoa-tests"
                :serial t
                :components ((:file "generate-program")))
               (:file "rewiring-analysis")
               (:file "qasm-benchmarks")
               (:file "suite")))
