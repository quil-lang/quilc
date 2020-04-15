(asdf:defsystem #:cl-quil-benchmarking
  :depends-on (#:cl-quil
               #:trivial-benchmark
               #:bordeaux-threads
               #:trivial-garbage
               #:lparallel
               #:qvm-examples)
  :license "Apache License 2.0 (See LICENSE.txt)"
  :pathname "benchmarking/"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "rewiring-analysis")
               (:file "qasm-benchmarks")
               (:file "chip-spec-benchmarks")
               (:file "qaoa-tests/generate-program")
               (:file "suite")))
