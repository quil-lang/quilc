;;;; bench/package.lisp
;;;;
;;;; Author: Mark Skilbeck

(benchmark:define-benchmark-package #:cl-quil-benchmarking
  (:use #:cl-quil)
  ;; Both CL-QUIL and TRIVIAL-BENCHMARK define a RESET function --
  ;; tell CL to import neither.
  (:shadow #:reset)
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))
  (:import-from #:cl-quil
                vnth
                build-gate
                chip-spec-adj-qubits
                chip-spec-live-qubit-cc
                chip-specification
                chip-specification-objects
                hardware-object-cxns
                make-memory-descriptor
                pragma-end-commuting-blocks
                pragma-end-block
                pragma-commuting-blocks
                pragma-initial-rewiring)
  ;; suite.lisp
  (:export
   #:run-benchmarks)
  (:shadowing-import-from #:cl-quil #:pi))
