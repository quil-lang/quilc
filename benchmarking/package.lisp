;;;; bench/package.lisp
;;;;
;;;; Author: Mark Skilbeck

(benchmark:define-benchmark-package #:cl-quil-benchmarking
  (:use #:cl-quil)
  ;; Both CL-QUIL and TRIVIAL-BENCHMARK define a RESET function --
  ;; tell CL to import neither.
  (:shadow #:reset)

  ;; suite.lisp
  (:export
   #:run-benchmarks)
  (:shadowing-import-from #:cl-quil #:pi))
