;;;; bench/package.lisp
;;;;
;;;; Author: Mark Skilbeck

(benchmark:define-benchmark-package #:cl-quil-benchmarking
  (:use #:cl-quil)
  ;; Both CL-QUIL and TRIVIAL-BENCHMARK define a RESET function --
  ;; tell CL to import neither.
  (:shadow #:reset)
  (:local-nicknames (:a :alexandria))

  ;; suite.lisp
  (:export
   #:run-benchmarks)
  (:shadowing-import-from #:cl-quil #:pi))
