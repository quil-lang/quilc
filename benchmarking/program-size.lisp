;;; program-size.lisp
;;;
;;;

;;; The purpose of these benchmarks is to see how program depth
;;; (namely 2Q) affects compilation time and memory usage.
;;;
;;; (run-benchmarks) is the entry point for this set of
;;; benchmarks. Benchmark data can be written to disk with
;;; (write-benchmarks-to-files (run-benchmarks)).

(benchmark:define-benchmark-package #:cl-quil-benchmarking.program-compilation
  (:use #:cl-quil)
  ;; Both CL-QUIL and TRIVIAL-BENCHMARK define a RESET function --
  ;; tell CL to import neither.
  (:shadow #:reset)
  
  (:shadowing-import-from #:cl-quil #:pi))

(in-package #:cl-quil-benchmarking.program-compilation)

(defun make-bell-program (n-qubits)
  ""
  (let ((pp (make-instance 'parsed-program)))
    (setf (parsed-program-executable-code pp)
          (coerce (loop :for i :from 1 :below n-qubits
                        :collect (quil::build-gate "CNOT" nil (1- i) i) :into res
                        :finally (return (append (list (quil::build-gate "H" nil 0))
                                                 res)))
                  'vector))
    pp))

(defun make-qft-like-program (n-qubits
                              &key
                                (oneq-gate "H")
                                (twoq-gate "CZ"))
  "Produce a PARSED-PROGRAM which has blocks of the form

    H i
    CZ i (i + 1)
    CZ i (i + 2)
    ...
    CZ i (N-QUBITS - 1)

with i ranging from 0 to N-QUBITS - 1."
  (let ((pp (make-instance 'parsed-program)))
    (setf (parsed-program-executable-code pp)
          (coerce (loop :for m :below n-qubits
                        :collect (quil::build-gate oneq-gate nil m)
                        :append
                        (loop :for n :from (1+ m) :below n-qubits
                              :collect (quil::build-gate twoq-gate nil m n)))
                  'vector))
    pp))

;; Force all numeric computations to float. They are stored as
;; rationals, but rationals are IMO hard to interpret when printed as
;; rationals.
;;
;; See https://github.com/Shinmera/trivial-benchmark/pull/10
;; (defmethod benchmark::compute :around (thing metric)
;;   (call-next-method thing metric))

(defparameter *metrics* (list 'benchmark:user-run-time 'benchmark:bytes-consed)
  "The benchmark metrics we are interested in.")

(defun benchmark-program (program samples
                          &key
                            (chip (error "Need a chip")))
  (let ((timer (benchmark:make-timer *metrics*)))
    (loop :repeat samples :do
      (benchmark:with-sampling (timer)
        (quil::compiler-hook program chip)))
    timer))

(defun get-metric (metric-type benchmark-metrics)
  (find metric-type benchmark-metrics :test 'eql :key 'type-of))

(defun get-average (metric)
  "Compute the average for METRIC-TYPE."
  (coerce (benchmark:compute ':average metric)
          'float))

(defun run-benchmarks ()
  (list
   (cons (print "Bell program compilation (fully connected nQ chip)")
         (loop :with samples := 10
               :for i :from 1 :upto 20
               :for chip := (quil::build-nq-fully-connected-chip i :architecture ':cz)
               :for program := (make-bell-program i)
               :for metrics := (benchmark:metrics (benchmark-program program samples :chip chip))
               :do (print i)
               :collect (list :num-qubits i
                              :user-run-time (get-average (get-metric 'user-run-time metrics))
                              :bytes-consed (get-average (get-metric 'bytes-consed metrics)))))
   (cons (print "Bell program compilation (Aspen-4-16Q)")
         (loop :with samples := 10
               :for i :from 1 :upto 16
               :for chip := (quil::read-chip-spec-file "../tests/qpu-test-files/Aspen-4-16Q-A.qpu")
               :for program := (make-bell-program i)
               :for metrics := (benchmark:metrics (benchmark-program program samples :chip chip))
               :collect (list :num-qubits i
                              :user-run-time (get-average (get-metric 'user-run-time metrics))
                              :bytes-consed (get-average (get-metric 'bytes-consed metrics)))))
   (cons (print "QFT program compilation (fully connected nQ chip)")
         (loop :with samples := 10
               :for i :from 1 :upto 20
               :for chip    := (quil::build-nq-fully-connected-chip i :architecture ':cz)
               :for program := (make-qft-like-program i)
               :for metrics := (benchmark:metrics (benchmark-program program samples :chip chip))
               :collect (list :num-qubits i
                              :user-run-time (get-average (get-metric 'user-run-time metrics))
                              :bytes-consed (get-average (get-metric 'bytes-consed metrics)))))
   (cons (print "QFT program compilation (Aspen-4-16Q)")
         (loop :with samples := 10
               :for i :from 1 :upto 16
               :for chip := (quil::read-chip-spec-file "../tests/qpu-test-files/Aspen-4-16Q-A.qpu")
               :for program := (make-qft-like-program i)
               :for metrics := (benchmark:metrics (benchmark-program program samples :chip chip))
               :collect (list :num-qubits i
                              :user-run-time (get-average (get-metric 'user-run-time metrics))
                              :bytes-consed (get-average (get-metric 'bytes-consed metrics)))))))

(defun write-benchmark (benchmark &optional stream)
  (write-line "number_qubits user_run_time bytes_consed" stream)
  (dolist (bm benchmark)
    (format stream "~A ~A ~A~%"
            (getf bm :num-qubits)
            (getf bm :user-run-time)
            (getf bm :bytes-consed))))

(defun write-benchmarks-to-files (benchmarks &optional (prefix "./"))
  (dolist (bm benchmarks)
    (let* ((name (first bm))
           (path (concatenate 'string prefix name ".csv")))
      (with-open-file (s path :direction ':output :if-exists ':overwrite :if-does-not-exist ':create)
        (write-benchmark (rest bm) s)))))
