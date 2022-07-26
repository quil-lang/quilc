;;;; benchmarking/suite.lisp
;;;;
;;;; Author: Mark Skilbeck

(in-package #:cl-quil-benchmarking)

(defun measure-rewiring-performance (assignments quil-file-path chip &key (break-on-error t))
  (let (failed)
    (benchmark:with-benchmark-sampling
      (loop
        :named morf-nruter
        :for (label assn) :on assignments :by #'cddr
        ;; COMPILER-HOOK modifies the input program, and so we cannot
        ;; reuse the same parsed program (result of READ-QUIL-FILE) in
        ;; the the compilation.
        :for quil-file := (read-quil-file quil-file-path)
        :do
           (handler-bind ((simple-error
                            (lambda (e)
                              (when break-on-error
                                (break "~A" e))
                              (setq failed t)
                              (return-from morf-nruter))))
             (funcall assn (lambda () (compiler-hook quil-file chip :destructive t))))))
    (when failed
      ;; The sets all sample counts to zero, and all metrics to :N/A,
      ;; and this is how any caller should test for failure.
      (benchmark:reset benchmark::*current-timer*))))

(defmacro generate-rewiring-benchmarks ()
  (labels
      ((enough-qubits-p (chip quil-file)
         (<= (cl-quil::qubits-needed quil-file) (cl-quil::chip-spec-n-qubits chip)))

       (bench-name (quil-file rewiring chip)
         (intern (string-upcase (concatenate 'string "bench-" quil-file
                                             "-" rewiring "-" chip))))

       (make-benchmark (bench-name form-to-eval)
         `(benchmark:define-benchmark ,bench-name ()
            ,form-to-eval))

       (make-benchmark-forms ()
         ;; That the rewiring benchmarks are dynamically produced complicates things. For the QVM
         ;; benchmarks, we hand-wrote all the (define-benchmark ...)'s no problem. But here we have
         ;; to be clever and employ macros/magic to produce the benchmark forms for us.
         (let* ((benchmarks
                  ;; (bench-name . function-to-benchmark) pairs for all benchmarks
                  (loop :for quil-file-path :in (rewiring-test-files)
                        :for quil-file := (read-quil-file quil-file-path)
                        ;; To produce a name for the benchmark, we concatenate the quil file,
                        ;; assignment name, and chip label. That's why we have the quoted list of
                        ;; assignments below.  I think this could all be cleaned up with a
                        ;; refactoring of rewiring-analysis.lisp.
                        :append
                        (loop :for rewiring-name
                                :in '(*basic-swap-search-assn* *2q-tiers-assn* *swap-search-assn*
                                      *initial-rewiring-assn* *depth-vs-swaps-assn*)
                              :append
                              (loop :for (chip-label chip-var) :in *rewiring-explicit-test-chips*
                                    :for bench-name := (bench-name (pathname-name quil-file-path)
                                                                   (string rewiring-name)
                                                                   (string chip-label))
                                    :when (enough-qubits-p (symbol-value chip-var) quil-file)
                                      :collect
                                      (cons bench-name
                                            `(measure-rewiring-performance ,rewiring-name
                                                                           ,quil-file-path
                                                                           ,chip-var
                                                                           :break-on-error nil)))))))
           `(progn
              ,@(loop :for (bm . form-to-eval) :in benchmarks
                      :collect (make-benchmark bm form-to-eval))))))
    (make-benchmark-forms)))

(generate-rewiring-benchmarks)

(defun run-benchmarks (&key (headless nil) (verbose nil))
  "Run all CL-QUIL benchmarks. If HEADLESS is T, quit on completion."
  (let* ((results (run-package-benchmarks :package ':cl-quil-benchmarking
                                          :verbose verbose)))
    (cond
      ((null headless)
       (when verbose (benchmark:report results))
       results)
      (t
       (uiop:quit (if results 0 1))))))

