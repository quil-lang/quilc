(in-package :cl-quil-benchmarking)

(defun benchmark-fully-connected-chip (size)
  "Builds a fully-connected-chip with SIZE many qubits."
  (quil::build-nq-fully-connected-chip size))

(defun benchmark-linear-chip (size)
  "Builds a linearly-connected chip with SIZE many qubits."
  (quil::build-nq-linear-chip size))

(defun benchmark-tiled-octagon (size)
  "Builds a tiled-octagon chip of max-width 5 and SIZE total octagons."
  (build-tiled-octagon size 5))

(defun run-chip-spec-benchmark (chip-constructor
                                max
                                &key
                                  (runs 1)
                                  (min 1)
                                  (step 1)
                                  setup-fn
                                  completion-fn)
  "Benchmark the runtime of the function CHIP-CONSTRUCTOR which takes a single argument, ranging between [MIN, MAX] incrementing by STEP. Repeat the benchmark RUNS times. Before any benchmarks are run, call the (zero arity) function SETUP-FN. After each call to CHIP-CONSTRUCTOR call COMPLETION-FN with the value given to CHIP-CONSTRUCTOR and the observed runtime."
  (when setup-fn
    (funcall setup-fn))
  (loop :for i :from min :upto max :by step
        :do (format t "~a of ~a~%" i max)
        :append (loop :repeat runs
                      :for avg-time := (with-timing (1)
                                         (funcall chip-constructor i))
                      :when completion-fn :do
                        (funcall completion-fn i avg-time)
                      :collect (cons i avg-time))))

(defun run-all-chip-spec-benchmarks (&key (prefix "chip-spec-benchmark-") (suffix ""))
  (let ((benchmarks (list (list "fully-connected" #'benchmark-fully-connected-chip 10 160 10)
                          (list "linearly-connected" #'benchmark-linear-chip 10 160 10)
                          (list "tiled" #'benchmark-tiled-octagon 5 40 5))))
    (loop :for (name fn min max step) :in benchmarks
          :for file := (merge-pathnames *benchmarks-results-directory*
                                        (format nil "results/~a~a~a.txt" prefix name suffix))
          :do
             (format t "Benchmarking chip constructor ~A~%" name)
             (run-chip-spec-benchmark fn max :min min :step step
                                             :setup-fn (a:curry #'clear-file file)
                                             :completion-fn (lambda (i avg) (file>> file "~D ~F~%" i avg)))
             (terpri))))

