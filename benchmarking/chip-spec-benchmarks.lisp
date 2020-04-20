(in-package :cl-quil-benchmarking)

(defun benchmark-fully-connected-chip (size)
  (quil::build-nq-fully-connected-chip size))

(defun benchmark-linear-chip (size)
  (quil::build-nq-linear-chip size))

(defun benchmark-banyan-type-chip (size)
  (build-tiled-octagon size 5))

(defun run-chip-spec-benchmark (chip-constructor
                                max
                                &key
                                  (min 1)
                                  (step 1)
                                  completion-callback)
  (loop :for i :from min :upto max :by step
        :for avg-time := (with-timing (2)
                           (funcall chip-constructor i))
        :do (format t "~a of ~a~%" i max)
        :when completion-callback :do
          (funcall completion-callback i avg-time)
        :collect (cons i avg-time)))

(defun run-all-chip-spec-benchmarks ()
  (let ((benchmarks (list (list "fully-connected" #'benchmark-fully-connected-chip 10 160 10)
                          (list "linearly-connected" #'benchmark-linear-chip 10 160 10)
                          (list "banyan" #'benchmark-banyan-type-chip 5 40 5))))
    (loop :for (name fn min max step) :in benchmarks
          :for file := (format nil "results/~a.txt" name) :do
            (with-open-file (s file :direction :output :if-exists :supersede :if-does-not-exist :create)
              (write-line "# size avg-time" s))
            (format t "Benchmarking chip constructor ~A~%" name)
            (run-chip-spec-benchmark fn max :min min :step step :completion-callback (alexandria:curry #'save-benchmark file))
            (terpri))))

(defun save-benchmark (filename size avg-time)
  (with-output-appending-file (s filename)
    (format s "~D ~F~%" size avg-time)))

(defun save-benchmarks (filename benchmarks)
  (with-open-file (s filename :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
    (loop :for (size . time) :in benchmarks :do
      (format s "~D ~F~%" size time))))
