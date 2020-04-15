(in-package :cl-quil-benchmarking)

(defun benchmark-fully-connected-chip (size)
  (build-nq-fully-connected-chip size))

(defun benchmark-linear-chip (size)
  (build-nq-linear-chip size))

(defun benchmark-banyan-type-chip (size)
  (build-tiled-octagon size 5))

(defun run-chip-spec-benchmarks (chip-constructor
                                 max
                                 &key
                                   (min 1)
                                   (step 1)
                                   completion-callback)
  (loop :for i :from min :upto max :by step
        :for avg-time := (with-timing (3)
                           (funcall chip-constructor i))
        :do (format t "~4F%~%" (* 100 (/ i (- max min))))
        :when completion-callback :do
          (funcall completion-callback i avg-time)
        :collect (cons i avg-time)))

;; (defun run-banyan-type-chip-benchmarks ()
;;   (loop :for width := 5
;;         :for height :from 1 :upto 10
;;         :for avg-time := (with-timing (2) (build-tiled-octagon width height))
;;         :do (format t "~4F%~%" (* 100 (/ height 4)))
;;         :collect (cons height avg-time)))

(defun save-benchmark (filename size avg-time)
  (with-output-appending-file (s filename)
    (format s "~D ~F~%" size avg-time)))

(defun save-benchmarks (filename benchmarks)
  (with-open-file (s filename :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
    (loop :for (size . time) :in benchmarks :do
      (format s "~D ~F~%" size time))))
