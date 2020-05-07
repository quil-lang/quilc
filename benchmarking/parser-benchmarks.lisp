(in-package #:cl-quil-benchmarking)

(defun do-parser-benchmark (program-length
                            &key
                              (instruction-generators *2q-program-generators*)
                              (parser-fn #'quil:parse))
  (let ((program (with-output-to-string (s)
                   (quil:print-parsed-program
                    (random-nq-program program-length :instruction-generators instruction-generators)
                    s))))
    (float (with-timing (1)
             (funcall parser-fn program)))))

(defun run-parser-benchmark (max-program-length
                             &key
                               (min-program-length 1)
                               (step 1)
                               (runs 1)
                               setup-fn
                               completion-fn)
  (when setup-fn
    (funcall setup-fn))
  (loop :for i :from min-program-length :upto max-program-length :by step
        :append (loop :repeat runs
                      :for avg := (do-parser-benchmark i)
                      :when completion-fn :do
                        (funcall completion-fn i avg)
                      :collect (cons i avg))
        :do (format t "~a of ~a~%" i max-program-length)))

(defun benchmark-parser ()
  (let ((output (merge-pathnames *benchmarks-results-directory* "/parser.txt")))
    (run-parser-benchmark 1000000
                          :min-program-length 0
                          :step 1000
                          :runs 5
                          :setup-fn (lambda () (confirm-clear-file output))
                          :completion-fn (lambda (i avg) (file>> output "~a ~a~%" i avg)))))
