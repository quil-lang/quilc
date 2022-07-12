;;;; qaoa-bench.lisp
;;;;
;;;; Authors: Eric Peterson, Robert Smith

; benchmark
(in-package :cl-user)
(defvar *this-random-state* (sb-ext:seed-random-state 1337))
(setf *random-state* (make-random-state *this-random-state*))
(ql:quickload :cl-quil-benchmarking)
(in-package #:cl-quil-benchmarking)

(defun reload (file)
  (format t "~&;; reloading ~S~%" file)
  (let ((*compile-verbose* nil)
        (*compile-print* nil))
    (handler-bind ((warning #'muffle-warning)
                   (sb-int:simple-compiler-note #'muffle-warning))
      (let ((outfile (compile-file (asdf:system-relative-pathname ':cl-quil file)
                                   :verbose nil :print nil)))
        (load outfile)
        (uiop:delete-file-if-exists outfile)
        nil))))

(defun print-stats (name numbers)
  (let ((mean (a:mean numbers))
        (sdev (a:standard-deviation numbers)))
    (format t "~&~A: ~F (~F)~%" name mean sdev)))

(load (asdf:system-relative-pathname
   ':cl-quil-benchmarking
   "./benchmarking/qaoa-tests/generate-program.lisp"))
(sb-ext:gc :full t) 


(labels
    ((make-program (qubits valency)
       (generate-random-qaoa-program (1+ qubits)
                                     :self-connectivity 0.5d0
                                     :graph-valency valency))
     (benchmark (&key (repetitions 3)
                      (valencies (list 3))
                      (qubit-counts (list 127)))
       (sb-ext:gc :full t)
       (dolist (valency valencies)
         (dolist (qubits qubit-counts)
           (let ((pp (let ((*random-state* (make-random-state
                                            cl-user::*this-random-state*)))
                       (make-program qubits valency)))
                 (chip (cl-quil::build-nQ-trivalent-chip -1 1 16 16))) ;; 128 qubit chip
             (sb-ext:gc :full t)
             (loop :for count :below repetitions
                   :with runtime := nil
                   :with gc-time := nil
                   :with bytes := nil
                   :do (flet ((process-timing-info (&key real-time-ms
                                                         gc-run-time-ms
                                                         bytes-consed
                                                    &allow-other-keys)
                                (push real-time-ms runtime)
                                (push gc-run-time-ms gc-time)
                                (push bytes-consed bytes)
                                nil)
                              (thing-to-time ()
                                (let ((*random-state* (make-random-state
                                                       cl-user::*this-random-state*)))
                                  (compiler-hook pp chip))))
                         (declare (dynamic-extent #'process-timing-info #'thing-to-time))
                         ;;(sb-ext:gc :full t)
                         (sb-ext:call-with-timing #'process-timing-info #'thing-to-time)
                         (format t "~D " (1+ count)) (finish-output))
                   :finally (progn
                              (format t "Completed run on ~A qubits with valency ~A.~%" qubits valency)
                              (print-stats "Time   " runtime)
                              (print-stats "GC     " gc-time)
                              (print-stats "Consing" bytes))))))))
  (benchmark :repetitions 3
             ;; note: you can make these into longer lists to get more
             ;; benchmarks at once
             :valencies (list 3)
             :qubit-counts (list 127)))

(sb-ext:exit)
