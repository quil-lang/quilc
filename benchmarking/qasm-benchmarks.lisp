;;;; qasm-benchmarks.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-benchmarking)

(defvar *qasm-benchmark-path*
  (asdf:system-relative-pathname :cl-quil "benchmarking/ibm_qx_mapping/examples/"))

(defun qasm-test-files ()
  (uiop:directory-files *qasm-benchmark-path* #P"*.qasm"))

(defun trim-long-string (string length)
  (if (>= length (length string))
      string
      (concatenate 'string
                   (subseq string 0 (- length 3))
                   "...")))

(defun calculate-multiqubit-gate-depth (code-vector)
  (let ((lschedule (quil::make-lscheduler)))
    (loop :for instr :across code-vector
          :when (and (typep instr 'gate-application)
                     (<= 2 (length (application-arguments instr))))
            :do (quil::append-instruction-to-lschedule lschedule instr)
          :finally (return (quil::lscheduler-calculate-depth lschedule)))))

(defun benchmark-qasm-suite (&key (timeout 30))
  (format t "+------------------+----------+-------+----------+~%")
  (format t "|       NAME       | TIME (s) | SWAPS | 2Q DEPTH |~%")
  (format t "+------------------+----------+-------+----------+~%")
  (let ((chip (quil::build-ibm-qx5))
        (quil::*default-addresser-state-class* 'quil::temporal-addresser-state))
    (dolist (file (qasm-test-files))
      (format t "| ~Va " 16 (trim-long-string (pathname-name file) 16))
      (handler-case
          (bordeaux-threads:with-timeout (timeout)
            (let ((text (alexandria:read-file-into-string file)))
              #+sbcl (sb-ext:gc :full t)
              (with-stopwatch elapsed-time
                (multiple-value-bind (cpp swaps)
                    (quil::compiler-hook (quil::parse text
                                                      :originating-file (first (directory file)))
                                         chip
                                         :protoquil t)
                  (format t "| ~Vf | ~Vd | ~Vd |~%"
                          8 (/ elapsed-time 1000)
                          5 swaps
                          8 (calculate-multiqubit-gate-depth (parsed-program-executable-code cpp)))))))
        (bt:timeout ()
          (format t "| ~8,'>d | ????? | ???????? |~%"
                  timeout)
          )
        )))
  (format t "+------------------+----------+-------+----------+~%"))
