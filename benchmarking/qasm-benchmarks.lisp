;;;; qasm-benchmarks.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-benchmarking)

(defvar *qasm-benchmark-path*
  (asdf:system-relative-pathname :cl-quil "benchmarking/ibm_qx_mapping/examples/"))

(defun qasm-test-files ()
  (or (uiop:directory-files *qasm-benchmark-path* #P"*.qasm")
      (error "QASM test files not found in ~a" *qasm-benchmark-path*)))

(defun idiomatic-package-variables (package &key include-internal)
  (let ((symbols nil))
    (labels ((bookended-by (char symbol)
               (let* ((string (symbol-name symbol))
                      (len (length string)))
                 (and (<= 2 len)
                      (char= char (char string 0) (char string (1- len))))))
             (variable-looking-symbol-p (s)
               (and (boundp s)
                    (or (bookended-by #\+ s)
                        (bookended-by #\* s))))
             (ponder-symbol (s)
               (when (variable-looking-symbol-p s)
                 (push s symbols))))
      (do-symbols (s package (sort symbols #'string<))
        (multiple-value-bind (s status)
            (find-symbol (symbol-name s) package)
          (case status
            (:internal
             (when include-internal
               (ponder-symbol s)))
            (:external
             (ponder-symbol s))))))))

(defun print-configuration ()
  (format t "Current CL-QUIL configuration:~2%")
  (with-standard-io-syntax
    (dolist (symb (idiomatic-package-variables ':cl-quil :include-internal t))
      (format t "~A~%    " symb)
      (write (symbol-value symb) :pretty nil :escape t :readably nil :length 10 :level 2)
      (terpri)))
  (terpri)
  (finish-output))

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
  (let ((timed-out nil))
    (flet ((print-rule ()
             (format t "+------------------+----------+-------+----------+~%")))
      (print-configuration)
      (print-rule)
      (format t "|       NAME       | TIME (s) | SWAPS | 2Q DEPTH |~%")
      (print-rule)
      (let ((chip (quil::build-ibm-qx5))
            (quil::*default-addresser-state-class* 'quil::temporal-addresser-state)
            (quil::*addresser-use-1q-queues* t)
            (quil::*safe-include-directory* (asdf:system-relative-pathname :cl-quil "tests/qasm-files/")))
        (dolist (file (qasm-test-files))
          (format t "| ~Va " 16 (trim-long-string (pathname-name file) 16))
          (finish-output)
          (handler-case
              (let ((text (alexandria:read-file-into-string file)))
                (tg:gc :full t)
                (bordeaux-threads:with-timeout (timeout)
                  (with-stopwatch elapsed-time
                    (multiple-value-bind (cpp swaps)
                        (quil::compiler-hook (quil::parse text
                                                          :originating-file file)
                                             chip
                                             :protoquil t
                                             :destructive t)
                      (format t "| ~Vf | ~Vd | ~Vd |~%"
                              8 (/ elapsed-time 1000)
                              5 swaps
                              8 (calculate-multiqubit-gate-depth (parsed-program-executable-code cpp)))))))
            (bt:timeout ()
              (format t "| ~8,'>d | ????? | ???????? |~%"
                      timeout)
              (push (pathname-name file) timed-out)))
          (finish-output)))
      (print-rule)
      (terpri)
      (when timed-out
        (with-standard-io-syntax
          (format t "~D file~:P timed out: " (length timed-out))
          (write timed-out :escape t :pretty nil :readably nil)
          (terpri)))
      (finish-output))))
