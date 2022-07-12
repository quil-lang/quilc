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
  (let ((lschedule (cl-quil::make-lscheduler)))
    (loop :for instr :across code-vector
          :when (and (typep instr 'gate-application)
                     (<= 2 (length (application-arguments instr))))
            :do (cl-quil::append-instruction-to-lschedule lschedule instr)
          :finally (return (cl-quil::lscheduler-calculate-depth lschedule)))))

(defun benchmark-qasm-suite (&key (timeout 30) named)
  "Run benchmarks from qasm suite. If NAMED is not nil, the specified test(s) will be the ONLY one(s) run; otherwise, all the tests are run. NAMED should be a short name (as shown in the output) of a test, either as a symbol or string, or a list thereof (i.e., matching mutiple tests), to be compared using string-equal. TIMEOUT specifies a timeout in seconds, defaulting to 30 seconds."
  (let ((timed-out nil))
    (flet ((print-rule ()
             (format t "+------------------+----------+-------+----------+~%")))
      (print-configuration)
      (print-rule)
      (format t "|       NAME       | TIME (s) | SWAPS | 2Q DEPTH |~%")
      (print-rule)
      (let ((chip (cl-quil::build-ibm-qx5))
            (cl-quil::*default-addresser-state-class* 'cl-quil::temporal-addresser-state)
            (cl-quil::*addresser-use-1q-queues* t)
            (cl-quil::*safe-include-directory* (asdf:system-relative-pathname :cl-quil "tests/qasm-files/")))
        (dolist (file (qasm-test-files))
          (let ((short-name (trim-long-string (pathname-name file) 16)))
            (when (or (null named)
                      (if (atom named)
                          (string-equal named short-name)
                          (member short-name named :test 'string-equal)))
              (format t "| ~Va " 16 short-name)
              (finish-output)
              (handler-case
                  (let ((text (alexandria:read-file-into-string file)))
                    (tg:gc :full t)
                    (bordeaux-threads:with-timeout (timeout)
                      (with-stopwatch elapsed-time
                        (multiple-value-bind (cpp swaps)
                            (cl-quil::compiler-hook (cl-quil::parse text
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
              (finish-output)))))
      (print-rule)
      (terpri)
      (when timed-out
        (with-standard-io-syntax
          (format t "~D file~:P timed out: " (length timed-out))
          (write timed-out :escape t :pretty nil :readably nil)
          (terpri)))
      (finish-output))))
