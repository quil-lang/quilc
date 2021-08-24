;;;; quilc-perf.lisp
;;;;
;;;; Author: Mark David

;; Based on qasm-benchmarks.lisp by Eric Peterson, and uses some of
;; its functionality.

(in-package #:cl-quil-benchmarking)


;;; This module has benchmarking tools primarily to detect performance
;;; bottlenecks as the number of qubits (nQ, hereafter) increases.
;;;
;;; We provide a few kinds of chips and Quil programs and several
;;; methods of benchmarking as a function of nQ.
;;;
;;; The chip types: :fully-connectes, :linear 
;;;
;;; The program types:
;;;
;;;   :static - a small rather trivial Quil program, namely:
;;;
;;;       H 0; CNOT 2 0; H 1; CNOT 0 1; X 0
;;;
;;;     Note that the program is the same (static) no matter the value
;;;     of nQ.
;;;
;;;   :bell - the result of (qvm-app::bell-program nQ)
;;;
;;;   :qft - the result of (qvm-app::qft-program nQ)
;;;
;;;   :hadamard - the result of (qvm-app::hadamard-program nQ)

;;; BENCHMARK-NQ: run the default series of nQ values on all the chip
;;; types for all the program types, printing out results on
;;; *standard-output* as CSV lines in two forms: (1) raw timings (in
;;; seconds) and (2) max-of-series style.

(defparameter *default-benchmark-nq-start* 10
  "Starting nQ value used by benchmark-nq.")

(defparameter *default-benchmark-nq-step* 20
  "Step to next nQ used by benchmark-nq.")

(defparameter *default-benchmark-nq-end* 70
  "End nQ value used by benchmark-nq.")

(defvar *benchmark-quilc-perf-series* '()
  "Initially an empty list, gets set to the list of results by each
  run of benchmark-nq, provided as a developer convenience for later
  perusal, e.g., in a REPL.")

(defun benchmark-nq ()
  (setq *benchmark-quilc-perf-series*
        (benchmark-quilc-perf
         :start *default-benchmark-nq-start*
         :step *default-benchmark-nq-step*
         :end *default-benchmark-nq-end*))
  (csv-raw-timings *benchmark-quilc-perf-series*)
  (terpri)
  (csv-max-of-series-style *benchmark-quilc-perf-series*)
  *benchmark-quilc-perf-series*)

(defparameter *benchmark-program-types*
  '(:static :bell :qft :hadamard))

(defparameter *benchmark-chip-connectedness-types*
  '(:fully-connected :linear))

(defun build-benchmark-chip (nq type)
  (ecase type
    (:fully-connected
     (cl-quil::build-nq-fully-connected-chip nq))
    (:linear
     (cl-quil::build-nq-linear-chip nq))))

(defun build-benchmark-program (nq type)
  (ecase type
    (:static
     (quil:parse "H 0; CNOT 2 0; H 1; CNOT 0 1; X 0"))
    (:bell
     (qvm-app::bell-program nq))
    (:qft
     (qvm-app::qft-program nq))
    (:hadamard
     (qvm-app::hadamard-program nq))))


(defun benchmark-print-rule (&optional stream)
  (when (null stream)
    (setq stream *standard-output*))
  (format stream "+----------------------------+----------+-------+----------+~%"))

(defun internal-time-to-seconds (internal-time-units)
  "Convert integer INTERNAL-TIME-UNITS to number of seconds as float."
  (/ (float internal-time-units) internal-time-units-per-second))

(defun prepare-environment-for-perf-run ()
  (tg:gc :full t))

(defun do-one-quilc-perf-run (program chip)
  "Run compiler on PROGRAM for CHIP; see above re required pre-warm."
  (quil::compiler-hook
   program chip
   :protoquil t
   :destructive t))

(defun benchmark-one-quilc-perf-run (program chip)
  (handler-case
      (progn
        (prepare-environment-for-perf-run)
        (let* (cpp 
               swaps
               (time            ; in cl:internal-time-units-per-second
                 (let ((t1 (get-internal-run-time)))
                   (multiple-value-setq (cpp swaps)
                     (do-one-quilc-perf-run program chip))
                   (- (get-internal-run-time) t1)))
               (2q-depth
                 (calculate-multiqubit-gate-depth
                  (parsed-program-executable-code cpp))))
          (values time swaps 2q-depth)))
    (error (condition)
      (values 'error condition))))



(defparameter *enable-quilc-perf-chip-cache* t
  "Enable (if true) or disable (if false) caching chips for
   benchmarking/performance runs. This defaults to true. If enabled,
   get-or-build-benchmark-chip uses *quilc-perf-chip-cache* to cache
   chips.")

(defvar *quilc-perf-chip-cache* (make-hash-table :test 'equal)
  "Hash table whose keys are (<chip-type> nQ) lists, tested using
  equal, mapping to chip-specification instances created by
  build-benchmark-chip for these performance runs.")

(defun get-or-build-benchmark-chip (nq chip-type)
  (if *enable-quilc-perf-chip-cache*
      (let ((key (list chip-type nq)))
        (or (gethash key *quilc-perf-chip-cache*)
            (setf (gethash key *quilc-perf-chip-cache*)
                  (build-benchmark-chip nq chip-type))))
      (build-benchmark-chip nq chip-type)))


(defparameter *enable-quilc-perf-program-cache* t
  "Enable (if true) or disable (if false) caching programs for
   benchmarking/performance runs. This defaults to true. If enabled,
   get-or-build-benchmark-program uses *quilc-perf-program-cache* to
   cache programs.")

(defvar *quilc-perf-program-cache* (make-hash-table :test 'equal)
  "Hash table whose keys are (<program-type> nQ) lists, tested using
  equal, mapping to parses created by build-benchmark-program for
  these performance runs.")

(defun get-or-build-benchmark-program (nq program-type)
  (if *enable-quilc-perf-program-cache*
      (let ((key (list program-type nq)))
        (or (gethash key *quilc-perf-program-cache*)
            (setf (gethash key *quilc-perf-program-cache*)
                  (build-benchmark-program nq program-type))))
      (build-benchmark-program nq program-type)))



(defun benchmark-one-quilc-perf (nq program-type chip-type)
  (let* ((program (get-or-build-benchmark-program nq program-type))
         (chip (get-or-build-benchmark-chip nq chip-type)))
    (benchmark-one-quilc-perf-run program chip)))

(defun benchmark-quilc-perf-series (start step end program-type chip-type)
  (benchmark-print-rule)
  (format t "|        NAME                | TIME (s) | SWAPS | 2Q DEPTH |~%")
  (benchmark-print-rule)
  (loop :with time :with swaps :with 2q-depth
        :for nq := start :then (+ nq step)
        :while (<= nq end)
        :do (format t "| ~Va " 26 (format nil "~a/~a ~d" program-type chip-type nq))
            (finish-output)
            (multiple-value-setq (time swaps 2q-depth)
              (benchmark-one-quilc-perf nq program-type chip-type))
            (cond
              ((eq time 'error)
               (print (list 'error time))) ; for now (do something better?)
              (t
               (format t "| ~V,2f | ~Vd | ~Vd |~%"
                       8 (internal-time-to-seconds time)
                       5 swaps
                       8 2q-depth)))
        :collect `(,nq ,time) :into results
        :finally
           (benchmark-print-rule)
           (return results)))

(defun benchmark-quilc-perf (&key start step end skip exclusively)
  ;; (print-configuration)  ; a bit messy, so leave off for now
  (loop :with start := (or start 10)
        :with step := (or step 10)
        :with end := (or end 30)
        :for chip-type :in *benchmark-chip-connectedness-types*
        :nconc (loop :for program-type :in *benchmark-program-types*
                     :as skip-this
                       := (and (member program-type skip :key #'first)
                               (member chip-type skip :key #'second))
                     :as exclusively-this
                       := (and (member program-type exclusively :key #'first)
                               (member chip-type exclusively :key #'second))
                     :if skip-this
                       :do (format t "(** Skipping ~a/~a **)~%" program-type chip-type)
                     :else :nconc (and (or (null exclusively)
                                           exclusively-this)
                                       (let ((perf-series
                                               (benchmark-quilc-perf-series
                                                start step end
                                                program-type chip-type)))
                                         `(((,Program-type ,chip-type)
                                            ,@perf-series)))))))

(defun csv-raw-timings (x)
  ;; Arrange like so:
  ;; header row:
  ;; "nQ", program1/chip1, ..., programn, chipn"
  ;; rest of rows:
  ;; <nQ>, <time of program1/chip1>, ..., <time of programn/chipn>

  ;; header row:
  (princ "nQ")
  (loop :with rows := '()
        :for ((chip-type program-type) . timings) :in x
        :do (format t ", ~(~a/~a~)" chip-type program-type)
            (loop :for (nq time) :in timings
                  :as time-in-seconds-float
                    := (internal-time-to-seconds time)
                  :as row := (assoc nq rows)
                  :when (null row)
                    :do (setq row (list nq))
                        (setq rows (nconc rows (list row)))
                  :do (nconc row (list time-in-seconds-float)))
        :finally (terpri)
                 (loop :for (nq . times) :in rows
                       :do (format t "~d" nq)
                           (loop :for time :in times
                                 :do (format t ", ~,3f" time))
                           (terpri))
                 (return rows)))

(defun transform-val-to-ratio-of-max (original-val max-of-series)
  (/ (float original-val) max-of-series))

(defun csv-max-of-series-style (x)
  ;; Arrange like so:
  ;; header row:
  ;; "nQ", program1/chip1, ..., programn, chipn"
  ;; rest of rows:
  ;; <nQ>, <time of program1/chip1>, ..., <time of programn/chipn>

  ;; header row:
  (princ "nQ")
  (loop :with rows := '()
        :for ((chip-type program-type) . timings) :in x
        :as max-of-series
          := (loop :for (nil time) :in timings
                   :maximize time)
        :do (format t ", ~(~a/~a~)" chip-type program-type)
            (loop :for (nq time) :in timings
                  :as time-ratio
                    := (transform-val-to-ratio-of-max time max-of-series)
                  :as row := (assoc nq rows)
                  :when (null row)
                    :do (setq row (list nq))
                        (setq rows (nconc rows (list row)))
                  :do (nconc row (list time-ratio)))
        :finally (terpri)
                 (loop :for (nq . times) :in rows
                       :do (format t "~d" nq)
                           (loop :for time :in times
                                 :do (format t ", ~,3f" time))
                           (terpri))
                 (return rows)))





