;;;; quilc-mon-prof.lisp
;;;;
;;;; Author: Mark David

(in-package #:cl-quil-benchmarking)



;;;; Monitoring and Profiling

;; This builds on quilc-perf.lisp and is for monitoring and profiling,
;; as opposed to simple benchmarks, and is a WIP.


(defparameter *monitor-types*
  '(:mon #+sbcl :sb-sprof))

(defun monitor-run (program-type chip-type nq repeats monitor
                    &optional report-type sample-interval)
  (when (not (member monitor *monitor-types*))
    (unless (null monitor) ; if so, just silently default with no warning
      (warn "unrecognized monitor, should be one of ~s; using ~s"
            *monitor-types* (first *monitor-types*)))
    (setq monitor (first *monitor-types*)))
  (let (program chip)
    (format t "~2%****~%Building ~a program (nQ = ~d)... " program-type nq)
    (setq program (get-or-build-benchmark-program nq program-type))
    (format t "DONE (~a).~%" program)
    (format t "Building ~a chip (nQ = ~d)... " chip-type nq)
    (setq chip (get-or-build-benchmark-chip nq chip-type))
    (format t "DONE (~a).~%" chip)
    (format t "** Doing ~d run~p... **~%" repeats repeats)
    (let* ((*package* (find-package :cl-quil))
           (thunk
             #'(lambda ()                 
                 (dotimes (i repeats)
                   (prepare-environment-for-perf-run)
                   (format t "#~d: Compiling program/chip ... " i)
                   (let* ((t1 (get-internal-run-time))
                          (t2 (progn (do-one-quilc-perf-run program chip)
                                     (get-internal-run-time)))
                          (elapsed-time (- t2 t1)))
                     (format
                      t
                      "DONE (~,2f sec compiler, ~2df sec real time w/overhead).~%"
                      ;; -- 2nd timing is real time, as opposed to
                      ;; internal run time. 'Overhead' is primarily GC +
                      ;; warming.
                      (internal-time-to-seconds elapsed-time)
                      (internal-time-to-seconds (- t2 t1))))))))
      (ecase monitor
        (:mon (mon:monitor-form (funcall thunk)))
        #+sbcl
        (:sb-sprof
         (progn
           (sb-sprof:reset)
           (sb-sprof:start-profiling
            ;; :sample-interval (or sample-interval 0.005)    ; default = 0.01
            :sample-interval 0.0001
            :threads (list sb-thread:*current-thread*))
           (funcall thunk)
           (sb-sprof:report
            :min-percent 3
            :type (ecase report-type
                    ((nil :flat) :flat)
                    (:graph :graph)))))))
    (format t "** DONE with ~d runs. **~%" repeats)))

(defun do-monitor-runs (&key start step end
                             repeats monitor
                             report-type sam sample-interval)
  (or repeats (setq repeats 3))
  (loop :for nq := (or start 10)
          :then (+ nq (or step 10))
        :when (> nq (or end start))
          :do (return)
        :do (format t "~%**** NQ: ~d ****~%" nq)
            (loop :for program-type :in *benchmark-program-types*
                  :do (loop :for chip-type :in *benchmark-chip-connectedness-types*
                            :do (monitor-run
                                 program-type chip-type nq repeats monitor 
                                 report-type sample-interval)))))

;; Try this on SBCL: (do-monitor-runs :monitor ':sb-sprof) 




(defun do-one (nq)
  (do-one-nq-program-chip nq :hadamard :fully-connected))

(defparameter *min-sb-sprof-perf-pct* 1)

(defun do-one-nq-program-chip (nq program-type chip-type)
  (let ((program (build-benchmark-program nq :hadamard))
        (chip (build-benchmark-chip nq :fully-connected)))
    (sb-sprof:reset)
    (tg:gc :full t)
    (sb-sprof:start-profiling :threads (list sb-thread:*current-thread*))
    (time (benchmark-one-quilc-perf-run program chip))
    (sb-sprof:stop-profiling)
    (sb-sprof:report :type :graph :min-percent *min-sb-sprof-perf-pct*)
    (sb-sprof:report :type :flat :min-percent *min-sb-sprof-perf-pct*)))

(defun do-one-mon (nq &optional program-type)
  (let ((program (build-benchmark-program nq (or program-type :hadamard)))
        (chip (build-benchmark-chip nq :fully-connected)))
    (tg:gc :full t)
    (sb-sprof:with-profiling (:max-samples 1000
                              :report :flat
                              :loop nil
                                   
                              :reset t
                              :sample-interval 0.01) ; default .01
      (benchmark-one-quilc-perf-run program chip))))




;;;; depth check

(defun quil-parse-to-logical-schedule (parse)
  (let* ((instructions-vector (quil:parsed-program-executable-code parse))
         (instructions-list (concatenate 'list instructions-vector))
         (lsched (quil.si:make-lscheduler)))
    (quil.si:append-instructions-to-lschedule lsched instructions-list)
    lsched))

(defun depth-check (nq)
  (let* ((parse (build-benchmark-program nq :hadamard))
         (logical-schedule (quil-parse-to-logical-schedule parse)))
    (depth-check-1 logical-schedule)))

(defun depth-check-1 (logical-schedule)
  (let* ((old-walk-depth
           (let ((quil::*new-walk* nil))
             (quil::lscheduler-calculate-depth logical-schedule)))
         (new-walk-depth
           (let ((quil::*new-walk* t))
             (quil::lscheduler-calculate-depth logical-schedule))))
    (list
     (= old-walk-depth new-walk-depth)
     old-walk-depth new-walk-depth logical-schedule)))



;;;; SB-SPROF run 20, 50, 80, 110, 140

(defun sb-sprof-run ()
  (loop for nq in '(20 50 80 110)
        do (do-monitor-runs
             :start nq 
             :monitor :sb-sprof
             :repeats 1
             :report-type :flat)))

(defun sb-sprof-graph ()
  (loop for nq in '(20 50 80 110)
        do (do-monitor-runs
             :start nq 
             :monitor :sb-sprof
             :repeats 1
             :report-type :flat)))

(defun sb-sprof-run-high-sample ()
  (loop for nq in '(20 50 80 110)
        do (do-monitor-runs
             :start nq 
             :monitor :sb-sprof
             :repeats 1
             
             :sample-interval 0.001     ; our default: 0.005
             :report-type :flat)))

(defun sb-sprof-run-high-sample-plus-graph ()
  (loop for nq in '(20 50 80 110)
        do (do-monitor-runs
             :start nq 
             :monitor :sb-sprof
             :repeats 1
             
             :sample-interval 0.001     ; our default: 0.005
             :report-type :graph)))     ; our default: :flat

(defun sb-sprof-run-mon ()
  (loop for nq in '(20 50 80 110)
        do (do-monitor-runs
             :start nq 
             :monitor :mon
             :repeats 1
             
             :sample-interval 0.001     ; our default: 0.005
             :report-type :graph)))     ; our default: :flat

(defun one-monitor-run-bill-linear (nq)
  (monitor-run :bell :linear nq 1 :mon :flat .0001))

