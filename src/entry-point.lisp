;;;; pipe-compiler-entry-points.lisp
;;;;
;;;; fast entry point for compilation methods
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; eventually, we will want this to look at argv for a filename from which it
;;;; can parse a chip/ISA specification. for now, we're going to bake such a
;;;; specification in.
;;;; ===sample input===
;;;; ANY CLASSICAL-FREE QUIL PROGRAM
;;;; ^D

(in-package #:quilc)

(defparameter *program-name* "quilc")
(defparameter *compute-gate-depth* nil)
(defparameter *compute-runtime* nil)
(defparameter *compute-matrix-reps* nil)
(defparameter *topological-swaps* nil)

(defparameter *option-spec*
  '((("compute-gate-depth" #\d) :type boolean :optional t :documentation "prints compiled circuit gate depth")
    (("compute-runtime" #\r) :type boolean :optional t :documentation "prints compiled circuit expected runtime")
    (("compute-matrix-reps" #\m) :type boolean :optional t :documentation "prints matrix representations for comparison")
    (("show-topological-overhead" #\t) :type boolean :optional t :documentation "prints the number of SWAPs incurred for topological reasons")
    (("help" #\? #\h) :optional t :documentation "prints this help information and exits")))

(defun slurp-lines (&optional (stream *standard-input*))
  (flet ((line () (read-line stream nil nil nil)))
    (with-output-to-string (s)
      (loop :for line := (line) :then (line)
         :while line
         :do (write-line line s)))))

(defun print-quil-list (executable-code stream)
  (loop :for instr :in executable-code :do
     (progn
       (quil::print-instruction instr stream)
       (format stream "~%"))))

(defun reload-foreign-libraries ()
  (locally
        (declare #+sbcl (sb-ext:muffle-conditions style-warning))
      (handler-bind (#+sbcl (style-warning #'muffle-warning))
        (cffi:load-foreign-library 'magicl.foreign-libraries::libgfortran)
        (cffi:load-foreign-library 'magicl.foreign-libraries::libblas)
        (cffi:load-foreign-library 'magicl.foreign-libraries::liblapack)
        (cffi:load-foreign-library 'magicl.foreign-libraries::libexpokit))))

(defun print-matrix-with-comment-hashes (matrix &optional (stream *standard-output*))
  (format stream "~d"
          (cl-ppcre:regex-replace-all
           (coerce #(#\Newline) 'string)
           (with-output-to-string (s)
             (princ matrix s))
           (coerce #(#\Newline #\#) 'string))))

(defun show-help ()
  (format t "Usage:~%")
  (format t "  ~A [options]~%" *program-name*)
  (format t "Options:~%")
  (command-line-arguments:show-option-help *option-spec* :sort-names t))

(defun process-options (&key (compute-gate-depth nil)
                             (compute-runtime nil)
                             (compute-matrix-reps nil)
                             (show-topological-overhead nil)
                             (help nil))
  (when help
    (show-help)
    (uiop:quit 0))
  (setf *compute-gate-depth* compute-gate-depth)
  (setf *compute-runtime* compute-runtime)
  (setf *compute-matrix-reps* compute-matrix-reps)
  (setf *topological-swaps* show-topological-overhead))

(defun print-matrix-representations (initial-l2p processed-quil final-l2p program)
  (let* ((original-matrix (quil::make-matrix-from-quil (coerce (quil::parsed-program-executable-code program) 'list) program))
         (initial-l2p (quil::trim-rewiring initial-l2p))
         (final-l2p (quil::trim-rewiring final-l2p))
         (raw-new-matrix (quil::make-matrix-from-quil processed-quil program))
         (qubit-count (max (1- (integer-length (magicl:matrix-rows raw-new-matrix)))
                           (length initial-l2p)
                           (length final-l2p)))
         (wire-out (apply #'quil::kq-gate-on-lines
                          (quil::rewiring-to-permutation-matrix-p2l final-l2p)
                          qubit-count
                          (alexandria:iota (length final-l2p) :start (1- (length final-l2p)) :step -1)))
         (wire-in (apply #'quil::kq-gate-on-lines
                         (quil::rewiring-to-permutation-matrix-l2p initial-l2p)
                         qubit-count
                         (alexandria:iota (length initial-l2p) :start (1- (length initial-l2p)) :step -1)))
         (stretched-raw-new-matrix (apply #'quil::kq-gate-on-lines
                                          raw-new-matrix
                                          qubit-count
                                          (alexandria:iota (1- (integer-length (magicl:matrix-rows raw-new-matrix)))
                                                           :start (- (integer-length (magicl:matrix-rows raw-new-matrix)) 2)
                                                           :step -1)))
         (new-matrix
           (reduce #'magicl:multiply-complex-matrices
                   (list
                    wire-out
                    stretched-raw-new-matrix
                    wire-in))))
    (format *error-output* "~%#Matrix read off from input code~%")
    (print-matrix-with-comment-hashes original-matrix *error-output*)
    (format *error-output* "~%#Matrix read off from compiled code~%")
    (print-matrix-with-comment-hashes new-matrix *error-output*)
    (format *error-output* "~%")
    (finish-output *standard-output*)
    (finish-output *error-output*)))

(defun print-gate-depth (lschedule)
  (format *debug-io* "# Compiled gate depth: ~d~%" (quil::lscheduler-calculate-depth lschedule)))

(defun print-program-runtime (lschedule chip-specification)
  (format *debug-io* "# Compiled program duration: ~5d~%" (quil::lscheduler-calculate-duration lschedule chip-specification)))

(defun print-topological-swap-count (topological-swaps)
  (format *debug-io* "# SWAPs incurred by topological considerations: ~d~%" topological-swaps))


(defun entry-point (argv)
  (handler-case (%entry-point argv)
    (sb-sys:interactive-interrupt (c)
      (declare (ignore c))
      (uiop:quit 0))
    (error (c)
      (format *error-output* "~&! ! ! Error: ~A~%" c)
      (uiop:quit 1))))

(defun %entry-point (argv)
  ;; grab the CLI arguments
  (setf *program-name* (pop argv))

  (command-line-arguments:handle-command-line
   *option-spec*
   'process-options
   :command-line argv
   :name "quilc"
   :positional-arity 0
   :rest-arity nil)

  ;; rebind the MAGICL libraries
  (magicl:with-blapack
    (reload-foreign-libraries)
    ;; slurp the program from *standard-in*
    (let* ((program-text (slurp-lines))
           (program (quil::parse-quil program-text))
           (chip-specification (quil::build-8Q-chip)))
      ;; do the compilation
      (multiple-value-bind (initial-l2p processed-quil final-l2p topological-swaps)
          (quil::compiler-hook program chip-specification)
        ;; now that we've compiled the program, we have various things to output
        ;; one thing we're always going to want to output is the program itself.
        (let ((*print-pretty* nil))
          (format *standard-output* "PRAGMA EXPECTED_REWIRING \"~s\"~%" initial-l2p))
        (print-quil-list processed-quil *standard-output*)
        (let ((*print-pretty* nil))
          (format *standard-output* "PRAGMA CURRENT_REWIRING \"~s\"~%" final-l2p))
        
        (when *topological-swaps*
          (print-topological-swap-count topological-swaps))
        
        (when (or *compute-gate-depth* *compute-runtime*)
          ;; calculate some statistics based on logical scheduling
          (let ((lschedule (make-instance 'quil::lscheduler-empty)))
            (quil::append-instructions-to-lschedule lschedule processed-quil)
            (when *compute-gate-depth*
              (print-gate-depth lschedule))
            (when *compute-runtime*
              (print-program-runtime lschedule chip-specification))))
        
        (when *compute-matrix-reps*
          (print-matrix-representations initial-l2p processed-quil final-l2p program))))))
