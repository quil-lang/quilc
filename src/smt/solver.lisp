;; solver.lisp
;;
;; Author: Erik Davis

;; This provides an extremely thin layer over CL-SMT-LIB for starting
;; a solver and giving it constraints.

(in-package #:cl-quil.smt)

(defvar *smt-debug-stream* nil
  "When non-NIL, this indicates the stream at which debug lines get printed.")

(defvar *constraint-solver-command* '("z3" "-in" "-smt2")
  "The default command for invoking the constraint solver.")

(defun smt-debug-line (ctxt fmt-msg &rest fmt-args)
  (when *smt-debug-stream*
    (apply #'format *smt-debug-stream* (format nil "~A: ~A~%" ctxt fmt-msg) fmt-args)))

(defun initiate-smt-solver (command)
  "Get a CL-SMT-LIB:PROCESS-TWO-WAY-STREAM by invoking COMMAND."
  (smt-debug-line 'initiate-smt-solver "Starting solver:~{ ~A~}" command)
  (let ((smt (apply #'cl-smt-lib:make-smt command)))
    (unless (uiop:process-alive-p (cl-smt-lib::process smt))
      (error "Tried~{ ~A~}, but solver process is dead on arrival." command))
    smt))

(defun write-smt-forms (forms &optional (stream *standard-output*))
  "Write constraint FORMS to STREAM."
  (let ((*package* #.*package*))	; we don't want packages printed in variable names
    (cl-smt-lib:write-to-smt stream forms)))

(defun read-smt-form (&optional (stream *standard-input*))
  (let ((*package* #.*package*))
    (read stream)))
