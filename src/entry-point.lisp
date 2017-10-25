;;;; pipe-compiler-entry-points.lisp
;;;;
;;;; fast entry point for compilation methods
;;;;
;;;; Author: Eric Peterson

(in-package #:quilc)

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

;; eventually, we will want this to look at argv for a filename from which it
;; can parse a chip/ISA specification. for now, we're going to bake such a
;; specification in.
;; ===sample input===
;; ANY CLASSICAL-FREE QUIL PROGRAM
;; ^D
(defun entry-point (argv)
  (declare (ignore argv))
  (magicl:with-blapack
    (reload-foreign-libraries)
    (let* ((program-text (slurp-lines))
           (program (quil::parse-quil program-text))
           (original-matrix (quil::make-matrix-from-quil (coerce (quil::parsed-program-executable-code program) 'list) program))
           (chip-specification (quil::build-8Q-chip)))
      (multiple-value-bind (initial-l2p processed-quil final-l2p)
          (quil::compiler-hook program chip-specification)
        ;; print the program
        (let ((*print-pretty* nil))
          (format *standard-output* "PRAGMA EXPECTED_REWIRING \"~s\"~%" initial-l2p))
        (print-quil-list processed-quil *standard-output*)
        (let ((*print-pretty* nil))
          (format *standard-output* "PRAGMA CURRENT_REWIRING \"~s\"~%" final-l2p))
        ;; now calculate some statistics based on logical scheduling
        (let ((lschedule (make-instance 'quil::lscheduler-empty)))
          (quil::append-instructions-to-lschedule lschedule processed-quil)
          (format *debug-io* "# Compiled gate depth: ~d~%" (quil::lscheduler-calculate-depth lschedule))
          (format *debug-io* "# Compiled program duration: ~5d~%" (quil::lscheduler-calculate-duration lschedule chip-specification)))
        ;; now do the slurps and check that you get the same matrix in and out
        (let* ((initial-l2p (quil::trim-rewiring initial-l2p))
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
          (finish-output *error-output*))))))
