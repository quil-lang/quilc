;;;; printers.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; stuff responsible for writing quil out to something readable / processable
;;;; all goes in here.

(in-package #:quilc)

(defun print-matrix-representations (initial-l2p processed-quil final-l2p original-matrix)
  (let* ((initial-l2p (quil::trim-rewiring initial-l2p))
         (final-l2p (quil::trim-rewiring final-l2p))
         (raw-new-matrix (quil::make-matrix-from-quil processed-quil))
         (qubit-count (max (1- (integer-length (magicl:matrix-rows raw-new-matrix)))
                           (1- (integer-length (magicl:matrix-rows original-matrix)))
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
         (stretched-original-matrix (apply #'quil::kq-gate-on-lines
                                           original-matrix
                                           qubit-count
                                           (alexandria:iota (1- (integer-length (magicl:matrix-rows original-matrix)))
                                                            :start (- (integer-length (magicl:matrix-rows original-matrix)) 2)
                                                            :step -1)))
         (new-matrix
           (reduce #'magicl:multiply-complex-matrices
                   (list
                    wire-out
                    stretched-raw-new-matrix
                    wire-in))))
    (setf new-matrix (quil::scale-out-matrix-phases new-matrix stretched-original-matrix))
    (format *human-readable-stream* "~%#Matrix read off from input code~%")
    (print-matrix-with-comment-hashes stretched-original-matrix *human-readable-stream*)
    (setf (gethash "original_matrix" *statistics-dictionary*)
          (with-output-to-string (s)
            (print-matrix-with-comment-hashes stretched-original-matrix s)))
    (format *human-readable-stream* "~%#Matrix read off from compiled code~%")
    (print-matrix-with-comment-hashes new-matrix *human-readable-stream*)
    (setf (gethash "compiled_matrix" *statistics-dictionary*)
          (with-output-to-string (s)
            (print-matrix-with-comment-hashes new-matrix s)))
    (format *human-readable-stream* "~%")
    (finish-output *standard-output*)
    (finish-output *human-readable-stream*)))

(defun print-gate-depth (lschedule)
  (let ((depth (quil::lscheduler-calculate-depth lschedule)))
    (setf (gethash "gate_depth" *statistics-dictionary*) depth)
    (format *human-readable-stream*
            "# Compiled gate depth: ~d~%"
            depth)))

(defun print-gate-volume (lschedule)
  (let ((volume (quil::lscheduler-calculate-volume lschedule)))
    (setf (gethash "gate_volume" *statistics-dictionary*) volume)
    (format *human-readable-stream*
            "# Compiled gate volume: ~d~%"
            volume)))

(defun print-program-runtime (lschedule chip-specification)
  (let ((duration (quil::lscheduler-calculate-duration lschedule
                                                       chip-specification)))
    (setf (gethash "program_duration" *statistics-dictionary*) duration)
    (format *human-readable-stream*
            "# Compiled program duration: ~5d~%"
            duration)))

(defun print-program-fidelity (lschedule chip-specification)
  (let ((fidelity (quil::lscheduler-calculate-fidelity lschedule
                                                       chip-specification)))
    (setf (gethash "program_fidelity" *statistics-dictionary*) duration)
    (format *human-readable-stream*
            "# Estimated compiled program fidelity: ~5d~%"
            fidelity)))

(defun print-topological-swap-count (topological-swaps)
  (setf (gethash "topological_swaps" *statistics-dictionary*) topological-swaps)
  (format *human-readable-stream*
          "# SWAPs incurred by topological considerations: ~d~%"
          topological-swaps))

(defun print-program (initial-l2p processed-quil final-l2p &optional (stream *standard-output*))
  (let ((*print-pretty* nil))
    (format stream "PRAGMA EXPECTED_REWIRING \"~s\"~%" initial-l2p))
  (let ((quil::*print-fractional-radians* (not *without-pretty-printing*)))
    (print-quil-list processed-quil stream))
  (let ((*print-pretty* nil))
    (format stream "PRAGMA CURRENT_REWIRING \"~s\"~%" final-l2p)))

(defun publish-json-statistics ()
  (yason:encode *statistics-dictionary* *json-stream*)
  *statistics-dictionary*)
