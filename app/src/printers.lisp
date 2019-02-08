;;;; printers.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; stuff responsible for writing quil out to something readable / processable
;;;; all goes in here.

(in-package #:quilc)


(declaim (special *without-pretty-printing*
                  *statistics-dictionary*
                  *human-readable-stream*
                  *json-stream*))


(defun print-matrix-representations (initial-l2p processed-quil final-l2p original-matrix)
  (let* ((initial-l2p (quil::trim-rewiring initial-l2p))
         (final-l2p (quil::trim-rewiring final-l2p))
         (raw-new-matrix (quil::make-matrix-from-quil processed-quil))
         (qubit-count (max (1- (integer-length (magicl:matrix-rows raw-new-matrix)))
                           (1- (integer-length (magicl:matrix-rows original-matrix)))
                           (quil::rewiring-length initial-l2p)
                           (quil::rewiring-length final-l2p)))
         (wire-out (quil::kq-gate-on-lines (quil::rewiring-to-permutation-matrix-p2l final-l2p)
                                           qubit-count
                                           (alexandria:iota (quil::rewiring-length final-l2p) :start (1- (quil::rewiring-length final-l2p)) :step -1)))
         (wire-in (quil::kq-gate-on-lines (quil::rewiring-to-permutation-matrix-l2p initial-l2p)
                                          qubit-count
                                          (alexandria:iota (quil::rewiring-length initial-l2p) :start (1- (quil::rewiring-length initial-l2p)) :step -1)))
         (stretched-raw-new-matrix (quil::kq-gate-on-lines raw-new-matrix
                                                           qubit-count
                                                           (alexandria:iota (1- (integer-length (magicl:matrix-rows raw-new-matrix)))
                                                                            :start (- (integer-length (magicl:matrix-rows raw-new-matrix)) 2)
                                                                            :step -1)))
         (stretched-original-matrix (quil::kq-gate-on-lines original-matrix
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

(defun print-2Q-gate-depth (lschedule)
  (let ((depth (quil::lscheduler-calculate-depth lschedule)))
    (setf (gethash "multiqubit_gate_depth" *statistics-dictionary*) depth)
    (format *human-readable-stream*
            "# Compiled multiqubit gate depth: ~d~%"
            depth)))

(defun print-gate-volume (lschedule)
  (let ((volume (quil::lscheduler-calculate-volume lschedule)))
    (setf (gethash "gate_volume" *statistics-dictionary*) volume)
    (format *human-readable-stream*
            "# Compiled gate volume: ~d~%"
            volume)))

(defun print-unused-qubits (lschedule chip-specification)
  (let* ((lscheduler-resources
           (let ((collect (quil::make-null-resource)))
             (quil::lscheduler-walk-graph
              lschedule
              :bump-value (lambda (instr value)
                            (setf collect
                                  (quil::resource-union collect
                                                        (quil::instruction-resources instr)))
                            value))
             collect))
         (unused-qubits
           (loop :for i :below (length (quil::vnth 0 (quil::chip-specification-objects chip-specification)))
                 :unless (quil::resources-intersect-p (quil::make-qubit-resource i)
                                                      lscheduler-resources)
                   :collect i)))
    (setf (gethash "unused_qubits" *statistics-dictionary*) unused-qubits)
    (format *human-readable-stream*
            "# Unused qubit list: ~{~a~^, ~}~%"
            unused-qubits)))

(defun print-program-runtime (lschedule chip-specification)
  (let* ((duration (quil::lscheduler-calculate-duration lschedule
                                                       chip-specification))
         (duration-picos (* duration 1000))
         (seconds  (floor duration 1000000000))
         (millis   (floor duration 1000000))
         (micros   (floor duration 1000))
         (nanos    (floor duration 1))
         (picos    (mod duration-picos 1000)))
    (setf (gethash "program_duration" *statistics-dictionary*) duration)
    (format *human-readable-stream*
            "# Compiled program duration: ~4ds ~4dms ~4dus ~4dns ~4dps    (= ~a ps total)~%"
            seconds millis micros nanos picos duration-picos)))

(defun print-program-fidelity (lschedule chip-specification)
  (let ((fidelity (quil::lscheduler-calculate-fidelity lschedule
                                                       chip-specification)))
    (setf (gethash "program_fidelity" *statistics-dictionary*) fidelity)
    (format *human-readable-stream*
            "# Estimated compiled program fidelity: ~5d~%"
            fidelity)))

(defun print-topological-swap-count (topological-swaps)
  (setf (gethash "topological_swaps" *statistics-dictionary*) topological-swaps)
  (format *human-readable-stream*
          "# SWAPs incurred by topological considerations: ~d~%"
          topological-swaps))

(defun print-program (processed-program &optional (stream *standard-output*))
  (let ((program-as-string
          (with-output-to-string (s)
            (let ((quil::*print-fractional-radians* (not *without-pretty-printing*)))
              (quil::print-parsed-program processed-program s)))))
    (setf (gethash "processed_program" *statistics-dictionary*)
          program-as-string)
    (write-string program-as-string stream)))

(defun publish-json-statistics ()
  (yason:encode *statistics-dictionary* *json-stream*)
  *statistics-dictionary*)


;; custom encoder for rewiring objects
(defmethod yason:encode ((object quil::rewiring) &optional (stream *standard-output*))
  (yason:encode (quil::rewiring-l2p object) stream))
