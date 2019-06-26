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
    (setf (gethash "program_duration" *statistics-dictionary*) (float duration))
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
