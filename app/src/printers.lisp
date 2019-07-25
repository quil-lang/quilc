;;;; printers.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; stuff responsible for writing quil out to something readable / processable
;;;; all goes in here.

(in-package #:quilc)


(declaim (special *without-pretty-printing*
                  *human-readable-stream*))

(defun print-matrix-comparision (m1 m2 &optional (stream *human-readable-stream*))
  (format stream "~%#Matrix read off from input code~%")
  (print-matrix-with-comment-hashes m1 stream)
  (format stream "~%#Matrix read off from compiled code~%")
  (print-matrix-with-comment-hashes m2 stream)
  (format stream "~%#Matrices are~:[ not~;~] equal~%" (quil::matrix-equals-dwim m1 m2))
  (finish-output stream))

(defun print-program (processed-program &optional (stream *standard-output*))
  (let ((program-as-string
          (with-output-to-string (s)
            (let ((quil::*print-fractional-radians* (not *without-pretty-printing*)))
              (quil::print-parsed-program processed-program s)))))
    (if stream
        (write-string program-as-string stream)
        program-as-string)))

;; custom encoder for rewiring objects
(defmethod yason:encode ((object quil::rewiring) &optional (stream *standard-output*))
  (yason:encode (quil::rewiring-l2p object) stream))

(defun print-statistics (statistics &optional (stream *human-readable-stream*))
  (format stream
          "~@[# Compiled gate depth: ~D~%~]"
          (gethash "gate_depth" statistics))
  (format stream
          "~@[# Compiled multiqubit gate depth: ~D~%~]"
          (gethash "multiqubit_gate_depth" statistics))
  (format stream
          "~@[# Compiled gate volume: ~D~%~]"
          (gethash "gate_volume" statistics))
  (format stream
          "~@[# Unused qubit list: ~{~a~^, ~}~%~]"
          (gethash "unused_qubits" statistics))
  (format stream
          "~@[# Estimated compiled program fidelity: ~5d~%~]"
          (gethash "program_fidelity" statistics))
  (format stream
          "~@[# SWAPs incurred by topological considerations: ~d~%~]"
          (gethash "topological_swaps" statistics))
  (a:when-let ((duration (gethash "program_duration" statistics)))
    (let* ((duration-picos (* duration 1000))
           (seconds  (floor duration 1000000000))
           (millis   (floor duration 1000000))
           (micros   (floor duration 1000))
           (nanos    (floor duration 1))
           (picos    (mod duration-picos 1000)))
      (format stream
              "# Compiled program duration: ~4Ds ~4Dms ~4Dus ~4Dns ~4Dps    (= ~a ps total)~%"
              seconds millis micros nanos picos duration-picos))))
