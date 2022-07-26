;;;; printers.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; stuff responsible for writing quil out to something readable / processable
;;;; all goes in here.

(in-package #:quilc)


(declaim (special *without-pretty-printing*
                  *human-readable-stream*))

(defun print-matrix-comparision (m1 m2 &key (stream *human-readable-stream*)
                                         (tolerance nil))
  "Given to matrices M1 and M2, print to a STREAM their entries and whether they
are numerically unitary. If TOLERANCE is nil, check if if they are equal and
error if they are not in the same projective class. If TOLERANCE is t, instead
print out their `global-phase-invariant-distance' with no error."
  (format stream "~%#Matrix read off from input code (~:[not ~;~]unitary)~%"
          (magicl:unitary-matrix-p m1))
  (print-matrix-with-comment-hashes m1 stream)
  (format stream "~%#Matrix read off from compiled code (~:[not ~;~]unitary)~%"
          (magicl:unitary-matrix-p m2))
  (print-matrix-with-comment-hashes m2 stream)
  (if tolerance
      (format stream "~%#Matrices have a global phase invariant distance of ~E~%"
              (cl-quil::global-phase-invariant-distance m1 m2))
      (format stream "~%#Matrices are~:[ not~;~] equal~%"
              (cl-quil::matrix-equals-dwim m1 m2)))
  (finish-output stream))

(defun print-program (processed-program &optional (stream *standard-output*))
  (let ((program-as-string
          (with-output-to-string (s)
            (let ((cl-quil::*print-fractional-radians* (not *without-pretty-printing*)))
              (cl-quil::print-parsed-program processed-program s)))))
    (if stream
        (write-string program-as-string stream)
        program-as-string)))

;; custom encoder for rewiring objects
(defmethod yason:encode ((object cl-quil::rewiring) &optional (stream *standard-output*))
  (yason:encode (cl-quil::rewiring-l2p object) stream))

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
          "~@[# Unused qubit list: ~{~A~^, ~}~%~]"
          (gethash "unused_qubits" statistics))
  (format stream
          "~@[# Estimated compiled program fidelity: ~5D~%~]"
          (gethash "program_fidelity" statistics))
  (format stream
          "~@[# SWAPs incurred by topological considerations: ~D~%~]"
          (gethash "topological_swaps" statistics))
  (a:when-let ((duration (gethash "program_duration" statistics)))
    (let* ((duration-picos (* duration 1000))
           (seconds  (floor duration 1000000000))
           (millis   (floor duration 1000000))
           (micros   (floor duration 1000))
           (nanos    (floor duration 1))
           (picos    (mod duration-picos 1000)))
      (format stream
              "# Compiled program duration: ~4Ds ~4Dms ~4Dus ~4Dns ~4Dps    (= ~A ps total)~%"
              seconds millis micros nanos picos duration-picos))))
