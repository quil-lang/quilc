;;;; processors.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:boondoggle)

(defclass processor ()
  ())

(defgeneric apply-process (processor &rest data)
  (:documentation "Applies the transformation embodied by PROCESSOR to DATA."))

(defclass processor-quilc (processor)
  ((executable-path :initform ""
                    :initarg :executable-path
                    :reader processor-quilc-executable-path)
   (flags :initform ""
          :initarg :flags
          :reader processor-quilc-flags)))

(defmethod apply-process ((processor processor-quilc) &rest data)
  (let ((data (first data)))
    (quil::parse-quil
     (uiop:run-program (processor-quilc-executable-path processor)
                       :input (make-string-input-stream
                               (with-output-to-string (s)
                                 (quil::print-parsed-program data s)))
                       :output :string))))

(defclass processor-identity (processor)
  ())

(defmethod apply-process ((processor processor-identity) &rest data)
  (first data))

(defclass processor-measures (processor)
  ((chip-specification :initform nil
                       :initarg :chip-specification
                       :reader processor-measures-chip-specification)))

(defun measure-at-close-instrs (chip-specification)
  (loop :for j :below (length (elt (quil::chip-specification-objects chip-specification) 0))
        :collect (make-instance 'quil::measure
                                :address (quil:mref "ro" j)
                                :qubit (quil::qubit j))))

(defmethod apply-process ((processor processor-measures) &rest data)
  (let ((data (quil::copy-instance (first data))))
    (setf (quil::parsed-program-executable-code data)
          (concatenate 'vector
                       (quil::parsed-program-executable-code data)
                       (measure-at-close-instrs
                        (processor-measures-chip-specification processor))))
    data))

(defclass processor-L1-distance (processor)
  ())

(defmethod apply-process ((processor processor-L1-distance) &rest data)
  (reduce #'+ (mapcar #'abs (mapcar #'- (first data) (second data)))))
