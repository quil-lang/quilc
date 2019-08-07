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

(defclass processor-L1-distance (processor)
  ())

(defmethod apply-process ((processor processor-L1-distance) &rest data)
  "Calculate the L1 distance between two arguments. &rest expects only 2 position arguments, each of which is expected to be a normalized histogram count of bitstring outputs from the QVM for a given quil program."
  (reduce #'+ (mapcar #'abs (mapcar #'- (first data) (second data)))))
