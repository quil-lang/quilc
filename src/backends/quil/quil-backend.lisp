;;;; quil-backend.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

;;; The BACKEND protocol

(defclass quil-backend (backend)
  ((print-circuit-definitions :initarg :print-circuit-definitions
                              :initform nil
                              :reader print-circuit-definitions))
  (:documentation "A backend the emits plain old Quil. In some sense, this backend does nothing."))

(defmethod backend-name ((backend-class (eql 'quil-backend)))
  "quil")

(defmethod backend-supports-chip-p ((backend quil-backend) chip)
  (declare (ignore backend chip))
  t)

;;; The EXECUTABLE protocol

(defclass quil-executable ()
  ((utf8-quil-output :initarg :utf8-quil-output
                     :type (array (unsigned-byte 8) (*))
                     :reader utf8-quil-output)))

(defmethod write-executable ((executable quil-executable) stream)
  (write-sequence (utf8-quil-output executable) stream))

;;; The COMPILATION protocol

(defmethod backend-compile (program chip-spec (backend quil-backend))
  (declare (ignore chip-spec))
  (labels ((print-program-to-stream (stream)
             ;; If we want circuit definitons, keep them. Otherwise
             ;; delete so they don't get printed.
             (unless (print-circuit-definitions backend)
               (setf (parsed-program-circuit-definitions program) nil))

             ;; TODO: When statistics are added to backends, we should
             ;; print them here.

             (print-parsed-program program stream)))
    
    (make-instance 'quil-executable
                   :utf8-quil-output
                   (flexi-streams:with-output-to-sequence (stream :element-type '(unsigned-byte 8))
                     (let ((stream (flexi-streams:make-flexi-stream stream :external-format ':UTF-8)))
                       (print-program-to-stream stream))))))
