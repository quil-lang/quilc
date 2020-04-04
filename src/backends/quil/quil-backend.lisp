;;;; quil-backend.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

;;; The BACKEND protocol

(defclass quil-backend (backend)
  ()
  (:metaclass singleton-class)
  (:documentation "A backend the emits plain old Quil. In some sense, this backend does nothing."))

(defmethod backend-supports-chip-p ((backend quil-backend) chip)
  (declare (ignore backend chip))
  t)

;;; The EXECUTABLE protocol

(defclass quil-executable ()
  ((utf8-bytes :initarg :utf8-bytes
               :type (array (unsigned-byte 8) (*))
               :reader utf8-bytes)
   (signature :accessor signature ; This implements SIGNATURE
              :initform nil)))    ; and (SETF SIGNATURE).


(defmethod write-executable ((executable quil-executable) stream)
  (let ((stream (flexi-streams:make-flexi-stream stream :external-format ':UTF-8)))
    ;; Write the signature as a comment.
    (alexandria:when-let ((sig (signature executable)))
      (write-string "# Signature: " stream)
      (loop :for byte :across sig :do (format stream "~2,'0X " byte))
      (terpri stream))
    (loop :for byte :across (utf8-bytes executable)
          :do (write-byte byte stream))))

(defmethod sign-executable ((executable quil-executable) (private-key ironclad::rsa-private-key))
  (setf (signature executable)
        (ironclad:sign-message private-key (binary-hash (utf8-bytes executable)))))

(defmethod verify-signature ((executable quil-executable) (public-key ironclad::rsa-public-key))
  (ironclad:verify-signature public-key
                             (binary-hash (utf8-bytes executable))
                             (signature executable)))

;;; The COMPILATION protocol

(defmethod compile-to-backend (program chip-spec (backend quil-backend))
  (declare (ignore chip-spec))
  (make-instance 'quil-executable
                 :utf8-bytes
                 (flexi-streams:with-output-to-sequence (stream :element-type '(unsigned-byte 8))
                   (let ((stream (flexi-streams:make-flexi-stream stream :external-format ':UTF-8)))
                     (print-parsed-program program stream)))))
