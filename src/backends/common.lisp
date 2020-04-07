;;;; common.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

;;; The BACKEND protocol
;;;
;;; The BACKEND class is used primarily to determine available
;;; backends primarily by computing subclasses.

(defclass backend ()
  ()
  (:documentation "Every backend must be represented by a subclass of this abstract base class.")
  (:metaclass abstract-class))

(defgeneric backend-supports-chip-p (backend chip)
  (:documentation "Does BACKEND support the chip CHIP-SPECIFICATION?"))

(define-condition incompatible-memory-model-error (error)
  ((backend :initarg :backend
            :reader incompatible-memory-model-error-backend)
   (descriptors :initarg :descriptors
                :reader incompatible-memory-model-error-descriptors))
  (:documentation "A condition signaled if a set of descriptors is incompatible with a backend."))

;;; TODO: Should we add anything to the BACKEND protocol to deal with
;;; memory allocation, the memory model (cf. classical-memory.lisp),
;;; etc.?

;;; The EXECUTABLE protocol
;;;
;;; An executable is an artifact which may be written to a binary
;;; output stream. (The executable may itself represent characters, of
;;; course, but we force implementers of this protocol to select an
;;; encoding.)
;;;
;;; Multi-file executables are expected to be managed by the
;;; implementer with e.g. tarballs.
;;;
;;; Many of the functioms below deal with signing the executable,
;;; which verifies the identity of the creator of the executable.
;;;

(defgeneric write-executable (executable stream)
  (:documentation "Write EXECUTABLE to the binary output STREAM."))

(defgeneric signature (executable)
  (:documentation "The signature of the executable, or NIL of not present."))

(defgeneric (setf signature) (executable new-signature)
  (:documentation "Set the signature of the executbale."))

(defgeneric sign-executable (executable private-key)
  (:documentation "Sign the EXECUTABLE with PRIVATE-KEY. This should set the signature with (SETF SIGNATURE)."))

(defgeneric verify-signature (executable public-key)
  (:documentation "Verify the EXECUTABLE was signed by the owner of PUBLIC-KEY."))

;;; The COMPILATION protocol

(defgeneric compile-to-backend (program chip-spec backend)
  (:documentation "Compile the PROGRAM which comports to the CHIP-SPEC to an executable for BACKEND."))

;;;;;;;;;;;;;;; Backend Construction Library Functions ;;;;;;;;;;;;;;;

;;; The things below aren't part of the protocol, but generally useful.

(defun list-available-backends ()
  ;; This is simpleminded and assumes there isn't some crazy
  ;; inheritance structure.
  (mapcar #'class-name (c2mop:class-direct-subclasses (find-class 'backend))))

(defconstant +binary-hash-size+ 64)

(defun binary-hash (binary)
  (ironclad:digest-sequence ':sha3 binary))

(defconstant +rsa-recommended-size-bits+ 3072)

(defun generate-rsa-key-pair (&optional (num-bits +rsa-recommended-size-bits+))
  (ironclad:generate-key-pair ':rsa :num-bits +rsa-recommended-size-bits+))
