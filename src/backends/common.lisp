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

(defgeneric backend-name (backend-class)
  (:documentation "The user-accessible name for BACKEND-CLASS.")
  (:method ((backend backend))
    (backend-name (class-name (class-of backend)))))

(defgeneric backend-supports-chip-p (backend chip)
  (:documentation "Does BACKEND support the chip CHIP-SPECIFICATION?"))

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

(defgeneric write-executable (executable stream)
  (:documentation "Write EXECUTABLE to the binary output STREAM."))


;;; The COMPILATION protocol

(defgeneric backend-compile (program chip-spec backend)
  (:documentation "Compile the PROGRAM which comports to the CHIP-SPEC to an executable for BACKEND."))


;;;;;;;;;;;;;;; Backend Construction Library Functions ;;;;;;;;;;;;;;;

;;; The things below aren't part of the protocol, but generally useful.


(defun list-available-backends ()
  "Return a list of all subclasses of the class named BACKEND"
  (let ((backends nil))
    (labels ((add-backends (class)
               (let ((direct
                       (c2mop:class-direct-subclasses class)))
                 (when direct
                   (setf backends (append backends direct))
                   (dolist (cl direct)
                     (add-backends cl))))))
      (add-backends (find-class 'backend))
      (mapcar #'class-name (remove-duplicates backends :test #'eq)))))

(defun find-backend (backend-name)
  "Returns the backend class associated with the string BACKEND-NAME."
  (find backend-name (list-available-backends) :test #'string= :key #'backend-name))
