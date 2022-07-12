;;;; src/chip-library/chip-table.lisp
;;;;
;;;; Author: A.J. Nyquist

(in-package cl-quil/chip-library)

;;; Mutable table that stores functions for defining chips

(defvar *chip-table*
  (make-hash-table :test 'equalp)
  "Table that match hashed keywords to functions that build chips.")

(defun available-chips ()
  "Returns a list of chip builder string-keys available."
  (a:hash-table-keys *chip-table*))

(defun get-chip-builder (chip)
  "Returns a chip building function with the case-insensitive string-key CHIP."
  (declare (type string chip))
  (nth-value 0 (gethash chip *chip-table*)))

(defun install-chip-builder (chip func &key (no-warn nil))
  "Stores the chip building function FUNC with the string-key CHIP, if CHIP is
already utilized return t and unless NO-WARN signal a warning."
  (declare (type string chip))
  (when (shiftf (gethash chip *chip-table*) func)
    (unless no-warn
      (a:simple-style-warning "Overwriting existing chip ~A" chip))
    t))

(defun call-chip-builder (chip &rest args)
  "Calls the chip building function associated with CHIP while passing ARGS, or
returns nil if not found."
  (a:when-let ((chip-builder (get-chip-builder chip)))
    (apply chip-builder args)))

;; Install default chips

(install-chip-builder "8Q" 'q::build-8Q-chip)
(install-chip-builder "20Q" (lambda () (q::build-skew-rectangular-chip 0 4 5)))
(install-chip-builder "16QMUX" 'q::build-16QMUX-chip)
(install-chip-builder "bristlecone" 'q::build-bristlecone-chip)
(install-chip-builder "ibmqx5" 'q::build-ibm-qx5)
