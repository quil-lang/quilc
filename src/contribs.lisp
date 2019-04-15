(in-package :cl-quil)

(defvar *contribs* (make-hash-table)
  "List of loaded contrib modules.")

(defclass contrib ()
  ())

(defgeneric contrib-load (contrib-name)
  (:documentation "Perform run-time loading of the module"))

(defgeneric contrib-unload (contrib-name)
  (:documentation "Perform any run-time un-loading of the module"))
