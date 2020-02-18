(in-package #:quilc)

(defparameter *program-name* "quilc")
(defparameter *without-pretty-printing* nil)
(defparameter *log-level* ':info)
(defparameter *protoquil* nil)

;; NOTE: these can't have default values b/c they don't survive serialization
(defparameter *human-readable-stream* (make-broadcast-stream))
(defparameter *quil-stream* (make-broadcast-stream))

(defparameter *logger* (make-instance 'cl-syslog:rfc5424-logger
                                      :app-name "quilc"
                                      :facility ':local0
                                      :maximum-priority ':info
                                      :log-writer (cl-syslog:null-log-writer)))
