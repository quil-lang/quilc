;;;; versions.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:quilc)

;;; We compile this stuff earlier so it's available everywhere.

;; load and store bits of version information at compile time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun system-version (system-designator)
    (let ((sys (asdf:find-system system-designator nil)))
      (if (and sys (slot-boundp sys 'asdf:version))
          (asdf:component-version sys)
          "unknown")))

  (defun git-hash (system)
    "Get the short git hash of the system SYSTEM."
    (let ((sys-path (namestring (asdf:system-source-directory system))))
      (multiple-value-bind (output err-output status)
          (uiop:run-program `("git" "-C" ,sys-path "rev-parse" "--short" "HEAD")
                            :output '(:string :stripped t)
                            :ignore-error-status t)
        (declare (ignore err-output))
        (if (not (zerop status))
            "unknown"
            output)))))

(alexandria:define-constant +QUILC-VERSION+
    (system-version '#:quilc)
  :test #'string=
  :documentation "The version of the quilc application.")

(alexandria:define-constant +GIT-HASH+
    (git-hash '#:quilc)
  :test #'string=
  :documentation "The git hash of the quilc repo.")

(defun latest-sdk-version (&key (proxy nil))
  "Get the latest SDK quilc version, or NIL if unavailable."
  (handler-case
      (let* ((s (drakma:http-request "http://downloads.rigetti.com/qcs-sdk/version"
                                     :want-stream t
                                     :proxy proxy))
             (p (yason:parse s)))
        (multiple-value-bind (version success)
            (gethash "quilc" p)
          (when success
            version)))
    (usocket:ns-error (condition)
      (cl-syslog:rfc-log (*logger* :warning "Encountered a name resolution error when fetching latest SDK version. (~A)" condition)
        (:msgid "LOG0001"))
      nil)
    (usocket:socket-error (condition)
      (cl-syslog:rfc-log (*logger* :warning "Encountered a socket error when fetching latest SDK version. (~A)" condition)
        (:msgid "LOG0001"))
      nil)
    (sb-bsd-sockets:socket-error (condition)
      (cl-syslog:rfc-log (*logger* :warning "Encountered a socket error when fetching latest SDK version. (~A)" condition)
        (:msgid "LOG0001"))
      nil)))

(defun sdk-update-available-p (current-version &key (proxy nil))
  "Test whether the current SDK version is the latest SDK
version. Second value returned indicates the latest version."
  (let ((latest (latest-sdk-version)))
    (values (and latest (uiop:version< current-version latest))
            latest)))
