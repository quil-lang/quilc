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

(a:define-constant +QUILC-VERSION+
    (system-version '#:quilc)
  :test #'string=
  :documentation "The version of the quilc application.")

(a:define-constant +GIT-HASH+
    (git-hash '#:quilc)
  :test #'string=
  :documentation "The git hash of the quilc repo.")

(declaim (special *logger*))

(defun query-latest-sdk-version (&key (proxy nil))
  "Get the latest SDK quilc version, or NIL if unavailable."
  (handler-case
      (let* ((s (drakma:http-request
                 (format nil "http://downloads.rigetti.com/qcs-sdk/versions?quilc=~A"
                         +QUILC-VERSION+)
                 :want-stream t
                 :proxy proxy))
             (p (yason:parse s)))
        (multiple-value-bind (version success)
            (gethash "quilc" (gethash "latest" p))
          (when success
            version)))
    (usocket:ns-error (condition)
      (cl-syslog:rfc-log (*logger* :warning "Encountered a name resolution error when fetching latest SDK version. (~A)" condition)
        (:msgid "LOG0000"))
      nil)
    (usocket:socket-error (condition)
      (cl-syslog:rfc-log (*logger* :warning "Encountered a socket error when fetching latest SDK version. (~A)" condition)
        (:msgid "LOG0000"))
      nil)
    (usocket:ns-try-again-condition (condition)
      (cl-syslog:rfc-log (*logger* :warning "Encountered EAGAIN when fetching latest SDK version. Not retrying; just ignoring. (~A)" condition)
        (:msgid "LOG0000"))
      nil)
    (usocket:ns-condition (condition)
      (cl-syslog:rfc-log (*logger* :warning "Encountered a presumably benign socket condition that prohibits fetching latest SDK version. Just ignoring. (~A)" condition)
        (:msgid "LOG0000"))
      nil)
    (error (condition)
      (cl-syslog:rfc-log (*logger* :warning "Encountered an error when fetching latest SDK version. (~A)" condition)
        (:msgid "LOG0000"))
      nil)))

(defun sdk-update-available-p (current-version &key (proxy nil))
  "Test whether the current SDK version is the latest SDK
version. Second value returned indicates the latest version."
  (let ((latest (query-latest-sdk-version :proxy proxy)))
    (values (and latest (uiop:version< current-version latest))
            latest)))

(defun asynchronously-indicate-update-availability (current-version &key (proxy nil))
  "Write to the logger the state of the software version (whether it's the latest, if there's an update, if an update couldn't be queried)."
  (bt:make-thread
   (lambda ()
     (multiple-value-bind (available? latest) (sdk-update-available-p current-version :proxy proxy)
       (cond
         ((null latest)
          ;; There was some kind of issue getting the version and a warning was already emitted.
          )
         (available?
          (cl-syslog:rfc-log (*logger* :notice "An update is available to the SDK. You have version ~A. ~
Version ~A is available from https://qcs.rigetti.com/sdk-downloads~%"
                                       +QUILC-VERSION+ latest)
            (:msgid "LOG0001"))))))
   :name "Version Check"))
