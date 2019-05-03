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

(defun version-string-values (version &key (delimiter #\.))
  (check-type version string)
  (split-sequence:split-sequence delimiter version))

(defun version> (version-a version-b &key (test #'>) (key #'parse-integer))
  "Test whether VERSION-A is \"greater than\" VERSION-B, where both
are version strings with components \"major.minor.patch\". Comparison
is made left-to-right component-wise with the binary predicate TEST on
the result of applying KEY to each component, terminating with the
first non-nil result."
  (check-type version-a string)
  (check-type version-b string)
  (loop :for a :in (mapcar key (version-string-values version-a))
        :for b :in (mapcar key (version-string-values version-b))
        :when (funcall test a b) :do
          (return-from version> t)))

(defun latest-sdk-version ()
  "Get the latest SDK quilc version, or NIL if unavailable."
  (handler-case
      (let* ((s (drakma:http-request "http://downloads.rigetti.com/qcs-sdk/version"
                                     :want-stream t))
             (p (yason:parse s)))
        (multiple-value-bind (version success)
            (gethash "quilc" p)
          (when success
            version)))
    (usocket:ns-host-not-found-error (condition)
      (declare (ignore condition))
      nil)))

(defun sdk-update-available-p (current-version)
  "Test whether the current SDK version is the latest SDK
version. Second value returned indicates the latest version."
  (let ((latest (latest-sdk-version)))
    (values (and latest (version> latest current-version))
            latest)))
