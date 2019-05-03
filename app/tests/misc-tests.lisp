;;;; misc.lisp
;;;;
;;;; Author: Mark Skilbeck

(in-package #:quilc-tests)

(deftest test-update-available ()
  (multiple-value-bind (update-available-p update)
      (quilc::sdk-update-available-p "0.0.0")
    (if update-available-p
        ;; If the network is down, then update-available-p is NIL, but
        ;; we don't want to error in that case. Skip instead.
        (is update)
        (skip)))

  (with-mocked-function-definitions
      ((quilc::latest-sdk-version (lambda () "1.0.0")))
    (multiple-value-bind (update-available-p update)
        (quilc::sdk-update-available-p "1.5.0")
      (is (not update-available-p)))))
