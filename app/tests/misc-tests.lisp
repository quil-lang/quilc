;;;; misc.lisp
;;;;
;;;; Author: Mark Skilbeck

(in-package #:quilc-tests)

(deftest test-update-available ()
  (is (quilc::sdk-update-available-p "0.0.0")))
