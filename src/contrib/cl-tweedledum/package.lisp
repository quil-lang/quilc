(defpackage #:cl-tweedledum.foreign-libraries
  (:use #:common-lisp)
  (:export #:libtweedledum))

(defpackage #:cl-tweedledum.cffi
  (:use))

(defpackage #:cl-tweedledum
  (:use #:common-lisp
        #:cffi
        #:cl-quil)
  (:export #:synthesis-dbs))
