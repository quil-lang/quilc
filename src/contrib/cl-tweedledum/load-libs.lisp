(in-package #:cl-tweedledum.foreign-libraries)

(cffi:define-foreign-library libtweedledum
  (:darwin (:or #.(merge-pathnames "libtweedledum.dylib"
                                   (or *compile-file-truename*
                                       *load-truename*))
                "libtweedledum.dylib"
                "tweedledum.dylib"))
  (:unix  (:or #.(merge-pathnames "libtweedledum.so"
                                  (or *compile-file-truename*
                                      *load-truename*))
               "libtweedledum.so"
               "tweedledum.so"))

  (t (:default "libtweedledum")))

(defvar *tweedledum-libs-loaded* nil)

(unless *tweedledum-libs-loaded*
  (cffi:load-foreign-library 'libtweedledum)
  (setf *tweedledum-libs-loaded* t))
