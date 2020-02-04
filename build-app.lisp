;;;; build-app.lisp
;;;;
;;;; This file is loaded by the Makefile to produce a quilc[.exe] binary.
;;;;

(unless *load-truename*
  (error "This file is meant to be loaded."))

#+forest-sdk (pushnew :drakma-no-ssl *features*)

(require 'asdf)

(let ((*default-pathname-defaults* (make-pathname :type nil
                                                  :name nil
                                                  :defaults *load-truename*))
      (output-file (make-pathname :name "quilc"
                                  :type #+win32 "exe" #-win32 nil))
      (system-table (make-hash-table :test 'equal))
      (entry-point "quilc::entry-point"))
  (flet ((option-present-p (name)
           (find name sb-ext:*posix-argv* :test 'string=))
         (make-toplevel-function (entry)
           (lambda ()
             (with-simple-restart (abort "Abort")
               (funcall (read-from-string entry)
                        sb-ext:*posix-argv*))))
         (load-systems-table ()
           (unless (probe-file "system-index.txt")
             (error "Generate system-index.txt with 'make system-index.txt' first."))
           (setf (gethash "quilc" system-table) (merge-pathnames "quilc.asd"))
           (with-open-file (stream "system-index.txt")
             (loop
               :for system-file := (read-line stream nil)
               :while system-file
               :do (setf (gethash (pathname-name system-file) system-table)
                         (merge-pathnames system-file)))))
         (local-system-search (name)
           (values (gethash name system-table)))
         (strip-version-githash (version)
           (subseq version 0 (position #\- version :test #'eql))))
    (load-systems-table)
    (push #'local-system-search asdf:*system-definition-search-functions*)
    (asdf:load-system "quilc")
    ;; TODO Fix tweedledum
    ;; #-win32
    ;; (asdf:load-system "cl-quil/tweedledum")
    ;; TODO Something is broken here. If zap-info is left to do it's thing on
    ;; Windows or SBCL 1.5.6+, there is a weird error. This is a short-term fix.
    #-win32
    (when (uiop:version< (strip-version-githash (lisp-implementation-version)) "1.5.6")
      (funcall (read-from-string "quilc::zap-info")))
    (funcall (read-from-string "quilc::setup-debugger"))
    (when (option-present-p "--quilc-sdk")
      (load "app/src/mangle-shared-objects.lisp"))
    (when (option-present-p "--unsafe")
      (format t "~&Using unsafe entry point~%")
      (setf entry-point "quilc::%entry-point"))
    (force-output)
    (sb-ext:save-lisp-and-die output-file
                              :compression #+sb-core-compression t
                                           #-sb-core-compression nil
                              :save-runtime-options t
                              :executable t
                              :toplevel (make-toplevel-function entry-point))))
