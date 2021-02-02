;;;; build-app.lisp
;;;;
;;;; This file is loaded by the Makefile to produce a quilc[.exe] binary.
;;;;

(unless *load-truename*
  (error "This file is meant to be loaded."))

#+forest-sdk (pushnew :drakma-no-ssl *features*)

(require 'asdf)

(defun unload-libraries ()
  "Unloads foreign libraries using CFFI and returns a list of the closed libraries"
  ;; A dumped lisp image will use the *exact* foreign library name
  ;; that was provided at the time the image was dumped. If the
  ;; library is moved/renamed, CFFI will not try to load an
  ;; alternative, thus we unload all foreign libraries and tell CFFI
  ;; to load them at run-time allowing it to pick up alternatives.
  ;;
  ;; For a concrete example: when we build the SDK packages for
  ;; debian, the FFI library available is typically libffi.so.6. On
  ;; newer versions of debian the library is named libffi.so.7, so
  ;; when we try to load the lisp image on a newer debian version, it
  ;; fails with an error about not finding libffi.so.6.
  (let ((foreign-libraries
          (mapcar (lambda (library) (funcall (find-symbol "FOREIGN-LIBRARY-NAME" :cffi) library))
                  (funcall (find-symbol "LIST-FOREIGN-LIBRARIES" :cffi) :loaded-only t))))
    (map nil (find-symbol "CLOSE-FOREIGN-LIBRARY" :cffi) foreign-libraries)
    foreign-libraries))

(defun load-libraries (libraries)
  "Loads all foreign libraries in LIBRARIES"
  (pushnew #P"/usr/local/lib/rigetti/" (symbol-value (find-symbol "*foreign-library-directories*" :cffi))
           :test #'equal)
  (map nil (find-symbol "LOAD-FOREIGN-LIBRARY" :cffi) libraries)
  nil)

(defun option-present-p (name)
  (find name sb-ext:*posix-argv* :test 'string=))

(let ((*default-pathname-defaults* (make-pathname :type nil
                                                  :name nil
                                                  :defaults *load-truename*))
      (output-file (make-pathname :name "quilc"
                                  :type #+win32 "exe" #-win32 nil))
      (system-table (make-hash-table :test 'equal))
      (entry-point "ENTRY-POINT")
      (libraries nil))
  (labels ((load-systems-table ()
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

    ;; Load systems defined by environment variable $ASDF_SYSTEMS_TO_LOAD,
    ;; or if that does not exist just load quilc
    (let ((systems (uiop:getenv "ASDF_SYSTEMS_TO_LOAD")))
      (dolist (sys (uiop:split-string (or systems "quilc") :separator " "))
        (unless (uiop:emptyp sys)
          (asdf:load-system sys))))
    ;; TODO Fix tweedledum
    ;; #-win32
    ;; (asdf:load-system "cl-quil/tweedledum")
    (funcall (find-symbol "SETUP-DEBUGGER" :quilc))
    (when (option-present-p "--quilc-sdk")
      (load "app/src/mangle-shared-objects.lisp"))
    (when (option-present-p "--unsafe")
      (format t "~&Using unsafe entry point~%")
      (setf entry-point "%ENTRY-POINT"))
    (force-output)
    (setf libraries (unload-libraries))
    (sb-ext:save-lisp-and-die output-file
                              :compression #+sb-core-compression t
                              #-sb-core-compression nil
                              :save-runtime-options t
                              :executable t
                              :toplevel
                              (let ((entry-sym (find-symbol entry-point :quilc)))
                                (lambda ()
                                  (load-libraries libraries)
                                  (with-simple-restart (abort "Abort")
                                    (funcall entry-sym
                                             sb-ext:*posix-argv*)))))))
