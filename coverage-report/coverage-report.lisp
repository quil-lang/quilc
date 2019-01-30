;;;; coverage-report.lisp
;;;;
;;;; Author: Zach Beane

(require :sb-cover)

(defvar *system* "cl-quil")

(defun system-lisp-files (system)
  (unless (typep system 'asdf:system)
    (setf system (asdf:find-system system)))
  (let ((result '()))
    (labels ((explore (thing)
               (typecase thing
                 (asdf:parent-component
                  (mapc #'explore (asdf:component-children thing)))
                 (asdf:cl-source-file
                  (push (namestring (asdf:component-pathname thing)) result)))))
      (explore system)
      result)))

#-quicklisp
(load "~/quicklisp/setup.lisp")

(format *query-io* "Compiling and loading ~A...~%" *system*)

;; Compile system and prerequisites outside of coverage
ql-dist::(ensure-installed (release "cffi"))
ql-dist::(ensure-installed (release "fiasco"))
(ql:quickload *system* :silent t)

(declaim (optimize sb-cover:store-coverage-data))
(asdf:load-system *system* :force t)
(format *query-io* "done~%")

(let ((*compile-verbose* nil)
      (*load-verbose* nil))
  (asdf:test-system *system*))

(handler-bind ((warning #'muffle-warning))
  (let ((interesting-files (system-lisp-files *system*))
        (base (asdf:system-relative-pathname *system*
                                             "coverage-report/html/")))
    (sb-cover:report base
                     :if-matches (lambda (file) (member file interesting-files :test 'string=)))
    (let* ((cover (merge-pathnames "cover-index.html" base))
           (index (merge-pathnames "index.html" base)))
      (rename-file cover index)
      (format *query-io* "Coverage report written to ~A~%"
              index))))

