;;;; mangle-shared-objects.lisp
;;;;
;;;; This is loaded (as with CL:LOAD) before the final image is saved
;;;; by buildapp.
;;;;
;;;; Rewrites shared library references to libblas.dylib and
;;;; liblapack.dylib on Mac SDK targets to use the Rigetti package
;;;; path /usr/local/lib/rigetti. This is so the exact
;;;; Rigetti-provided versions of these libraries are found when the
;;;; SDK binary starts up. If these were not rewritten, the SDK binary
;;;; could try to load from non-existent Brew paths or inadequate
;;;; Accelerate-related system paths.
;;;;
;;;; Has no effect on non-Mac targets.

(in-package #:cl-user)

(dolist (shared-object sb-sys:*shared-objects*)
  (let ((dylibs-to-replace '("libblas.dylib"
                             "liblapack.dylib"))
        (original-path (sb-alien::shared-object-pathname shared-object)))
    (let ((dylib (first (member (file-namestring original-path)
                                dylibs-to-replace
                                :test 'string-equal))))
      (when dylib
        (let ((new-path
               (merge-pathnames dylib "/usr/local/lib/rigetti/")))
          (format *trace-output* ";;; Rewriting ~A to ~A~%"
                  original-path new-path)
          (setf (sb-alien::shared-object-pathname shared-object)
                new-path
                (sb-alien::shared-object-namestring shared-object)
                (sb-ext:native-namestring new-path)))))))


