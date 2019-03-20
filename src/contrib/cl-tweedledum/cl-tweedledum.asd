;; Adapted from magicl's magicl-transcendental adapted from commonqt's
;; qt.asd.
(defclass c->so (asdf:source-file)
  ()
  (:default-initargs
   :type "c"))

(defmethod output-files ((operation compile-op) (component c->so))
  (values (list (make-pathname :name "libtweedledum"
                               :type #-darwin "so" #+darwin "dylib"
                               :defaults (component-pathname component)))
          t))

(defmethod perform ((operation load-op) (component c->so))
  t)

(defmethod perform ((operation compile-op) (component c->so))
  (flet ((nn (x) (uiop:native-namestring x)))
    (let* ((c-file (component-pathname component))
           (shared-object (make-pathname :type #+darwin "dylib" #-darwin "so"
                                         :name "libtweedledum"
                                         :defaults c-file))
           (dir (namestring (make-pathname :directory (pathname-directory c-file)))))
      (uiop:run-program
       ;; TODO This needs to be platform agnostic. The difficulty is
       ;; in picking up a c++17 compiler automatically.
       (list "/usr/local/opt/llvm/bin/clang++"
             "-L/usr/local/opt/llvm/lib"
             "-shared"
             "-fPIC"
             "-std=c++17"
             "-Wl,-rpath,/usr/local/opt/llvm/lib"
             "-DFMT_HEADER_ONLY"
             (format nil "-I~a/tweedledum/libs/fmt" dir)
             (format nil "-I~a/tweedledum/libs/easy" dir)
             (format nil "-I~a/tweedledum/libs/glucose" dir)
             (format nil "-I~a/tweedledum/libs/kitty" dir)
             (format nil "-I~a/tweedledum/include" dir)
             "-o" (nn shared-object)
             (nn c-file))))))

(asdf:defsystem #:cl-tweedledum
  :license "BSD 3-Clause (See LICENSE.txt)"
  :maintainer "Rigetti Computing"
  :author "Rigetti Computing"
  :description "C++17 Library for writing, manipulating, and optimizing quantum circuits"
  :serial t
  :components
  ((c->so "tweedledum")
   (:file "package")
   (:file "load-libs")
   (:file "cl-tweedledum")))
