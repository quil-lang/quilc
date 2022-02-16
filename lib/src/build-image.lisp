(require '#:asdf)

(asdf:load-system '#:sbcl-librarian)
(asdf:load-system '#:libquilc)

(in-package #:libquilc)

(sbcl-librarian:define-aggregate-library libquilc (:function-linkage "QUILC_API")
  quilc sbcl-librarian:handles)

(sbcl-librarian:build-bindings libquilc ".")
(sbcl-librarian:build-python-bindings libquilc ".")
(sbcl-librarian:build-core-and-die libquilc ".")
