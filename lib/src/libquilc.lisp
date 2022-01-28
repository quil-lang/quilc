(require '#:asdf)

(asdf:load-system '#:quilc)
(asdf:load-system '#:sbcl-librarian)

(in-package #:quilc)

(sbcl-librarian:define-handle-type quil-program "quil_program")
(sbcl-librarian:define-handle-type chip-specification "chip_specification")

(sbcl-librarian:define-enum-type error-type "error_t"
  ("ERROR_SUCCESS" 0)
  ("ERROR_FAIL" 1))
(sbcl-librarian:define-error-map error-map error-type 0
  (t (condition) (declare (ignore condition)) 1))

(defun compile-protoquil (parsed-program chip-specification)
  (compiler-hook parsed-program chip-specification :protoquil t))

(sbcl-librarian:define-library libquilc (:error-map error-map
                                         :function-linkage "QUILC_API"
                                         :function-prefix "quilc_")
  (:literal "/* types */")
  (:type quil-program chip-specification error-type)
  (:literal "/* functions */")
  (:function
   (("parse_quil" safely-parse-quil) quil-program ((source :string)))
   (("print_program" print-program) :void ((program quil-program)))
   (("compile_quil" compiler-hook) quil-program ((program quil-program) (chip-spec chip-specification)))
   (("compile_protoquil" compile-protoquil) quil-program ((program quil-program) (chip-spec chip-specification)))
   (("build_nq_linear_chip" cl-quil::build-nq-linear-chip) chip-specification ((n :int)))
   (("chip_spec_from_isa_descriptor" lookup-isa-descriptor-for-name) chip-specification ((descriptor :string)))
   (("print_chip_spec" cl-quil::debug-print-chip-spec) :void ((chip-spec chip-specification)))))

(sbcl-librarian:build-bindings libquilc ".")
(sbcl-librarian:build-python-bindings libquilc ".")
(sbcl-librarian:build-core-and-die libquilc ".")
