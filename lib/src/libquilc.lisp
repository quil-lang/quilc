(in-package #:libquilc)

(defvar *last-error* "")

(sbcl-librarian:define-handle-type quil-program "quil_program")
(sbcl-librarian:define-handle-type chip-specification "chip_specification")

(sbcl-librarian:define-enum-type error-type "error_t"
  ("ERROR_SUCCESS" 0)
  ("ERROR_FAIL" 1))
(sbcl-librarian:define-error-map error-map error-type 0
  ((t (lambda (condition)
        (setf *last-error* (format nil "~a" condition))
        (return-from error-map 1)))))

(defun compile-protoquil (parsed-program chip-specification)
  (let ((compiled-program (compiler-hook parsed-program chip-specification :protoquil t)))
    (cl-quil.frontend::transform 'cl-quil.frontend::process-protoquil compiled-program)
    compiled-program))

(sbcl-librarian:define-api quilc (:error-map error-map
                                  :function-prefix "quilc_")
  (:literal "/* types */")
  (:type quil-program chip-specification error-type)
  (:literal "/* functions */")
  (:function
   (("parse_quil" cl-quil.frontend:safely-parse-quil) quil-program ((source :string)))
   (("print_program" cl-quil.frontend:print-parsed-program) :void ((program quil-program)))
   (("compile_quil" cl-quil:compiler-hook) quil-program ((program quil-program) (chip-spec chip-specification)))
   (("compile_protoquil" compile-protoquil) quil-program ((program quil-program) (chip-spec chip-specification)))
   (("build_nq_linear_chip" cl-quil::build-nq-linear-chip) chip-specification ((n :int)))
   (("chip_spec_from_isa_descriptor" quilc::lookup-isa-descriptor-for-name) chip-specification ((descriptor :string)))
   (("print_chip_spec" cl-quil::debug-print-chip-spec) :void ((chip-spec chip-specification)))
   (("parse_chip_spec_isa_json" parse-chip-spec-isa-json) chip-specification ((isa-json :string)))
   (("program_string" program-to-string) :string ((program quil-program)))
   (("error" quilc-last-error) :string ())))

(defun program-to-string (program)
  (with-output-to-string (s)
    (cl-quil.frontend:print-parsed-program program s)))

(defun quilc-last-error ()
  "Returns the most recent error raised by quilc. The error is then cleared."
  (let ((last-error *last-error*))
    (setf *last-error* "")
    last-error))

(defun parse-chip-spec-isa-json (isa-json)
  (cl-quil::qpu-hash-table-to-chip-specification (yason:parse isa-json)))
