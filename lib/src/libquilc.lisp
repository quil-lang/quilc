(in-package #:libquilc)

(sbcl-librarian:define-handle-type quil-program "quil_program")
(sbcl-librarian:define-handle-type chip-specification "chip_specification")

(sbcl-librarian:define-enum-type error-type "error_t"
  ("ERROR_SUCCESS" 0)
  ("ERROR_FAIL" 1))
(sbcl-librarian:define-error-map error-map error-type 0
  ((t (lambda (condition)
        (declare (ignore condition))
        (return-from error-map 1)))))

(defun compile-protoquil (parsed-program chip-specification)
  (compiler-hook parsed-program chip-specification :protoquil t))

(defun generate-rb-sequence (depth qubits gateset seed interleaver)
  (let* ((request (make-instance 'rpcq::|RandomizedBenchmarkingRequest|
                                 :|depth| depth :|qubits| qubits
                                 :|gateset| (uiop:split-string gateset :separator ",")
                                                :|seed| seed :|interleaver| interleaver))
         (response (quilc::generate-rb-sequence-handler request)))
    (with-output-to-string (stream)
      (yason:encode (rpcq::|RandomizedBenchmarkingResponse-sequence| response)
                    stream))))

(defun conjugate-pauli-by-clifford (pauli-indices pauli-symbols clifford)
  (let* ((indices (mapcar #'parse-integer
                          (uiop:split-string pauli-indices
                                             :separator ",")))
         (pauli (make-instance 'rpcq::|PauliTerm|
                               :|indices| indices
                               :|symbols| (uiop:split-string pauli-symbols :separator ",")))
         (request (make-instance 'rpcq::|ConjugateByCliffordRequest|
                                 :|pauli| pauli :|indices| indices
                                 :|clifford| clifford))
         (response (quilc::conjugate-pauli-by-clifford-handler request)))
    (rpcq::to-json-string response)))

(defun %rewrite-arithmetic (quil)
  (let* ((request (make-instance 'rpcq::|RewriteArithmeticRequest|
                                 :|quil| quil))
         (response (quilc::rewrite-arithmetic-handler request)))
    (rpcq::to-json-string response)))

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
   (("generate_rb_sequence" generate-rb-sequence) :string ((depth :int) (qubits :int) (gateset :string) (seed :int) (interleaver :string)))
   (("conjugate_pauli_by_clifford" conjugate-pauli-by-clifford) :string ((pauli-indices :string) (pauli-symbols :string) (clifford :string)))
   (("rewrite_arithmetic" %rewrite-arithmetic) :string ((quil :string)))
   (("get_version_info" quilc::get-version-info-handler) :string ())))

