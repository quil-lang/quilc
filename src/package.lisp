;;;; src/package.lisp
;;;;
;;;; Author: Robert Smith

;;; Allegro (and other Lisps) don't support the non-standard "package
;;; local nicknames".
#-(or sbcl ecl ccl)
(rename-package :alexandria :alexandria '(:a))

(defpackage #:cl-quil.resource
  (:use #:cl)
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))
  (:export
   #:resource-collection                ; TYPE
   #:make-resource-collection           ; FUNCTION
   #:make-qubit-resource                ; FUNCTION
   #:make-null-resource                 ; FUNCTION
   #:make-all-resource                  ; FUNCTION
   #:make-resource-range                ; FUNCTION

   #:resource=                          ; FUNCTION
   #:resource-union                     ; FUNCTION
   #:resource-intersection              ; FUNCTION
   #:resource-difference                ; FUNCTION

   #:resources-intersect-p              ; PREDICATE
   #:resource-subsetp                   ; PREDICATE
   #:resource-null-p                    ; PREDICATE
   #:resource-all-p                     ; PREDICATE

   #:build-qubit-pair-resource          ; FUNCTION
   ))

(defpackage #:cl-quil
  (:nicknames #:quil)
  (:use #:cl
        #:parse-float
        #:abstract-classes
        #:singleton-classes
        #:cl-quil.resource)
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))
  ;; options.lisp
  (:export
   #:*allow-unresolved-applications*    ; VARIABLE
   #:*resolve-include-pathname*         ; VARIABLE
   )

  ;; transformable-mixin.lisp
  (:export
   #:unsatisfied-transform-dependency   ; CONDITION
   #:unsatisfied-transform-dependency-object
                                        ; READER
   #:unsatisfied-transform-dependency-needed-transform
                                        ; READER
   #:unsatisfied-transform-dependency-attempted-transform
                                        ; READER
   #:transformable                      ; CLASS
   #:transformedp                       ; GENERIC, METHOD
   #:transform                          ; GENERIC, METHOD
   #:define-transform                   ; MACRO
   )

  ;; classical-memory.lisp
  (:export
   #:quil-memory-model-error      ; CONDITION
   #:quil-type                    ; TYPE (ADT)
   #:quil-bit                     ; TYPE (ADT), FUNCTION (CONSTRUCTOR)
   #:quil-octet                   ; TYPE (ADT), FUNCTION (CONSTRUCTOR)
   #:quil-integer                 ; TYPE (ADT), FUNCTION (CONSTRUCTOR)
   #:quil-real                    ; TYPE (ADT), FUNCTION (CONSTRUCTOR)

   #:memory-descriptor                  ; TYPE (STRUCTURE)
   #:memory-descriptor-name             ; FUNCTION (READER)
   #:memory-descriptor-type             ; FUNCTION (READER)
   #:memory-descriptor-length           ; FUNCTION (READER)
   #:memory-alias                       ; TYPE (STRUCTURE)
   #:memory-alias-name                  ; FUNCTION (READER)
   #:memory-alias-root-memory           ; FUNCTION (READER)
   #:memory-alias-type                  ; FUNCTION (READER)
   #:memory-alias-length                ; FUNCTION (READER)
   #:memory-alias-starting-bit          ; FUNCTION (READER)
   #:memory-alias-size-in-bits          ; FUNCTION (READER)
   #:memory-model                       ; TYPE (STRUCTURE)
   #:memory-model-alignment             ; FUNCTION (READER)
   #:memory-model-real-bits             ; FUNCTION (READER)
   #:memory-model-integer-bits          ; FUNCTION (READER)
   #:memory-model-sizeof                ; FUNCTION (READER)
   #:memory-model-names                 ; FUNCTION (READER)
   #:memory-model-roots                 ; FUNCTION (READER)
   #:memory-model-aliases               ; FUNCTION (READER)
   #:memory-descriptors-to-model        ; FUNCTION
   #:**empty-memory-model**             ; STATIC GLOBAL
   )

  ;; ast.lisp
  (:export
   #:copy-instance                      ; GENERIC, METHOD
   #:address                            ; STRUCTURE
   #:address-value                      ; READER

   #:qubit                              ; STRUCTURE
   #:qubit-index                        ; READER
   #:qubit=                             ; FUNCTION
   #:qubit-p                            ; FUNCTION

   #:constant                           ; STRUCTURE
   #:constant-value                     ; READER
   #:constant=                          ; FUNCTION
   #:is-constant                        ; FUNCTION

   #:label                              ; STRUCTURE
   #:label-name                         ; ACCESSOR

   #:param                              ; STRUCTURE
   #:param-name                         ; READER
   #:is-param                           ; FUNCTION (PREDICATE)
   #:param=                             ; FUNCTION

   #:formal                             ; STRUCTURE
   #:formal-name                        ; READER
   #:is-formal                          ; FUNCTION (PREDICATE)
   #:formal=                            ; FUNCTION

   #:argument=                          ; FUNCTION

   #:memory-ref                         ; TYPE (STRUCTURE)
   #:mref                               ; FUNCTION (CONSTRUCTOR)
   #:memory-ref-position                ; READER
   #:memory-ref-name                    ; READER
   #:memory-ref=                        ; FUNCTION
   #:is-mref                            ; FUNCTION (PREDICATE)
   #:memory-name                        ; TYPE (STRUCTURE)
   #:memory-name-region-name            ; READER

   #:jump-target                        ; CLASS
   #:jump-target-p                      ; FUNCTION
   #:jump-target-label                  ; READER

   #:include                            ; CLASS
   #:include-pathname                   ; READER

   #:instruction                        ; ABSTRACT CLASS

   #:no-operation                       ; CLASS

   #:pragma                             ; CLASS
   #:pragma-words                       ; READER
   #:pragma-freeform-string             ; READER
   #:make-pragma                        ; FUNCTION

   #:halt                               ; CLASS
   #:reset                              ; CLASS
   #:reset-qubit                        ; CLASS
   #:reset-qubit-target                 ; READER

   #:wait                               ; CLASS
   #:wait-address                       ; READER

   #:unary-classical-instruction        ; ABSTRACT CLASS
   #:classical-target                   ; READER

   #:classical-negate                   ; CLASS
   #:classical-not                      ; CLASS

   #:classical-binary-instruction       ; ABSTRACT CLASS
   #:classical-left-operand             ; READER
   #:classical-right-operand            ; READER

   #:classical-and                      ; CLASS
   #:classical-inclusive-or             ; CLASS
   #:classical-exclusive-or             ; CLASS
   #:classical-addition                 ; CLASS
   #:classical-subtraction              ; CLASS
   #:classical-multiplication           ; CLASS
   #:classical-division                 ; CLASS
   #:classical-move                     ; CLASS
   #:classical-exchange                 ; CLASS
   #:classical-convert                  ; CLASS

   #:classical-trinary-instruction      ; ABSTRACT CLASS

   #:classical-load                                ; CLASS
   #:classical-store                               ; CLASS
   #:classical-equality                            ; CLASS
   #:classical-greater-than                        ; CLASS
   #:classical-greater-equal                       ; CLASS
   #:classical-less-than                           ; CLASS
   #:classical-less-equal                          ; CLASS
   #:classical-division-real/immediate             ; TYPED-CLASS
   #:classical-less-equal-bit/bit/immediate        ; TYPED-CLASS
   #:classical-less-than-bit/integer/integer       ; TYPED-CLASS
   #:classical-equality-bit/bit/immediate          ; TYPED-CLASS
   #:classical-less-than-bit/integer/immediate     ; TYPED-CLASS
   #:classical-move-bit/bit                        ; TYPED-CLASS
   #:classical-less-equal-bit/integer/immediate    ; TYPED-CLASS
   #:classical-and-octet/octet                     ; TYPED-CLASS
   #:classical-convert-real/integer                ; TYPED-CLASS
   #:classical-convert-integer/real                ; TYPED-CLASS
   #:classical-move-integer/immediate              ; TYPED-CLASS
   #:classical-convert-integer/bit                 ; TYPED-CLASS
   #:classical-store-octet*/integer/immediate      ; TYPED-CLASS
   #:classical-greater-equal-bit/real/immediate    ; TYPED-CLASS
   #:classical-exclusive-or-integer/immediate      ; TYPED-CLASS
   #:classical-store-octet*/integer/octet          ; TYPED-CLASS
   #:classical-addition-integer/integer            ; TYPED-CLASS
   #:classical-store-integer*/integer/immediate    ; TYPED-CLASS
   #:classical-not-integer                         ; TYPED-CLASS
   #:classical-convert-bit/integer                 ; TYPED-CLASS
   #:classical-exclusive-or-bit/immediate          ; TYPED-CLASS
   #:classical-move-integer/integer                ; TYPED-CLASS
   #:classical-greater-equal-bit/bit/bit           ; TYPED-CLASS
   #:classical-move-real/immediate                 ; TYPED-CLASS
   #:classical-not-octet                           ; TYPED-CLASS
   #:classical-store-real*/integer/immediate       ; TYPED-CLASS
   #:classical-greater-equal-bit/octet/octet       ; TYPED-CLASS
   #:classical-less-than-bit/octet/octet           ; TYPED-CLASS
   #:classical-and-octet/immediate                 ; TYPED-CLASS
   #:classical-inclusive-or-integer/integer        ; TYPED-CLASS
   #:classical-less-than-bit/bit/immediate         ; TYPED-CLASS
   #:classical-addition-integer/immediate          ; TYPED-CLASS
   #:classical-equality-bit/octet/octet            ; TYPED-CLASS
   #:classical-multiplication-real/immediate       ; TYPED-CLASS
   #:classical-greater-equal-bit/real/real         ; TYPED-CLASS
   #:classical-inclusive-or-octet/octet            ; TYPED-CLASS
   #:classical-less-than-bit/real/real             ; TYPED-CLASS
   #:classical-exchange-integer/integer            ; TYPED-CLASS
   #:classical-load-octet/octet*/integer           ; TYPED-CLASS
   #:classical-inclusive-or-bit/bit                ; TYPED-CLASS
   #:classical-and-bit/immediate                   ; TYPED-CLASS
   #:classical-division-integer/immediate          ; TYPED-CLASS
   #:classical-and-integer/integer                 ; TYPED-CLASS
   #:classical-less-equal-bit/real/real            ; TYPED-CLASS
   #:classical-move-octet/immediate                ; TYPED-CLASS
   #:classical-division-integer/integer            ; TYPED-CLASS
   #:classical-greater-than-bit/integer/immediate  ; TYPED-CLASS
   #:classical-subtraction-integer/integer         ; TYPED-CLASS
   #:classical-move-bit/immediate                  ; TYPED-CLASS
   #:classical-exclusive-or-octet/octet            ; TYPED-CLASS
   #:classical-and-integer/immediate               ; TYPED-CLASS
   #:classical-greater-equal-bit/bit/immediate     ; TYPED-CLASS
   #:classical-greater-than-bit/bit/immediate      ; TYPED-CLASS
   #:classical-subtraction-real/real               ; TYPED-CLASS
   #:classical-store-bit*/integer/immediate        ; TYPED-CLASS
   #:classical-convert-real/bit                    ; TYPED-CLASS
   #:classical-not-bit                             ; TYPED-CLASS
   #:classical-and-bit/bit                         ; TYPED-CLASS
   #:classical-less-than-bit/octet/immediate       ; TYPED-CLASS
   #:classical-move-octet/octet                    ; TYPED-CLASS
   #:classical-multiplication-integer/immediate    ; TYPED-CLASS
   #:classical-convert-bit/real                    ; TYPED-CLASS
   #:classical-addition-real/immediate             ; TYPED-CLASS
   #:classical-store-bit*/integer/bit              ; TYPED-CLASS
   #:classical-exclusive-or-integer/integer        ; TYPED-CLASS
   #:classical-multiplication-integer/integer      ; TYPED-CLASS
   #:classical-exclusive-or-octet/immediate        ; TYPED-CLASS
   #:classical-equality-bit/integer/immediate      ; TYPED-CLASS
   #:classical-exchange-octet/octet                ; TYPED-CLASS
   #:classical-store-integer*/integer/integer      ; TYPED-CLASS
   #:classical-move-real/real                      ; TYPED-CLASS
   #:classical-equality-bit/octet/immediate        ; TYPED-CLASS
   #:classical-less-equal-bit/octet/octet          ; TYPED-CLASS
   #:classical-less-equal-bit/integer/integer      ; TYPED-CLASS
   #:classical-store-real*/integer/real            ; TYPED-CLASS
   #:classical-subtraction-integer/immediate       ; TYPED-CLASS
   #:classical-less-than-bit/bit/bit               ; TYPED-CLASS
   #:classical-inclusive-or-octet/immediate        ; TYPED-CLASS
   #:classical-greater-than-bit/bit/bit            ; TYPED-CLASS
   #:classical-inclusive-or-bit/immediate          ; TYPED-CLASS
   #:classical-inclusive-or-integer/immediate      ; TYPED-CLASS
   #:classical-equality-bit/real/real              ; TYPED-CLASS
   #:classical-equality-bit/bit/bit                ; TYPED-CLASS
   #:classical-greater-than-bit/octet/immediate    ; TYPED-CLASS
   #:classical-multiplication-real/real            ; TYPED-CLASS
   #:classical-less-than-bit/real/immediate        ; TYPED-CLASS
   #:classical-greater-than-bit/real/immediate     ; TYPED-CLASS
   #:classical-greater-than-bit/real/real          ; TYPED-CLASS
   #:classical-subtraction-real/immediate          ; TYPED-CLASS
   #:classical-equality-bit/integer/integer        ; TYPED-CLASS
   #:classical-greater-than-bit/octet/octet        ; TYPED-CLASS
   #:classical-exchange-real/real                  ; TYPED-CLASS
   #:classical-load-real/real*/integer             ; TYPED-CLASS
   #:classical-negate-integer                      ; TYPED-CLASS
   #:classical-greater-equal-bit/integer/immediate ; TYPED-CLASS
   #:classical-exchange-bit/bit                    ; TYPED-CLASS
   #:classical-exclusive-or-bit/bit                ; TYPED-CLASS
   #:classical-negate-real                         ; TYPED-CLASS
   #:classical-less-equal-bit/real/immediate       ; TYPED-CLASS
   #:classical-load-integer/integer*/integer       ; TYPED-CLASS
   #:classical-greater-equal-bit/octet/immediate   ; TYPED-CLASS
   #:classical-division-real/real                  ; TYPED-CLASS
   #:classical-equality-bit/real/immediate         ; TYPED-CLASS
   #:classical-less-equal-bit/bit/bit              ; TYPED-CLASS
   #:classical-greater-than-bit/integer/integer    ; TYPED-CLASS
   #:classical-less-equal-bit/octet/immediate      ; TYPED-CLASS
   #:classical-addition-real/real                  ; TYPED-CLASS
   #:classical-load-bit/bit*/integer               ; TYPED-CLASS
   #:classical-greater-equal-bit/integer/integer   ; TYPED-CLASS

   #:jump                               ; ABSTRACT CLASS
   #:jump-label                         ; READER
   #:unconditional-jump                 ; CLASS
   #:conditional-jump-address           ; READER
   #:jump-when                          ; CLASS
   #:jump-unless                        ; CLASS

   #:measurement                        ; ABSTRACT CLASS
   #:measurement-qubit                  ; READER

   #:measure                            ; CLASS
   #:measure-address                    ; READER

   #:measure-discard                    ; CLASS

   #:operator-description               ; ADT
   #:named-operator                     ; ADT CONSTRUCTOR
   #:controlled-operator                ; ADT CONSTRUCTOR
   #:dagger-operator                    ; ADT CONSTRUCTOR
   #:operator-description-root-name     ; FUNCTION
   #:operator-description=              ; FUNCTION
   #:operator-description-hash          ; FUNCTION
   #:plain-operator-p                   ; FUNCTION
   #:print-operator-description         ; FUNCTION

   #:application                        ; ABSTRACT CLASS
   #:application-operator               ; READER
   #:application-parameters             ; READER
   #:application-arguments              ; READER

   #:unresolved-application             ; CLASS

   #:gate-application                   ; CLASS
   #:gate-application-gate              ; GENERIC, METHOD

   #:lexical-context                    ; GENERIC

   #:gate-definition                    ; ABSTRACT CLASS
   #:gate-definition-name               ; READER
   #:gate-definition-entries            ; READER
   #:matrix-gate-definition             ; CLASS
   #:static-gate-definition             ; CLASS
   #:parameterized-gate-definition      ; CLASS
   #:gate-definition-parameters         ; READER
   #:permutation-gate-definition            ; CLASS
   #:permutation-gate-definition-parameters ; READER

   #:circuit-definition                 ; CLASS
   #:circuit-definition-name            ; READER
   #:circuit-definition-parameters      ; READER
   #:circuit-definition-arguments       ; READER
   #:circuit-definition-body            ; READER

   #:parsed-program                         ; CLASS
   #:parsed-program-gate-definitions        ; READER
   #:parsed-program-circuit-definitions     ; READER
   #:parsed-program-waveform-definitions    ; READER
   #:parsed-program-calibration-definitions ; READER
   #:parsed-program-frame-definitions       ; READER
   #:parsed-program-memory-definitions      ; READER
   #:parsed-program-executable-code         ; READER
   #:print-parsed-program                   ; FUNCTION

   #:*print-fractional-radians*         ; PARAMETER
   #:print-instruction                  ; FUNCTION
   #:instruction-fmt                    ; FUNCTION (format directive)
   #:real-fmt                           ; FUNCTION (format directive)
   #:complex-fmt                        ; FUNCTION (format directive)

   #:comment                            ; FUNCTION
   )

  ;; gates.lisp
  (:export
   #:gate-matrix                        ; GENERIC, METHOD
   #:gate-dimension                     ; GENERIC, METHOD

   #:gate                               ; ABSTRACT CLASS
   #:gate-name                          ; READER

   #:static-gate                        ; ABSTRACT CLASS
   #:dynamic-gate                       ; ABSTRACT CLASS
   #:dynamic-gate-arity                 ; READER

   #:simple-gate                        ; CLASS
   #:simple-gate-matrix                 ; READER

   #:permutation-gate                   ; CLASS
   #:permutation-gate-permutation       ; READER
   #:make-permutation-gate              ; FUNCTION

   #:parameterized-gate                 ; CLASS
   #:parameterized-gate-function        ; READER

   #:controlled-gate                    ; CLASS
   #:forked-gate                        ; CLASS
   #:dagger-gate                        ; CLASS
   #:target                             ; READER

   #:standard-gate-names                ; FUNCTION
   #:lookup-standard-gate               ; FUNCTION

   #:gate-definition-to-gate            ; GENERIC, METHOD
   )

  ;; parser.lisp
  (:export
   #:parse-quil-into-raw-program        ; FUNCTION
   #:quil-parse-error                   ; CONDITION
   )

  ;; build-gate.lisp
  (:export
   #:build-gate                         ; FUNCTION
   #:anon-gate                          ; FUNCTION
   )

  ;; define-compiler.lisp
  (:export
   #:with-inst                          ; MACRO
   #:inst                               ; LOCAL FUNCTION
   #:inst*                              ; LOCAL FUNCTION
   #:finish-compiler                    ; LOCAL MACRO
   )

  ;; desugar.lisp
  (:export
   #:qubits-needed                      ; FUNCTION
   )

  (:export
   #:rewrite-arithmetic                 ; FUNCTION/TRANSFORMATION
   )

  ;; analysis/compress-qubits.lisp
  (:export
   #:compress-qubits
   #:relabel-rewiring
   #:qubits-used
   )

  ;; cl-quil.lisp
  (:export
   #:parse-quil                         ; FUNCTION
   #:read-quil-file                     ; FUNCTION
   )

  ;; compilation-methods.lisp
  (:export
   #:compiler-hook                      ; FUNCTION
   #:check-protoquil-program            ; FUNCTION
   #:protoquil-program-p                ; FUNCTION
   #:not-protoquil                      ; CONDITION
   #:not-protoquil-program              ; FUNCTION
   #:not-protoquil-index                ; FUNCTION
   )

  ;; chip-specification.lisp
  (:export
   #:build-8q-chip                      ; FUNCTION
   #:warm-hardware-objects              ; FUNCTION
   )

  ;; pragmas.lisp
  (:export
   #:pragma-qubit-index                 ; READER
   #:pragma-matrix-entries              ; READER
   #:pragma-qubit-arguments             ; READER
   #:pragma-operator-name               ; READER
   )

  ;; matrix-operations.lisp
  (:export
   #:parsed-program-to-logical-matrix   ; FUNCTION
   )

  ;; expansion.lisp
  (:export
   #:quil-expansion-error               ; CONDITION
   )

  ;; type-safety.lisp
  (:export
   #:quil-type-error                    ; FUNCTION
   )

  ;; utilities.lisp
  (:export
   #:ilog2                              ; FUNCTION
   #:rotate-byte                        ; FUNCTION
   #:with-output-to-quil                ; MACRO
   )

  ;; relabeling.lisp
  (:export
   #:standard-qubit-relabeler           ; FUNCTION
   )

  ;; from utilities.lisp
  (:export
   #:-pi                                ; CONSTANT
   #:pi/2                               ; CONSTANT
   #:-pi/2                              ; CONSTANT
   #:2pi                                ; CONSTANT
   #:4pi                                ; CONSTANT
   )
  (:shadow
   #:pi))

(defpackage #:cl-quil.clifford
  (:nicknames #:quil.clifford)
  (:use #:cl
        #:cl-permutation)
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))

  ;; clifford/ module
  (:export
   ;; qubit-algebra.lisp
   ;; (some methods defined in later files)
   #:qubit-algebra                      ; CLASS
   #:num-qubits                         ; GENERIC FUNCTION, METHOD
   #:group-mul                          ; GENERIC FUNCTION
   #:group-inv                          ; GENERIC FUNCTION
   #:group-conj                         ; GENERIC FUNCTION, METHOD
   #:tensor-mul                         ; GENERIC FUNCTION
   #:embed                              ; GENERIC FUNCTION, METHOD

   ;; pauli.lisp
   #:pauli                              ; CLASS
   #:make-pauli                         ; FUNCTION
   #:+I+ #:+X+ #:+Y+ #:+Z+              ; CONSTANT
   #:pauli-identity                     ; FUNCTION
   #:pauli-identity-p                   ; FUNCTION
   #:pauli-from-string                  ; FUNCTION
   #:pauli-from-symbols                 ; FUNCTION
   #:pauli-basis-decompose              ; FUNCTION
   #:pauli=                             ; FUNCTION
   #:make-pauli-hash-table              ; FUNCTION
   #:exp-pauli                          ; FUNCTION

   ;; clifford.lisp
   #:count-clifford                     ; FUNCTION
   #:pauli-to-index                     ; FUNCTION
   #:pauli-from-index                   ; FUNCTION
   #:map-pauli-basis                    ; FUNCTION
   #:enumerate-pauli-basis              ; FUNCTION
   #:clifford                           ; STRUCTURE
   #:apply-clifford                     ; FUNCTION
   #:clifford-identity                  ; FUNCTION
   #:clifford-identity-p                ; FUNCTION
   #:clifford-element                   ; MACRO
   #:clifford=                          ; FUNCTION
   #:clifford-from-pauli                ; FUNCTION
   #:random-clifford                    ; FUNCTION
   #:make-clifford-hash-table           ; FUNCTION

   ;; god-table.lisp
   #:hadamard                           ; FUNCTION
   #:phase-gate                         ; FUNCTION
   #:cnot                               ; FUNCTION
   #:swap                               ; FUNCTION
   #:gateset                            ; CLASS
   #:god-table                          ; CLASS
   #:make-god-table                     ; FUNCTION
   #:sample                             ; FUNCTION
   #:reconstruct                        ; FUNCTION
   #:default-gateset                    ; FUNCTION


   ;; swap-representation.lisp
   #:canonical-swap-representative      ; FUNCTION

   ;; benchmarking-procedures.lisp
   #:apply-clifford-string              ; FUNCTION
   #:rb-sequence                        ; FUNCTION
   #:serialize-clifford                 ; FUNCTION
   #:serialize-clifford-sequence        ; FUNCTION
   #:clifford-element-string            ; FUNCTION
   #:apply-clifford-string              ; FUNCTION
   #:pauli-matrix-p                     ; FUNCTION
   #:n-qubit-pauli-basis-matrices       ; FUNCTION
   #:matrix-to-clifford                 ; FUNCTION
   #:clifford-from-quil                 ; FUNCTION
   )
  )
