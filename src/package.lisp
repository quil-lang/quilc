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
   ))

(defpackage #:cl-quil.frontend
  (:use #:cl
        #:parse-float
        #:abstract-classes
        #:singleton-classes)
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))
  ;; frontend-options.lisp
  (:export
   #:*allow-unresolved-applications*    ; VARIABLE
   #:*resolve-include-pathname*         ; VARIABLE
   #:*print-fractional-radians*         ; VARIABLE
   #:*print-polar-form*                 ; VARIABLE
   #:*compiler-noise*                   ; VARIABLE
   )

  ;; types.lisp
  (:export
   #:integer-list                       ; TYPE
   #:integer-list-p                     ; FUNCTION
   #:number-list                        ; TYPE
   #:number-list-p                      ; FUNCTION
   #:symbol-list                        ; TYPE
   #:symbol-list-p                      ; FUNCTION
   #:string-sequence                    ; TYPE
   #:string-sequence-p                  ; FUNCTION
   #:integeropt-vector                  ; TYPE
   #:integeropt-vector-p                ; FUNCTION
   #:integer-vector                     ; TYPE
   #:integer-vector-p                   ; FUNCTION
   #:unsigned-fixnum                    ; TYPE
   )

  ;; frontend-utilities.lisp
  (:export
   #:dohash                             ; FUNCTION
   #:define-global-counter              ; MACRO
   #:ilog2                              ; FUNCTION
   #:power-of-two-p                     ; FUNCTION
   #:positive-power-of-two-p            ; FUNCTION
   #:perfect-square-p                   ; FUNCTION
   #:rotate-byte                        ; FUNCTION
   #:program-fidelity                   ; FUNCTION
   #:with-output-to-quil                ; MACRO
   #:pi                                 ; CONSTANT
   #:-pi                                ; CONSTANT
   #:pi/2                               ; CONSTANT
   #:-pi/2                              ; CONSTANT
   #:2pi                                ; CONSTANT
   #:4pi                                ; CONSTANT
   #:double-float-positive-infinity     ; CONSTANT
   #:+double-comparison-threshold-loose+  ; CONSTANT
   #:+double-comparison-threshold-strict+ ; CONSTANT
   #:double~                            ; FUNCTION
   #:double=                            ; FUNCTION
   #:double>=                           ; FUNCTION
   #:format-noise                       ; FUNCTION
   )

  ;; queue.lisp
  (:export
   #:queue                              ; TYPE
   #:queuep                             ; FUNCTION
   #:make-queue                         ; CONSTRUCTOR
   #:queue-empty-p                      ; FUNCTION
   #:enqueue                            ; FUNCTION
   #:dequeue                            ; FUNCTION
   )

  ;; magicl-constructors.lisp
  (:export
   #:empty                              ; FUNCTION
   #:const                              ; FUNCTION
   #:rand                               ; FUNCTION
   #:eye                                ; FUNCTION
   #:arange                             ; FUNCTION
   #:from-array                         ; FUNCTION
   #:from-list                          ; FUNCTION
   #:from-diag                          ; FUNCTION
   #:zeros                              ; FUNCTION
   #:ones                               ; FUNCTION
   #:random-unitary                     ; FUNCTION
   #:matrix-equality                    ; FUNCTION
   #:pauli-term->matrix                 ; FUNCTION
   )

  ;; classical-memory.lisp
  (:export
   #:quil-memory-model-error            ; CONDITION
   #:quil-type                          ; TYPE (ADT)
   #:quil-bit                           ; TYPE (ADT), FUNCTION (CONSTRUCTOR)
   #:quil-octet                         ; TYPE (ADT), FUNCTION (CONSTRUCTOR)
   #:quil-integer                       ; TYPE (ADT), FUNCTION (CONSTRUCTOR)
   #:quil-real                          ; TYPE (ADT), FUNCTION (CONSTRUCTOR)
   #:string-to-quil-type                ; FUNCTION
   #:quil-type-string                   ; FUNCTION

   #:memory-descriptor                  ; TYPE (STRUCTURE)
   #:make-memory-descriptor             ; FUNCTION (CONSTRUCTOR)
   #:memory-descriptor-name             ; FUNCTION (READER)
   #:memory-descriptor-type             ; FUNCTION (READER)
   #:memory-descriptor-length           ; FUNCTION (READER)
   #:memory-descriptor-sharing-parent   ; FUNCTION (READER)
   #:memory-descriptor-sharing-offset-alist ; FUNCTION (READER)
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

   #:qubit                              ; STRUCTURE
   #:qubit-index                        ; ACCESSOR
   #:qubit=                             ; FUNCTION
   #:qubit-p                            ; FUNCTION

   #:constant                           ; STRUCTURE
   #:constant-value                     ; READER
   #:constant-value-type                ; READER
   #:constant=                          ; FUNCTION
   #:is-constant                        ; FUNCTION (PREDICATE)

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

   #:delayed-expression                 ; TYPE (STRUCTURE)   
   #:delayed-expression-p               ; FUNCTION
   #:make-delayed-expression            ; FUNCTION
   #:delayed-expression-params          ; READER
   #:delayed-expression-lambda-params   ; READER
   #:delayed-expression-expression      ; READER
   #:evaluate-delayed-expression        ; FUNCTION

   #:memory-ref                         ; TYPE (STRUCTURE)
   #:mref                               ; FUNCTION (CONSTRUCTOR)
   #:memory-ref-name                    ; READER
   #:memory-ref-position                ; READER
   #:memory-ref-descriptor              ; READER
   #:memory-ref=                        ; FUNCTION
   #:memory-ref-hash                    ; FUNCTION
   #:is-mref                            ; FUNCTION (PREDICATE)
   #:memory-name                        ; TYPE (STRUCTURE)
   #:memory-name-descriptor             ; READER
   #:memory-name-region-name            ; READER
   #:memory-offset                      ; TYPE (STRUCTURE), FUNCTION
   #:memory-offset-offset               ; READER

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
   #:haltp                              ; FUNCTION
   #:reset                              ; CLASS
   #:reset-qubit                        ; CLASS
   #:reset-qubit-target                 ; READER

   #:wait                               ; CLASS

   #:unary-classical-instruction        ; ABSTRACT CLASS
   #:classical-target                   ; READER

   #:classical-negate                   ; CLASS
   #:classical-not                      ; CLASS

   #:binary-classical-instruction       ; ABSTRACT CLASS
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

   #:trinary-classical-instruction      ; ABSTRACT CLASS

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
   #:forked-operator                    ; ADT CONSTRUCTOR
   #:operator-description-root-name     ; FUNCTION
   #:operator-description=              ; FUNCTION
   #:operator-description-hash          ; FUNCTION
   #:operator-description-string        ; FUNCTION
   #:plain-operator-p                   ; FUNCTION
   #:print-operator-description         ; FUNCTION

   #:application                        ; ABSTRACT CLASS
   #:application-operator               ; ACCESSOR
   #:application-parameters             ; ACCESSOR
   #:application-arguments              ; ACCESSOR
   #:application-operator-name          ; FUNCTION
   #:application-operator-root-name     ; FUNCTION

   #:unresolved-application             ; CLASS

   #:gate-application                   ; CLASS
   #:gate-application-gate              ; GENERIC, READER
   #:gate-application-resolution        ; READER
   #:anonymous-gate-application-p       ; FUNCTION
   #:swap-application-p                 ; FUNCTION

   #:circuit-application                ; CLASS
   #:circuit-application-definition     ; ACCESSOR

   #:lexical-context                    ; GENERIC

   #:gate-definition                    ; ABSTRACT CLASS
   #:gate-definition-name               ; READER
   #:gate-definition-entries            ; READER
   #:matrix-gate-definition             ; CLASS
   #:static-gate-definition             ; CLASS
   #:parameterized-gate-definition      ; CLASS
   #:gate-definition-parameters         ; READER
   #:permutation-gate-definition             ; CLASS
   #:permutation-gate-definition-permutation ; READER
   #:exp-pauli-sum-gate-definition      ; CLASS
   #:exp-pauli-sum-gate-definition-terms       ; READER
   #:exp-pauli-sum-gate-definition-parameters  ; READER
   #:exp-pauli-sum-gate-definition-arguments   ; READER

   #:circuit-definition                 ; CLASS
   #:circuit-definition-name            ; READER
   #:circuit-definition-parameters      ; READER
   #:circuit-definition-arguments       ; READER
   #:circuit-definition-body            ; READER

   #:parsed-program                         ; CLASS
   #:parsed-program-gate-definitions        ; READER
   #:parsed-program-circuit-definitions     ; READER
   #:parsed-program-memory-definitions      ; READER
   #:parsed-program-executable-code         ; ACCESSOR
   #:print-parsed-program                   ; FUNCTION

   #:*print-parsed-program-text*        ; PARAMETER
   #:*print-fractional-radians*         ; PARAMETER
   #:print-instruction                  ; FUNCTION
   #:print-instruction-to-string        ; FUNCTION
   #:instruction-fmt                    ; FUNCTION (format directive)
   #:real-fmt                           ; FUNCTION (format directive)
   #:complex-fmt                        ; FUNCTION (format directive)

   #:comment                            ; FUNCTION

   #:rewiring                           ; TYPE (STRUCTURE)
   #:init-rewiring                      ; FUNCTION (CONSTRUCTOR)
   #:rewiring-l2p                       ; ACCESSOR
   #:rewiring-p2l                       ; ACCESSOR
   #:make-rewiring-from-string          ; FUNCTION
   #:make-rewiring-pair-from-string     ; FUNCTION
   #:extract-final-exit-rewiring-vector ; FUNCTION
   
   #:make-rewiring-comment              ; FUNCTION
   #:rewiring-comment-type              ; FUNCTION
   #:instruction-rewirings              ; FUNCTION

   #:nth-instr                          ; FUNCTION
   )

  ;; define-pragma.lisp
  (:export
   #:global-pragma-instruction-p        ; FUNCTION
   )

  ;; pragmas.lisp
  (:export
   #:pragma-qubit-index                 ; READER
   #:pragma-matrix-entries              ; READER
   #:pragma-qubit-arguments             ; READER
   #:pragma-operator-name               ; READER
   #:parsed-program-has-pragma-p        ; FUNCTION
   #:parsed-program-has-preserve-blocks-p ; FUNCTION

   #:pragma-preserve-block              ; CLASS
   #:pragma-end-preserve-block          ; CLASS
   
   #:pragma-commuting-blocks            ; CLASS
   #:pragma-end-commuting-blocks        ; CLASS
   
   #:pragma-block                       ; CLASS
   #:pragma-end-block                   ; CLASS
   
   #:pragma-add-kraus                   ; CLASS
   #:pragma-add-kraus-operator-name     ; ACCESSOR
   #:pragma-add-kraus-qubit-arguments   ; ACCESSOR
   #:pragma-add-kraus-matrix-entries    ; ACCESSOR  
   
   #:pragma-readout-povm                ; CLASS
   #:pragma-readout-povm-qubit-index    ; ACCESSOR
   #:pragma-readout-povm-matrix-entries ; ACCESSOR
   
   #:pragma-expected-rewiring           ; CLASS
   #:pragma-expected-rewiring-rewiring  ; ACCESSOR
   
   #:pragma-current-rewiring            ; CLASS
   #:pragma-current-rewiring-rewiring   ; ACCESSOR
   
   #:pragma-initial-rewiring            ; CLASS
   #:pragma-initial-rewiring-rewiring   ; ACCESSOR

   #:pragma-rewiring
   #:pragma-rewiring-type               ; FUNCTION
   )

  ;; parser.lisp
  (:export
   #:token                              ; TYPE (STRUCTURE)
   #:tok                                ; FUNCTION (CONSTRUCTOR)
   #:token-line                         ; ACCESSOR
   #:token-pathname                     ; ACCESSOR
   #:token-type                         ; ACCESSOR
   #:token-payload                      ; ACCESSOR
   #:tokenize                           ; FUNCTION
   #:disappointing-token-error          ; CONDITION

   #:*parse-context*                    ; VARIABLE
   #:quil-parse-error                   ; CONDITION
   #:parse-program-lines                ; FUNCTION
   #:parse-quil-into-raw-program        ; FUNCTION
   #:resolve-safely                     ; FUNCTION
   #:safely-read-quil                   ; FUNCTION
   #:safely-parse-quil                  ; FUNCTION
   )

  ;; cl-quil.lisp
  (:export
   #:parse
   #:parse-quil                         ; FUNCTION
   #:read-quil-file                     ; FUNCTION

   #:*safe-include-directory*           ; VARIABLE

   #:ambiguous-definition-condition     ; CONDITION
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
   #:parameterized-gate-matrix-function ; READER
   #:unknown-gate-parameter             ; CONDITION

   #:exp-pauli-sum-gate                 ; CLASS
   #:exp-pauli-sum-gate-parameters      ; READER
   #:exp-pauli-sum-gate-arguments       ; READER
   #:exp-pauli-sum-gate-terms           ; READER

   #:pauli-term                         ; STRUCT
   #:make-pauli-term                    ; FUNCTION
   #:pauli-term-pauli-word              ; FUNCTION
   #:pauli-term-prefactor               ; FUNCTION
   #:pauli-term-arguments               ; FUNCTION

   #:controlled-gate                    ; CLASS
   #:control-gate                       ; FUNCTION
   #:forked-gate                        ; CLASS
   #:fork-gate                          ; FUNCTION
   #:dagger-gate                        ; CLASS, FUNCTION
   #:target                             ; READER

   #:standard-gate-names                ; FUNCTION
   #:lookup-standard-gate               ; FUNCTION

   #:gate-definition-to-gate            ; GENERIC, METHOD

   #:operator-description-gate-lifter    ; FUNCTION
   )
  
  ;; build-gate.lisp
  (:export
   #:%capture-arg                       ; FUNCTION
   #:%capture-param                     ; FUNCTION
   #:build-gate                         ; FUNCTION
   #:anon-gate                          ; FUNCTION
   #:kq-gate-on-lines                   ; FUNCTION
   #:repeatedly-fork                    ; FUNCTION
   #:build-UCR                          ; FUNCTION
   #:repeatedly-control                 ; FUNCTION
   #:build-multiple-controlled-gate     ; FUNCTION
   #:param-binary-op                    ; FUNCTION
   #:param-+                            ; FUNCTION
   #:param-*                            ; FUNCTION
   #:param-mod                          ; FUNCTION
   )

  ;; with-inst.lisp
  (:export
   #:with-inst
   #:finish-compiler
   #:inst
   #:inst*)

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
   #:transform-if                       ; FUNCTION
   #:define-transform                   ; MACRO
   #:find-transform                     ; FUNCTION
   #:transforms-performed               ; FUNCTION

   #:transform-description              ; STRUCT
   #:make-transform-description         ; FUNCTION (CONSTRUCTOR)
   #:transform-description-name         ; ACCESSOR
   #:transform-description-documentation ; ACCESSOR   
   #:transform-description-predecessors ; ACCESSOR
   #:transform-description-function     ; ACCESSOR
   )

  ;; analysis/process-includes.lisp
  (:export
   #:process-includes                   ; FUNCTION
   )

  ;; analysis/type-safety.lisp
  (:export
   #:type-check                         ; FUNCTION
   #:type-check-instr                   ; GENERIC
   #:quil-type-error                    ; CONDITION, FUNCTION
   )

  ;; analysis/patch-labels.lisp
  (:export
   #:patch-labels                       ; FUNCTION, TRANSFORM
   )

  ;; analysis/resolve-objects.lisp
  (:export
   #:resolve-instruction                ; GENERIC
   #:resolve-objects                    ; GENERIC
   )
  
  ;; analysis/rewrite-arithmetic.lisp
  (:export
   #:rewrite-arithmetic                 ; FUNCTION, TRANSFORM
   )

  ;; analysis/compress-qubits.lisp
  (:export
   #:qubits-used                        ; GENERIC
   #:relabel-rewiring                   ; FUNCTION
   #:compute-qubit-mapping              ; FUNCTION
   #:compress-qubits                    ; FUNCTION, TRANSFORM   
   )

  ;; analysis/expansion.lisp
  (:export
   #:*expansion-context*                ; VARIABLE
   #:*expansion-depth*                  ; VARIABLE
   #:*expansion-limit*                  ; VARIABLE
   #:quil-expansion-error               ; CONDITION, FUNCTION
   #:instantiate-instruction            ; GENERIC, METHOD
   )

  ;; analysis/expand-circuits.lisp
  (:export
   #:expand-circuits                    ; FUNCTION, TRANSFORM
   )

  ;; analysis/process-includes.lisp
  (:export
   #:process-includes                   ; FUNCTION
   )

  ;; analysis/qubits-needed.lisp
  (:export
   #:qubits-needed                      ; FUNCTION
   )

  ;; analysis/simplify-arithmetic.lisp
  (:export
   #:simplify-arithmetic                ; GENERIC
   )
  
  ;; analysis/resolve-objects.lisp
  (:export
   #:resolve-objects                    ; GENERIC
   #:resolve-instruction                ; GENERIC
   )

  ;; analysis/fusion.lisp
  (:export
   #:gate-fusion                        ; TRANSFORM
   #:fuse-gates-in-program              ; FUNCTION
   #:fuse-objects                       ; GENERIC
   )

  ;; analysis/simplification-grab-bag.lisp
  (:export
   #:simplify-individual-instructions   ; TRANSFORM
   )

  (:shadow
   #:pi)
  )

(defpackage #:cl-quil
  (:nicknames #:quil)
  (:use #:cl
        #:cl-quil.resource
        #:cl-quil.frontend
        #:abstract-classes)
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
   #:memory-descriptor-sharing-parent   ; FUNCTION (READER)
   #:memory-descriptor-sharing-offset-alist ; FUNCTION (READER)
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

   #:qubit                              ; STRUCTURE
   #:qubit-index                        ; ACCESSOR
   #:qubit=                             ; FUNCTION
   #:qubit-p                            ; FUNCTION

   #:constant                           ; STRUCTURE
   #:constant-value                     ; READER
   #:constant=                          ; FUNCTION
   #:is-constant                        ; FUNCTION (PREDICATE)

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

   #:unary-classical-instruction        ; ABSTRACT CLASS
   #:classical-target                   ; READER

   #:classical-negate                   ; CLASS
   #:classical-not                      ; CLASS

   #:binary-classical-instruction       ; ABSTRACT CLASS
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

   #:trinary-classical-instruction      ; ABSTRACT CLASS

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
   #:forked-operator                    ; ADT CONSTRUCTOR
   #:operator-description-root-name     ; FUNCTION
   #:operator-description=              ; FUNCTION
   #:operator-description-hash          ; FUNCTION
   #:plain-operator-p                   ; FUNCTION
   #:print-operator-description         ; FUNCTION

   #:application                        ; ABSTRACT CLASS
   #:application-operator               ; ACCESSOR
   #:application-parameters             ; ACCESSOR
   #:application-arguments              ; ACCESSOR

   #:unresolved-application             ; CLASS

   #:gate-application                   ; CLASS
   #:gate-application-gate              ; GENERIC, METHOD

   #:anonymous-gate-application-p       ; FUNCTION

   #:lexical-context                    ; GENERIC

   #:gate-definition                    ; ABSTRACT CLASS
   #:gate-definition-name               ; READER
   #:gate-definition-entries            ; READER
   #:matrix-gate-definition             ; CLASS
   #:static-gate-definition             ; CLASS
   #:parameterized-gate-definition      ; CLASS
   #:gate-definition-parameters         ; READER
   #:permutation-gate-definition             ; CLASS
   #:permutation-gate-definition-permutation ; READER
   #:exp-pauli-sum-gate-definition      ; CLASS
   #:exp-pauli-sum-gate-definition-terms       ; READER
   #:exp-pauli-sum-gate-definition-parameters  ; READER
   #:exp-pauli-sum-gate-definition-arguments   ; READER


   #:circuit-definition                 ; CLASS
   #:make-circuit-definition            ; FUNCTION (CONSTRUCTOR)
   #:circuit-definition-name            ; READER
   #:circuit-definition-parameters      ; READER
   #:circuit-definition-arguments       ; READER
   #:circuit-definition-body            ; READER

   #:parsed-program                         ; CLASS
   #:parsed-program-gate-definitions        ; READER
   #:parsed-program-circuit-definitions     ; READER
   #:parsed-program-memory-definitions      ; READER
   #:parsed-program-executable-code         ; ACCESSOR
   #:print-parsed-program                   ; FUNCTION

   #:*print-parsed-program-text*        ; PARAMETER
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
   #:parameterized-gate-matrix-function ; READER
   #:unknown-gate-parameter             ; CONDITION

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
   #:tok                                ; FUNCTION (CONSTRUCTOR)
   #:token-type                         ; ACCESSOR
   #:token-payload                      ; ACCESSOR
   #:parse-quil-into-raw-program        ; FUNCTION
   #:quil-parse-error                   ; CONDITION
   #:resolve-safely                     ; FUNCTION
   #:safely-read-quil                   ; FUNCTION
   #:safely-parse-quil                  ; FUNCTION
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
   #:parse
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
   #:parsed-program-has-pragma-p        ; FUNCTION
   #:parsed-program-has-preserve-blocks-p ; FUNCTION
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

  ;; backend/ stuff
  (:export
   #:backend                            ; CLASS
   #:backend-name                       ; GENERIC
   #:backend-supports-chip-p            ; GENERIC
   #:write-executable                   ; GENERIC
   #:backend-compile                    ; GENERIC
   #:list-available-backends            ; FUNCTION
   #:find-backend                       ; FUNCTION

   #:quil-backend                       ; CLASS
   #:quil-executable                    ; CLASS
   )


  ;; frontend-utilities.lisp
  (:export
   #:ilog2                              ; FUNCTION
   #:rotate-byte                        ; FUNCTION
   #:with-output-to-quil                ; MACRO
   #:-pi                                ; CONSTANT
   #:pi/2                               ; CONSTANT
   #:-pi/2                              ; CONSTANT
   #:2pi                                ; CONSTANT
   #:4pi                                ; CONSTANT
   )

  ;; queue.lisp
  (:export
   #:queue                              ; TYPE
   #:queuep                             ; FUNCTION
   #:make-queue                         ; CONSTRUCTOR
   #:queue-empty-p                      ; FUNCTION
   #:enqueue                            ; FUNCTION
   #:dequeue                            ; FUNCTION
   )

  ;; relabeling.lisp
  (:export
   #:standard-qubit-relabeler           ; FUNCTION
   )

  ;; utilities.lisp
  (:export
   #:program-fidelity                   ; FUNCTION
   )

  (:shadowing-import-from #:cl-quil.frontend
                          #:pi))

(defpackage #:cl-quil.clifford
  (:nicknames #:quil.clifford)
  (:use #:cl
        #:cl-permutation)
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria)
                    (:quil :cl-quil.frontend))

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
   #:rb-sequence                        ; FUNCTION
   #:serialize-clifford                 ; FUNCTION
   #:serialize-clifford-sequence        ; FUNCTION
   #:pauli-matrix-p                     ; FUNCTION
   #:n-qubit-pauli-basis-matrices       ; FUNCTION
   #:matrix-to-clifford                 ; FUNCTION
   #:clifford-from-quil                 ; FUNCTION
   )
  )

(defpackage #:cl-quil.qasm
  (:nicknames #:quil.qasm)
  (:use #:cl)
  (:local-nicknames (:a :alexandria)
                    (:quil :cl-quil.frontend))
  (:import-from #:cl-quil.frontend #:tok #:token-type #:token-payload)

  (:export
   #:parse-qasm))





;;;; The CL-Quil.SI Package

;;; This package is special: it "exports" symbols internal to the
;;; CL-Quil package for "system internal" use.  Note that these
;;; symbols are intended only to be used by various "friends" of the
;;; implementation. These symbols should be considered INTERNAL and
;;; are *not* to be considered exported in the usual sense. They are
;;; not supported for general use. Rather, they are for use only by
;;; closely-related systems that extend cl-quil and therefore need to
;;; access "internal" symbols.  Having such symbols cataloged here is
;;; preferable to simply accessing any symbol whatsoever via full
;;; qualification, ala the use of package name "quil" with double
;;; colons (quil::).

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun pre-intern-exposed-symbols (symbols package-name)
  "This is a subfunction of define-exposing-package, used to ensure all of the symbols are interned in the 'from' package ahead of time.
   They're expected to be interned by the reader when the actual source files are processed later, but this is not guaranteed."
  (let* ((pkg (find-package package-name))
         name)
    (dolist (symbol symbols)
      (setq name (string symbol))
      (multiple-value-bind (found-symbol status)
          (find-symbol name pkg)
        (cond
          (found-symbol
           (when (eq status ':external)
             (uiop:style-warn
              "The symbol named ~A is external in ~A. Why re-expose it?"
              name package-name)))
          (t
           (intern name pkg)))))))
)  ; closes (eval-when ...)



(defmacro define-exposing-package (name (from &rest options) &body symbols)
  "Define a package named NAME that exposes the symbols from a package named FROM as exported symbols.
   This creates package NAME, interns SYMBOLS in package FROM, imports SYMBOLS into package NAME, and exports SYMBOLS from package NAME.
   Any additional options to defpackage can be specified as OPTIONS, e.g., (:nicknames ...)."
  `(progn
     ;; The following must run at least at compile and load
     ;; times. (Defpackage can automatically be expected to run at
     ;; those times.)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       ;; Do we have the FROM package?
       (when (null (find-package ',from))
         (error "Unknown package: ~A" ',from))
       (pre-intern-exposed-symbols ',symbols ',from))
     (defpackage ,name
       ,@options
       (:import-from ,from ,@symbols)
       (:export ,@symbols))))



(define-exposing-package #:cl-quil.si (#:cl-quil (:nicknames #:quil.si))
  
  ;; logical-schedule.lisp
  #:append-instructions-to-lschedule    ; FUNCTION
  #:lscheduler-earlier-instrs           ; ACCESSOR
  #:lscheduler-last-instrs              ; ACCESSOR
  #:make-lscheduler                     ; FUNCTION
  )

;; After all the dust settles, you should be able to see in a REPL:
;; 
;; (find-symbol "PARSE-PARAMETERS" (find-package "QUIL"))
;;   => CL-QUIL::PARSE-PARAMETERS, :INTERNAL
;; (find-symbol "PARSE-PARAMETERS" (find-package "QUIL.SI"))
;;   => CL-QUIL::PARSE-PARAMETERS, :EXTERNAL
