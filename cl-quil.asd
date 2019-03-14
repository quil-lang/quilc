;;;; cl-quil.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:cl-quil
  :description "A parser and optimizing compiler for the Quantum Instruction Language (Quil)."
  :author "Robert Smith <robert@rigetti.com>, Eric Peterson <eric@rigetti.com>, Rigetti Computing"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :version (:read-file-form "VERSION.txt")
  :pathname "src/"
  :depends-on ((:version #:alexa "2.1.1")
                                        ; Lexical analysis
               #:yacc                   ; Arithmetic parsing
               #:alexandria             ; Utilities
               #:parse-float            ; Float parsing
               (:version #:magicl "0.6.1")
                                        ; internal linear algebra library
               #:cl-grnm                ; nelder-mead implementation
               #:singleton-classes
               #:abstract-classes
               #:yason                  ; JSON generation
               #:uiop
               #:split-sequence
               #:closer-mop
               #:optima
               #:cl-algebraic-data-type
               #:global-vars            ; Static globals
               #:salza2                 ; God table compression
               #:cl-permutation
               #:queues.priority-queue
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-quil-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl(sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :serial t
  :components ((:file "package")
               (:file "options")
               (:file "utilities")
               (:file "matrix-operations")
               (:file "transformable-mixin")
               (:file "classical-memory")
               (:file "ast")
               (:file "resource")
               (:file "define-pragma")
               (:file "pragmas")
               (:file "parser")
               (:file "gates")
               (:file "environment")
               (:file "operator-bind")
               (:module "clifford"
                :serial t
                :components ((:file "qubit-algebra")
                             (:file "pauli")
                             (:file "symplectic")
                             (:file "clifford")
                             (:file "god-table")
                             (:file "god-table-utilities")
                             (:file "swap-representation")
                             (:file "benchmarking-procedures")))
               (:module "compilers"
                :serial t
                :components ((:file "ucr-explode")
                             (:file "cs-compile")
                             (:file "euler-compile")
                             (:file "qs-compile")
                             (:file "optimal-2q")
                             (:file "ucr-recognize")
                             (:file "state-prep")
                             (:file "translators")
                             (:file "approx")))
               (:module "analysis"
                :serial t
                :components ((:file "process-includes")
                             (:file "type-safety")
                             (:file "patch-labels")
                             (:file "resolve-applications")
                             (:file "qubits-needed")
                             (:file "compress-qubits")
                             (:file "expand-circuits")
                             (:file "rewrite-arithmetic")))
               (:file "cfg")
               (:file "compilation-methods")
               (:file "chip-specification")
               (:file "chip-reader")
               (:module "addresser"
                :serial t
                :components ((:file "rewiring")
                             (:file "initial-rewiring")
                             (:file "logical-schedule")
                             (:file "outgoing-schedule")
                             (:file "cost-function")
                             (:file "astar-rewiring-search")
                             (:file "path-heuristic")
                             (:file "temporal-addresser")))
               (:module "compressor"
                :serial t
                :components ((:file "rewriting-rule-data-type")
                             (:file "compressor-configuration")
                             (:file "context")
                             (:file "compressor")
                             (:file "wavefunctions")
                             (:file "rewriting-rules")))
               (:file "cl-quil")))
