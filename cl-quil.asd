;;;; cl-quil.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:cl-quil
  :description "A parser and optimizing compiler for the Quantum Instruction Language (Quil)."
  :author "Robert Smith <robert@rigetti.com>, Eric Peterson <eric@rigetti.com>, Rigetti Computing"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :version (:read-file-form "VERSION.txt")
  :pathname "src/"
  :depends-on (#:cl-quil/frontend
               #:alexandria
               (:version #:magicl/core "0.10.0")
               #:magicl/ext-lapack      ; for CSD
                                        ; internal linear algebra library
               #:qvm
               #:cl-grnm                ; nelder-mead implementation
               #:singleton-classes
               #:abstract-classes
               #:yason                  ; JSON generation
               #:uiop
               #:split-sequence
               #:closer-mop
               #:optima
               #:cl-algebraic-data-type
               #:global-vars
               #:trivial-garbage        ; weak hash tables
               #:flexi-streams          ; For executable writing
               #:cl-heap
               #:cl-permutation
               #:queues.priority-queue
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-quil-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :serial t
  :components ((:file "package")
               (:file "options")
               (:file "utilities")
               (:file "relabeling")
               (:file "matrix-operations")
               (:file "csd")
               (:file "resource")
               (:file "context")
               (:file "define-compiler")
               (:module "compilers"
                :serial t
                :components ((:file "ucr-explode")
                             (:file "euler-compile")
                             (:file "qs-compile")
                             (:file "ucr-recognize")
                             (:file "approx")
                             (:file "state-prep")
                             (:file "translators")
                             (:file "modifiers")
                             (:file "linear-paulis")
                             (:file "ansatz-search")
                             (:file "truth-table")
                             (:file "linear-reversible-circuits")
                             (:file "permutation")
                             ;; attic'd files / pedagogical purposes only
                             (:static-file "optimal-2q")
                             (:static-file "cs-compile")))
               (:file "cfg")
               (:file "compilation-methods")
               (:file "compiler-hook")
               (:module "chip"
                :serial t
                :components ((:file "chip-specification")
                             (:file "chip-reader")))
               (:module "backends"
                :serial t
                :components ((:file "common")
                             (:module "quil"
                              :serial t
                              :components ((:file "quil-backend")))))
               (:module "addresser"
                :serial t
                :components ((:file "conditions")
                             (:file "rewiring")
                             (:file "initial-rewiring")
                             (:file "logical-schedule")
                             (:file "outgoing-schedule")
                             (:file "addresser-common")
                             (:file "addresser-state")
                             (:file "1q-queues")
                             (:file "astar-rewiring-search")
                             (:file "path-heuristic")
                             (:file "qubit-heuristic")
                             (:file "temporal-addresser")
                             (:file "fidelity-addresser")))
               (:module "compressor"
                :serial t
                :components ((:file "compressor-configuration")
                             (:file "compressor")
                             (:file "wavefunctions")
                             (:file "rewriting-rules")))))

(asdf:defsystem #:cl-quil/frontend
  :description "Syntax, parsers, and basic analysis routines for the Quil language."
  :version (:read-file-form "VERSION.txt")
  :depends-on ((:version #:alexa "2.1.1")
                                        ; Lexical analysis
               #:yacc                   ; Arithmetic parsing
               #:alexandria             ; Utilities
               #:parse-float            ; Float parsing
               #:singleton-classes
               #:abstract-classes
               #:split-sequence
               #:cl-algebraic-data-type
               #:cl-permutation
               #:trivial-garbage
               #:magicl/core
               #:magicl/ext-lapack      ; for gate fusion
               #:global-vars            ; Static globals
               #:salza2                 ; God table compression
               #:optima

               #+sbcl #:sb-rotate-byte
               )
  :pathname "src/"
  :serial t
  :components ((:module "quil"
                :serial t
                :components ((:static-file "stdgates.quil")))
               (:file "package")
               (:file "frontend-options")
               (:file "types")
               (:file "frontend-utilities")
               (:file "queue")
               (:file "magicl-constructors")
               (:file "classical-memory")
               (:file "ast")
               (:file "define-pragma")
               (:file "pragmas")
               (:file "parser")
               (:file "cl-quil")
               (:file "qasm")
               (:file "gates")
               (:file "build-gate")
               (:file "with-inst")
               (:file "transformable-mixin") 
               (:module "clifford"
                :serial t
                :components ((:file "qubit-algebra")
                             (:file "pauli")
                             (:file "symplectic")
                             (:file "clifford")
                             (:file "stabilizer")
                             (:file "god-table")
                             (:file "god-table-utilities")
                             (:file "swap-representation")
                             (:file "benchmarking-procedures")
                             (:file "perm")))
               (:module "analysis"
                :serial t
                :components ((:file "process-includes")
                             (:file "type-safety")
                             (:file "patch-labels")
                             (:file "resolve-objects")
                             (:file "qubits-needed")
                             (:file "compress-qubits")
                             (:file "expansion")
                             (:file "expand-circuits")
                             (:file "rewrite-arithmetic")
                             (:file "fusion")
                             (:file "simplify-arithmetic")
                             (:file "validate-sequence-gate")
                             (:file "simplification-grab-bag")))
               (:file "initialize-standard-gates")))

;;; Contribs

;; Adapted from magicl's MAGICL/EXT-EXPOKIT adapted from commonqt's
;; qt.asd.

(asdf:defsystem #:cl-quil/quilt
  :description "Quil language extensions for pulse level quantum control."
  :license "Apache License 2.0 (See LICENSE.txt)"
  :maintainer "Rigetti Computing"
  :author "Rigetti Computing"
  :depends-on (#:cl-quil/frontend)
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-quil/quilt-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/quilt/"
  :serial t
  :components ((:file "package")
               (:file "ast")
               (:file "parser")
               (:file "waveform")
               (:module "analysis"
                :serial t
                :components ((:file "resolve-objects")
                             (:file "expand-calibrations")
                             (:file "type-safety")
                             (:file "fill-delays")))
               (:file "cl-quilt")))

(asdf:defsystem #:cl-quil/quilt-tests
  :description "Regression tests for Quilt language extensions to CL-QUIL."
  :author "Rigetti Computing"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:cl-quil-tests
               #:cl-quil/quilt)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':cl-quil.quilt-tests
                                           '#:run-quilt-tests))
  :pathname "tests/quilt/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "parser-tests")
               (:file "calibration-tests")
               (:file "analysis-tests")))

(asdf:defsystem #:cl-quil/quilec
  :description "Quantum error correction toolkit."
  :author "Juan M. Bello-Rivas <jbellorivas@rigetti.com>"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:alexandria
               #:cl-quil)
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-quil/quilec-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/quilec/"
  :serial t
  :components ((:file "package")
               (:file "matrix")
               (:file "stabilizer-group")
               (:file "cleve-gottesman")))

(asdf:defsystem #:cl-quil/quilec-tests
  :description "Test suite for cl-quil/quilec."
  :author "Juan M. Bello-Rivas <jbellorivas@rigetti.com>"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:cl-quil/quilec
               #:qvm
               #:fiasco)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':cl-quil.quilec-tests
                                           '#:run-quilec-tests))
  :pathname "tests/quilec/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "stabilizer-group-tests")
               (:file "cleve-gottesman-tests")))

(asdf:defsystem #:cl-quil/tools
  :description "Tools for cl-quil developers."
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:cl-quil
               #:common-lisp-jupyter
               #:swank)
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-quil/tools-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/tools/"
  :serial t
  :components ((:file "package")
               (:file "hasse-schedule")
               (:file "circuit-diagram")))

(asdf:defsystem #:cl-quil/tools-tests
  :description "Regression tests for tools for cl-quil developers."
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:cl-quil-tests
               #:cl-quil/tools)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':cl-quil.tools-tests
                                           '#:run-tools-tests))
  :pathname "tests/tools/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "hasse-diagram-tests")
               (:file "circuit-diagram-tests")))
