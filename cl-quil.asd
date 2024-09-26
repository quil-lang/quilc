;;;; cl-quil.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:cl-quil
  :description "A parser and optimizing compiler for the Quantum Instruction Language (Quil)."
  :author "Robert Smith, Eric Peterson, Rigetti Computing, HRL Laboratories, and Quil-Lang contributors"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :version (:read-file-form "VERSION.txt")
  :pathname "src/"
  :depends-on (#:cl-quil/frontend
               #:alexandria
               (:version #:magicl/core "0.10.0")
               #:qvm
               #:cl-grnm                ; nelder-mead implementation
               #:clos-encounters
               #:yason                  ; JSON generation
               #:uiop
               #:split-sequence
               #:closer-mop
               #:cl-algebraic-data-type
               #:global-vars
               #:trivial-garbage        ; weak hash tables
               #:flexi-streams          ; For executable writing
               #:cl-heap
               #:cl-permutation
               #:queues.priority-queue)
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
                             (:file "sqisw-decomposition")
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
               #:clos-encounters
               #:split-sequence
               #:cl-algebraic-data-type
               #:cl-permutation
               #:trivial-garbage
               #:magicl/core
               #:magicl/ext-lapack      ; for gate fusion
               #:global-vars            ; Static globals
               #:salza2                 ; God table compression
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
               (:file "print-program")
               (:file "initialize-standard-gates")))

(asdf:defsystem #:cl-quil/coalton/ast
  :description "Coalton implementation src/cl-quil/ast.lisp and related types" 
  :depends-on (#:cl-quil #:coalton)
  :pathname "src/coalton/ast/"
  :serial t
  :components ((:file "memory")
               (:file "expression")
               (:file "classical")
               (:file "gate")
               (:file "macro")
               (:file "unresolved")
               (:file "native")))


(asdf:defsystem #:cl-quil/chip-library
  :description "Holds definitions for various chip ISAs."
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:cl-quil)
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-quil/chip-library-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/chip-library/"
  :serial t
  :components ((:file "package")
               (:file "chip-table")))

(asdf:defsystem #:cl-quil/chip-library-tests
  :description "Test suite for cl-quil/chip-library."
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:cl-quil
               #:cl-quil/chip-library
               #:fiasco)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':cl-quil.chip-library-tests
                                           '#:run-chip-library-tests))
  :pathname "tests/chip-library/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "chip-table-tests")))

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

(asdf:defsystem "cl-quil/coalton"
  :description "Coalton integration in `cl-quil`."
  :author "Yarin Heffes"
  :license "Apache License 2.0"
  :depends-on ("cl-quil"
               "coalton")
  :pathname "src/coalton"
  :serial t
  :components ((:file "coalton-quil")))

(asdf:defsystem "cl-quil/foust"
  :description "???"
  :author "Yarin Heffes"
  :license "Apache License 2.0"
  :depends-on ("coalton"
               "cl-quil/coalton")
  :pathname "src/foust/"
  :serial t
  :components ((:file "sign")
               (:file "angle")
               (:file "pauli-operator")
               (:file "pauli")
               (:file "frame")
               (:file "assignments")
               (:file "node")
               (:file "gate")
               (:file "circuit")
               (:file "graph")
               (:file "cost")
               (:file "reduce")
               (:file "optimize")
               (:file "compile")
               (:file "foust")
               (:file "foust-quil"))
  :in-order-to ((test-op (test-op "cl-quil-tests/foust-tests"))))

(asdf:defsystem #:cl-quil/discrete
  :description "Extensions to CL-QUIL to allow compilation to a discrete gate set."
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:cl-quil
               #:cl-quil/frontend
               #:cl-quil/chip-library
               #:closer-mop
               #:parse-float
               #:coalton
               #:coalton/library/big-float)
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-quil-tests/discrete-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/discrete/"
  :serial t
  :components ((:file "package")
               (:file "discrete-common")
               (:file "discrete-chip")
               (:module "numeric"
                :serial t
                :components ((:file "utilities")
                             (:file "linear-algebra")
                             (:file "naturals")
                             (:file "modulo")
                             (:file "interval")
                             (:file "root2plex")
                             (:file "cyclotomic8")
                             (:file "dyadic")
                             (:file "circle")))
               (:module "operators"
                :serial t
                :components ((:file "utilities")
                             (:file "mat2")
                             (:file "sunitary2")
                             (:file "unitary2")
                             (:file "hadamardt")
                             (:file "rz")
                             (:file "c2root2")
                             (:file "pauli")
                             (:file "clifford")
                             (:file "gates1")
                             (:file "ma-normal-form")))
               (:module "rz-approx"
                :serial t
                :components ((:file "candidate-generation")
                             (:file "candidate-verification")
                             (:file "generate-solution")))
               (:module "compilers"
                :serial t
                :components ((:file "clifford-t")))))

(asdf:defsystem #:cl-quil/quilec
  :description "Quantum error correction toolkit."
  :author "Juan M. Bello-Rivas"
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
  :author "Juan M. Bello-Rivas"
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
  :components ((:file "unicode-diagram")
               (:file "package")
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

(asdf:defsystem #:cl-quil/smt
  :description "Constraint-programming extensions to CL-QUIL."
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:cl-quil #:cl-smt-lib #:named-readtables)
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-quil/smt-tests)))
  :pathname "src/smt/"
  :serial t
  :components ((:file "package")
               (:file "solver")
               (:file "constraint-addresser")
               (:file "segment-2q")
               (:file "tan-cong")))

(asdf:defsystem #:cl-quil/smt-tests
  :description "Tests for constraint-programming extensions."
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:cl-quil #:cl-quil/smt #:fiasco)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':cl-quil.smt-tests
                                           '#:run-smt-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "smt-tests")))

(asdf:defsystem #:cl-quil/match
  :description "An implementation of the pattern matcher by Iten et al."
  :author "Brennen Hill, HRL Laboratories"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-quil/match-tests)))
  :pathname "src/match/"
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "gate")
               (:file "data-structures")
               (:file "forward-match")
               (:file "backward-match")
               (:file "pattern-match")
               (:file "pattern-replace")
               (:file "test-gate")
               (:file "print")))

(asdf:defsystem #:cl-quil/match-tests
  :description "Tests for cl-quil/match"
  :author "Brennen Hill, HRL Laboratories"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:cl-quil/match #:fiasco)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':cl-quil.match-tests
                                           '#:run-match-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "match-tests")))
