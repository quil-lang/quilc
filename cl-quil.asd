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
               #:trivial-garbage        ; weak hash tables
               #:cl-permutation
               #:queues.priority-queue
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-quil-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl(sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :serial t
  :components ((:module "quil"
                :serial t
                :components ((:static-file "stdgates.quil")))
               (:file "package")
               (:file "options")
               (:file "utilities")
               (:file "relabeling")
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
                             (:file "ucr-recognize")
                             (:file "approx")
                             (:file "state-prep")
                             (:file "translators")
                             (:file "optimal-2q")))
               (:module "analysis"
                :serial t
                :components ((:file "process-includes")
                             (:file "type-safety")
                             (:file "patch-labels")
                             (:file "resolve-applications")
                             (:file "qubits-needed")
                             (:file "compress-qubits")
                             (:file "expand-circuits")
                             (:file "rewrite-arithmetic")
                             (:file "fusion")))
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
               (:file "cl-quil")
               (:file "contribs")))

;;; Contribs

;; Adapted from magicl's magicl-transcendental adapted from commonqt's
;; qt.asd.
(defclass c->so (asdf:source-file)
 ())

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

(asdf:defsystem #:cl-quil/tweedledum
  :license "Apache License 2.0 (See LICENSE.txt)"
  :maintainer "Rigetti Computing"
  :author "Rigetti Computing"
  :description "C++17 Library for writing, manipulating, and optimizing quantum circuits"
  :depends-on (#:cffi #:cl-quil)
  :pathname "src/contrib/tweedledum/"
  :serial t
  :components
  ((c->so "tweedledum.c")
   (:file "tweedledum")))
