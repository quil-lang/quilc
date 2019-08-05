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
               (:version #:magicl "0.6.2")
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
               #:cl-heap
               #:cl-permutation
               #:queues.priority-queue
               #+sbcl #:sb-rotate-byte
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
               (:file "types")
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
               (:file "context")
               (:file "build-gate")
               (:file "define-compiler")
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
               (:module "compilers"
                :serial t
                :components ((:file "ucr-explode")
                             (:file "euler-compile")
                             (:file "qs-compile")
                             (:file "ucr-recognize")
                             (:file "approx")
                             (:file "state-prep")
                             (:file "translators")
                             (:file "optimal-2q")
                             ;; attic'd file / pedagogical purposes only
                             (:static-file "cs-compile")))
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
                :components ((:file "compressor-configuration")
                             (:file "compressor")
                             (:file "wavefunctions")
                             (:file "rewriting-rules")))
               (:file "cl-quil")))

;;; Contribs

;; Adapted from magicl's magicl-transcendental adapted from commonqt's
;; qt.asd.

;; NOTE: The following contrib requires a C++17 compiler, and is untested on
;; Windows
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
  (labels ((nn (x) (uiop:native-namestring x))
           (compile-it (c++17 c++17-args)
             (uiop:run-program
              ;; TODO This needs to be platform agnostic. The difficulty is
              ;; in picking up a c++17 compiler automatically.
              (cons c++17 c++17-args)
              :error-output t)))
    (let* ((c-file (component-pathname component))
           (cwd (namestring (make-pathname :directory (pathname-directory c-file))))
           (tweedlelibdir (merge-pathnames "tweedledum/" cwd))
           (shared-object (make-pathname :type #+darwin "dylib" #-darwin "so"
                                         :name "libtweedledum"
                                         :defaults c-file)))
      (unless (uiop:directory-exists-p tweedlelibdir)
        (error "tweedledum library directory missing. Did you run ~
                `git submodule init && git submodule update --init`?"))
      (let ((c++17 (or (uiop:getenv "CXX")
                       "g++"))
            (c++17-args (list "-std=c++17"
                              "-shared"
                              "-fPIC"
                              "-DFMT_HEADER_ONLY"
                              (format nil "-I~a" (merge-pathnames "libs/fmt" tweedlelibdir))
                              (format nil "-I~a" (merge-pathnames "libs/easy" tweedlelibdir))
                              (format nil "-I~a" (merge-pathnames "libs/glucose" tweedlelibdir))
                              (format nil "-I~a" (merge-pathnames "libs/kitty" tweedlelibdir))
                              (format nil "-I~a" (merge-pathnames "include" tweedlelibdir))
                              "-o" (nn shared-object)
                              (nn c-file))))
        (restart-case
            (compile-it c++17 c++17-args)
          (restart-with-c++17-path-input (c++17)
            :report "Enter path to C++17 compiler and restart"
            :interactive (lambda ()
                           (format *query-io* "Enter path to C++17 compiler and restart: ")
                           (force-output *query-io*)
                           (list (read-line)))
            (compile-it c++17 c++17-args)))))))

(asdf:defsystem #:cl-quil/tweedledum
  :license "Apache License 2.0 (See LICENSE.txt)"
  :maintainer "Rigetti Computing"
  :author "Rigetti Computing"
  :description "C++17 Library for writing, manipulating, and optimizing quantum circuits"
  :depends-on (#:cffi #:cl-quil)
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-quil/tweedledum-tests)))
  :pathname "src/contrib/tweedledum/"
  :serial t
  :components
  ((c->so "tweedledum.c")
   (:file "tweedledum")))

(asdf:defsystem #:cl-quil/tweedledum-tests
  :depends-on (#:cl-quil-tests #:cl-quil/tweedledum)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':cl-quil.tweedledum
                                           '#:run-tweedledum-tests)))
