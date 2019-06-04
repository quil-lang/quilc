;;;; cl-quil-tests.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:cl-quil-tests
  :description "Regression tests for CL-QUIL."
  :author "Robert Smith <robert@rigetti.com>"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:cl-quil
               #:magicl
               #:alexandria
               #:fiasco
               #:uiop
               #:yacc                     ; for the conditions
               (:version #:alexa "1.0.1") ; for the conditions
               )
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':cl-quil-tests
                                           '#:run-cl-quil-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "utilities")
               (:file "initial-rewiring-tests")
               (:file "lexer-tests")
               (:file "parser-tests")
               (:file "printer-tests")
               (:file "classical-memory-tests")
               (:file "resource-tests")
               (:file "misc-tests")
               (:file "analysis-tests")
               (:file "cfg-tests")
               (:file "defcircuit-tests")
               (:file "compilation-tests")
               (:file "clifford-tests")
               (:file "translator-tests")
               (:file "state-prep-tests")
               (:file "compiler-hook-tests")
               (:file "benchmarking-procedures-tests")
               (:file "typed-memory-tests")
               (:file "approximation-tests")))
