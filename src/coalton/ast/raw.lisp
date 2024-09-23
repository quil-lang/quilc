(defpackage #:quil/ast/raw
  (:use #:coalton)
  (:documentation "Native Cl-QUIL and CL-QUIL.FRONTEND Types")
  (:export
   #:Program
   #:Instruction
   #:QubitArg
   #:Param
   #:MRef
   #:RefArg
   #:QuilType))

(in-package #:quil/ast/raw)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :native cl-quil:quil-type)
  (define-type QuilType)

  (repr :native cl-quil:parsed-program)
  (define-type Program)

  (repr :native (cl:or cl-quil:qubit cl-quil:formal))
  (define-type QubitArg)

  (repr :native (cl:or cl-quil:formal cl-quil:memory-ref))
  (define-type RefArg)

  (repr :native cl-quil:memory-ref)
  (define-type MRef)

  (repr :native (cl:or cl-quil:param
                       cl-quil.frontend:delayed-expression
                       cl-quil:constant cl:null))
  (define-type Param)

  (repr :native (cl:or cl-quil:instruction
                       cl-quil:memory-descriptor
                       cl-quil:jump-target
                       cl-quil:gate-definition
                       cl-quil:circuit-definition))
  (define-type Instruction))
