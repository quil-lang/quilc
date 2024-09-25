(defpackage #:quil/ast/classical
  (:use #:coalton)
  (:shadow #:And)
  (:export
   #:Arg
   #:Ref
   #:Const
   #:Operation
   #:Not
   #:Neg
   #:Move
   #:Exchange
   #:Convert
   #:And
   #:IOr
   #:XOr
   #:Add
   #:Sub
   #:Mul
   #:Div
   #:Load
   #:Store
   #:Eq
   #:Gt
   #:Ge
   #:Lt
   #:Le))

(in-package #:quil/ast/classical)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-type (Arg :num)
    (Ref String Ufix)
    (Const :num))
  
  (define-type (Operation :arg)
    ;; Unary
    (Not :arg)
    (Neg :arg)
    ;; Binary
    (Move :arg :arg)
    (Exchange :arg :arg)
    (Convert :arg :arg)
    (And :arg :arg)
    (IOr :arg :arg)
    (XOr :arg :arg)
    (Add :arg :arg)
    (Sub :arg :arg)
    (Mul :arg :arg)
    (Div :arg :arg)
    ;; Ternary
    (Load :arg :arg :arg)
    (Store :arg :arg :arg)
    (Eq :arg :arg :arg)
    (Gt :arg :arg :arg)
    (Ge :arg :arg :arg)
    (Lt :arg :arg :arg)
    (Le :arg :arg :arg)))
