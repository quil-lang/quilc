(defpackage #:quil/ast/expression
  (:use #:coalton)
  (:export
   #:Expr
   #:Add
   #:Sub
   #:Mul
   #:Div
   #:Pow
   #:Neg
   #:Const
   #:Call
   #:Var
   #:Ref))
  
(in-package #:quil/ast/expression)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type (Expr :num)
    "Arithmetic expressions appearing in gate application parameter
     positions and in gate definitions.  In the latter case, Memory
     references may not appear."
    (Add (Expr :num) (Expr :num))
    (Sub (Expr :num) (Expr :num))
    (Mul (Expr :num) (Expr :num))
    (Div (Expr :num) (Expr :num))
    (Pow (Expr :num) (Expr :num))
    (Neg (Expr :num))
    (Const :num)
    (Call String (Expr :num))
    (Var String)
    (Ref String Ufix)))
