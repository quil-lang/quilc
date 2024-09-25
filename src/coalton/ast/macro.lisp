(defpackage #:quil/ast/macro
  (:use #:coalton)
  (:documentation
   "A subset of instructions which are permitted to appear inside circuit
    definitions.")
  (:local-nicknames
   (#:classical #:quil/ast/classical)
   (#:gate #:quil/ast/gate)
   (#:mem #:quil/ast/memory)
   (#:expr #:quil/ast/expression))
  (:export
   #:MaybeFormal
   #:Actual
   #:Formal

   #:Instruction
   #:ApplyGate
   #:ApplyOp
   #:ApplyCirc
   #:Pragma
   #:Label
   #:Jump
   #:JumpWhen
   #:JumpUnless
   #:Noop
   #:Halt
   #:Wait
   #:ResetAll
   #:Reset
   #:Measure
   #:MeasureDiscard))

(in-package #:quil/ast/macro)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type (MaybeFormal :t)
    (Actual :t)
    (Formal String))
  
  (define-type (Instruction :num)
    (ApplyGate (gate:Gate :num (MaybeFormal Ufix)))
    (ApplyOp (classical:Operation (MaybeFormal (classical:Arg :num))))
    (ApplyCirc String                        ; name
               (List (expr:Expr :num))       ; params 
               (List (MaybeFormal Ufix))     ; qubit arguments
               (List (Maybeformal mem:Ref))) ; memory refernce arguments

    (Pragma String)
    (Label String)
    (Jump String)
    (JumpWhen String (MaybeFormal mem:Ref))
    (JumpUnless String (MaybeFormal mem:Ref))

    Noop
    Halt
    Wait

    ResetAll
    (Reset (MaybeFormal Ufix ))
    
    (Measure (MaybeFormal Ufix) (MaybeFormal mem:Ref))
    (MeasureDiscard (MaybeFormal Ufix))))


