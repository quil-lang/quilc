(defpackage #:quil/ast/gate
  (:use #:coalton)
  (:local-nicknames
   (#:expr #:quil/ast/expression))
  (:export
   #:Gate
   #:I
   #:X
   #:Y
   #:Z
   #:H
   #:S
   #:T
   #:RX
   #:RY
   #:RZ
   #:Phase
   #:CNOT
   #:CZ
   #:SWAP
   #:ISWAP
   #:SQISWAP
   #:CSWAP
   #:CCNOT
   #:PSWAP
   #:PISWAP
   #:XY
   #:CAN
   #:BLOCH
   #:DAGGER
   #:CONTROLLED
   #:FORKED))
  
(in-package #:quil/ast/gate)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel 

  (define-type (Gate :num :arg)
    ;; User-defeind Gate
    (Gate  String                       ; Name
           (List (expr:Expr :num))           ; Params
           (List :arg))                 ; Args
    ;; Built-in Gates
    ;; -- one qubit gates
    (I :arg)
    (X :arg)
    (Y :arg)
    (Z :arg)
    (H :arg)
    (S :arg)
    (T :arg)

    ;; -- one parameter one qubit gates
    (RX (expr:Expr :num) :arg)
    (RY (expr:Expr :num) :arg)
    (RZ (expr:Expr :num) :arg)
    (Phase (expr:Expr :num) :arg)    

    ;; -- two qubit gates
    (CNOT :arg :arg)
    (CZ :arg :arg)
    (SWAP :arg :arg)
    (ISWAP :arg :arg)
    (SQISWAP :arg :arg)

    ;; -- three qubit gates
    (CSWAP :arg :arg :arg)
    (CCNOT :arg :arg :arg)

    ;; -- parameterized two qubit gates
    (PSWAP (expr:Expr :num) :arg :arg)
    (PISWAP (expr:Expr :num) :arg :arg)
    (XY (expr:Expr :num) :arg :arg)

    ;; -- multi-parameter gates
    (CAN (expr:Expr :num) (expr:Expr :num) (expr:Expr :num) :arg :arg)
    (BLOCH (expr:Expr :num) (expr:Expr :num) (expr:Expr :num) :arg)

    ;; --operators
    (DAGGER (Gate :num :arg))
    (CONTROLLED :arg (Gate :num :arg))
    (FORKED :arg (List (expr:Expr :num)) (Gate :num :arg))))
