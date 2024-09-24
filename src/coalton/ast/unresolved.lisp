(defpackage #:quil/ast/unresolved
  (:use #:coalton)
  (:import-from #:coalton)
  (:export
   #:Ref
   #:Add
   #:Sub
   #:Mul
   #:Div
   #:Pow
   #:Neg
   #:Const
   #:Call
   #:Var
   #:RefExpr
   #:Expr

   #:MaybeFormal
   #:Actual
   #:Formal

   #:Gate
   #:I #:X #:Y #:Z #:H #:S #:T
   #:RX #:RY #:RZ #:Phase
   #:CNOT #:CZ #:SWAP #:ISWAP #:SQISWAP
   #:CSWAP #:CCNOT
   #:PSWAP #:PISWAP #:XY
   #:CAN
   #:BLOCH
   #:DAGGER
   #:CONTROLLED
   #:FORKED

   #:QuilType
   #:QuilBit #:QuilOctet #:QuilInteger #:QuilReal

   #:MemOffset
   #:offset-type #:offset-amount

   #:PauliTerm
   #:pauli-term-word
   #:pauli-term-prefactor
   #:pauli-term-arguments

   #:GateDef
   #:PermuationGateDef
   #:StaticGateDef
   #:ParameterizedGateDef
   #:PauliSumGateDef

   #:ClassicalOperation
   #:NotOp
   #:NegOp
   #:MoveOp
   #:ExchangeOp
   #:ConvertOp
   #:AndOp
   #:IOrOp
   #:XOrOp
   #:AddOp
   #:SubOp
   #:MulOp
   #:DivOp
   #:LoadOp
   #:StoreOp
   #:EqOp
   #:GtOp
   #:GeOp
   #:LtOp
   #:LeOp

   #:Instruction
   #:ApplyGate
   #:Include
   #:Pragma
   #:Memory
   #:GateDefinition
   #:CircuitDefinition
   #:Label
   #:ApplyOp
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
     

  
(in-package #:quil/ast/unresolved)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  
  (define-type Ref
    "A Memory Reference"
    (Ref String                         ; name
         Ufix                           ; position
         ))

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
    (RefExpr Ref))

  (define-type (MaybeFormal :t)
    "A wrapper type representing a value which may in some contexts be
     represented by a formal name."
    (Actual :t)
    (Formal String))

  (define-type (Gate :num)
    ;; User-defeind Gate
    (Gate  String                       ; Name
           (List (Expr :num))           ; Params
           (List (MaybeFormal Ufix)))   ; Args
    ;; Built-in Gates
    ;; -- one qubit gates
    (I (MaybeFormal Ufix))
    (X (MaybeFormal Ufix))
    (Y (MaybeFormal Ufix))
    (Z (MaybeFormal Ufix))
    (H (MaybeFormal Ufix))
    (S (MaybeFormal Ufix))
    (T (MaybeFormal Ufix))

    ;; -- one parameter one qubit gates
    (RX (Expr :num) (MaybeFormal Ufix))
    (RY (Expr :num) (MaybeFormal Ufix))
    (RZ (Expr :num) (MaybeFormal Ufix))
    (Phase (Expr :num) (MaybeFormal Ufix))    

    ;; -- two qubit gates
    (CNOT (MaybeFormal Ufix) (MaybeFormal Ufix))
    (CZ (MaybeFormal Ufix) (MaybeFormal Ufix))
    (SWAP (MaybeFormal Ufix) (MaybeFormal Ufix))
    (ISWAP (MaybeFormal Ufix) (MaybeFormal Ufix))
    (SQISWAP (MaybeFormal Ufix) (MaybeFormal Ufix))

    ;; -- three qubit gates
    (CSWAP (MaybeFormal Ufix) (MaybeFormal Ufix) (MaybeFormal Ufix))
    (CCNOT (MaybeFormal Ufix) (MaybeFormal Ufix) (MaybeFormal Ufix))

    ;; -- parameterized two qubit gates
    (PSWAP (Expr :num) (MaybeFormal Ufix) (MaybeFormal Ufix))
    (PISWAP (Expr :num) (MaybeFormal Ufix) (MaybeFormal Ufix))
    (XY (Expr :num) (MaybeFormal Ufix) (MaybeFormal Ufix))

    ;; -- multi-parameter gates
    (CAN (Expr :num) (Expr :num) (Expr :num) (MaybeFormal Ufix) (MaybeFormal Ufix))
    (BLOCH (Expr :num) (Expr :num) (Expr :num) (MaybeFormal Ufix))

    ;; --operators
    (DAGGER (Gate :num))
    (CONTROLLED (MaybeFormal Ufix) (Gate :num))
    (FORKED (MaybeFormal Ufix) (List (Expr :num)) (Gate :num)))

  (repr :enum)
  (define-type QuilType
    "A valid data type for Quil memory."
    QuilBit QuilOctet QuilInteger QuilReal)

  (define-type MemOffset
    "Used in declaring offsets into shared memory declarations"
    (MemOffset QuilType Ufix))

  (define (offset-type (MemOffset type _)) type)
  (define (offset-amount (MemOffset _ amount)) amount)

  (define-type (PauliTerm :num)
    (PauliTerm String              ; pauli word, string of pauli gate names X,Y,Z,I
               :num                ; prefactor
               (List String)))     ; arguments

  (define (pauli-term-word (PauliTerm word _ _)) word)
  (define (pauli-term-prefactor (PauliTerm _ pf _)) pf)
  (define (pauli-term-arguments (PauliTerm _ _ args)) args)

  (define-type (GateDef :num)
    (PermuationGateDef (List UFix))
    (StaticGateDef (List :num))
    (ParameterizedGateDef (List String)
                          (List (Expr :num)))
    (PauliSumGateDef (List (PauliTerm :num)) ; terms
                     (List String)           ; params
                     (List String)))         ; arguments

  (define-type ClassicalOperation
    ;; Unary
    (NotOp (MaybeFormal Ref))
    (NegOp (MaybeFormal Ref))
    ;; Binary
    (MoveOp (MaybeFormal Ref) (MaybeFormal Ref))
    (ExchangeOp (MaybeFormal Ref) (MaybeFormal Ref))
    (ConvertOp (MaybeFormal Ref) (MaybeFormal Ref))
    (AndOp (MaybeFormal Ref) (MaybeFormal Ref))
    (IOrOp (MaybeFormal Ref) (MaybeFormal Ref))
    (XOrOp (MaybeFormal Ref) (MaybeFormal Ref))
    (AddOp (MaybeFormal Ref) (MaybeFormal Ref))
    (SubOp (MaybeFormal Ref) (MaybeFormal Ref))
    (MulOp (MaybeFormal Ref) (MaybeFormal Ref))
    (DivOp (MaybeFormal Ref) (MaybeFormal Ref))
    ;; Ternary
    (LoadOp (MaybeFormal Ref) (MaybeFormal Ref) (MaybeFormal Ref))
    (StoreOp (MaybeFormal Ref) (MaybeFormal Ref) (MaybeFormal Ref))
    (EqOp (MaybeFormal Ref) (MaybeFormal Ref) (MaybeFormal Ref))
    (GtOp (MaybeFormal Ref) (MaybeFormal Ref) (MaybeFormal Ref))
    (GeOp (MaybeFormal Ref) (MaybeFormal Ref) (MaybeFormal Ref))
    (LtOp (MaybeFormal Ref) (MaybeFormal Ref) (MaybeFormal Ref))
    (LeOp (MaybeFormal Ref) (MaybeFormal Ref) (MaybeFormal Ref)))

  (define-type (Instruction :num)
    ;; generic gate/circuit application 
    (ApplyGate (Gate :num))
    ;; include a .quil file
    (Include String)
    ;; E.g. (Pragma "REWIRING_SEARCH \"A*\"")
    (Pragma String)
    ;; Classical Memory Declaration
    (Memory String                      ; name 
            QuilType                    ; type 
            UFix                        ; length 
            Boolean                     ; sharing parent
            (List MemOffset))           ; sharing offsets

    (GateDefinition String              ; name
                    (GateDef :num))     ; definition

    (CircuitDefinition
     String                             ; name
     (List String)                      ; parameter names
     (List String)                      ; arguments names
     (List (Instruction :num)))         ; body

    (Label String)

    (ApplyOp ClassicalOperation)

    (Jump String)
    (JumpWhen String (MaybeFormal Ref))
    (JumpUnless String (MaybeFormal Ref))

    Noop
    Halt
    Wait

    ResetAll
    (Reset (MaybeFormal Ufix))
    
    (Measure (MaybeFormal Ufix) (MaybeFormal Ref))
    (MeasureDiscard (MaybeFormal Ufix)))
  ;; end module
  )






