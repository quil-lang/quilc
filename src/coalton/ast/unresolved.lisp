(defpackage #:quil/ast/unresolved
  (:use #:coalton)
  (:import-from #:coalton)
  (:local-nicknames
   (#:expr #:quil/ast/expression)
   (#:macro #:quil/ast/macro)
   (#:gate #:quil/ast/gate)
   (#:mem #:quil/ast/memory)
   (#:classical #:quil/ast/classical))

  (:export
   #:PauliTerm
   #:pauli-term-word
   #:pauli-term-prefactor
   #:pauli-term-arguments
   #:GateDef
   #:PermuationGateDef
   #:StaticGateDef
   #:ParameterizedGateDef
   #:PauliSumGateDef
   #:Instruction
   #:ApplyGate
   #:ApplyOp
   #:ApplyCirc
   #:Include
   #:Pragma
   #:DeclareMem
   #:GateDefinition
   #:CircuitDefinition
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
  
(in-package #:quil/ast/unresolved)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  
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
                          (List (expr:Expr :num)))
    (PauliSumGateDef (List (PauliTerm :num)) ; terms
                     (List String)           ; params
                     (List String)))         ; arguments

  (define-type (Instruction :num)
    ;; generic gate/circuit application 
    (ApplyGate (gate:Gate :num Ufix))
    (ApplyOp (classical:Operation (classical:Arg :num)))
    (ApplyCirc String                   ; name
               (List (expr:Expr :num))  ; params 
               (List Ufix)              ; qubit arguments
               (List mem:Ref)           ; memory refernce argumetns
               )

    ;; include a .quil file
    (Include String)
    ;; E.g. (Pragma "REWIRING_SEARCH \"A*\"")
    (Pragma String)
    ;; Classical Memory Declaration
    (DeclareMem mem:Descriptor)

    (GateDefinition String              ; name
                    (GateDef :num))     ; definition

    (CircuitDefinition
     String                             ; name
     (List String)                      ; parameter names
     (List String)                      ; qubit arg names
     (List String)                      ; memref arg names
     (List (macro:Instruction :num)))   ; body

    (Label String)

    (Jump String)
    (JumpWhen String mem:Ref)
    (JumpUnless String mem:Ref)

    Noop
    Halt
    Wait

    ResetAll
    (Reset Ufix)
    
    (Measure Ufix mem:Ref)
    (MeasureDiscard Ufix))
  ;; end module
  )






