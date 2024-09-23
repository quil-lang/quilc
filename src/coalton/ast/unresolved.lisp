(defpackage #:quil/ast/unresolved
  (:use #:coalton)
  (:import-from #:coalton-library/classes #:map)
  (:import-from #:coalton)
  (:local-nicknames
   (#:quil #:cl-quil.frontend)
   (#:complex #:coalton-library/math/complex)
   (#:raw #:quil/ast/raw)))

(in-package #:quil/ast/unresolved)

(named-readtables:in-readtable coalton:coalton)


;;; HELPERS TO COPE WITH RESOLVING ARITHMETIC EXPRESSIONS TO THEIR
;;; "RAW" CL-QUIL REPRESENTATIONS

(cl:defun get-expr-params (expr)
  "Helper function.  Returns a list suitable for filling the PARAMETERS
slot of a DELAYED-EXPRESSION instance."
  (cl:etypecase expr
    (cl:null cl:nil)
    (quil:param (cl:list expr))
    (quil:constant cl:nil)
    (quil:delayed-expression
     (quil:delayed-expression-params expr))))

(cl:defun get-expr-lambda-params (expr)
  "Helper function. Returns a list of symbols suitable for filling the
LAMBDA-PARAMETERS slot of a DELAYED-EXPRESSION instance"
  (cl:etypecase expr
    (cl:null cl:nil)
    (quil:param (cl:list (cl:make-symbol (quil:param-name expr))))
    (quil:constant cl:nil)
    (quil:delayed-expression
     (quil:delayed-expression-lambda-params expr))))

(cl:defun get-expr-exprs (expr)
  "Helper function, returns a value that can appear as a
sub-expression (a SEXP) in a DELAYED-EXPRESSIONS instance's EXPRESSION
slot."
  (cl:etypecase expr
    (cl:null cl:nil)
    (quil:param (cl:make-symbol (quil:param-name expr)))
    (quil:constant (quil:constant-value expr))
    (quil:delayed-expression
     (quil:delayed-expression-expression expr))))

(cl:defmacro combine-params (str arg1 cl:&optional arg2)
  "Generates a form to produce a DELAYED-EXPRESSION out of the values of
forms ARG1 and ARG2, which may be anything that can appear in a
parameter postion of a gate application."
  (cl:let* ((var1 (cl:gensym "VAR"))
            (var2 (cl:gensym "VAR"))
            (vars (cl:list var1 var2))
            (new-params (cl:gensym "PARAMS"))
            (new-lambda-params (cl:gensym "LAMBDAPARAMS"))
            (new-expression (cl:gensym "EXPR"))
            (op (cl:gensym "OP")))
    `(let ((,var1 (expr-to-raw ,arg1))
           (,var2 ,(cl:if arg2
                          `(expr-to-raw ,arg2)
                          `(lisp raw:Param () cl:nil))))
       (lisp raw:Param ,(cl:if (cl:stringp str) vars (cl:cons str vars))
         (cl:let* ((,op (cl:intern (cl:string-upcase ,str) :common-lisp))
                   (,new-params
                     (cl:union (get-expr-params ,var1)
                               (get-expr-params ,var2)
                               :test 'cl:equalp))
                   (,new-lambda-params
                     (cl:union (get-expr-lambda-params ,var1)
                               (get-expr-lambda-params ,var2)
                               :test 'cl:string=))
                   (,new-expression
                     (cl:list* ,op
                               (get-expr-exprs ,var1)
                               (cl:if ,var2
                                      (cl:list
                                       (get-expr-exprs ,var2))
                                      cl:nil))))
           (quil:make-delayed-expression
            ,new-params ,new-lambda-params ,new-expression))))))


;; HELPER MACROS TO INSTANTIATE CLASSICAL MEMORY OPERATION INSTRUCTIONS

(cl:defmacro unary-classical-op (name ref)
  (cl:let ((rawvar (cl:gensym)))
    `(let ((,rawvar (raw-ref-arg ,ref)))
       (lisp raw:Instruction (,rawvar)
         (cl:make-instance (cl:find-symbol ,name :cl-quil.frontend)
           :target ,rawvar)))))

(cl:defmacro binary-classical-op (name left right)
  (cl:let ((lvar (cl:gensym))
           (rvar (cl:gensym)))
    `(let ((,lvar (raw-ref-arg ,left))
           (,rvar (raw-ref-arg ,right)))
       (lisp raw:Instruction (,lvar ,rvar)
         (cl:make-instance (cl:find-symbol ,name :cl-quil.frontend)
           :left ,lvar :right ,rvar)))))

(cl:defmacro ternary-classical-op (name target left right)
  (cl:let ((tvar (cl:gensym))
           (lvar (cl:gensym))
           (rvar (cl:gensym)))
    `(let ((,tvar (raw-ref-arg ,target))
           (,lvar (raw-ref-arg ,left))
           (,rvar (raw-ref-arg ,right)))
       (lisp raw:Instruction (,tvar ,lvar ,rvar)
         (cl:make-instance (cl:find-symbol ,name :cl-quil.frontend)
           :left ,lvar :right ,rvar :target ,tvar)))))

(coalton-toplevel
  
  (define-type Ref
    "A Memory Reference"
    (Ref String                         ; name
         Ufix                           ; position
         ))

  (declare ref-to-raw (Ref -> raw:Mref))
  (define (ref-to-raw (Ref name loc))
    (lisp raw:Mref (name loc)
      (quil:mref name loc)))

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

  (declare raw-quil-type (QuilType -> raw:QuilType))
  (define (raw-quil-type qt)
    (match qt
      ((QuilBit) (lisp raw:QuilType () quil:quil-bit))
      ((QuilOctet) (lisp raw:QuilType () quil:quil-octet))
      ((QuilInteger) (lisp raw:QuilType () quil:quil-integer))
      ((QuilReal) (lisp raw:QuilType () quil:quil-real))))

  (define-type MemOffset
    "Used in declaring offsets into shared memory declarations"
    (MemOffset QuilType Ufix))

  (define (offset-type (MemOffset type _)) type)
  (define (offset-amount (MemOffset _ amount)) amount)

  (define-type (PauliTerm :num)
    (PauliTerm String ; pauli word, string of pauli gate names X,Y,Z,I
               :num   ; prefactor
               (List String)))          ; arguments

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

  ;; If non-literal numbertypes are supported, this should be altered
  ;; to be something like (real :num, complex :num => ..)
  ;; and the Const case be handled to interperet the :num to a type
  ;; that quilc can already handle
  (declare expr-to-raw (Expr :num -> raw:Param))
  (define (expr-to-raw e)
    (match e
      ((Add e1 e2) (combine-params "+" e1 e2))

      ((Sub e1 e2) (combine-params "-" e1 e2))

      ((Mul e1 e2) (combine-params "*" e1 e2))

      ((Div e1 e2) (combine-params "/" e1 e2))

      ((Pow e1 e2) (combine-params "EXPT" e1 e2))

      ((Neg e1) (combine-params  "-" e1))
      
      ((Const e)
       (lisp raw:Param (e)
         (quil:constant e)))

      ((RefExpr rf)
       (let ((rr (ref-to-raw rf)))
         (lisp raw:Param (rr)
           (quil:make-delayed-expression cl:nil cl:nil rr))))

      ((Var name)
       (lisp raw:Param (name)
         (quil:param name)))

      ((Call name e)
       (combine-params name e))))

  (declare arg-to-raw ((MaybeFormal Ufix) -> raw:QubitArg))
  (define (arg-to-raw a)
    (match a
      ((Actual qb) (lisp raw:QubitArg (qb) (quil:qubit qb)))
      ((Formal name) (lisp raw:QubitArg (name) (quil:formal name)))))

  (declare raw-gate-application
           (String -> (List (Expr :num)) -> (List (MaybeFormal Ufix)) -> raw:Instruction))
  (define (raw-gate-application name params args)
    (let ((params* (map expr-to-raw params))
          (args* (map arg-to-raw args)))
      (lisp raw:Instruction (name params* args*)
        (cl:make-instance 'quil:unresolved-application
          :operator (quil:named-operator name)
          :parameters params*
          :arguments args*))))

  (define (raw-gate-def name gdef)
    (match gdef
      ((PermuationGateDef vals)
       (lisp raw:Instruction (name vals)
         (cl:make-instance 'quil:permutation-gate-definition
           :name name :permutation vals :context cl:nil)))

      ((StaticGateDef entries)
       (lisp raw:Instruction (name entries)
         (cl:make-instance 'quil:static-gate-definition
           :name name
           :entries entries)))

      ((ParameterizedGateDef params exprs)
       (let ((exprs* (map expr-to-raw exprs)))
         (lisp raw:instruction (name params exprs*)
           (cl:make-instance 'quil:parameterized-gate-definition
             :name name
             :entries (cl:mapcar 'get-expr-exprs exprs*)
             :parameters (cl:mapcar 'cl:make-symbol params)))))

      ((PauliSumGateDef terms params args)
       (lisp raw:Instruction (name terms params args)
         (cl:make-instance 'quil:exp-pauli-sum-gate-definition
           :name name
           :arguments (cl:mapcar #'quil:formal args)
           :parameters (cl:mapcar 'cl:make-symbol params)
           :terms (cl:loop :for term :in terms
                     :collect (quil:make-pauli-term
                               :pauli-word (pauli-term-word term)
                               :prefactor (pauli-term-prefactor term)
                               :arguments (cl:mapcar #'quil:formal (pauli-term-arguments term)))))))))

  
  (define (raw-classical-op op)
    (match op
      ((NegOp r)
       (unary-classical-op "CLASSICAL-NEGATE" r))
      ((NotOp r)
       (unary-classical-op "CLASSICAL-NOT" r))

      ((MoveOp a b)
       (binary-classical-op "CLASSICAL-MOVE" a b))
      ((ExchangeOp a b)
       (binary-classical-op "CLASSICAL-EXCHANGE" a b))
      ((ConvertOp a b)
       (binary-classical-op "CLASSICAL-CONVERT" a b))
      ((AndOp a b)
       (binary-classical-op "CLASSICAL-AND" a b))
      ((IOrOp a b)
       (binary-classical-op "CLASSICAL-INCLUSIVE-OR" a b))
      ((XOrOp a b)
       (binary-classical-op "CLASSICAL-EXCLUSIVE-OR" a b))
      ((AddOp a b)
       (binary-classical-op "CLASSICAL-ADDITION" a b))
      ((SubOp a b)
       (binary-classical-op "CLASSICAL-SUBTRACTION" a b))
      ((MulOp a b)
       (binary-classical-op "CLASSICAL-MULTIPLICATION" a b))
      ((DivOp a b)
       (binary-classical-op "CLASSICAL-DIVISION" a b))

      ((LoadOp a b c)
       (ternary-classical-op "CLASSICAL-LOAD" a b c))
      ((StoreOp a b c)
       (ternary-classical-op "CLASSICAL-STORE" a b c))
      ((EqOp a b c)
       (ternary-classical-op "CLASSICAL-EQUALITY" a b c))
      ((GtOp a b c)
       (ternary-classical-op "CLASSICAL-GREATER-THAN" a b c))
      ((GeOp a b c)
       (ternary-classical-op "CLASSICAL-GREATER-EQUAL" a b c))
      ((LtOp a b c)
       (ternary-classical-op "CLASSICAL-LESS-THAN" a b c))
      ((LeOp a b c)
       (ternary-classical-op "CLASSICAL-LESS-EQUAL" a b c))))

  (declare raw-ref-arg (MaybeFormal Ref -> raw:RefArg))
  (define (raw-ref-arg ja)
    (match ja
      ((Formal name)
       (lisp raw:RefArg (name)
         (quil:formal name)))
      ((Actual (Ref name loc))
       (lisp raw:RefArg (name loc)
         (quil:mref name loc)))))

  (declare instr-to-raw (Instruction :num -> raw:Instruction))
  (define (instr-to-raw instr)
    (match instr
      ((ApplyGate g) (gate-to-raw g))

      ((Include name)
       (lisp raw:Instruction (name)
         (cl:make-instance 'quil:include :pathname name)))

      ((Pragma pstring)
       (lisp raw:Instruction (pstring)
         (quil::parse-pragma
          (quil::tokenize
           (cl:with-output-to-string (out)
             (cl:write-string "PRAGMA ")
             (cl:write-string pstring))))))

      ((Memory name type length sharing offsets)
       (lisp raw:instruction (name type length sharing offsets)
         (quil:make-memory-descriptor
          :name name
          :type (raw-quil-type type)
          :length length
          :sharing-parent sharing
          :sharing-offset-alist
          (cl:loop :for offset :in offsets
             :collect (cl:cons
                       (raw-quil-type (offset-type offset))
                       (offset-amount offset))))))
      
      ((GateDefinition name gdef)
       (raw-gate-def name gdef))

      ((CircuitDefinition name params args body)
       (let ((instrs (map instr-to-raw body)))
         (lisp raw:Instruction (name params args instrs)
           (cl:make-instance 'quil:circuit-definition
             :name name
             :parameters (cl:mapcar 'quil:param params)
             :arguments (cl:mapcar 'quil:formal args)
             :body instrs))))

      ((ApplyOp op)
       (raw-classical-op op))

      ((Label l)
       (lisp raw:Instruction (l)
         (cl:make-instance 'quil:jump-target
           :label (quil:label l))))

      ((Jump l)
       (lisp raw:Instruction (l)
         (cl:make-instance 'quil:unconditional-jump
           :label (quil:label l))))

      ((JumpWhen l a)
       (lisp raw:Instruction (l a)
         (cl:make-instance 'quil:jump-when
           :label (quil:label l)
           :address (raw-ref-arg a))))

      ((JumpUnless l a)
       (lisp raw:Instruction (l a)
         (cl:make-instance 'quil:jump-unless
           :label (quil:label l)
           :address (raw-ref-arg a))))

      ((Noop)
       (lisp raw:Instruction ()
         (cl:make-instance 'quil:no-operation)))

      ((Halt)
       (lisp raw:Instruction ()
         (cl:make-instance 'quil:halt)))


      ((Wait)
       (lisp raw:Instruction ()
         (cl:make-instance 'quil:wait)))

      ((ResetAll)
       (lisp raw:Instruction ()
         (cl:make-instance 'quil:reset)))

      ((Reset qb)
       (let ((qb* (arg-to-raw qb)))
         (lisp raw:Instruction (qb*)
           (cl:make-instance 'quil:reset-qubit :target qb*))))

      ((Measure qb loc)
       (let ((qb* (arg-to-raw qb))
             (loc* (raw-ref-arg loc)))
         (lisp raw:Instruction (qb* loc*)
           (cl:make-instance 'quil:measure :address loc* :qubit qb*))))


      ((MeasureDiscard qb)
       (let ((qb* (arg-to-raw qb)))
         (lisp raw:Instruction (qb*)
           (cl:make-instance 'quil:measure-discard :qubit qb*))))))

  (declare gate-to-raw ((Gate :num) -> raw:Instruction))
  (define (gate-to-raw g)
    (match g
      ((Gate name params args)
       (raw-gate-application name params args))
      ((I a)
       (raw-gate-application "I" nil (make-list a)))
      ((X a)
       (raw-gate-application "X" nil (make-list a)))
      ((Y a)
       (raw-gate-application "Y" nil (make-list a)))
      ((Z a)
       (raw-gate-application "Z" nil (make-list a)))
      ((H a)
       (raw-gate-application "H" nil (make-list a)))
      ((S a)
       (raw-gate-application "S" nil (make-list a)))
      ((T a)
       (raw-gate-application "T" nil (make-list a)))
      ((RX e a)
       (raw-gate-application "RX" (make-list e) (make-list a)))
      ((RY e a)
       (raw-gate-application "RY" (make-list e) (make-list a)))
      ((RZ e a)
       (raw-gate-application "RZ" (make-list e) (make-list a)))
      ((PHASE e a)
       (raw-gate-application "PHASE" (make-list e) (make-list a)))    
      ((CNOT a b)
       (raw-gate-application "CNOT" nil (make-list a b)))
      ((CZ a b)
       (raw-gate-application "CZ" nil (make-list a b)))
      ((SWAP a b)
       (raw-gate-application "SWAP" nil (make-list a b)))
      ((ISWAP a b)
       (raw-gate-application "ISWAP" nil (make-list a b)))
      ((SQISWAP a b)
       (raw-gate-application "SQISWAP" nil (make-list a b)))
      ((CSWAP a b c)
       (raw-gate-application "CSWAP" nil (make-list a b c)))
      ((CCNOT a b c)
       (raw-gate-application "CCNOT" nil (make-list a b c)))
      ((PSWAP e a b)
       (raw-gate-application "PSWAP" (make-list e) (make-list a b)))
      ((PISWAP e a b)
       (raw-gate-application "PISWAP" (make-list e) (make-list a b)))
      ((XY e a b)
       (raw-gate-application "XY" (make-list e) (make-list a b)))
      ((CAN alpha beta gamma a b)
       (raw-gate-application "CAN"
                             (make-list alpha beta gamma)
                             (make-list a b)))
      ((BLOCH a b c q)
       (raw-gate-application "BLOCH" (make-list a b c) (make-list q)))
      ((DAGGER g)
       (let ((gapp (gate-to-raw g)))
         (lisp raw:Instruction (gapp)
           (cl:setf (quil:application-operator gapp)
                    (quil:dagger-operator (quil:application-operator gapp)))
           gapp)))
      ((CONTROLLED q g)
       (let ((gapp (gate-to-raw g))
             (rawq (arg-to-raw q)))
         (lisp raw:Instruction (gapp rawq)
           (cl:setf (quil:application-operator gapp)
                    (quil:controlled-operator (quil:application-operator gapp)))
           (cl:setf (quil:application-arguments gapp)
                    (cons rawq
                          (quil:application-arguments gapp)))
           gapp)))
      
      ((FORKED ctl ps g)
       (let ((gapp (gate-to-raw g))
             (rawctl (arg-to-raw ctl))
             (rawps (map expr-to-raw ps)))
         (lisp raw:Instruction (gapp rawctl rawps)
           (cl:setf (quil:application-operator gapp)
                    (quil:forked-operator (quil:application-operator gapp)))
           (cl:setf (quil:application-parameters gapp)
                    (cl:nconc rawps (quil:application-parameters gapp)))
           (cl:setf (quil:application-arguments gapp)
                    (cons rawctl (quil:application-arguments gapp)))
           gapp)))))
  

  (declare build-resolved-program (List (Instruction :num) -> raw:Program))
  (define (build-resolved-program instrs)
    (let ((raw-instrs (map instr-to-raw instrs)))
      (lisp raw:Program (raw-instrs)
        (cl:let ((pp (quil:resolve-objects
                      (quil::raw-quil-to-unresolved-program
                       raw-instrs))))
          (cl:dolist (xform quil::*standard-post-parsing-transforms*)
            (cl:setf pp (quil:transform xform pp)))
          pp))))

  ;; end module
  )






