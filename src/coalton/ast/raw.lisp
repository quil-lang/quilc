(defpackage #:quil/ast/raw
  (:use #:coalton)
  (:documentation "Native Cl-QUIL and CL-QUIL.FRONTEND Types")
  (:import-from #:coalton-library/classes #:map)
  (:local-nicknames
   (#:quil #:cl-quil.frontend)
   (#:ast #:quil/ast/unresolved)))

(in-package #:quil/ast/raw)

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
                          `(lisp Param () cl:nil))))
       (lisp Param ,(cl:if (cl:stringp str) vars (cl:cons str vars))
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
       (lisp Instruction (,rawvar)
         (cl:make-instance (cl:find-symbol ,name :cl-quil.frontend)
           :target ,rawvar)))))

(cl:defmacro binary-classical-op (name left right)
  (cl:let ((lvar (cl:gensym))
           (rvar (cl:gensym)))
    `(let ((,lvar (raw-ref-arg ,left))
           (,rvar (raw-ref-arg ,right)))
       (lisp Instruction (,lvar ,rvar)
         (cl:make-instance (cl:find-symbol ,name :cl-quil.frontend)
           :left ,lvar :right ,rvar)))))

(cl:defmacro ternary-classical-op (name target left right)
  (cl:let ((tvar (cl:gensym))
           (lvar (cl:gensym))
           (rvar (cl:gensym)))
    `(let ((,tvar (raw-ref-arg ,target))
           (,lvar (raw-ref-arg ,left))
           (,rvar (raw-ref-arg ,right)))
       (lisp Instruction (,tvar ,lvar ,rvar)
         (cl:make-instance (cl:find-symbol ,name :cl-quil.frontend)
           :left ,lvar :right ,rvar :target ,tvar)))))



(coalton-toplevel

  (repr :native cl-quil:quil-type)
  (define-type QuilType)

  (declare raw-quil-type (ast:QuilType -> QuilType))
  (define (raw-quil-type qt)
    (match qt
      ((ast:QuilBit) (lisp QuilType () quil:quil-bit))
      ((ast:QuilOctet) (lisp QuilType () quil:quil-octet))
      ((ast:QuilInteger) (lisp QuilType () quil:quil-integer))
      ((ast:QuilReal) (lisp QuilType () quil:quil-real))))

  (repr :native cl-quil:parsed-program)
  (define-type Program)

  (repr :native (cl:or cl-quil:qubit cl-quil:formal))
  (define-type QubitArg)

  (repr :native (cl:or cl-quil:formal cl-quil:memory-ref))
  (define-type RefArg)

  (repr :native cl-quil:memory-ref)
  (define-type MRef)

  (declare ref-to-raw (ast:Ref -> Mref))
  (define (ref-to-raw (ast:Ref name loc))
    (lisp Mref (name loc)
      (quil:mref name loc)))

  (repr :native (cl:or cl-quil:param
                       cl-quil.frontend:delayed-expression
                       cl-quil:constant cl:null))
  (define-type Param)

  (repr :native (cl:or cl-quil:instruction
                       cl-quil:memory-descriptor
                       cl-quil:jump-target
                       cl-quil:gate-definition
                       cl-quil:circuit-definition))
  (define-type Instruction)

  ;; If non-literal numbertypes are supported, this should be altered
  ;; to be something like (real :num, complex :num => ..)
  ;; and the Const case be handled to interperet the :num to a type
  ;; that quilc can already handle
  (declare expr-to-raw (ast:Expr :num -> Param))
  (define (expr-to-raw e)
    (match e
      ((ast:Add e1 e2) (combine-params "+" e1 e2))

      ((ast:Sub e1 e2) (combine-params "-" e1 e2))

      ((ast:Mul e1 e2) (combine-params "*" e1 e2))

      ((ast:Div e1 e2) (combine-params "/" e1 e2))

      ((ast:Pow e1 e2) (combine-params "EXPT" e1 e2))

      ((ast:Neg e1) (combine-params  "-" e1))
      
      ((ast:Const e)
       (lisp Param (e)
         (quil:constant e)))

      ((ast:RefExpr rf)
       (let ((rr (ref-to-raw rf)))
         (lisp Param (rr)
           (quil:make-delayed-expression cl:nil cl:nil rr))))

      ((ast:Var name)
       (lisp Param (name)
         (quil:param name)))

      ((ast:Call name e)
       (combine-params name e))))

  (declare arg-to-raw ((ast:MaybeFormal Ufix) -> QubitArg))
  (define (arg-to-raw a)
    (match a
      ((ast:Actual qb) (lisp QubitArg (qb) (quil:qubit qb)))
      ((ast:Formal name) (lisp QubitArg (name) (quil:formal name)))))

  (declare raw-gate-application
           (String -> (List (ast:Expr :num)) -> (List (ast:MaybeFormal Ufix)) -> Instruction))
  (define (raw-gate-application name params args)
    (let ((params* (map expr-to-raw params))
          (args* (map arg-to-raw args)))
      (lisp Instruction (name params* args*)
        (cl:make-instance 'quil:unresolved-application
          :operator (quil:named-operator name)
          :parameters params*
          :arguments args*))))

  
  (declare raw-classical-op (ast:ClassicalOperation -> Instruction))
  (define (raw-classical-op op)
    (match op
      ((ast:NegOp r)
       (unary-classical-op "CLASSICAL-NEGATE" r))
      ((ast:NotOp r)
       (unary-classical-op "CLASSICAL-NOT" r))

      ((ast:MoveOp a b)
       (binary-classical-op "CLASSICAL-MOVE" a b))
      ((ast:ExchangeOp a b)
       (binary-classical-op "CLASSICAL-EXCHANGE" a b))
      ((ast:ConvertOp a b)
       (binary-classical-op "CLASSICAL-CONVERT" a b))
      ((ast:AndOp a b)
       (binary-classical-op "CLASSICAL-AND" a b))
      ((ast:IOrOp a b)
       (binary-classical-op "CLASSICAL-INCLUSIVE-OR" a b))
      ((ast:XOrOp a b)
       (binary-classical-op "CLASSICAL-EXCLUSIVE-OR" a b))
      ((ast:AddOp a b)
       (binary-classical-op "CLASSICAL-ADDITION" a b))
      ((ast:SubOp a b)
       (binary-classical-op "CLASSICAL-SUBTRACTION" a b))
      ((ast:MulOp a b)
       (binary-classical-op "CLASSICAL-MULTIPLICATION" a b))
      ((ast:DivOp a b)
       (binary-classical-op "CLASSICAL-DIVISION" a b))

      ((ast:LoadOp a b c)
       (ternary-classical-op "CLASSICAL-LOAD" a b c))
      ((ast:StoreOp a b c)
       (ternary-classical-op "CLASSICAL-STORE" a b c))
      ((ast:EqOp a b c)
       (ternary-classical-op "CLASSICAL-EQUALITY" a b c))
      ((ast:GtOp a b c)
       (ternary-classical-op "CLASSICAL-GREATER-THAN" a b c))
      ((ast:GeOp a b c)
       (ternary-classical-op "CLASSICAL-GREATER-EQUAL" a b c))
      ((ast:LtOp a b c)
       (ternary-classical-op "CLASSICAL-LESS-THAN" a b c))
      ((ast:LeOp a b c)
       (ternary-classical-op "CLASSICAL-LESS-EQUAL" a b c))))

  (declare raw-gate-def (String -> (ast:GateDef :num) -> Instruction))
  (define (raw-gate-def name gdef)
    (match gdef
      ((ast:PermuationGateDef vals)
       (lisp Instruction (name vals)
         (cl:make-instance 'quil:permutation-gate-definition
           :name name :permutation vals :context cl:nil)))

      ((ast:ParameterizedGateDef params exprs)
       (let ((exprs* (map expr-to-raw exprs)))
         (lisp Instruction (name params exprs*)
           (cl:make-instance 'quil:parameterized-gate-definition
             :name name
             :entries (cl:mapcar 'get-expr-exprs exprs*)
             :parameters (cl:mapcar 'cl:make-symbol params)))))

      ((ast:StaticGateDef entries)
       (lisp Instruction (name entries)
         (cl:make-instance 'quil:static-gate-definition
           :name name
           :entries entries)))
      
      ((ast:PauliSumGateDef terms params args)
       (lisp Instruction (name terms params args)
         (cl:make-instance 'quil:exp-pauli-sum-gate-definition
           :name name
           :arguments (cl:mapcar #'quil:formal args)
           :parameters (cl:mapcar 'cl:make-symbol params)
           :terms (cl:loop :for term :in terms
                     :collect (quil:make-pauli-term
                               :pauli-word (ast:pauli-term-word term)
                               :prefactor (ast:pauli-term-prefactor term)
                               :arguments (cl:mapcar #'quil:formal (ast:pauli-term-arguments term)))))))))


  (declare raw-ref-arg (ast:MaybeFormal ast:Ref -> RefArg))
  (define (raw-ref-arg ja)
    (match ja
      ((ast:Formal name)
       (lisp RefArg (name)
         (quil:formal name)))
      ((ast:Actual (ast:Ref name loc))
       (lisp RefArg (name loc)
         (quil:mref name loc)))))

  (declare instr-to-raw (ast:Instruction :num -> Instruction))
  (define (instr-to-raw instr)
    (match instr
      ((ast:ApplyGate g) (gate-to-raw g))

      ((ast:Include name)
       (lisp Instruction (name)
         (cl:make-instance 'quil:include :pathname name)))

      ((ast:Pragma pstring)
       (lisp Instruction (pstring)
         (quil::parse-pragma
          (quil::tokenize
           (cl:with-output-to-string (out)
             (cl:write-string "PRAGMA ")
             (cl:write-string pstring))))))

      ((ast:Memory name type length sharing offsets)
       (lisp instruction (name type length sharing offsets)
         (quil:make-memory-descriptor
          :name name
          :type (raw-quil-type type)
          :length length
          :sharing-parent sharing
          :sharing-offset-alist
          (cl:loop :for offset :in offsets
             :collect (cl:cons
                       (raw-quil-type (ast:offset-type offset))
                       (ast:offset-amount offset))))))
      
      ((ast:GateDefinition name gdef)
       (raw-gate-def name gdef))

      ((ast:CircuitDefinition name params args body)
       (let ((instrs (map instr-to-raw body)))
         (lisp Instruction (name params args instrs)
           (cl:make-instance 'quil:circuit-definition
             :name name
             :parameters (cl:mapcar 'quil:param params)
             :arguments (cl:mapcar 'quil:formal args)
             :body instrs))))

      ((ast:ApplyOp op)
       (raw-classical-op op))

      ((ast:Label l)
       (lisp Instruction (l)
         (cl:make-instance 'quil:jump-target
           :label (quil:label l))))

      ((ast:Jump l)
       (lisp Instruction (l)
         (cl:make-instance 'quil:unconditional-jump
           :label (quil:label l))))

      ((ast:JumpWhen l a)
       (lisp Instruction (l a)
         (cl:make-instance 'quil:jump-when
           :label (quil:label l)
           :address (raw-ref-arg a))))

      ((ast:JumpUnless l a)
       (lisp Instruction (l a)
         (cl:make-instance 'quil:jump-unless
           :label (quil:label l)
           :address (raw-ref-arg a))))

      ((ast:Noop)
       (lisp Instruction ()
         (cl:make-instance 'quil:no-operation)))

      ((ast:Halt)
       (lisp Instruction ()
         (cl:make-instance 'quil:halt)))


      ((ast:Wait)
       (lisp Instruction ()
         (cl:make-instance 'quil:wait)))

      ((ast:ResetAll)
       (lisp Instruction ()
         (cl:make-instance 'quil:reset)))

      ((ast:Reset qb)
       (let ((qb* (arg-to-raw qb)))
         (lisp Instruction (qb*)
           (cl:make-instance 'quil:reset-qubit :target qb*))))

      ((ast:Measure qb loc)
       (let ((qb* (arg-to-raw qb))
             (loc* (raw-ref-arg loc)))
         (lisp Instruction (qb* loc*)
           (cl:make-instance 'quil:measure :address loc* :qubit qb*))))


      ((ast:MeasureDiscard qb)
       (let ((qb* (arg-to-raw qb)))
         (lisp Instruction (qb*)
           (cl:make-instance 'quil:measure-discard :qubit qb*))))))

  (declare gate-to-raw (ast:Gate :num -> Instruction))
  (define (gate-to-raw g)
    (match g
      ((ast:Gate name params args)
       (raw-gate-application name params args))
      ((ast:I a)
       (raw-gate-application "I" nil (make-list a)))
      ((ast:X a)
       (raw-gate-application "X" nil (make-list a)))
      ((ast:Y a)
       (raw-gate-application "Y" nil (make-list a)))
      ((ast:Z a)
       (raw-gate-application "Z" nil (make-list a)))
      ((ast:H a)
       (raw-gate-application "H" nil (make-list a)))
      ((ast:S a)
       (raw-gate-application "S" nil (make-list a)))
      ((ast:T a)
       (raw-gate-application "T" nil (make-list a)))
      ((ast:RX e a)
       (raw-gate-application "RX" (make-list e) (make-list a)))
      ((ast:RY e a)
       (raw-gate-application "RY" (make-list e) (make-list a)))
      ((ast:RZ e a)
       (raw-gate-application "RZ" (make-list e) (make-list a)))
      ((ast:PHASE e a)
       (raw-gate-application "PHASE" (make-list e) (make-list a)))    
      ((ast:CNOT a b)
       (raw-gate-application "CNOT" nil (make-list a b)))
      ((ast:CZ a b)
       (raw-gate-application "CZ" nil (make-list a b)))
      ((ast:SWAP a b)
       (raw-gate-application "SWAP" nil (make-list a b)))
      ((ast:ISWAP a b)
       (raw-gate-application "ISWAP" nil (make-list a b)))
      ((ast:SQISWAP a b)
       (raw-gate-application "SQISWAP" nil (make-list a b)))
      ((ast:CSWAP a b c)
       (raw-gate-application "CSWAP" nil (make-list a b c)))
      ((ast:CCNOT a b c)
       (raw-gate-application "CCNOT" nil (make-list a b c)))
      ((ast:PSWAP e a b)
       (raw-gate-application "PSWAP" (make-list e) (make-list a b)))
      ((ast:PISWAP e a b)
       (raw-gate-application "PISWAP" (make-list e) (make-list a b)))
      ((ast:XY e a b)
       (raw-gate-application "XY" (make-list e) (make-list a b)))
      ((ast:CAN alpha beta gamma a b)
       (raw-gate-application "CAN"
                             (make-list alpha beta gamma)
                             (make-list a b)))
      ((ast:BLOCH a b c q)
       (raw-gate-application "BLOCH" (make-list a b c) (make-list q)))
      ((ast:DAGGER g)
       (let ((gapp (gate-to-raw g)))
         (lisp Instruction (gapp)
           (cl:setf (quil:application-operator gapp)
                    (quil:dagger-operator (quil:application-operator gapp)))
           gapp)))
      ((ast:CONTROLLED q g)
       (let ((gapp (gate-to-raw g))
             (rawq (arg-to-raw q)))
         (lisp Instruction (gapp rawq)
           (cl:setf (quil:application-operator gapp)
                    (quil:controlled-operator (quil:application-operator gapp)))
           (cl:setf (quil:application-arguments gapp)
                    (cons rawq
                          (quil:application-arguments gapp)))
           gapp)))
      
      ((ast:FORKED ctl ps g)
       (let ((gapp (gate-to-raw g))
             (rawctl (arg-to-raw ctl))
             (rawps (map expr-to-raw ps)))
         (lisp Instruction (gapp rawctl rawps)
           (cl:setf (quil:application-operator gapp)
                    (quil:forked-operator (quil:application-operator gapp)))
           (cl:setf (quil:application-parameters gapp)
                    (cl:nconc rawps (quil:application-parameters gapp)))
           (cl:setf (quil:application-arguments gapp)
                    (cons rawctl (quil:application-arguments gapp)))
           gapp)))))
  

  (declare build-resolved-program (List (ast:Instruction :num) -> Program))
  (define (build-resolved-program instrs)
    (let ((raw-instrs (map instr-to-raw instrs)))
      (lisp Program (raw-instrs)
        (cl:let ((pp (quil:resolve-objects
                      (quil::raw-quil-to-unresolved-program
                       raw-instrs))))
          (cl:dolist (xform quil::*standard-post-parsing-transforms*)
            (cl:setf pp (quil:transform xform pp)))
          pp))))

  ;;eof
  )
