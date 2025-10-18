(defpackage #:quil/ast/native
  (:use #:coalton)
  (:documentation "Native Cl-QUIL and CL-QUIL.FRONTEND Types")
  (:import-from #:coalton-library/classes #:map)
  (:local-nicknames
   (#:quil #:cl-quil.frontend)
   (#:mem #:quil/ast/memory)
   (#:expr #:quil/ast/expression)
   (#:ast #:quil/ast/unresolved)
   (#:macro #:quil/ast/macro)
   (#:gate #:quil/ast/gate)
   (#:op #:quil/ast/classical)))

(in-package #:quil/ast/native)

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


;; ;; HELPER MACROS TO INSTANTIATE CLASSICAL MEMORY OPERATION INSTRUCTIONS

(cl:defmacro unary-classical-op (mapper name ref)
  (cl:let ((rawvar (cl:gensym)))
    `(let ((,rawvar (,mapper ,ref)))
       (lisp Instruction (,rawvar)
         (cl:make-instance (cl:find-symbol ,name :cl-quil.frontend)
           :target ,rawvar)))))

(cl:defmacro binary-classical-op (mapper name left right)
  (cl:let ((lvar (cl:gensym))
           (rvar (cl:gensym)))
    `(let ((,lvar (,mapper ,left))
           (,rvar (,mapper ,right)))
       (lisp Instruction (,lvar ,rvar)
         (cl:make-instance (cl:find-symbol ,name :cl-quil.frontend)
           :left ,lvar :right ,rvar)))))

(cl:defmacro ternary-classical-op (mapper name target left right)
  (cl:let ((tvar (cl:gensym))
           (lvar (cl:gensym))
           (rvar (cl:gensym)))
    `(let ((,tvar (,mapper ,target))
           (,lvar (,mapper ,left))
           (,rvar (,mapper ,right)))
       (lisp Instruction (,tvar ,lvar ,rvar)
         (cl:make-instance (cl:find-symbol ,name :cl-quil.frontend)
           :left ,lvar :right ,rvar :target ,tvar)))))

(coalton-toplevel

  (repr :native cl-quil:quil-type)
  (define-type QuilType)

  (declare raw-quil-type (mem:QuilType -> QuilType))
  (define (raw-quil-type qt)
    (match qt
      ((mem:QuilBit) (lisp QuilType () quil:quil-bit))
      ((mem:QuilOctet) (lisp QuilType () quil:quil-octet))
      ((mem:QuilInteger) (lisp QuilType () quil:quil-integer))
      ((mem:QuilReal) (lisp QuilType () quil:quil-real))))

  (repr :native cl-quil:parsed-program)
  (define-type Program)

  (repr :native (cl:or cl-quil:qubit cl-quil:formal))
  (define-type QubitArg)

  (repr :native (cl:or cl-quil:formal cl-quil:memory-ref))
  (define-type RefArg)

  (repr :native cl-quil:memory-ref)
  (define-type MRef)

  (declare ref-to-raw (mem:Ref -> Mref))
  (define (ref-to-raw (mem:Ref name loc))
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

  (declare expr-to-raw (expr:Expr :num -> Param))
  (define (expr-to-raw e)
    (match e
      ((expr:Add e1 e2) (combine-params "+" e1 e2))

      ((expr:Sub e1 e2) (combine-params "-" e1 e2))

      ((expr:Mul e1 e2) (combine-params "*" e1 e2))

      ((expr:Div e1 e2) (combine-params "/" e1 e2))

      ((expr:Pow e1 e2) (combine-params "EXPT" e1 e2))

      ((expr:Neg e1) (combine-params  "-" e1))
      
      ((expr:Const e)
       (lisp Param (e)
         (quil:constant e)))

      ((expr:Ref name loc)
       (let ((rr (lisp Mref (name loc) (quil:mref name loc))))
         (lisp Param (rr)
           (quil:make-delayed-expression cl:nil cl:nil rr))))

      ((expr:Var name)
       (lisp Param (name)
         (quil:param name)))

      ((expr:Call name e)
       (combine-params name e))))

  (define (ufix-to-qubit u) (lisp QubitArg (u) (quil:qubit u)))

  (declare raw-unresolved-application
           (String -> (List (expr:Expr :num)) -> (List Ufix) -> Instruction))
  (define (raw-unresolved-application name params args)
    (let ((params* (map expr-to-raw params))
          (args* (map ufix-to-qubit args)))
      (lisp Instruction (name params* args*)
        (cl:make-instance 'quil:unresolved-application
          :operator (quil:named-operator name)
          :parameters params*
          :arguments args*))))

  (repr :native (cl:or cl-quil:memory-ref cl-quil:constant cl-quil:formal))
  (define-type ClassicalArg)

  (declare raw-classical-arg (op:Arg :num -> ClassicalArg))
  (define (raw-classical-arg arg)
    (match arg
      ((op:Mem (mem:Ref name loc))
       (lisp ClassicalArg (name loc) (quil:mref name loc)))
      ((op:Const n)
       (lisp ClassicalArg (n) (quil:constant n)))))
    
  (declare raw-classical-op (op:Operation (op:Arg :num) -> Instruction))
  (define (raw-classical-op op)
    (match op
      ((op:Neg r)
       (unary-classical-op raw-classical-arg "CLASSICAL-NEGATE" r))
      ((op:Not r)
       (unary-classical-op raw-classical-arg "CLASSICAL-NOT" r))

      ((op:Move a b)
       (binary-classical-op raw-classical-arg "CLASSICAL-MOVE" a b))
      ((op:Exchange a b)
       (binary-classical-op raw-classical-arg "CLASSICAL-EXCHANGE" a b))
      ((op:Convert a b)
       (binary-classical-op raw-classical-arg "CLASSICAL-CONVERT" a b))
      ((op:And a b)
       (binary-classical-op raw-classical-arg "CLASSICAL-AND" a b))
      ((op:IOr a b)
       (binary-classical-op raw-classical-arg "CLASSICAL-INCLUSIVE-OR" a b))
      ((op:XOr a b)
       (binary-classical-op raw-classical-arg "CLASSICAL-EXCLUSIVE-OR" a b))
      ((op:Add a b)
       (binary-classical-op raw-classical-arg "CLASSICAL-ADDITION" a b))
      ((op:Sub a b)
       (binary-classical-op raw-classical-arg "CLASSICAL-SUBTRACTION" a b))
      ((op:Mul a b)
       (binary-classical-op raw-classical-arg "CLASSICAL-MULTIPLICATION" a b))
      ((op:Div a b)
       (binary-classical-op raw-classical-arg "CLASSICAL-DIVISION" a b))

      ((op:Load a b c)
       (ternary-classical-op raw-classical-arg "CLASSICAL-LOAD" a b c))
      ((op:Store a b c)
       (ternary-classical-op raw-classical-arg "CLASSICAL-STORE" a b c))
      ((op:Eq a b c)
       (ternary-classical-op raw-classical-arg "CLASSICAL-EQUALITY" a b c))
      ((op:Gt a b c)
       (ternary-classical-op raw-classical-arg "CLASSICAL-GREATER-THAN" a b c))
      ((op:Ge a b c)
       (ternary-classical-op raw-classical-arg "CLASSICAL-GREATER-EQUAL" a b c))
      ((op:Lt a b c)
       (ternary-classical-op raw-classical-arg "CLASSICAL-LESS-THAN" a b c))
      ((op:Le a b c)
       (ternary-classical-op raw-classical-arg "CLASSICAL-LESS-EQUAL" a b c))))
  
  (define (raw-macro-classical-arg arg)
    (match arg
      ((macro:Formal s)
       (lisp :a (s) (quil:formal s)))
      ((macro:Actual a)
       (raw-classical-arg a))))

  (declare raw-macro-classical-op (op:Operation (macro:MaybeFormal (op:Arg :num)) -> Instruction))
  (define (raw-macro-classical-op op)
    (match op
      ((op:Neg r)
       (unary-classical-op raw-macro-classical-arg "CLASSICAL-NEGATE" r))
      ((op:Not r)
       (unary-classical-op raw-macro-classical-arg "CLASSICAL-NOT" r))

      ((op:Move a b)
       (binary-classical-op raw-macro-classical-arg "CLASSICAL-MOVE" a b))
      ((op:Exchange a b)
       (binary-classical-op raw-macro-classical-arg "CLASSICAL-EXCHANGE" a b))
      ((op:Convert a b)
       (binary-classical-op raw-macro-classical-arg "CLASSICAL-CONVERT" a b))
      ((op:And a b)
       (binary-classical-op raw-macro-classical-arg "CLASSICAL-AND" a b))
      ((op:IOr a b)
       (binary-classical-op raw-macro-classical-arg "CLASSICAL-INCLUSIVE-OR" a b))
      ((op:XOr a b)
       (binary-classical-op raw-macro-classical-arg "CLASSICAL-EXCLUSIVE-OR" a b))
      ((op:Add a b)
       (binary-classical-op raw-macro-classical-arg "CLASSICAL-ADDITION" a b))
      ((op:Sub a b)
       (binary-classical-op raw-macro-classical-arg "CLASSICAL-SUBTRACTION" a b))
      ((op:Mul a b)
       (binary-classical-op raw-macro-classical-arg "CLASSICAL-MULTIPLICATION" a b))
      ((op:Div a b)
       (binary-classical-op raw-macro-classical-arg "CLASSICAL-DIVISION" a b))

      ((op:Load a b c)
       (ternary-classical-op raw-macro-classical-arg "CLASSICAL-LOAD" a b c))
      ((op:Store a b c)
       (ternary-classical-op raw-macro-classical-arg "CLASSICAL-STORE" a b c))
      ((op:Eq a b c)
       (ternary-classical-op raw-macro-classical-arg "CLASSICAL-EQUALITY" a b c))
      ((op:Gt a b c)
       (ternary-classical-op raw-macro-classical-arg "CLASSICAL-GREATER-THAN" a b c))
      ((op:Ge a b c)
       (ternary-classical-op raw-macro-classical-arg "CLASSICAL-GREATER-EQUAL" a b c))
      ((op:Lt a b c)
       (ternary-classical-op raw-macro-classical-arg "CLASSICAL-LESS-THAN" a b c))
      ((op:Le a b c)
       (ternary-classical-op raw-macro-classical-arg "CLASSICAL-LESS-EQUAL" a b c))))

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
  

  (declare raw-mem-descriptor (mem:Descriptor -> Instruction))
  (define (raw-mem-descriptor (mem:Descriptor name type length sharing offsets))
    (lisp Instruction (name type length sharing offsets)
      (quil:make-memory-descriptor
       :name name
       :type (raw-quil-type type)
       :length length
       :sharing-parent sharing
       :sharing-offset-alist
       (cl:loop :for offset :in offsets
          :collect (cl:cons
                    (raw-quil-type (mem:offset-type offset))
                    (mem:offset-amount offset))))))

  (repr :native (cl:or quil:memory-ref quil:formal))
  (define-type MemRefArg)
  
  (define (raw-maybeformal-mem-ref m)
    (match m
      ((macro:Formal s) (lisp MemRefArg (s) (quil:formal s)))
      ((macro:Actual (mem:Ref name loc))
       (lisp MemRefArg (name loc) (quil:mref name loc)))))

  (repr :native (cl:or quil:qubit quil:formal))
  (define-type FormalQbArg)

  (define (raw-maybeformal-qubit q)
    (match q
      ((macro:Formal s) (lisp FormalQbArg (s) (quil:formal s)))
      ((macro:Actual n)  (lisp FormalQbArg (n) (quil:qubit n)))))


  (declare macro-instr-to-raw (macro:Instruction :num -> Instruction))
  (define (macro-instr-to-raw instr)
    (match instr
      ((macro:ApplyGate g)
       (macro-gate-to-raw g))

      ((macro:ApplyOp op)
       (raw-macro-classical-op op))

      ((macro:ApplyCirc name params qargs refargs)
       (let ((params* (map expr-to-raw params))
             (qargs* (map raw-maybeformal-qubit qargs))
             (refargs* (map raw-maybeformal-mem-ref refargs)))
         (lisp Instruction (name params* qargs* refargs*)
           (cl:make-instance 'quil:unresolved-application
             :operator (quil:named-operator name)
             :parameters params*
             :arguments (cl:nconc qargs* refargs*)))))

      ((macro:Pragma pstring)
       (lisp Instruction (pstring)
         (quil::parse-pragma
          (quil::tokenize
           (cl:with-output-to-string (out)
             (cl:write-string "PRAGMA " out)
             (cl:write-string pstring out))))))

      ((macro:Label s)
       (lisp Instruction (s)
         (cl:make-instance 'quil:jump-target
           :label (quil:label s))))

      ((macro:Jump s)
       (lisp Instruction (s)
         (cl:make-instance 'quil:unconditional-jump
           :label (quil:label s))))

      ((macro:Noop)
       (lisp Instruction ()
         (cl:make-instance 'quil:no-operation)))

      ((macro:Halt)
       (lisp Instruction ()
         (cl:make-instance 'quil:halt)))


      ((macro:Wait)
       (lisp Instruction ()
         (cl:make-instance 'quil:wait)))

      ((macro:ResetAll)
       (lisp Instruction ()
         (cl:make-instance 'quil:reset)))

      ((macro:JumpWhen s rf)
       (let ((rf* (raw-maybeformal-mem-ref rf)))
         (lisp Instruction (s rf*)
           (cl:make-instance 'quil:jump-when
             :label (quil:label s)
             :address rf*))))

      ((macro:JumpUnless s rf)
       (let ((rf* (raw-maybeformal-mem-ref rf)))
         (lisp Instruction (s rf*)
           (cl:make-instance 'quil:jump-unless
             :label (quil:label s)
             :address rf*))))

      ((macro:reset qb)
       (let ((qb* (raw-maybeformal-qubit qb)))
         (lisp Instruction (qb*)
           (cl:make-instance 'quil:reset-qubit :target qb*))))

      ((macro:Measure qb rf)
       (let ((qb* (raw-maybeformal-qubit qb))
             (rf* (raw-maybeformal-mem-ref rf)))
         (lisp Instruction (qb* rf*)
           (cl:make-instance 'quil:measure :address rf* :qubit qb*))))

      ((macro:MeasureDiscard qb)
       (let ((qb* (raw-maybeformal-qubit qb)))
         (lisp Instruction (qb*)
           (cl:make-instance 'quil:measure-discard :qubit qb*))))))


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
             (cl:write-string "PRAGMA " out)
             (cl:write-string pstring out))))))

      ((ast:DeclareMem descriptor)
       (raw-mem-descriptor descriptor))
      
      ((ast:GateDefinition name gdef)
       (raw-gate-def name gdef))

      ((ast:CircuitDefinition name params qargs rargs body)
       (let ((instrs (map macro-instr-to-raw body)))
         (lisp Instruction (name params qargs rargs instrs)
           (cl:make-instance 'quil:circuit-definition
             :name name
             :parameters (cl:mapcar 'quil:param params)
             :arguments (cl:mapcar 'quil:formal (cl:nconc qargs rargs))
             :body instrs))))

      ((ast:ApplyOp op)
       (raw-classical-op op))

      ((ast:ApplyCirc name params qargs refargs)
       (let ((params* (map expr-to-raw params))
             (qargs* (map ufix-to-qubit qargs))
             (refargs* (map ref-to-raw refargs)))
         (lisp Instruction (name params* qargs* refargs*)
           (cl:make-instance 'quil:unresolved-application
             :operator (quil:named-operator name)
             :parameters params*
             :arguments (cl:nconc qargs* refargs*)))))

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
           :address (ref-to-raw a))))

      ((ast:JumpUnless l a)
       (lisp Instruction (l a)
         (cl:make-instance 'quil:jump-unless
           :label (quil:label l)
           :address (ref-to-raw a))))

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
       (let ((qb* (ufix-to-qubit qb)))
         (lisp Instruction (qb*)
           (cl:make-instance 'quil:reset-qubit :target qb*))))

      ((ast:Measure qb loc)
       (let ((qb* (ufix-to-qubit qb))
             (loc* (ref-to-raw loc)))
         (lisp Instruction (qb* loc*)
           (cl:make-instance 'quil:measure :address loc* :qubit qb*))))

      ((ast:MeasureDiscard qb)
       (let ((qb* (ufix-to-qubit qb)))
         (lisp Instruction (qb*)
           (cl:make-instance 'quil:measure-discard :qubit qb*))))))

  (declare gate-to-raw ((gate:Gate :num Ufix) -> Instruction))
  (define (gate-to-raw g)
    (match g
      ((gate:Gate name params args)
       (raw-unresolved-application name params args))
      ((gate:I a)
       (raw-unresolved-application "I" nil (make-list a)))
      ((gate:X a)
       (raw-unresolved-application "X" nil (make-list a)))
      ((gate:Y a)
       (raw-unresolved-application "Y" nil (make-list a)))
      ((gate:Z a)
       (raw-unresolved-application "Z" nil (make-list a)))
      ((gate:H a)
       (raw-unresolved-application "H" nil (make-list a)))
      ((gate:S a)
       (raw-unresolved-application "S" nil (make-list a)))
      ((gate:T a)
       (raw-unresolved-application "T" nil (make-list a)))
      ((gate:RX e a)
       (raw-unresolved-application "RX" (make-list e) (make-list a)))
      ((gate:RY e a)
       (raw-unresolved-application "RY" (make-list e) (make-list a)))
      ((gate:RZ e a)
       (raw-unresolved-application "RZ" (make-list e) (make-list a)))
      ((gate:PHASE e a)
       (raw-unresolved-application "PHASE" (make-list e) (make-list a)))    
      ((gate:CNOT a b)
       (raw-unresolved-application "CNOT" nil (make-list a b)))
      ((gate:CZ a b)
       (raw-unresolved-application "CZ" nil (make-list a b)))
      ((gate:SWAP a b)
       (raw-unresolved-application "SWAP" nil (make-list a b)))
      ((gate:ISWAP a b)
       (raw-unresolved-application "ISWAP" nil (make-list a b)))
      ((gate:SQISWAP a b)
       (raw-unresolved-application "SQISWAP" nil (make-list a b)))
      ((gate:CSWAP a b c)
       (raw-unresolved-application "CSWAP" nil (make-list a b c)))
      ((gate:CCNOT a b c)
       (raw-unresolved-application "CCNOT" nil (make-list a b c)))
      ((gate:PSWAP e a b)
       (raw-unresolved-application "PSWAP" (make-list e) (make-list a b)))
      ((gate:PISWAP e a b)
       (raw-unresolved-application "PISWAP" (make-list e) (make-list a b)))
      ((gate:XY e a b)
       (raw-unresolved-application "XY" (make-list e) (make-list a b)))
      ((gate:CAN alpha beta gamma a b)
       (raw-unresolved-application "CAN"
                             (make-list alpha beta gamma)
                             (make-list a b)))
      ((gate:BLOCH a b c q)
       (raw-unresolved-application "BLOCH" (make-list a b c) (make-list q)))
      ((gate:DAGGER g)
       (let ((gapp (gate-to-raw g)))
         (lisp Instruction (gapp)
           (cl:setf (quil:application-operator gapp)
                    (quil:dagger-operator (quil:application-operator gapp)))
           gapp)))
      ((gate:CONTROLLED q g)
       (let ((gapp (gate-to-raw g))
             (rawq (ufix-to-qubit q)))
         (lisp Instruction (gapp rawq)
           (cl:setf (quil:application-operator gapp)
                    (quil:controlled-operator (quil:application-operator gapp)))
           (cl:setf (quil:application-arguments gapp)
                    (cons rawq
                          (quil:application-arguments gapp)))
           gapp)))
      
      ((gate:FORKED ctl ps g)
       (let ((gapp (gate-to-raw g))
             (rawctl (ufix-to-qubit ctl))
             (rawps (map expr-to-raw ps)))
         (lisp Instruction (gapp rawctl rawps)
           (cl:setf (quil:application-operator gapp)
                    (quil:forked-operator (quil:application-operator gapp)))
           (cl:setf (quil:application-parameters gapp)
                    (cl:nconc rawps (quil:application-parameters gapp)))
           (cl:setf (quil:application-arguments gapp)
                    (cons rawctl (quil:application-arguments gapp)))
           gapp)))))

  (declare raw-macro-unresolved-application
           (String ->
            (List (expr:Expr :num)) ->
            (List (macro:MaybeFormal Ufix)) ->
            Instruction))
  (define (raw-macro-unresolved-application name params args)
    (let ((params* (map expr-to-raw params))
          (args* (map raw-maybeformal-qubit args)))
      (lisp Instruction (name params* args*)
        (cl:make-instance 'quil:unresolved-application
          :operator (quil:named-operator name)
          :parameters params*
          :arguments args*))))

  (define (macro-gate-to-raw g)
    (match g
      ((gate:Gate name params args)
       (raw-macro-unresolved-application name params args))
      ((gate:I a)
       (raw-macro-unresolved-application "I" nil (make-list a)))
      ((gate:X a)
       (raw-macro-unresolved-application "X" nil (make-list a)))
      ((gate:Y a)
       (raw-macro-unresolved-application "Y" nil (make-list a)))
      ((gate:Z a)
       (raw-macro-unresolved-application "Z" nil (make-list a)))
      ((gate:H a)
       (raw-macro-unresolved-application "H" nil (make-list a)))
      ((gate:S a)
       (raw-macro-unresolved-application "S" nil (make-list a)))
      ((gate:T a)
       (raw-macro-unresolved-application "T" nil (make-list a)))
      ((gate:RX e a)
       (raw-macro-unresolved-application "RX" (make-list e) (make-list a)))
      ((gate:RY e a)
       (raw-macro-unresolved-application "RY" (make-list e) (make-list a)))
      ((gate:RZ e a)
       (raw-macro-unresolved-application "RZ" (make-list e) (make-list a)))
      ((gate:PHASE e a)
       (raw-macro-unresolved-application "PHASE" (make-list e) (make-list a)))    
      ((gate:CNOT a b)
       (raw-macro-unresolved-application "CNOT" nil (make-list a b)))
      ((gate:CZ a b)
       (raw-macro-unresolved-application "CZ" nil (make-list a b)))
      ((gate:SWAP a b)
       (raw-macro-unresolved-application "SWAP" nil (make-list a b)))
      ((gate:ISWAP a b)
       (raw-macro-unresolved-application "ISWAP" nil (make-list a b)))
      ((gate:SQISWAP a b)
       (raw-macro-unresolved-application "SQISWAP" nil (make-list a b)))
      ((gate:CSWAP a b c)
       (raw-macro-unresolved-application "CSWAP" nil (make-list a b c)))
      ((gate:CCNOT a b c)
       (raw-macro-unresolved-application "CCNOT" nil (make-list a b c)))
      ((gate:PSWAP e a b)
       (raw-macro-unresolved-application "PSWAP" (make-list e) (make-list a b)))
      ((gate:PISWAP e a b)
       (raw-macro-unresolved-application "PISWAP" (make-list e) (make-list a b)))
      ((gate:XY e a b)
       (raw-macro-unresolved-application "XY" (make-list e) (make-list a b)))
      ((gate:CAN alpha beta gamma a b)
       (raw-macro-unresolved-application "CAN"
                             (make-list alpha beta gamma)
                             (make-list a b)))
      ((gate:BLOCH a b c q)
       (raw-macro-unresolved-application "BLOCH" (make-list a b c) (make-list q)))
      ((gate:DAGGER g)
       (let ((gapp (macro-gate-to-raw g)))
         (lisp Instruction (gapp)
           (cl:setf (quil:application-operator gapp)
                    (quil:dagger-operator (quil:application-operator gapp)))
           gapp)))
      ((gate:CONTROLLED q g)
       (let ((gapp (macro-gate-to-raw g))
             (rawq (raw-maybeformal-qubit q)))
         (lisp Instruction (gapp rawq)
           (cl:setf (quil:application-operator gapp)
                    (quil:controlled-operator (quil:application-operator gapp)))
           (cl:setf (quil:application-arguments gapp)
                    (cons rawq
                          (quil:application-arguments gapp)))
           gapp)))
      
      ((gate:FORKED ctl ps g)
       (let ((gapp (macro-gate-to-raw g))
             (rawctl (raw-maybeformal-qubit ctl))
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
