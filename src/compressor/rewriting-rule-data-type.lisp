;;;; rewriting-rule-data-type.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

;;; We define the REWRITING-RULE data structure which is made use of
;;; frequently. Actual rewriting rules are defined in
;;; rewriting-rules.lisp.

(defstruct (rewriting-rule (:constructor %make-rewriting-rule))
  "An individual rewriting rule for use by the compressor.

READABLE-NAME is a string used to identify the rewriting rule during debugging.

COUNT is an integer that specifies the number of adjacent instructions the rule takes as input.

CONSUMER is a function that consumes a COMPRESSOR-CONTEXT followed by a list of instructions.  Its possible results are:
   * The error condition COMPILER-DOES-NOT-APPLY if the rule is inapplicable.
   * a LIST of instructions if the rule applies.
   * Some other error condition if compilation unexpectedly fails."
  (readable-name "Unnamed Rewriting Rule" :type string :read-only t)
  (count 1 :read-only t :type (and fixnum unsigned-byte))
  (consumer (lambda (context item)
              (declare (ignore context item))
              (error 'compiler-rewrite-does-not-apply))
   :read-only t))

(defmacro make-rewriting-rule (readable-name (context-var &rest bind-clauses) &body body)
  "Create a new REWRITING-RULE object.

READABLE-NAME is an evaluated parameter that should result in a STRING.

CONTEXT-VAR should be a symbol that will be bound to a context, or the symbol _ should it be ignored.

BINDING-CLAUSES is a list of bindings, as if by OPERATOR-MATCH. Specifically, it has the following grammar:

    <BINDING-CLAUSES> ::= (<MATCH-CLAUSE> <VARIABLE>)
    <MATCH-CLAUSE>    ::= (<GATE-NAME> <PARAM-LIST> <QUBIT> <QUBIT>*)
    <PARAM-LIST>      ::= (<PARAM>*) | _
    <GATE-NAME>       ::= <STRING>
    <PARAM>           ::= <VARIABLE> | <FLOAT>
    <QUBIT>           ::= <VARIABLE> | <INTEGER>
    <VARIABLE>        ::= <SYMBOL> | _           ; symbol will be bound in BODY

BODY is a list of forms that, by the end, should construct a list of instructions, should the rule succeed. It might use CONTEXT-VAR to help decide what instructions to produce. (BODY is free to GIVE-UP-COMPILATION as well.)"
  (check-type context-var symbol)
  (let* ((context-var-p (not (wildcard-pattern-p context-var)))
         (context-var-name (if context-var-p
                               context-var
                               (gensym "CONTEXT-VAR-"))))
    `(%make-rewriting-rule
      :readable-name ,readable-name
      :count ,(length bind-clauses)
      :consumer (lambda (,context-var-name ,@(mapcar #'second bind-clauses))
                  ,@(unless context-var-p
                      `((declare (ignore ,context-var-name))))
                  (operator-match
                    (,bind-clauses
                     ,@body)
                    (_
                     (give-up-compilation)))))))

;;; Some helpers for writing REWRITING-RULEs

;;; functions for dealing with mixed constant vs delayed-expression types

(defun param-binary-op (op arg1 arg2)
  "Binary operator that safely applies to possibly mixed arguments of NUMBER / DELAYED-EXPRESSION objects."
  (flet ((decompose (arg)
           "Returns a values tuple: PARAMS LAMBDA-PARAMS EXPRESSION DELAYEDP"
           (optima:match arg
             ((delayed-expression (params arg-params)
                                  (lambda-params arg-lambda-params)
                                  (expression arg-expression))
              (values
               arg-params
               arg-lambda-params
               arg-expression
               t))
             ((constant (value val))
              (values nil nil val nil))
             (_
              (values nil nil arg nil)))))
    (multiple-value-bind (params-1 lambda-params-1 expression-1 delayedp-1) (decompose arg1)
      (multiple-value-bind (params-2 lambda-params-2 expression-2 delayedp-2) (decompose arg2)
        (let ((params-zipped (union (mapcar #'list params-1 lambda-params-1)
                                    (mapcar #'list params-2 lambda-params-2)
                                    :test #'equal)))
          (loop :with zipped-tail := params-zipped
                :for head :in params-zipped
                :do (setf zipped-tail (rest zipped-tail))
                :when (find (second head) zipped-tail :key #'second :test #'equal)
                  :do (error "Duplicate definition of lambda-param: ~a" (second head)))
          (if (or delayedp-1 delayedp-2)
              (make-delayed-expression (mapcar #'first params-zipped)
                                       (mapcar #'second params-zipped)
                                       `(,op ,expression-1 ,expression-2))
              (funcall op expression-1 expression-2)))))))

(defun param-+ (arg1 arg2)
  (param-binary-op '+ arg1 arg2))

(defun param-* (arg1 arg2)
  (param-binary-op '* arg1 arg2))


;;; dumb little macros for cond guards that we were repeating a million times

(defmacro give-up-compilation-unless (test &body body)
  "If TEST passes, proceed to execute BODY (and return the value of the final expression). If TEST fails, signal compiler-rewrite-does-not-apply."
  `(cond
     (,test
      ,@body)
     (t
      (error 'compiler-rewrite-does-not-apply))))

(defmacro unpack-wf (instr context (psi qubits) &body body)
  "Extracts from CONTEXT the wavefunction component related to the instruction INSTR.  Upon success, bind PSI to the wavefunction contents, bind QUBITS to the qubit ordering convention of PSI, execute BODY, and return the result of the final expression.  Upon failure, call GIVE-UP-COMPILATION."
  `(destructuring-bind (,psi ,qubits)
       (aqvm-extract-state (compressor-context-aqvm ,context)
                           (mapcar #'qubit-index (application-arguments ,instr)))
     (give-up-compilation-unless
         (not (eql ':not-simulated ,psi))
       ,@body)))
