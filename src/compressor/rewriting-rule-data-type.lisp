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

(defun make-rewriting-rule (readable-name compiler)
  "Create a new REWRITING-RULE object."
  (%make-rewriting-rule
      :readable-name readable-name
      :count (compiler-instruction-count compiler)
      :consumer compiler))

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

(defmacro give-up-rewriting-unless (test &body body)
  "If TEST passes, proceed to execute BODY (and return the value of the final expression). If TEST fails, signal compiler-rewrite-does-not-apply."
  `(cond
     (,test
      ,@body)
     (t
      (error 'compiler-rewrite-does-not-apply))))

(defmacro unpack-wf (instr context (psi qubits) &body body)
  "Extracts from CONTEXT the wavefunction component related to the instruction INSTR.  Upon success, bind PSI to the wavefunction contents, bind QUBITS to the qubit ordering convention of PSI, execute BODY, and return the result of the final expression.  Upon failure, signal COMPILER-REWRITE-DOES-NOT-APPLY"
  `(destructuring-bind (,psi ,qubits)
       (aqvm-extract-state (compressor-context-aqvm ,context)
                           (mapcar #'qubit-index (application-arguments ,instr)))
     (give-up-rewriting-unless
         (not (eql ':not-simulated ,psi))
       ,@body)))
