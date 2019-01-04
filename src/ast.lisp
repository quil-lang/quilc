;;;; src/ast.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric copy-instance (instance)
  (:documentation
   "Create a shallow copy of the object INSTANCE.

WARNING: The default will work for instances of \"idiomatic\" classes that aren't doing too many crazy things.")
  (:method ((instance t))
    (let* ((class (class-of instance))
           (copy (allocate-instance class)))
      (dolist (slot (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots class)))
        (when (slot-boundp instance slot)
          (setf (slot-value copy slot)
                (slot-value instance slot))))
      copy)))

;;;;;;;;;;;;;;;;;;;;;;;;;; Atomic Elements ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (qubit (:constructor qubit (index)))
  "A qubit address."
  (index nil :type unsigned-byte))

(defstruct (memory-name (:constructor memory-name (region-name &optional descriptor)))
  "A bare name of a memory region, used for LOAD and STORE operands."
  (region-name nil :read-only t :type string)
  ;; The originating memory descriptor. Filled in during analysis.
  (descriptor nil :type (or null memory-descriptor)))

(defstruct (memory-offset (:constructor memory-offset (offset)))
  "A bare offset into a memory region, used for LOAD and STORE operands."
  (offset nil :read-only t :type integer))

(defstruct (memory-ref (:constructor mref (name position &optional descriptor))
                       (:predicate is-mref))
  "A reference into classical memory."
  (name nil :read-only t :type string)
  (position nil :read-only t :type unsigned-byte)
  ;; The originating memory descriptor. Filled in during analysis.
  (descriptor nil :type (or null memory-descriptor)))

(defun memory-ref= (a b)
  (and (string= (memory-ref-name a) (memory-ref-name b))
       (= (memory-ref-position a) (memory-ref-position b))))

(defun memory-ref-hash (m)
  #+sbcl
  (sb-int:mix (sxhash (memory-ref-name m)) (sxhash (memory-ref-position m)))
  #-sbcl
  (logxor (sxhash (memory-ref-name m)) (sxhash (memory-ref-position m))))

;; Used to make a memory reference - keyed hashmap in analysis/rewrite-arithmetic.lisp
(sb-ext:define-hash-table-test memory-ref= memory-ref-hash)

(defmethod print-object ((mref memory-ref) stream)
  (print-unreadable-object (mref stream :type t :identity nil)
    (format stream "~A[~D]~:[~;*~]"
            (memory-ref-name mref)
            (memory-ref-position mref)
            (memory-ref-descriptor mref))))

(defstruct (constant (:constructor constant (value &optional (value-type quil-real)))
                     (:predicate is-constant))
  "A constant numerical value."
  (value nil :type number)
  (value-type quil-real :type quil-type))

(defstruct (label (:constructor label (name)))
  "A label name. Corresponds to names prepended with `@' in Quil."
  (name nil :type (or string unsigned-byte)))

(defstruct (param (:constructor param (name))
                  (:predicate is-param))
  "A formal parameter. Corresponds to names prepended with `%' in Quil. Represents a numerical value or a classical memory reference."
  (name nil :read-only t :type string))

(defstruct (formal (:constructor formal (name))
                   (:predicate is-formal))
  "A formal argument. Represents a placeholder for a qubit or a memory reference."
  (name nil :read-only t :type string))

;;; Memory descriptors are a part of the parsing process, but are
;;; defined in classical-memory.lisp.

(defstruct (delayed-expression (:constructor %delayed-expression))
  "A representation of an arithmetic expression with fillable \"slots\".

PARAMS is a list of values to fill the slots with. These can be formal variables, but eventually must be a list of CONSTANTs.

LAMBDA-PARAMS is a list of symbols that EXPRESSION refers to.

EXPRESSION should be an arithetic (Lisp) form which refers to LAMBDA-PARAMS."
  (params nil)
  (lambda-params nil :read-only t)
  (expression nil :read-only t))

(defun make-delayed-expression (params lambda-params expression)
  "Make a DELAYED-EXPRESSION object initially with parameters PARAMS (probably a list of PARAM objects), lambda parameters LAMBDA-PARAMS, and the form EXPRESSION."
  (assert (every #'symbolp lambda-params))
  (%delayed-expression :params params
                       :lambda-params lambda-params
                       :expression expression))

(defun evaluate-delayed-expression (de &optional (memory-model-evaluator #'identity))
  "Evaluate the delayed expression DE to a numerical value (represented in a CONSTANT data structure). MEMORY-MODEL is an association list with keys MEMORY-REF structures and values the value stored at that location."
  (labels ((lookup-function (expr)
             (case expr
               ((+ - * / cos sin tan) expr)
               (otherwise (error "Illegal function in arithmetic expression: ~a." expr))))
           (evaluate-parameter (param)
             (etypecase param
               (constant (constant-value param))
               (delayed-expression (constant-value (evaluate-delayed-expression param memory-model-evaluator)))))
           (evaluate-expr (params lambda-params expression)
             (etypecase expression
               (memory-ref
                (funcall memory-model-evaluator expression))
               (cons
                (let ((args (mapcar (lambda (e) (evaluate-expr params lambda-params e))
                                    (cdr expression))))
                  (if (every (lambda (thing) (typep thing 'number)) args)
                      (apply (lookup-function (first expression)) args)
                      (cdr expression))))
               (symbol
                (cond
                  ((eql expression 'pi)
                   pi)
                  ((member expression lambda-params)
                   (evaluate-parameter (nth (position expression lambda-params)
                                            params)))
                  (t
                   (error "Bad symbol ~a in delayed expression." expression))))
               (number
                expression))))
    (let ((eval-attempt (evaluate-expr (delayed-expression-params de)
                                       (delayed-expression-lambda-params de)
                                       (delayed-expression-expression de))))
      (if (typep eval-attempt 'number)
          (constant eval-attempt)
          de))))


(defun map-de-params (f de)
  "Create a new DELAYED-EXPRESSION from DE, applying F to all of the parameters of DE."
  (let ((c (copy-structure de)))
    (setf (delayed-expression-params c) (mapcar f (delayed-expression-params de)))
    c))

;;;;;;;;;;;;;;;;;;;;;;;; Pseudo-Instructions ;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jump-target ()
  ((label :initarg :label
          :reader jump-target-label))
  (:documentation "A target which can be jumped to. Corresponds to the LABEL directive."))

(defun jump-target-p (x)
  "Is X a jump target?"
  (typep x 'jump-target))

(defclass include ()
  ((pathname :initarg :pathname
             :reader include-pathname))
  (:documentation "A directive to include another file in a Quil file."))


;;;;;;;;;;;;;;;;;;;; Gate and Circuit Definitions ;;;;;;;;;;;;;;;;;;;;

(defclass gate-definition ()
  ((name :initarg :name
         :reader gate-definition-name)
   (entries :initarg :entries
            :reader gate-definition-entries))
  (:metaclass abstract-class)
  (:documentation "A representation of a raw, user-specified gate definition. This is *not* supposed to be an executable representation."))

(defclass static-gate-definition (gate-definition)
  ()
  (:documentation "A gate definition that has no parameters."))

(defclass parameterized-gate-definition (gate-definition)
  ((parameters :initarg :parameters
               :reader gate-definition-parameters
               :documentation "A list of symbol parameter names."))
  (:documentation "A gate definition that has named parameters."))

(defun make-gate-definition (name parameters entries)
  "Make a static or parameterized gate definition instance, depending on the existence of PARAMETERS."
  (check-type name string)
  (assert (every #'symbolp parameters))
  (if (null parameters)
      (make-instance 'static-gate-definition
                     :name name
                     :entries entries)
      (make-instance 'parameterized-gate-definition
                     :name name
                     :parameters parameters
                     :entries entries)))

(defclass circuit-definition ()
  ((name :initarg :name
         :reader circuit-definition-name)
   (parameters :initarg :parameters
               :reader circuit-definition-parameters)
   (arguments :initarg :arguments
              :reader circuit-definition-arguments)
   (body :initarg :body
         :reader circuit-definition-body)))

(defun make-circuit-definition (name params args body)
  (check-type name string)
  (assert (every #'is-param params))
  (assert (every #'is-formal args))
  (make-instance 'circuit-definition
                 :name name
                 :parameters params
                 :arguments args
                 :body body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Instructions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Instructions and their protocol.

(defclass instruction ()
  ()
  (:documentation "Abstract class representing an executable instruction.")
  (:metaclass abstract-class))

(defgeneric arguments (instruction)
  (:documentation "Return a simple vector of arguments to an instruction."))

(defgeneric mnemonic (instruction)
  (:documentation "Return the string mnemonic and base class name of the instruction."))

(defun instructionp (x)
  "Is X an instruction?"
  (typep x 'instruction))


;;; Now for the actual instructions.

(defclass no-operation (instruction)
  ()
  (:documentation "The \"do-nothing\" instruction.")
  (:metaclass singleton-class))

(defmethod arguments ((instruction no-operation)) #())
(defmethod mnemonic  ((instruction no-operation)) (values "NOP" 'no-operation))

(defclass pragma (instruction)
  ((words :initarg :words
          :reader pragma-words
          :documentation "A list of strings derived from identifiers or
numbers. It must start with a string.")
   (freeform-string :initarg :freeform-string
                    :reader pragma-freeform-string
                    :documentation "A freeform string."))
  (:default-initargs :words nil :freeform-string nil)
  (:documentation "A compiler/interpreter pragma. Usually reserved for non-standard extensions to Quil that don't affect its interpretation."))

(defmethod mnemonic ((inst pragma)) (values "PRAGMA" (class-name (class-of inst))))

(defun make-pragma (words &optional freeform)
  (assert (and (listp words)
               (not (endp words))))
  (assert (stringp (first words))
          ((first words)))
  (assert (every (alexandria:disjoin #'stringp #'integerp) words)
          (words))
  (check-type freeform (or null string))
  (specialize-pragma
   (make-instance 'pragma :words words :freeform-string freeform)))

(defclass halt (instruction)
  ()
  (:documentation "An instruction to immediately halt all execution.")
  (:metaclass singleton-class))

(defmethod arguments ((instruction halt)) #())
(defmethod mnemonic  ((instruction halt)) (values "HALT" 'halt))

(defclass reset (instruction)
  ()
  (:documentation "An instruction to reset all qubits to the |0>-state.")
  (:metaclass singleton-class))

(defmethod arguments ((instruction reset)) #())
(defmethod mnemonic  ((instruction reset)) (values "RESET" 'reset))

(defclass reset-qubit (instruction)
  ((target :initarg :target
           :accessor reset-qubit-target))
  (:documentation "An instruction to reset a specific qubit into the |0>-state.
If the targeted qubit is entangled with other qubits the resulting action on the wavefunction is non-deterministic,
as the reset is formally equivalent to measuring the qubit and then conditionally applying a NOT gate."))

(defmethod arguments ((instruction reset-qubit)) (vector (reset-qubit-target instruction)))
(defmethod mnemonic  ((instruction reset)) (values "RESET" 'reset-qubit))

(defclass wait (instruction)
  ()
  (:documentation "An instruction to wait for some signal from a classical processor.")
  (:metaclass singleton-class))

(defmethod arguments ((instruction wait)) #())
(defmethod mnemonic  ((instruction wait)) (values "WAIT" 'wait))

(defclass unary-classical-instruction (instruction)
  ((target :initarg :target
           :reader classical-target))
  (:documentation "An instruction representing a unary classical function.")
  (:metaclass abstract-class))

(defmethod arguments ((inst unary-classical-instruction))
  (vector (classical-target inst)))

(defclass binary-classical-instruction (instruction)
  ((left :initarg :left
         :reader classical-left-operand
         :accessor classical-target)
   (right :initarg :right
          :accessor classical-right-operand))
  (:documentation "An instruction representing a binary classical function.")
  (:metaclass abstract-class))

(defmethod arguments ((inst binary-classical-instruction))
  (vector (classical-left-operand inst)
          (classical-right-operand inst)))

(defclass trinary-classical-instruction (instruction)
  ((target :initarg :target
           :reader classical-target)
   (left :initarg :left
         :reader classical-left-operand)
   (right :initarg :right
          :reader classical-right-operand))
  (:documentation "An instruction representing a trinary classical function.")
  (:metaclass abstract-class))

(defmethod arguments ((inst trinary-classical-instruction))
  (vector (classical-target inst)
          (classical-left-operand inst)
          (classical-right-operand inst)))

(defgeneric specialize-types (instruction memory-descriptors)
  (:documentation "Specialize the types that a classical instruction represents.")
  ;; By default, just return the instruction itself. It's a sensible
  ;; default for almost all instructions, and lets us simply call
  ;; SPECIALIZE-TYPES idempotently, as well as call it within the type
  ;; checker.
  (:method (instruction memory-descriptors)
    (declare (ignore memory-descriptors))
    instruction))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (global-vars:define-global-var **mnemonic-types**
    (make-hash-table :test 'equal)
    "A map between mnemonic instruction strings and a list of possible argument types represented as vectors of type symbols.")

  (defun mnemonic-addressing-modes (mnemonic-string)
    "Given a Quil instruction mnemonic MNEMONIC-STRING, return the different \"addressing modes\" of the instruction as a list.

Each addressing mode will be a vector of symbols:

    IMMEDIATE: immediate value
    BIT*, OCTET*, INTEGER*, REAL*: A reference/address/name to a particular data type.
    BIT, OCTET, INTEGER, REAL: A memory lookup/dereference to a particular data type.
"
    (values (gethash mnemonic-string **mnemonic-types**)))

  (global-vars:define-global-var **classical-instruction-class-argument-types**
    (make-hash-table :test 'eq)
    "A map between a class name (symbol) and the vector of types.")

  (defun make-typed-name (name types)
    (alexandria:format-symbol (symbol-package name)
                              "~A-~{~A~^/~}"
                              name
                              types))

  (defun valid-type-symbol-p (s)
    (member s '(
                ;; Immediate values
                immediate
                ;; Type names
                bit integer real octet
                ;; Typed references
                bit* integer* real* octet*)))

  (defun memory-descriptors->type-resolver (descriptors)
    "Given a sequence of memory descriptors, produce a function which takes a name as a string, and returns the type (a QUIL-TYPE) associated with that name, or an error."
    (lambda (name)
      (check-type name string)
      (let ((descr (find name descriptors :key #'memory-descriptor-name
                                          :test #'string=)))
        (when (null descr)
          (cerror "Return NIL."
                  "Couldn't determine the type associated with ~S"
                  name)
          nil)
        (memory-descriptor-type descr))))

  (defun argument-type-matches-p (resolver type arg)
    "Given a type resolver (as if returned by MEMORY-DESCRIPTORS->TYPE-RESOLVER), along with a symbolic type name TYPE and an argument ARG (that would be found as an argument to any of the classical instructions), check that ARG conforms to the type specification TYPE."
    (etypecase arg
      ;; Only TYPE* satisfy names, but we need to check TYPE.
      (memory-name
       (adt:match quil-type (funcall resolver (memory-name-region-name arg))
         (quil-bit     (eq 'bit*     type))
         (quil-octet   (eq 'octet*   type))
         (quil-integer (eq 'integer* type))
         (quil-real    (eq 'real*    type))))
      ;; Only bare types are allowed for memory refs.
      (memory-ref
       (adt:match quil-type (funcall resolver (memory-ref-name arg))
         (quil-bit     (eq 'bit     type))
         (quil-octet   (eq 'octet   type))
         (quil-integer (eq 'integer type))
         (quil-real    (eq 'real    type))))
      ;; Only immediate's satisfy CONSTANT arguments.
      (constant
       (case type
         (immediate t)
         (otherwise nil)))
      ;; If we encounter a FORMAL, we haven't properly expanded.
      (formal (error "Can't runtime type check a formal because ~
                      circuits haven't been expanded."))))

  (defun argument-types-match-p (resolver types args)
    "Check that all of the arguments ARGS (a sequence) conform to each of the respective type specifications TYPES (a sequence, specifically the symbolic types as seen below), all according to the resolver."
    (every (lambda (type arg) (argument-type-matches-p resolver type arg))
           types
           args))
  ;; function to take a type tuple and produce a COND-compatible test.

  (defun expand-classical-instruction-definitions (name mnemonic num-args superclass docstring types)
    (check-type name symbol)
    (check-type mnemonic string)
    (check-type num-args (integer 1))
    (check-type superclass symbol)
    (check-type docstring string)
    (check-type types list)
    `(progn
       ;; Base instruction class. Can be specialized.
       (defclass ,name (,superclass)
         ()
         (:documentation ,docstring))

       ;; The type specializer.
       (defmethod specialize-types ((instr ,name) memory-descriptors)
         (let ((args (arguments instr))
               (resolver (memory-descriptors->type-resolver memory-descriptors)))
           (cond
             ,@(loop :for type-tuple :in types
                     :for typed-name := (make-typed-name name type-tuple)
                     :collect `((argument-types-match-p resolver ',type-tuple args)
                                (change-class instr ',typed-name)))
             (t (cerror "Return the original instruction."
                        "Could not specialize the type of ~A." instr)
                instr))))

       ;; The mnemonic for this group of instructions.
       (defmethod mnemonic ((inst ,name)) (values ',mnemonic ',name))

       ;; Each typed instantiation.
       ,@(loop :for type-tuple :in types
               :for typed-name := (make-typed-name name type-tuple)
               :do (assert (and (= num-args (length type-tuple))
                                (every #'valid-type-symbol-p type-tuple)))
               :collect typed-name :into typed-names
               :append (list
                        ;; Leaf class.
                        `(defclass ,typed-name (,name)
                           ()
                           (:documentation
                            ,(format nil "A ~A specialized on (~{~A~^, ~})."
                                     name
                                     type-tuple)))
                        ;; Since we specialize on the superclass, we
                        ;; will also specialize on the subclass, even
                        ;; though we have an overarching method
                        ;; specializing on T. We don't want to
                        ;; re-type-specialize something that has
                        ;; already been specialized.
                        `(defmethod specialize-types ((instr ,typed-name) memory-descriptors)
                           (declare (ignore memory-descriptors))
                           ;; Do nothing. Already specialized.
                           instr)
                        ;; Store the argument types in various fashions.
                        `(setf (gethash ',typed-name **classical-instruction-class-argument-types**)
                               ',(coerce type-tuple 'vector))
                        `(pushnew ',(coerce type-tuple 'vector)
                                  (gethash ',mnemonic **mnemonic-types**)
                                  :test 'equalp))))))

(defun classical-instruction-argument-types (instruction)
  "Given a classical instruction instance INSTRUCTION, return the types of the arguments."
  (let ((class-name (class-name (class-of instruction))))
    (or (gethash class-name **classical-instruction-class-argument-types**)
        (error "The instruction class ~A doesn't have an associated ~
                argument type."
               class-name))))

;;; Ok, so DEFINE-CLASSICAL-INSTRUCTION will generate a base class for
;;; the instruction associated with the mnemonic, along with a variety
;;; of type-specifc instructions, sometimes referred to as the various
;;; "addressing modes" of the mnemonic. (This isn't exactly right, but
;;; close enough for our analogy.)
;;;
;;; Why do this? Why not parameterize instructions on mnemonic-type
;;; pairs? In other words, why can't we have some general notion of a
;;; CLASSICAL-AND instruction, along with a handful of addressing
;;; modes like (OCTET, OCTET) and (OCTET, IMMEDIATE)? The reason is
;;; that we want our base instruction classes to refer to specific
;;; operational semantics. The (OCTET, OCTET) mode has distinct
;;; operational semantics from (OCTET, IMMEDIATE).
;;;
;;; In other parts of CL-QUIL, we will find it useful to work with
;;; this parameterization, however. So the functions MNEMONIC as well
;;; as CLASSICAL-INSTRUCTION-ARGUMENT-TYPES provide this facility.

(defmacro define-classical-instruction (name mnemonic &body body)
  (check-type mnemonic string)
  (multiple-value-bind (types decls docstring)
      (alexandria:parse-body body :documentation t)
    ;; Declarations are not allowed.
    (assert (null decls))
    ;; An empty body, or a body with non-lists aren't allowed.
    (assert (and (not (endp types))
                 (every #'listp types)))
    (let ((num-args (length (first types))))
      ;; All specializations must be the same length.
      (assert (every (lambda (spec) (= num-args (length spec))) types))
      (expand-classical-instruction-definitions
       name
       mnemonic
       num-args
       (ecase num-args
         ((1) 'unary-classical-instruction)
         ((2) 'binary-classical-instruction)
         ((3) 'trinary-classical-instruction))
       docstring
       types))))

(define-classical-instruction classical-negate "NEG"
  "The arithmetic negation instruction."
  (integer)
  (real))

(define-classical-instruction classical-not "NOT"
  "The bit toggling instruction."
  (octet)
  (integer)
  (bit))

(define-classical-instruction classical-and "AND"
  "An instruction representing bitwise AND."
  (octet   octet)
  (octet   immediate)
  (integer integer)
  (integer immediate)
  (bit     bit)
  (bit     immediate))

(define-classical-instruction classical-inclusive-or "IOR"
  "An instruction representing bitwise IOR."
  (octet   octet)
  (octet   immediate)
  (integer integer)
  (integer immediate)
  (bit     bit)
  (bit     immediate))

(define-classical-instruction classical-exclusive-or "XOR"
  "An instruction representing bitwise XOR."
  (octet   octet)
  (octet   immediate)
  (integer integer)
  (integer immediate)
  (bit     bit)
  (bit     immediate))

(define-classical-instruction classical-move "MOVE"
  "An instruction representing the movement of a value to another address."
  (octet   immediate)
  (octet   octet)
  (integer immediate)
  (integer integer)
  (real    immediate)
  (real    real)
  (bit     immediate)
  (bit     bit))

(define-classical-instruction classical-exchange "EXCHANGE"
  "The exchange of two values."
  (octet   octet)
  (integer integer)
  (real    real)
  (bit     bit))

(define-classical-instruction classical-convert "CONVERT"
  "An instruction representing a storing value cast from one memory location to another."
  (integer real)
  (integer bit)
  (real    integer)
  (real    bit)
  (bit     integer)
  (bit     real))

(define-classical-instruction classical-addition "ADD"
  "An instruction representing the sum of two values."
  (integer integer)
  (integer immediate)
  (real    real)
  (real    immediate))

(define-classical-instruction classical-subtraction "SUB"
  "An instruction representing the difference of two values."
  (integer integer)
  (integer immediate)
  (real    real)
  (real    immediate))

(define-classical-instruction classical-multiplication "MUL"
  "An instruction representing the product of two values."
  (integer integer)
  (integer immediate)
  (real    real)
  (real    immediate))

(define-classical-instruction classical-division "DIV"
  "An instruction representing the quotient of two values."
  (integer integer)
  (integer immediate)
  (real    real)
  (real    immediate))

(define-classical-instruction classical-load "LOAD"
  "An instruction representing an indirect load from an array."
  (octet   octet*   integer)
  (integer integer* integer)
  (real    real*    integer)
  (bit     bit*     integer))

(define-classical-instruction classical-store "STORE"
  "An instruction representing an indirect store to an array."
  (octet*   integer octet)
  (octet*   integer immediate)
  (integer* integer integer)
  (integer* integer immediate)
  (real*    integer real)
  (real*    integer immediate)
  (bit*     integer bit)
  (bit*     integer immediate))

(define-classical-instruction classical-equality "EQ"
  "An instruction representing the test #'=."
  (bit bit     bit)
  (bit bit     immediate)
  (bit octet   octet)
  (bit octet   immediate)
  (bit integer integer)
  (bit integer immediate)
  (bit real    real)
  (bit real    immediate))

(define-classical-instruction classical-greater-than "GT"
  "An instruction representing the test #'>."
  (bit bit     bit)
  (bit bit     immediate)
  (bit octet   octet)
  (bit octet   immediate)
  (bit integer integer)
  (bit integer immediate)
  (bit real    real)
  (bit real    immediate))

(define-classical-instruction classical-greater-equal "GE"
  "An instruction representing the test #'>=."
  (bit bit     bit)
  (bit bit     immediate)
  (bit octet   octet)
  (bit octet   immediate)
  (bit integer integer)
  (bit integer immediate)
  (bit real    real)
  (bit real    immediate))

(define-classical-instruction classical-less-than "LT"
  "An instruction representing the test #'<."
  (bit bit     bit)
  (bit bit     immediate)
  (bit octet   octet)
  (bit octet   immediate)
  (bit integer integer)
  (bit integer immediate)
  (bit real    real)
  (bit real    immediate))

(define-classical-instruction classical-less-equal "LE"
  "An instruction representing the test #'<=."
  (bit bit     bit)
  (bit bit     immediate)
  (bit octet   octet)
  (bit octet   immediate)
  (bit integer integer)
  (bit integer immediate)
  (bit real    real)
  (bit real    immediate))

(defclass jump (instruction)
  ((label :initarg :label
          :accessor jump-label))
  (:documentation "Superclass to all jump-like instructions.")
  (:metaclass abstract-class))

(defclass unconditional-jump (jump)
  ()
  (:documentation "The instruction representing an unconditional jump to a label."))

(defmethod mnemonic ((inst unconditional-jump)) (values "JUMP" 'unconditional-jump))

(defclass conditional-jump (jump)
  ((address :initarg :address
            :reader conditional-jump-address))
  (:documentation "Superclass to all conditional jump instructions.")
  (:metaclass abstract-class))

(defclass jump-when (conditional-jump)
  ()
  (:documentation "The instruction representing a jump to a target when a classical bit is 1."))

(defmethod mnemonic ((inst jump-when)) (values "JUMP-WHEN" 'jump-when))

(defclass jump-unless (conditional-jump)
  ()
  (:documentation "The instruction representing a jump to a target when a classical bit is 0."))

(defmethod mnemonic ((inst jump-unless)) (values "JUMP-UNLESS" 'jump-unless))

(defclass measurement (instruction)
  ((qubit :initarg :qubit
          :accessor measurement-qubit))
  (:documentation "Superclass to all measurement instructions.")
  (:metaclass abstract-class))

(defclass measure (measurement)
  ((address :initarg :address
            :reader measure-address))
  (:documentation "The instruction representing a measurement into a classical register."))

(defmethod arguments ((inst measure)) (vector (measure-address inst)))
(defmethod mnemonic  ((inst measure)) (values "MEASURE" 'measure))

(defclass measure-discard (measurement)
  ()
  (:documentation "The instruction representing a measurement, throwing away the result."))

(defmethod arguments ((inst measure-discard)) #())
(defmethod mnemonic  ((inst measure-discard)) (values "MEASURE" 'measure-discard))

(adt:defdata operator-description
  "A description of an operator that Quil takes."
  (named-operator      string)
  (controlled-operator operator-description)
  ;; Note that reduction of consecutive dagger operators is not
  ;; performed here. Use the alternative constructor
  ;; INVOLUTIVE-DAGGER-OPERATOR.
  (dagger-operator     operator-description))

(defun involutive-dagger-operator (od)
  "Instantiate a dagger operator on the operator description OD and
reduce consecutive dagger operators.

For example, `DAGGER DAGGER H 0` should produce `H 0`."
  (adt:match operator-description od
    ((dagger-operator inner-od) inner-od)
    (_ (dagger-operator od))))

(defun operator-description-name (od)
  (adt:match operator-description od
    ((named-operator name)   name)
    (_ (error "The application doesn't have a canonical name."))))

(defun operator-description-root-name (od)
  "The \"root name\" that the operator description represents. This is usually going to name a gate that said description modifies."
  (adt:match operator-description od
    ((named-operator name)   name)
    ((controlled-operator o) (operator-description-root-name o))
    ((dagger-operator o)     (operator-description-root-name o))))

(defun operator-description-additional-qubits (od)
  "The number of additional qubits incurred by this operator description (e.g., CONTROLLED adds one qubit)."
  (adt:match operator-description od
    ((named-operator _)   0)
    ((controlled-operator o) (1+ (operator-description-additional-qubits o)))
    ((dagger-operator o)     (operator-description-additional-qubits o))))

(defun print-operator-description (od stream)
  (adt:match operator-description od
    ((named-operator name) (write-string name stream))
    ((controlled-operator o) (write-string "CONTROLLED " stream)
                             (print-operator-description o stream))
    ((dagger-operator o) (write-string "DAGGER " stream)
                         (print-operator-description o stream))))

(defun operator-description-string (od)
  (adt:match operator-description od
    ((named-operator name) name)
    (_
     (with-output-to-string (s)
       (print-operator-description od s)))))

(defun plain-operator-p (od)
  "Is the operator description OD plain?

An operator is *plain* if it is described by a NAMED-OPERATOR."
  (adt:match operator-description od
    ((named-operator _) t)
    (_ nil)))

(defun simple-dagger-operator-p (od)
  "Is the operator description OD a simple dagger application?

An operator is considered to be a simple dagger application it
consists of a DAGGER-OPERATOR acting on a NAMED-OPERATOR."
  (adt:match operator-description od
    ((dagger-operator dod) (plain-operator-p dod))
    (_ nil)))

(defclass application (instruction)
  ((operator :initarg :operator
             :accessor application-operator
             :type operator-description)
   (parameters :initarg :parameters
               :accessor application-parameters)
   (arguments :initarg :arguments
              :accessor application-arguments))
  (:default-initargs
   :parameters nil
   :arguments nil)
  (:documentation "Superclass to all application-like instructions.")
  (:metaclass abstract-class))

(defun application-operator-name (app)
  (operator-description-name (application-operator app)))

(defclass unresolved-application (application)
  ()
  (:documentation "Represents an application that hasn't yet been resolved. Possibilities:

    * Application is a gate application.

    * Application is a circuit application.

    * Application is an invalid application.

Determining this requires the context of the surrounding program."))

(defclass gate-application (application)
  ((name-resolution :initarg :name-resolution
                    :reader gate-application-resolution
                    ;; We do this type check in INITIALIZE-INSTANCE.
                    :type gate-definition
                    :documentation "The resolved definition of a gate. This may be some kind of GATE-DEFINITION, or it may be a gate object directly. In general, this resolution is ultimately a way to interpret what the root name of the APPLICATION-OPERATOR means. This is also the object from which one can produce more optimized representations of the gate application.

This definition does *not* incorporate the operator description (i.e., any operator modifiers like CONTROLLED).

If this slot is not supplied, then the gate is considered *anonymous*. If this is the case, then the GATE slot must be supplied.")
   (gate :initarg :gate
         :accessor gate-application-gate
         :documentation "The actual gate object that is being applied. N.B. After applications are resolved, one can always look at the definition of a gate via GATE-APPLICATION-RESOLUTION. But this slot is reserved for actual *execution*, which may depend on the execution backend and how one wishes to optimize."))
  (:documentation "An instruction representing an application of a known gate."))

(defgeneric anonymous-gate-application-p (app)
  (:documentation "Is the gate application APP an anonymous application?")
  (:method ((app gate-application))
    (not (slot-boundp app 'name-resolution))))

(defmethod initialize-instance :after ((app gate-application) &rest initargs)
  (declare (ignore initargs))
  (assert (typep (application-operator app) 'operator-description)
          (app)
          "A gate application must have an operator description as its operator.")
  (cond
    ((slot-boundp app 'name-resolution)
     ;; Can't use CHECK-TYPE because it's not a place.
     (assert (typep (gate-application-resolution app) 'gate-definition)
             (app)
             "The NAME-RESOLUTION of a gate application must be a GATE-DEFINITION instance."))
    (t
     (assert (slot-boundp app 'gate) (app) "A gate application was made that doesn't have a definition. Please specify either its name resolution or a gate."))))

;;; XXX FIXME TODO: This doesn't actually check that SWAP is the same
;;; one as defined in the standard, because there is no notion of a
;;; "resolved" environment.
(defun swap-application-p (app)
  "Does APP look like a SWAP application?"
  (and (typep app 'gate-application)
       (adt:match operator-description (application-operator app)
         ((named-operator name) (string= "SWAP" name))
         (_ nil))))

(defclass circuit-application (application)
  ((circuit-definition :initarg :circuit-definition
                       :accessor circuit-application-definition
                       :documentation "The definition of the circuit used in this application."))
  (:documentation "An instruction representing an application of a known circuit."))

(defvar *print-fractional-radians* t
  "When true, FORMAT-COMPLEX pretty-prints some common fractions of pi in a more human-readable form.

N.B., The fractions of pi will be printed up to a certain precision!")

(defun format-complex (z stream)
  "Print the real or complex number Z nicely to the stream STREAM."
  (check-type z number)
  (cond
    ((zerop z)
     (format stream "0.0"))
    ((realp z)
     (when *print-fractional-radians*
       (when (double~ z pi)
         (return-from format-complex (format stream "pi")))
       (when (double~ z (- pi))
         (return-from format-complex (format stream "-pi")))
       (dolist (denom '(2 3 4 6 8 16))
         (dotimes (numer denom)
           (when (double~ z (/ (* pi numer) denom))
             (return-from format-complex
               (cond
                 ((= numer 1)
                  (format stream "pi/~d" denom))
                 ((zerop numer)
                  (format stream "~F" z))
                 (t (format stream "~d*pi/~d" numer denom)))))
           (when (double~ z (/ (* -1 pi numer) denom))
             (return-from format-complex
               (cond
                 ((= numer 1)
                  (format stream "-pi/~d" denom))
                 ((zerop numer)
                  (format stream "~F" z))
                 (t (format stream "-~d*pi/~d" numer denom))))))))
     (format stream "~F" z))
    ((complexp z)
     (cond
       ((zerop (imagpart z))
        (format-complex (realpart z) stream))
       (t
        (format stream "~a~a~a"
                (if (zerop (realpart z))
                    ""
                    (format nil "~F" (realpart z)))
                (if (and (plusp (imagpart z))
                         (not (zerop (realpart z))))
                    (format nil "+")
                    "")
                (format nil "~Fi" (imagpart z))))))))

(defun print-instruction (instr &optional (stream *standard-output*))
  (print-instruction-generic instr stream))

(defgeneric print-instruction-generic (instr stream) ; total function
  (:documentation "Print the Quil instruction INSTR nicely to the stream STREAM.")
  ;; Support NIL stream.
  (:method (instr (stream null))
    (with-output-to-string (s)
      (print-instruction-generic instr s)))

  ;; Atomic objects
  (:method ((thing qubit) (stream stream))
    (format stream "~D" (qubit-index thing)))

  (:method ((thing memory-ref) (stream stream))
    (format stream "~a[~a]"
            (memory-ref-name thing)
            (memory-ref-position thing)))

  (:method ((thing memory-offset) (stream stream))
    (format stream "~A" (memory-offset-offset thing)))

  (:method ((thing memory-name) (stream stream))
    (format stream "~A" (memory-name-region-name thing)))

  (:method ((thing label) (stream stream))
    (format stream "@~A" (label-name thing)))

  (:method ((thing constant) (stream stream))
    (adt:match quil-type (constant-value-type thing)
      (quil-bit (format stream "~a" (constant-value thing)))
      (quil-octet (format stream "~a" (constant-value thing)))
      (quil-integer (format stream "~a" (constant-value thing)))
      (quil-real (format-complex (constant-value thing) stream))))

  (:method ((thing param) (stream stream))
    (format stream "%~A" (param-name thing)))

  (:method ((thing formal) (stream stream))
    (format stream "~A" (formal-name thing)))

  (:method ((thing delayed-expression) (stream stream))
    (labels ((print-delayed-expression (expr stream)
               (typecase expr
                 (cons
                  (cond
                    ((eql 'mref (first expr))
                     (format stream "~a[~a]" (second expr) (third expr)))
                    ((= (length expr) 3)
                     (format stream "(~a~a~a)"
                             (print-delayed-expression (second expr) nil)
                             (first expr)
                             (print-delayed-expression (third expr) nil)))
                    ((= (length expr) 2)
                     (format stream "~a(~a)"
                             (first expr)
                             (print-delayed-expression (second expr) nil)))))
                 (number
                  (format stream "(~a)"
                          (format-complex expr nil)))
                 (symbol
                  (format stream "%~a" expr))
                 (otherwise
                  (print-instruction expr stream)))))
      (print-delayed-expression (delayed-expression-expression thing) stream)))

  ;; Actual instructions
  (:method ((instr halt) (stream stream))
    (format stream "HALT"))

  (:method ((instr reset) (stream stream))
    (format stream "RESET"))

  (:method ((instr reset-qubit) stream)
    (format stream "RESET ~a" (print-instruction (reset-qubit-target instr) nil)))

  (:method ((instr wait) (stream stream))
    (format stream "WAIT"))

  (:method ((instr no-operation) (stream stream))
    (format stream "NOP"))

  (:method ((instr classical-negate) (stream stream))
    (format stream "NEG ~a"
            (print-instruction (classical-target instr) nil)))

  (:method ((instr classical-not) (stream stream))
    (format stream "NOT ~a"
            (print-instruction (classical-target instr) nil)))

  (:method ((instr classical-and) (stream stream))
    (format stream "AND ~a ~a"
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-inclusive-or) (stream stream))
    (format stream "IOR ~a ~a"
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-exclusive-or) (stream stream))
    (format stream "XOR ~a ~a"
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-move) (stream stream))
    (format stream "MOVE ~a ~a"
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-exchange) (stream stream))
    (format stream "EXCHANGE ~a ~a"
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-convert) (stream stream))
    (format stream "CONVERT ~a ~a"
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-addition) (stream stream))
    (format stream "ADD ~a ~a"
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-subtraction) (stream stream))
    (format stream "SUB ~a ~a"
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-multiplication) (stream stream))
    (format stream "MUL ~a ~a"
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-division) (stream stream))
    (format stream "DIV ~a ~a"
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-load) (stream stream))
    (format stream "LOAD ~a ~a ~a"
            (print-instruction (classical-target instr) nil)
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-store) (stream stream))
    (format stream "STORE ~a ~a ~a"
            (print-instruction (classical-target instr) nil)
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-equality) (stream stream))
    (format stream "EQ ~a ~a ~a"
            (print-instruction (classical-target instr) nil)
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-greater-than) (stream stream))
    (format stream "GT ~a ~a ~a"
            (print-instruction (classical-target instr) nil)
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-greater-equal) (stream stream))
    (format stream "GE ~a ~a ~a"
            (print-instruction (classical-target instr) nil)
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-less-than) (stream stream))
    (format stream "LT ~a ~a ~a"
            (print-instruction (classical-target instr) nil)
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr classical-less-equal) (stream stream))
    (format stream "LE ~a ~a ~a"
            (print-instruction (classical-target instr) nil)
            (print-instruction (classical-left-operand instr) nil)
            (print-instruction (classical-right-operand instr) nil)))

  (:method ((instr pragma) (stream stream))
    (format stream "PRAGMA ~{~A~^ ~}~@[ ~S~]"
            (pragma-words instr)
            (pragma-freeform-string instr)))

  (:method ((instr jump-target) (stream stream))
    (format stream "LABEL ~a"
            (print-instruction (jump-target-label instr) nil)))

  (:method ((instr jump) (stream stream))
    (let ((l (jump-label instr)))
      (cond
        ((integerp l)
         (format stream "JUMP {absolute address ~D}" l))
        (t
         (format stream "JUMP ~a"
                 (print-instruction (jump-label instr) nil))))))

  (:method ((instr jump-when) (stream stream))
    (let ((l (jump-label instr)))
      (cond
        ((integerp l)
         (format stream "JUMP-WHEN {absolute address ~D} ~a"
                 l
                 (print-instruction (conditional-jump-address instr) nil)))
        (t
         (format stream "JUMP-WHEN ~a ~a"
                 (print-instruction (jump-label instr) nil)
                 (print-instruction (conditional-jump-address instr) nil))))))

  (:method ((instr jump-unless) (stream stream))
    (let ((l (jump-label instr)))
      (cond
        ((integerp l)
         (format stream "JUMP-UNLESS {absolute address ~D} ~a"
                 l
                 (print-instruction (conditional-jump-address instr) nil)))
        (t
         (format stream "JUMP-UNLESS ~a ~a"
                 (print-instruction (jump-label instr) nil)
                 (print-instruction (conditional-jump-address instr) nil))))))

  (:method ((instr measure) (stream stream))
    (format stream "MEASURE ~a ~a"
            (print-instruction (measurement-qubit instr) nil)
            (print-instruction (measure-address instr) nil)))

  (:method ((instr measure-discard) (stream stream))
    (format stream "MEASURE ~a"
            (print-instruction (measurement-qubit instr) nil)))

  (:method ((instr application) (stream stream))
    (print-operator-description (application-operator instr) stream)
    (format stream "~@[(~{~a~^, ~})~]~{ ~a~}"
            (mapcar (lambda (thing) (print-instruction thing nil))
                    (application-parameters instr))
            (mapcar (lambda (thing) (print-instruction thing nil))
                    (application-arguments instr)))))

(defmethod print-object ((object instruction) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (print-instruction object stream)))


;;;;;;;;;;;;;;;;;;;;;; Program Representations ;;;;;;;;;;;;;;;;;;;;;;;

(defclass parsed-program (transformable)
  ((gate-definitions :initarg :gate-definitions
                     :accessor parsed-program-gate-definitions)
   (circuit-definitions :initarg :circuit-definitions
                        :accessor parsed-program-circuit-definitions)
   (memory-definitions :initarg :memory-definitions
                       :accessor parsed-program-memory-definitions)
   (executable-program :initarg :executable-code
                       :accessor parsed-program-executable-code))
  (:default-initargs
   :gate-definitions nil
   :circuit-definitions nil
   :memory-definitions nil
   :executable-code #()))

(defun print-parsed-program (parsed-program &optional (s *standard-output*))
  ;; write out memory definitions
  (dolist (memory-defn (parsed-program-memory-definitions parsed-program))
    (format s "DECLARE ~a ~a"
            (memory-descriptor-name memory-defn)
            (quil-type-string (memory-descriptor-type memory-defn)))
    (format s "~[[0]~;~:;[~:*~a]~]" (memory-descriptor-length memory-defn))
    (when (memory-descriptor-sharing-parent memory-defn)
      (format s " SHARING ~a"
              (memory-descriptor-sharing-parent memory-defn))
      (alexandria:when-let (x (memory-descriptor-sharing-offset-alist memory-defn))
        (format s " OFFSET")
        (loop :for (type . count) :in x
              :do (format s " ~a ~a" count (quil-type-string type)))))
    (format s "~%"))
  (unless (endp (parsed-program-memory-definitions parsed-program))
    (format s "~%"))

  ;; write out gates
  (dolist (gate-defn (parsed-program-gate-definitions parsed-program))
    (let ((gate-size (isqrt (length (gate-definition-entries gate-defn)))))
      (format s "DEFGATE ~a~@[(~{%~a~^, ~})~]:~%"
              (gate-definition-name gate-defn)
              (if (typep gate-defn 'static-gate-definition)
                  nil
                  (gate-definition-parameters gate-defn)))
      (dotimes (i gate-size)
        (format s "    ~{~a~^, ~}~%"
                (mapcar (lambda (z)
                          (with-output-to-string (s)
                            (etypecase z
                              (number
                               (format-complex z s))
                              (list
                               (print-instruction (make-delayed-expression nil nil z) s)))))
                        (subseq (gate-definition-entries gate-defn)
                                (* i gate-size)
                                (* (1+ i) gate-size)))))
      (format s "~%")))
  (unless (endp (parsed-program-gate-definitions parsed-program))
    (format s "~%"))
  ;; write out circuits
  (dolist (circuit-defn (parsed-program-circuit-definitions parsed-program))
    (format s "DEFCIRCUIT ~a"
            (circuit-definition-name circuit-defn))
    (unless (endp (circuit-definition-parameters circuit-defn))
      (format s "(~{~a~^, ~})" (mapcar (lambda (thing) (print-instruction thing nil))
                                       (circuit-definition-parameters circuit-defn))))
    (unless (endp (circuit-definition-arguments circuit-defn))
      (format s "~{ ~a~}" (mapcar (lambda (thing) (print-instruction thing nil))
                                  (circuit-definition-arguments circuit-defn))))
    (format s ":~%")
    (dolist (instr (circuit-definition-body circuit-defn))
      (format s "    ~a~%"
              (with-output-to-string (sprime)
                (print-instruction instr sprime))))
    (format s "~%"))
  (unless (endp (parsed-program-circuit-definitions parsed-program))
    (format s "~%"))
  ;; write out main block
  (loop :for instr :across (parsed-program-executable-code parsed-program)
        :do (progn
              (print-instruction instr s)
              (terpri s))))
