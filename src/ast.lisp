;;;; src/ast.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil/frontend)

;;;;;;;;;;;;;;;;;;;;;;;;;; Atomic Elements ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (qubit (:constructor qubit (index)))
  "A qubit address."
  (index nil :type unsigned-byte))

(defun qubit= (x y)
  "Do the qubits X and Y have equal indices?"
  (check-type x qubit)
  (check-type y qubit)
  (= (qubit-index x) (qubit-index y)))

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
  "Do the memory refs A and B represent the same memory ref?"
  (check-type a memory-ref)
  (check-type b memory-ref)
  (and (string= (memory-ref-name a) (memory-ref-name b))
       (= (memory-ref-position a) (memory-ref-position b))))

(defun memory-ref-hash (m)
  (check-type m memory-ref)
  #+sbcl
  (sb-int:mix (sxhash (memory-ref-name m)) (sxhash (memory-ref-position m)))
  #-sbcl
  (logxor (sxhash (memory-ref-name m)) (sxhash (memory-ref-position m))))

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

(defun constant= (x y)
  "Do the constants X and Y have equal types and values?"
  (check-type x constant)
  (check-type y constant)
  (and (equal (constant-value-type x)
              (constant-value-type y))
       (= (constant-value x)
          (constant-value y))))

(defstruct (label (:constructor label (name)))
  "A label name. Corresponds to names prepended with `@' in Quil."
  ;; We allow an UNSIGNED-BYTE so that we can jump to absolute
  ;; positions in the program. This is *NOT* exposed in Quil directly.
  (name nil :type (or string unsigned-byte)))

(defstruct (param (:constructor param (name))
                  (:predicate is-param))
  "A formal parameter. Corresponds to names prepended with `%' in Quil. Represents a numerical value or a classical memory reference."
  (name nil :read-only t :type string))

(defun param= (x y)
  "Do parameters X and Y have the same name?"
  (check-type x param)
  (check-type y param)
  (string= (param-name x)
           (param-name y)))

(defstruct (formal (:constructor formal (name))
                   (:predicate is-formal))
  "A formal argument. Represents a placeholder for a qubit or a memory reference."
  (name nil :read-only t :type string))

(defun formal= (x y)
  "Do formal arguments X and Y have the same name?"
  (check-type x formal)
  (check-type y formal)
  (string= (formal-name x) (formal-name y)))

(defun argument= (x y)
  "Are the (qubit or formal) arguments X and Y equal?"
  (cond ((and (qubit-p x) (qubit-p y))
         (qubit= x y))
        ((and (is-formal x) (is-formal y))
         (formal= x y))
        (t nil)))

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
  (check-type lambda-params symbol-list)
  (%delayed-expression :params params
                       :lambda-params lambda-params
                       :expression expression))

(defun evaluate-delayed-expression (de &optional (memory-model-evaluator #'identity))
  "Evaluate the delayed expression DE to a numerical value (represented in a CONSTANT data structure). MEMORY-MODEL is an association list with keys MEMORY-REF structures and values the value stored at that location."
  (labels ((lookup-function (expr)
             (if (valid-quil-function-or-operator-p expr)
                 expr
                 (error "Illegal function in arithmetic expression: ~A." expr)))
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
                  (if (number-list-p args)
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
                   (error "Bad symbol ~A in delayed expression." expression))))
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


;;;;;;;;;;;;;;;;;;;;; Comment protocol for syntax tree objects  ;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-comment-table ()
    "Return an empty weak hash table suitable for use as the CL-QUIL::**COMMENTS** table.

This function can be used to re-initialize the **COMMENTS** table.

Keys are tested with EQ."
    (tg:make-weak-hash-table :test 'eq :weakness ':key)))

(global-vars:define-global-var **comments**
    (make-comment-table)
  "Weak hash table populated with comments associated to different parts of an AST.

The keys are typically INSTRUCTION instances and associated values are STRINGs.")

(defun comment (x)
  "Return the comment associated with X."
  (values (gethash x **comments**)))

(defun (setf comment) (comment-string x)
  (check-type comment-string string)
  (setf (gethash x **comments**) comment-string))

;;;;;;;;;;;;;;;;;;;;;;;; Rewiring and Rewiring Comments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We store both the l2p and p2l vectors so that lookups in both
;; directions can be constant time. Because all of our qubits are in
;; the range 0...n-1, we can store these as vectors instead of hashmaps.

(defstruct (rewiring
            (:constructor init-rewiring)
            (:copier nil))
  (l2p #() :type integeropt-vector)
  (p2l #() :type integeropt-vector))

(defmethod print-object :around ((object rewiring) stream)
  (let ((*print-pretty* nil))
    (call-next-method)))

(defun make-rewiring-from-l2p (l2p)
  "Safely extract a REWIRING from a logical-to-physical array."
  (let ((p2l (make-array (length l2p) :initial-element nil)))
    (dotimes (j (length l2p))
      (let ((loc (aref l2p j)))
        (when loc
          (assert (and (< -1 loc (length p2l))) ()
                  "Malformed rewiring string: value ~A at position ~A is out of range." loc j)
          (assert (null (aref p2l loc)) ()
                  "Malformed rewiring string: repeated value ~A at position ~A." loc j)
          (setf (aref p2l loc) j))))
    (init-rewiring :l2p l2p :p2l p2l)))

(defun make-rewiring-from-string (str)
  "Safely extract a REWIRING from a string representation of an integer vector corresponding to the logical-to-physical mapping."
  (assert (and (eql #\# (aref str 0))
               (eql #\( (aref str 1))
               (eql #\) (aref str (1- (length str)))))
          nil
          "Malformed rewiring string: input ~A is not of the form #(...)." str)
  (let* ((stripped-string (string-trim "#()" str))
         (tokens (first (tokenize stripped-string)))
         (integer-vec
           (map 'vector
                (lambda (token)
                  (cond
                    ((equalp (token-payload token) "nil")
                     nil)
                    ((eql (token-type token) :integer)
                     (token-payload token))
                    (t
                     (error "Malformed rewiring string: unexpected token ~A." token))))
                tokens)))
    (make-rewiring-from-l2p integer-vec)))

(defun make-rewiring-pair-from-string (str)
  "Safely extract a pair of REWIRINGs from a string representation of a CONS of two integer vectors."
  (multiple-value-bind (matchedp matches)
      ;; This monstrosity matches strings of the form "(#(\d+ ...) . #(\d+ ...))"
      (let ((match-int-vector
              '(:REGISTER
                (:SEQUENCE "#("
                 (:GREEDY-REPETITION 0 NIL
                  (:GROUP (:SEQUENCE :DIGIT-CLASS (:GREEDY-REPETITION 0 1 #\ ))))
                 #\)))))
        (cl-ppcre:scan-to-strings
         `(:SEQUENCE
           :START-ANCHOR
           #\(
           ,match-int-vector
           (:GREEDY-REPETITION 0 NIL :WHITESPACE-CHAR-CLASS)
           #\.
           (:GREEDY-REPETITION 0 NIL :WHITESPACE-CHAR-CLASS)
           ,match-int-vector
           #\)
           :END-ANCHOR)
         str))
    (assert matchedp
            nil
            "Malformed rewiring pair string: ~@
             input ~A is not of the form (#(...) . #(...))."
            str)
    (let ((first-rewiring-string (aref matches 0))
          (second-rewiring-string (aref matches 1)))
      (assert (= (length first-rewiring-string) (length second-rewiring-string))
              nil
              "Malformed rewiring pair string: length of rewirings don't match. ~@
               first:  ~A~@
               second: ~A"
              first-rewiring-string second-rewiring-string)
      (values (make-rewiring-from-string first-rewiring-string)
              (make-rewiring-from-string second-rewiring-string)))))

(a:define-constant +entering-rewiring-prefix+
  "Entering rewiring: "
  :test #'string=
  :documentation "STRING prefix for \"entering rewiring\" comments. ")

(a:define-constant +exiting-rewiring-prefix+
  "Exiting rewiring: "
  :test #'string=
  :documentation "STRING prefix for \"exiting rewiring\" comments. ")

(a:define-constant +entering/exiting-rewiring-prefix+
  "Entering/exiting rewiring: "
  :test #'string=
  :documentation "STRING prefix for \"entering/exiting rewiring\" comments. ")

(defun comment-entering-rewiring-p (rewiring-string)
  "Does REWIRING-STRING start with +ENTERING-REWIRING-PREFIX+?"
  (uiop:string-prefix-p +entering-rewiring-prefix+ rewiring-string))

(defun comment-exiting-rewiring-p (rewiring-string)
  "Does REWIRING-STRING start with +EXITING-REWIRING-PREFIX+?"
  (uiop:string-prefix-p +exiting-rewiring-prefix+ rewiring-string))

(defun comment-entering/exiting-rewiring-p (rewiring-string)
  "Does REWIRING-STRING start with +ENTERING/EXITING-REWIRING-PREFIX+?"
  (uiop:string-prefix-p +entering/exiting-rewiring-prefix+ rewiring-string))

(defun %parse-rewiring (prefix rewiring-string make-rewiring)
  "Call MAKE-REWIRING to parse a REWIRING from REWIRING-STRING after discarding PREFIX."
  (funcall make-rewiring (subseq rewiring-string (length prefix))))

(defun parse-entering-rewiring (rewiring-string)
  "Parse an entering REWIRING from REWIRING-STRING."
  (%parse-rewiring +entering-rewiring-prefix+ rewiring-string #'make-rewiring-from-string))

(defun parse-exiting-rewiring (rewiring-string)
  "Parse an exiting REWIRING from REWIRING-STRING."
  (%parse-rewiring +exiting-rewiring-prefix+ rewiring-string #'make-rewiring-from-string))

(defun parse-entering/exiting-rewiring (rewiring-string)
  "Parse entering and exiting REWIRINGs from REWIRING-STRING.

Return (VALUES ENTERING-REWIRING EXITING-REWIRING)."
  (%parse-rewiring +entering/exiting-rewiring-prefix+
                   rewiring-string
                   #'make-rewiring-pair-from-string))

(defun rewiring-comment-type (rewiring-string)
  "Return the type of the rewiring comment in REWIRING-STRING.

Possible return values are ':ENTERING, ':EXITING, and ':ENTERING/EXITING.

If REWIRING-STRING does not have a valid rewiring comment prefix, signal an error."
  (cond ((comment-entering-rewiring-p rewiring-string)
         ':ENTERING)
        ((comment-exiting-rewiring-p rewiring-string)
         ':EXITING)
        ((comment-entering/exiting-rewiring-p rewiring-string)
         ':ENTERING/EXITING)
        (t (error "Invalid rewiring comment: ~S" rewiring-string))))

(defun make-rewiring-comment (&key entering exiting)
  "Make a rewiring comment from the given ENTERING and EXITING rewirings.

ENTERING and EXITING are both of type (OR NULL INTEGER-VECTOR REWIRING).

If both ENTERING and EXITING are non-null, make an :ENTERING/EXITING rewiring comment.
If only ENTERING is non-null, make an :ENTERING rewiring comment.
If only EXITING is non-null, make and :EXITING rewiring comment.
If both ENTERING and EXITING are null, signal an error."
  (check-type entering (or null integer-vector rewiring))
  (check-type exiting (or null integer-vector rewiring))

  (when (typep entering 'rewiring)
    (setf entering (rewiring-l2p entering)))

  (when (typep exiting 'rewiring)
    (setf exiting (rewiring-l2p exiting)))

  (let ((*print-pretty* nil))
    (cond ((and (not (null entering)) (not (null exiting)))
           (format nil "~A(~A . ~A)" +entering/exiting-rewiring-prefix+ entering exiting))
          ((not (null entering))
           (format nil "~A~A" +entering-rewiring-prefix+ entering))
          ((not (null exiting))
           (format nil "~A~A" +exiting-rewiring-prefix+ exiting))
          (t (error "MAKE-REWIRING-COMMENT: Both ENTERING and EXITING cannot be NULL.")))))

(defun instruction-rewirings (instruction)
  "Return the pair of entering and exiting rewirings associated with instruction.

Return (VALUES ENTERING EXITING) if INSTRUCTION has a combined ENTERING/EXITING rewiring attached.
Return (VALUES ENTERING NIL) if INSTRUCTION has only an ENTERING rewiring.
Return (VALUES NIL EXITING) if INSTRUCTION has only an EXITING rewiring.
Return (VALUES NIL NIL) if INSTRUCTION has no rewiring attached."
  (a:if-let ((comment (comment instruction)))
    (ecase (rewiring-comment-type comment)
      (:ENTERING (values (parse-entering-rewiring comment) nil))
      (:EXITING  (values nil (parse-exiting-rewiring comment)))
      (:ENTERING/EXITING (parse-entering/exiting-rewiring comment)))
    ;; No comment attached to INSTRUCTION.
    (values nil nil)))

(defun extract-final-exit-rewiring-vector (parsed-program)
  "Extract the final exit rewiring comment from PARSED-PROGRAM and return it as a VECTOR.

If no exit rewiring is found, return NIL."
  (check-type parsed-program parsed-program)
  (loop :with code := (parsed-program-executable-code parsed-program)
        :for i :from (1- (length code)) :downto 0
        :for exiting-rewiring := (nth-value 1 (instruction-rewirings (aref code i)))
        :when (not (null exiting-rewiring))
          :return (rewiring-l2p exiting-rewiring)))


;;;;;;;;;;;;;;;;;;;;;;;; Pseudo-Instructions ;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jump-target ()
  ((label :initarg :label
          :reader jump-target-label))
  (:documentation "A target which can be jumped to. Corresponds to the LABEL directive."))

(declaim (inline jump-target-p))
(defun jump-target-p (x)
  "Is X a jump target?"
  (typep x 'jump-target))

(defclass include ()
  ((pathname :initarg :pathname
             :reader include-pathname))
  (:documentation "A directive to include another file in a Quil file."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Gate Definitions

;;; Note: In the future this might be expanded to include other objects.
(deftype lexical-context () '(or null token))

;; lexical-context is a generic function which should be implemented
;; for new token types. Note that this generic has methods defined
;; earlier than this point, for example in classical-memory.lisp.
(defmethod lexical-context (instr)
  nil)

(defclass gate-definition ()
  ((name :initarg :name
         :reader gate-definition-name)
   (entries :initarg :entries
            :reader gate-definition-entries)
   ;; This is a private slot and is here to increase the performance
   ;; of many repeated calculations of a GATE object. See the function
   ;; GATE-DEFINITION-TO-GATE.
   (cached-gate :initform nil
                :accessor %gate-definition-cached-gate)
   (context :initarg :context
            :type lexical-context
            :accessor lexical-context))
  (:metaclass abstract-class)
  (:documentation "A representation of a raw, user-specified gate definition. This is *not* supposed to be an executable representation."))

(defgeneric gate-definition-qubits-needed (gate)
  (:documentation "The number of qubits needed by GATE."))

(defclass matrix-gate-definition (gate-definition)
  ((entries :initarg :entries
            :reader gate-definition-entries))
  (:metaclass abstract-class)
  (:documentation "A representation of a raw, user-specified gate definition. This is *not* supposed to be an executable representation."))

(defmethod gate-definition-qubits-needed ((gate matrix-gate-definition))
  (ilog2 (isqrt (length (gate-definition-entries gate)))))

(defclass static-gate-definition (matrix-gate-definition)
  ()
  (:documentation "A gate definition that has no parameters."))

(defclass parameterized-gate-definition (matrix-gate-definition)
  ((parameters :initarg :parameters
               :reader gate-definition-parameters
               :documentation "A list of symbol parameter names."))
  (:documentation "A gate definition that has named parameters."))

(defclass permutation-gate-definition (gate-definition)
  ((permutation :initarg :permutation
                :reader permutation-gate-definition-permutation))
  (:documentation "A gate definition whose entries can be represented by a permutation of natural numbers."))

(defclass sequence-gate-definition (gate-definition)
  ((sequence  :initarg :sequence
              :reader sequence-gate-definition-sequence
              :documentation "List of gate operations describing the sequence")
   (arguments :initarg :arguments
              :reader sequence-gate-definition-arguments
              :documentation "List of arguments that appear in the sequence")
   (parameters :initarg :parameters
               :reader sequence-gate-definition-parameters
               :documentation "List of parameters to be integrated into the sequence"))
  (:documentation "Represents a gate definition as a sequence of other gates."))

(defclass exp-pauli-sum-gate-definition (gate-definition)
  ((terms :initarg :terms
          :reader exp-pauli-sum-gate-definition-terms
          :documentation "List of PAULI-TERMs comprising the sum.")
   (parameters :initarg :parameters
               :reader exp-pauli-sum-gate-definition-parameters
               :documentation "Ordered list of parameter names to be supplied to the definition, which can appear in arithmetical expressions weighting the definition's Pauli terms.")
   (arguments :initarg :arguments
              :reader exp-pauli-sum-gate-definition-arguments
              :documentation "Ordered list of formal arguments appearing in the definition's Pauli terms."))
  (:documentation "Represents a gate definition as the exponential of a weighted sum of Pauli matrices."))

(defstruct (pauli-term)
  "Records a word of Pauli operators, together with an ordered list of qubit formals on which they act, as well as a scalar prefix.  This is part of the internal representation of a EXP-PAULI-SUM-GATE-DEFINITION and probably shouldn't be instantiated otherwise.

This replicates some of the behavior of CL-QUIL/CLIFFORD::PAULI, but it extends it slightly: a Clifford Pauli is constrained to carry a phase which is a fourth root of unity, but the phase of a PAULI-TERM can be arbitrary (indeed, even a delayed expression).  The reader looking for an embodiment of Pauli words is better off using that data structure without CAREFUL CONSIDERATION."
  pauli-word
  prefactor
  arguments)

(defmethod copy-instance ((term pauli-term))
  (make-pauli-term :pauli-word (pauli-term-pauli-word term)
                   :prefactor (pauli-term-prefactor term)
                   :arguments (pauli-term-arguments term)))

(defmethod gate-definition-qubits-needed ((gate exp-pauli-sum-gate-definition))
  (length (exp-pauli-sum-gate-definition-arguments gate)))

(defmethod gate-definition-qubits-needed ((gate sequence-gate-definition))
  (length (sequence-gate-definition-arguments gate)))

(defmethod gate-definition-qubits-needed ((gate permutation-gate-definition))
  (ilog2 (length (permutation-gate-definition-permutation gate))))

(defun permutation-from-gate-entries (entries)
  "Create the permutation (list of natural numbers) that represents the input matrix ENTRIES. Return nil if ENTRIES cannot be represented as a permutation."
  (let* ((n (isqrt (length entries)))
         (perm (make-list n)))
    (dotimes (i n perm)
      (let ((found-one nil))
        (dotimes (j n)
          (case (pop entries)
            ((0.0d0) nil)
            ((1.0d0) (cond
                       ((or found-one (nth j perm))
                        (return-from permutation-from-gate-entries nil))
                       (t
                        (setf (nth j perm) i)
                        (setf found-one t))))
            (otherwise (return-from permutation-from-gate-entries nil))))
        (unless found-one
          (return-from permutation-from-gate-entries nil))))))

(defun make-gate-definition (name parameters entries &key context)
  "Make a static or parameterized gate definition instance, depending on the existence of PARAMETERS."
  (check-type name string)
  (check-type parameters symbol-list)
  (if parameters
      (make-instance 'parameterized-gate-definition
                     :name name
                     :parameters parameters
                     :entries entries
                     :context context)
      (a:if-let ((perm (permutation-from-gate-entries entries)))
        (make-instance 'permutation-gate-definition
                       :name name
                       :permutation perm
                       :context context)
        (make-instance 'static-gate-definition
                       :name name
                       :entries entries
                       :context context))))

;;; Circuit Definitions

(defclass circuit-definition ()
  ((name :initarg :name
         :reader circuit-definition-name)
   (parameters :initarg :parameters
               :reader circuit-definition-parameters)
   (arguments :initarg :arguments
              :reader circuit-definition-arguments)
   (body :initarg :body
         :reader circuit-definition-body)
   (context :initarg :context
            :type lexical-context
            :accessor lexical-context)))

(defun make-circuit-definition (name params args body &key context)
  (check-type name string)
  (assert (every #'is-param params))
  (assert (every #'is-formal args))
  (make-instance 'circuit-definition
                 :name name
                 :parameters params
                 :arguments args
                 :body body
                 :context context))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Instructions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Instructions and their protocol.

(defclass instruction ()
  ()
  (:documentation "Abstract class representing an executable instruction.")
  (:metaclass abstract-class))

(defclass classical-instruction (instruction)
  ()
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
  ;; The singleton-class is disabled rather than removed here (and elsewhere) as a reminder that
  ;; this is a quick fix. Ideally, we'd like to find a way to preserve the singleton nature of these
  ;; AST classes, but still work with rewiring comments and the guts of logical-scheduler. See
  ;; https://github.com/rigetti/quilc/issues/270 for more context.
  #+#:appleby-sufficiently-classy
  (:metaclass singleton-class))

(defmethod arguments ((instruction no-operation)) #())
(defmethod mnemonic  ((instruction no-operation)) (values "NOP" 'no-operation))

(defclass pragma (instruction)
  ((words :initarg :words
          :reader pragma-words
          :documentation "A list of strings derived from identifiers or numbers. It must start with a string.")
   (freeform-string :initarg :freeform-string
                    :reader pragma-freeform-string
                    :documentation "A freeform string."))
  (:default-initargs :words nil :freeform-string nil)
  (:documentation "A compiler/interpreter pragma. Usually reserved for non-standard extensions to Quil that don't affect its interpretation."))

(defmethod mnemonic ((inst pragma)) (values "PRAGMA" (class-name (class-of inst))))

(defun make-pragma (words &optional freeform)
  "Return a pragma, possibly specialized based on WORDS and FREEFORM."
  (assert (and (listp words)
               (not (endp words))))
  (assert (stringp (first words))
          ((first words)))
  (assert (every (a:disjoin #'stringp #'integerp) words)
          (words))
  (check-type freeform (or null string))
  (specialize-pragma
   (make-instance 'pragma :words words :freeform-string freeform)))

(defclass halt (instruction)
  ()
  (:documentation "An instruction to immediately halt all execution.")
  #+#:appleby-sufficiently-classy
  (:metaclass singleton-class))

(defun haltp (x)
  "Is X a HALT instruction?"
  (typep x 'halt))

(defmethod arguments ((instruction halt)) #())
(defmethod mnemonic  ((instruction halt)) (values "HALT" 'halt))

(defclass reset (instruction)
  ()
  (:documentation "An instruction to reset all qubits to the |0>-state.")
  #+#:appleby-sufficiently-classy
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
  #+#:appleby-sufficiently-classy
  (:metaclass singleton-class))

(defmethod arguments ((instruction wait)) #())
(defmethod mnemonic  ((instruction wait)) (values "WAIT" 'wait))


;;; Classical Instructions

(defclass unary-classical-instruction (classical-instruction)
  ((target :initarg :target
           :reader classical-target))
  (:documentation "An instruction representing a unary classical function.")
  (:metaclass abstract-class))

(defmethod arguments ((inst unary-classical-instruction))
  (vector (classical-target inst)))

(defclass binary-classical-instruction (classical-instruction)
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

(defclass trinary-classical-instruction (classical-instruction)
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
    (a:format-symbol (symbol-package name)
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
                ;; TODO This error is unhelpful.
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
      (a:parse-body body :documentation t)
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
  "A family of recipes for attaching meaning to a Quil operator, typically either by table look-up or by some prescribed mathematical combination."
  (named-operator      string)
  (controlled-operator operator-description)
  ;; Note that reduction of consecutive dagger operators is not
  ;; performed here. Use the alternative constructor
  ;; INVOLUTIVE-DAGGER-OPERATOR.
  (dagger-operator     operator-description)
  (forked-operator     operator-description))

(setf (documentation 'named-operator 'function)
      "Describes a gate using a string name, which is later looked up in a table of DEFGATE definitions.  In Quil code, this corresponds to a raw gate name, like ISWAP.")
(setf (documentation 'controlled-operator 'function)
      "Describes a gate as the direct sum of the identity gate (i.e., \"do nothing when the control bit is low\") and some other specified gate G (i.e., \"do G when the control bit is high\").  In Quil code, this corresponds to the descriptor CONTROLLED.")
(setf (documentation 'dagger-operator 'function)
      "Describes a gate as the inverse to some other gate.  In Quil code, this corresponds to the descriptor DAGGER.")
(setf (documentation 'forked-operator 'function)
      "Describes a gate as the direct sum of two instances of some other specified gate G with input parameters either p_low or p_high, conditioned on whether a control bit is low or high.  In Quil code, this corresponds to the descriptor FORKED.")

(defun involutive-dagger-operator (od)
  "Instantiate a dagger operator on the operator description OD and reduce consecutive dagger operators.

For example, `DAGGER DAGGER H 0` should produce `H 0`."
  (adt:match operator-description od
    ((dagger-operator inner-od) inner-od)
    (_ (dagger-operator od))))

(defun operator-description-name (od)
  (adt:match operator-description od
    ((named-operator name)   name)
    (_ (error "The application doesn't have a canonical name."))))

(defun operator-description= (od1 od2)
  "Check whether two operator descriptions have the same structure and the same names."
  (and (equalp od1 od2)                 ; case insensitive
       (string= (operator-description-root-name od1)
                (operator-description-root-name od2))))

(defun operator-description-hash (od)
  "Hash function for OPERATOR-DESCRIPTIONs."
  ;; If we have a convenient way of combining hashed values,
  ;; e.g. with SB-INT:MIX, use this explicitly. Otherwise,
  ;; fall back to hashing the string representation.
  #+sbcl
  (adt:match operator-description od
    ((named-operator name) (sxhash name))
    ((controlled-operator inner-od)
     (sb-int:mix
      (sxhash 'controlled)
      (operator-description-hash inner-od)))
    ((dagger-operator inner-od)
     (sb-int:mix
      (sxhash 'dagger)
      (operator-description-hash inner-od)))
    ((forked-operator inner-od)
     (sb-int:mix
      (sxhash 'forked)
      (operator-description-hash inner-od))))
  #-sbcl
  (sxhash (operator-description-string od)))

(defun operator-description-root-name (od)
  "The \"root name\" that the operator description represents. This is usually going to name a gate that said description modifies."
  (adt:match operator-description od
    ((named-operator name)   name)
    ((controlled-operator o) (operator-description-root-name o))
    ((dagger-operator o)     (operator-description-root-name o))
    ((forked-operator o)     (operator-description-root-name o))))

(defun operator-description-additional-qubits (od)
  "The number of additional qubits incurred by this operator description (e.g., CONTROLLED adds one qubit)."
  (adt:match operator-description od
    ((named-operator _)   0)
    ((controlled-operator o) (1+ (operator-description-additional-qubits o)))
    ((dagger-operator o)     (operator-description-additional-qubits o))
    ((forked-operator o)     (1+ (operator-description-additional-qubits o)))))

(defun print-operator-description (od stream)
  "Print the operator description OD to STREAM."
  (adt:match operator-description od
    ((named-operator name) (write-string name stream))
    ((controlled-operator o) (write-string "CONTROLLED " stream)
                             (print-operator-description o stream))
    ((dagger-operator o) (write-string "DAGGER " stream)
                         (print-operator-description o stream))
    ((forked-operator o) (write-string "FORKED " stream)
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

(defun simple-controlled-operator-p (od)
  "Is the operator description OD a simple controlled application?

An operator is considered to be a simple controlled application if it
consists of a CONTROLLED-OPERATOR acting on a NAMED-OPERATOR."
  (adt:match operator-description od
    ((controlled-operator cod) (plain-operator-p cod))
    (_ nil)))

(defclass application (instruction)
  ((operator :initarg :operator
             :accessor application-operator
             :type operator-description)
   (parameters :initarg :parameters
               :accessor application-parameters)
   (arguments :initarg :arguments
              :reader arguments
              :accessor application-arguments))
  (:default-initargs
   :parameters nil
   :arguments nil)
  (:documentation "Superclass to all application-like instructions.")
  (:metaclass abstract-class))

(defun application-operator-name (app)
  (operator-description-name (application-operator app)))

(defun application-operator-root-name (app)
  (operator-description-root-name (application-operator app)))

(defclass unresolved-application (application)
  ()
  (:documentation "Represents an application that hasn't yet been resolved. Possibilities:

    * Application is a gate application.

    * Application is a circuit application.

    * Application is an invalid application.

Determining this requires the context of the surrounding program."))

(declaim (inline gate-application-p))
(defun gate-application-p (x)
  "Is X a gate application?"
  (typep x 'gate-application))

(declaim (inline static-gate-application-p))
(defun static-gate-application-p (x)
  "Is X a gate application with constant parameters?"
  (and (gate-application-p x)
       (every #'is-constant (application-parameters x))))

(defclass gate-application (application)
  ((name-resolution :initarg :name-resolution
                    :reader gate-application-resolution
                    ;; We do this type check in INITIALIZE-INSTANCE.
                    :type gate-definition
                    :documentation "The resolved definition of a gate. This may be some kind of GATE-DEFINITION, or it may be a gate object directly. In general, this resolution is ultimately a way to interpret what the root name of the APPLICATION-OPERATOR means. This is also the object from which one can produce more optimized representations of the gate application.

This definition does *not* incorporate the operator description (i.e., any operator modifiers like CONTROLLED).

If this slot is not supplied, then the gate is considered *anonymous*. If this is the case, then the GATE slot must be supplied.")
   ;; N.B. See the generic function GATE-APPLICATION-GATE as well.
   (gate :initarg :gate
         :initform nil
         :reader %get-gate-application-gate
         :writer %set-gate-application-gate
         :documentation "The actual gate object that is being applied. N.B. After applications are resolved, one can always look at the definition of a gate via GATE-APPLICATION-RESOLUTION. But this slot is reserved for actual *execution*, which may depend on the execution backend and how one wishes to optimize.

N.B. This slot should not be accessed directly! Consider using GATE-APPLICATION-GATE, or, if you really know what you're doing, %SET-GATE-APPLICATION-GATE."))
  (:documentation "An instruction representing an application of a known gate."))

(defmethod copy-instance ((application gate-application))
  (if (slot-boundp application 'name-resolution)
      (make-instance 'gate-application
                     :operator (copy-instance (application-operator application))
                     :parameters (mapcar #'copy-instance
                                         (application-parameters application))
                     :arguments (mapcar #'copy-instance
                                        (application-arguments application))
                     :name-resolution (gate-application-resolution application))
      (make-instance 'gate-application
                     :operator (copy-instance (application-operator application))
                     :parameters (mapcar #'copy-instance
                                         (application-parameters application))
                     :arguments (mapcar #'copy-instance
                                        (application-arguments application))
                     :gate (copy-instance (gate-application-gate application)))))

(defgeneric gate-application-gate (app)
  ;; See the actual definition of this in gates.lisp.
  (:documentation "Return a gate-like object represented in the application APP.")
  (:method :around ((app gate-application))
    (a:if-let ((gate (%get-gate-application-gate app)))
      gate
      (%set-gate-application-gate (call-next-method) app))))

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

(declaim (type (vector rational) **reasonable-rationals**))
(global-vars:define-global-var **reasonable-rationals**
    (coerce (nconc (a:iota 10 :start 1)
                   (delete-duplicates
                    (loop :for denom :in '(2 3 4 6 8 16)
                          :nconc (loop :for numer :from 1 :below (* 2 denom)
                                       :when (/= numer denom)
                                         :collect (/ numer denom)))
                    :from-end t))
            'vector)
  "The list of RATIONAL multiples of PI that are checked by FORMAT-REAL when *PRINT-FRACTIONAL-RADIANS* is T.")

(defparameter *pi-literal* "pi"
  "String representation of a PI literal.")

(defun format-real (r stream)
  "Print the real number R nicely to the stream STREAM."
  (check-type r real)
  (check-type stream stream)
  (flet ((%format (r)
           (if (floatp r)
               (let ((*read-default-float-format* (type-of r)))
                 (format stream "~F" r))
               (format stream "~F" r))))
    (cond
      ((or (double~ (abs r) 0) (not *print-fractional-radians*))
       (%format r))
      (t
       (loop :with r-abs := (abs r)
             :for rr :across **reasonable-rationals**
             :for numer := (numerator rr)
             :for denom := (denominator rr)
             :when (double~ r-abs (/ (* pi numer) denom)) :do
               ;; Pretty-print "reasonable" integer and fractional multiples of pi.
               (format stream "~:[~;-~]~:[~D*~;~*~]~A~:[/~D~;~*~]"
                       (minusp r) (= 1 numer) numer *pi-literal* (= 1 denom) denom)
               (return-from format-real))
       ;; If we cannot find a nice fraction of pi, just print the real number.
       (%format r)))))

(defun real-fmt (stream r &optional colon-modifier at-modifier)
  "Like the function format-real, but is compatible with format strings using the ~/.../ directive.
For example,
    (format t \"the number was ~/cl-quil:real-fmt/\" r)"
  (declare (ignore colon-modifier at-modifier))
  (format-real r stream))

(defun format-complex (z stream)
  "Print the real or complex number Z nicely to the stream STREAM."
  (check-type z number)
  (check-type stream stream)
  (cond
    ((zerop z)
     (format stream "0.0"))
    ((realp z)
     (format-real z stream))
    ((complexp z)
     (cond
       ((zerop (imagpart z))
        (format-real (realpart z) stream))
       (*print-polar-form*
        (let* ((z-abs (abs z))
               (*read-default-float-format* (type-of z-abs)))
          (format stream "~F∠" z-abs))
        (format-real (mod (phase z) (* 2 pi)) stream))
       (t
        (format stream "~A~A~A"
                (if (zerop (realpart z))
                    ""
                    (let ((*read-default-float-format* (type-of (realpart z))))
                      (format nil "~F" (realpart z))))
                (if (and (plusp (imagpart z))
                         (not (zerop (realpart z))))
                    (format nil "+")
                    "")
                (let ((*read-default-float-format* (type-of (imagpart z))))
                  (format nil "~Fi" (imagpart z)))))))))

(defun complex-fmt (stream z &optional colon-modifier at-modifier)
  "Like the function format-complex, but is compatible with format strings using the ~/.../ directive.
For example,
    (format t \"the number was ~/cl-quil:complex-fmt/\" z)"
  (declare (ignore colon-modifier at-modifier))
  (format-complex z stream))

(defun print-instruction (instr &optional (stream *standard-output*))
  "Print the Qul instruction INSTR to the stream STREAM.

If STREAM is NIL, then it will be printed to a string."
  (print-instruction-generic instr stream))

(defun instruction-fmt (stream instr &optional colon-modifier at-modifier)
  "Like the function PRINT-INSTRUCTION, but is compatible with format strings using the ~/.../ directive.

For example,

    (format t \"the instruction was ~/cl-quil:instruction-fmt/\" instr)
"
  (declare (ignore colon-modifier at-modifier))
  (print-instruction instr stream))

(defun print-instruction-to-string (instr)
  "Print the Quil instruction INSTR to a string."
  (print-instruction instr nil))

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
    (format stream "~A[~A]"
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
      (quil-bit (format stream "~A" (constant-value thing)))
      (quil-octet (format stream "~A" (constant-value thing)))
      (quil-integer (format stream "~A" (constant-value thing)))
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
                     (format stream "~A[~A]" (second expr) (third expr)))
                    ((= (length expr) 3)
                     (format stream "(~A~A~A)"
                             (print-delayed-expression (second expr) nil)
                             (lisp-symbol->quil-infix-operator (first expr))
                             (print-delayed-expression (third expr) nil)))
                    ((= (length expr) 2)
                     (format stream "~A(~A)"
                             (lisp-symbol->quil-function-or-prefix-operator (first expr))
                             (print-delayed-expression (second expr) nil)))))
                 (number
                  (format stream "(~/cl-quil:complex-fmt/)" expr))
                 (symbol
                  (format stream "%~A" expr))
                 (otherwise
                  (print-instruction expr stream)))))
      (print-delayed-expression (delayed-expression-expression thing) stream)))

  ;; Actual instructions
  (:method ((instr halt) (stream stream))
    (format stream "HALT"))

  (:method ((instr reset) (stream stream))
    (format stream "RESET"))

  (:method ((instr reset-qubit) stream)
    (format stream "RESET ~/cl-quil:instruction-fmt/" (reset-qubit-target instr)))

  (:method ((instr wait) (stream stream))
    (format stream "WAIT"))

  (:method ((instr no-operation) (stream stream))
    (format stream "NOP"))

  (:method ((instr classical-instruction) (stream stream))
    (format stream "~A" (mnemonic instr))
    (loop :for arg :across (arguments instr)
          :do (format stream " ~/cl-quil:instruction-fmt/" arg)))

  (:method ((instr pragma) (stream stream))
    (format stream "PRAGMA ~{~A~^ ~}~@[ ~S~]"
            (pragma-words instr)
            (pragma-freeform-string instr)))

  (:method ((instr jump-target) (stream stream))
    (format stream "LABEL ~/cl-quil:instruction-fmt/"
            (jump-target-label instr)))

  (:method ((instr jump) (stream stream))
    (let ((l (jump-label instr)))
      (cond
        ((integerp l)
         (format stream "JUMP {absolute address ~D}" l))
        (t
         (format stream "JUMP ~/cl-quil:instruction-fmt/"
                 (jump-label instr))))))

  (:method ((instr jump-when) (stream stream))
    (let ((l (jump-label instr)))
      (cond
        ((integerp l)
         (format stream "JUMP-WHEN {absolute address ~D} ~/cl-quil:instruction-fmt/"
                 l
                 (conditional-jump-address instr)))
        (t
         (format stream "JUMP-WHEN ~/cl-quil:instruction-fmt/ ~/cl-quil:instruction-fmt/"
                 (jump-label instr)
                 (conditional-jump-address instr))))))

  (:method ((instr jump-unless) (stream stream))
    (let ((l (jump-label instr)))
      (cond
        ((integerp l)
         (format stream "JUMP-UNLESS {absolute address ~D} ~/cl-quil:instruction-fmt/"
                 l
                 (conditional-jump-address instr)))
        (t
         (format stream "JUMP-UNLESS ~/cl-quil:instruction-fmt/ ~/cl-quil:instruction-fmt/"
                 (jump-label instr)
                 (conditional-jump-address instr))))))

  (:method ((instr measure) (stream stream))
    (format stream "MEASURE ~/cl-quil:instruction-fmt/ ~/cl-quil:instruction-fmt/"
            (measurement-qubit instr)
            (measure-address instr)))

  (:method ((instr measure-discard) (stream stream))
    (format stream "MEASURE ~/cl-quil:instruction-fmt/"
            (measurement-qubit instr)))

  (:method ((instr application) (stream stream))
    (print-operator-description (application-operator instr) stream)
    (format stream "~@[(~{~/cl-quil:instruction-fmt/~^, ~})~]~{ ~/cl-quil:instruction-fmt/~}"
            (application-parameters instr)
            (application-arguments instr)))

  ;; The following are not actually instructions, but who cares.

  (:method ((defn memory-descriptor) (stream stream))
    (format stream "DECLARE ~A ~A"
            (memory-descriptor-name defn)
            (quil-type-string (memory-descriptor-type defn)))
    (format stream "~[[0]~;~:;[~:*~A]~]" (memory-descriptor-length defn))
    (when (memory-descriptor-sharing-parent defn)
      (format stream " SHARING ~A"
              (memory-descriptor-sharing-parent defn))
      (a:when-let (x (memory-descriptor-sharing-offset-alist defn))
        (format stream " OFFSET")
        (loop :for (type . count) :in x
              :do (format stream " ~A ~A" count (quil-type-string type))))))

  (:method ((gate matrix-gate-definition) (stream stream))
    (let ((gate-size (isqrt (length (gate-definition-entries gate)))))
      (format stream "DEFGATE ~A~@[(~{%~A~^, ~})~]:~%"
              (gate-definition-name gate)
              (if (typep gate 'static-gate-definition)
                  nil
                  (gate-definition-parameters gate)))
      (dotimes (i gate-size)
        (format stream "    ~{~A~^, ~}~%"
                (mapcar (lambda (z)
                          (with-output-to-string (s)
                            (etypecase z
                              (number
                               (format-complex z s))
                              ((or list symbol)
                               (print-instruction (make-delayed-expression nil nil z) s)))))
                        (subseq (gate-definition-entries gate)
                                (* i gate-size)
                                (* (1+ i) gate-size)))))))

  (:method ((gate permutation-gate-definition) (stream stream))
    (format stream "DEFGATE ~A AS PERMUTATION:~%    ~{~D~^, ~}~%"
            (gate-definition-name gate)
            (permutation-gate-definition-permutation gate)))

  (:method ((defn circuit-definition) (stream stream))
    (format stream "DEFCIRCUIT ~A"
            (circuit-definition-name defn))
    (unless (endp (circuit-definition-parameters defn))
      (format stream "(~{~/cl-quil:instruction-fmt/~^, ~})"
              (circuit-definition-parameters defn)))
    (unless (endp (circuit-definition-arguments defn))
      (format stream "~{ ~/cl-quil:instruction-fmt/~}"
              (circuit-definition-arguments defn)))
    (format stream ":~%")
    (print-instruction-sequence (circuit-definition-body defn)
                                :stream stream
                                :prefix "    "))
  
  (:method ((gate sequence-gate-definition) (stream stream))
    (format stream "DEFGATE ~A~@[(~{%~A~^, ~})~]~{ ~A~} AS SEQUENCE:~%"
            (gate-definition-name gate)
            (mapcar #'param-name (sequence-gate-definition-parameters gate))
            (mapcar #'formal-name (sequence-gate-definition-arguments gate)))
    (dolist (operation (sequence-gate-definition-sequence gate))
      (format stream "    ~/cl-quil:instruction-fmt/~%" operation)))
  
  (:method ((gate exp-pauli-sum-gate-definition) (stream stream))
    (format stream "DEFGATE ~A~@[(~{%~A~^, ~})~]~{ ~A~} AS PAULI-SUM:~%"
            (gate-definition-name gate)
            (mapcar #'string (exp-pauli-sum-gate-definition-parameters gate))
            (mapcar #'formal-name (exp-pauli-sum-gate-definition-arguments gate)))
    (dolist (pauli-term (exp-pauli-sum-gate-definition-terms gate))
      (with-slots (pauli-word prefactor arguments) pauli-term
        (format stream "    ~a(" pauli-word)
        (typecase prefactor
          (number
           (format stream "~a" prefactor))
          ((or symbol cons)
           (print-instruction (make-delayed-expression nil nil prefactor) stream)))
        (format stream ")")
        (dolist (arg arguments)
          (format stream " ")
          (print-instruction arg stream))
        (terpri stream)))))

(defmethod print-object ((object instruction) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (print-instruction object stream)))


;;;;;;;;;;;;;;;;;;;;;; Program Representations ;;;;;;;;;;;;;;;;;;;;;;;

;;; There are two primary representations used here. The most common is the
;;; PARSED-PROGRAM representation, which is produced by PARSE-QUIL. In almost
;;; all cases, the instructions in a PARSED-PROGRAM are resolved (e.g.
;;; applications are associated with the corresponding definition of the gate or
;;; circuit which is to be applied, cf. RESOLVE-OBJECTS for more info). The one
;;; exception to this is in the object resolution pass itself, where it is
;;; convenient to work with PARSED-PROGRAM objects.
;;;
;;; The other representation, used only by the frontend, is "raw quil", which is
;;; simply a list of AST objects.

(defclass parsed-program (transformable)
  ((gate-definitions :initarg :gate-definitions
                     :accessor parsed-program-gate-definitions
                     :type list
                     :documentation "The gate definitions introduced by DEFGATE.")
   (circuit-definitions :initarg :circuit-definitions
                        :accessor parsed-program-circuit-definitions
                        :type list
                        :documentation "The circuit definitions introduced by DEFCIRCUIT.")
   (memory-definitions :initarg :memory-definitions
                       :accessor parsed-program-memory-definitions
                       :type list
                       :documentation "The memory definitions introduced by DECLARE.")
   (executable-program :initarg :executable-code
                       :accessor parsed-program-executable-code
                       :type (vector instruction)
                       :documentation "A vector of executable Quil instructions."))
  (:default-initargs
   :gate-definitions '()
   :circuit-definitions '()
   :memory-definitions '()
   :executable-code #())
  (:documentation "A representation of a parsed Quil program, in which instructions have been duly sorted into their various categories (e.g. definitions vs executable code), and internal references have been resolved."))

(defmethod copy-instance ((parsed-program parsed-program))
  (let ((pp (make-instance 'parsed-program)))
    (setf (parsed-program-gate-definitions pp)
          (map 'list #'copy-instance
               (parsed-program-gate-definitions parsed-program)))
    (setf (parsed-program-circuit-definitions pp)
          (map 'list #'copy-instance
               (parsed-program-circuit-definitions parsed-program)))
    (setf (parsed-program-memory-definitions pp)
          (map 'list #'copy-instance
               (parsed-program-memory-definitions parsed-program)))
    (setf (parsed-program-executable-code pp)
          (map 'vector #'copy-instance
               (parsed-program-executable-code parsed-program)))
    pp))

(defvar *print-parsed-program-text* nil
  "When T, PRINT-OBJECT on a PARSED-PROGRAM will include the program text. Otherwise, only the number of instructions is printed.")

(defun %print-indented-string (string stream &key (indentation "    "))
  (loop :with last := (1- (length string))
        :for c :across string
        :for i :upto last
        :when (zerop i)
          :do (write-string indentation stream)
        :do (write-char c stream)
        :when (and (eql c #\Newline) (/= i last))
          :do (write-string indentation stream)))

(defmethod print-object ((parsed-program parsed-program) stream)
  (let ((*print-circle* nil))
    (print-unreadable-object (parsed-program stream :type t :identity t)
      (format stream "of ~D instruction~:P" (length (parsed-program-executable-code parsed-program)))
      (when *print-parsed-program-text*
        (terpri stream)
        (%print-indented-string (with-output-to-string (s)
                                  (print-parsed-program parsed-program s))
                                stream)))))

;; These NTH-INSTR functions prioritize caller convenience and error checking over speed. They could
;; possibly be sped up by doing away with type checking, making %NTH-INSTR into a macro that takes
;; an &BODY, rather than a CONTINUATION, etc. If you need speed, you're probably better served by
;; calling AREF or CL-QUIL::VNTH on the PARSED-PROGRAM-EXECUTABLE-CODE vector directly.

(defun %nth-instr (index parsed-program from-end continuation)
  (check-type index integer)
  (check-type parsed-program parsed-program)
  (let* ((code (parsed-program-executable-code parsed-program))
         (length (length code)))
    (funcall continuation code (if from-end (- length index 1) index))))

(defun nth-instr (index parsed-program &key from-end)
  "Return the INSTRUCTION at position INDEX in PARSED-PROGRAM's EXECUTABLE-PROGRAM vector.

If FROM-END is non-NIL, then INDEX is relative to the end of the instruction sequence, rather than the start. In either case, INDEX is zero-based and must fall in the range [0, length) where length is (LENGTH (PARSED-PROGRAM-EXECUTABLE-CODE PARSED-PROGRAM)).

Examples:

;; Get the first instruction of PP.
(nth-instr 0 pp)

;; Also the first instruction
(nth-instr (length (parsed-program-executable-code pp)) pp :from-end t)

;; Get the last instruction.
(nth-inst 0 pp :from-end t)"
  (%nth-instr index parsed-program from-end
              (lambda (code normalized-index)
                (aref code normalized-index))))

(defun (setf nth-instr) (value index parsed-program &key from-end)
  "Set the INSTRUCTION at position INDEX in PARSED-PROGRAM's EXECUTABLE-PROGRAM vector."
  (%nth-instr index parsed-program from-end
              (lambda (code normalized-index)
                (setf (aref code normalized-index) value))))

(defun print-instruction-sequence (seq
                                   &key
                                     (stream *standard-output*)
                                     (prefix ""))
  (let ((*print-pretty* nil))
    (flet ((print-one-line (instr)
             (write-string prefix stream)
             (print-instruction instr stream)
             (a:when-let ((c (comment instr)))
               (format stream "~40T# ~A" c))
             (terpri stream)))
      (map nil #'print-one-line seq))))

(defgeneric print-parsed-program-generic (parsed-program stream)
  (:documentation "Print the program PARSED-PROGRAM nicely to the stream STREAM.")
  (:method ((pp parsed-program) (stream stream))
    (flet ((print-definitions (defns)
             (dolist (defn defns)
               (print-instruction defn stream)
               (terpri stream))))

      (print-definitions (parsed-program-memory-definitions pp))
      ;; instructions and single-line definitions (e.g. DECLARE) do not introduce newlines
      ;; so here we add one if needed
      (unless (endp (parsed-program-memory-definitions pp))
        (terpri stream))
      (print-definitions (parsed-program-gate-definitions pp))
      (print-definitions (parsed-program-circuit-definitions pp))

      (print-instruction-sequence (parsed-program-executable-code pp) :stream stream))))

(defun print-parsed-program (parsed-program &optional (stream *standard-output*))
  "Print the text of a PARSED-PROGRAM to the stream STREAM."
  (assert (output-stream-p stream) (stream) "Provided something that's not an output ~
                                             stream to the STREAM argument of ~
                                             PRINT-PARSED-PROGRAM.")
  (print-parsed-program-generic parsed-program stream))
