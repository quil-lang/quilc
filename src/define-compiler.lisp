;;;; define-compiler.lisp
;;;;
;;;; Authors: Eric Peterson, Zach Beane
;;;;
;;;; The bulk of this file concerns itself with syntactic sugar for
;;;; destructuring gate objects and otherwise extracting information from them.
;;;; The main use of these routines is to support `define-compiler`, which does
;;;; a lot of clever destructuring of its arguments.

(in-package #:cl-quil)


;;; here are two utility macros for building compiler bodies. these are typically
;;; **not** used by the average programmer, but they are used in the expansion
;;; of define-compiler.

(defmacro give-up-compilation-unless (test &body body)
  "If TEST passes, proceed to execute BODY (and return the value of the final expression). If TEST fails, call GIVE-UP-COMPILATION."
  `(cond
     (,test
      ,@body)
     (t
      (give-up-compilation))))

(defmacro unpack-wf (instr context (psi qubits) &body body)
  "Extracts from CONTEXT the wavefunction component related to the instruction INSTR.  Upon success, bind PSI to the wavefunction contents, bind QUBITS to the qubit ordering convention of PSI, execute BODY, and return the result of the final expression.  Upon failure, call GIVE-UP-COMPILATION."
  `(cond
     ((null ,context)
      (give-up-compilation))
     (t
      (destructuring-bind (,psi ,qubits)
          (aqvm-extract-state (compilation-context-aqvm ,context)
                              (mapcar #'qubit-index (application-arguments ,instr)))
        (give-up-compilation-unless
            (not (eql ':not-simulated ,psi))
          ,@body)))))


;;; this data structure specifies a pattern-matching language which is used
;;; (1) to check whether an instruction satisfies whatever necessary
;;; preconditions a given compiler needs to operator and (2) to destructure such
;;; an input so that the compiler definition can work with the instruction internals.

(defstruct (compiler-binding (:constructor nil))
  "Represents a generic compiler argument binding.

NAME: Symbol to be bound in the compiler definition.
OPTIONS: plist of options governing applicability of the compiler binding."
  (name    nil :read-only t :type symbol)
  (options nil :read-only t :type list))

(defstruct (wildcard-binding (:include compiler-binding))
  "Represents a binding that matches on any gate application.")

(defstruct (gate-binding (:include compiler-binding))
  "Represents a compiler argument binding that destructures a gate application."
  (operator   nil :read-only t :type (or symbol operator-description))
  (parameters nil :read-only t :type (or symbol list))
  (arguments  nil :read-only t :type list))

(defstruct (measure-binding (:include compiler-binding))
  "Represents a destructuring compiler argument binding for a MEASURE instruction."
  (qubit  nil :type (or symbol unsigned-byte))
  (target nil :type (or symbol memory-ref)))

;;; here are various utility routines related to the pattern-matching bindings:
;;; testing whether one binding is more general than other, extracting a binding
;;; which minimally matches a given instruction, ... .

(defun copy-binding (binding &rest initargs-override)
  (check-type binding compiler-binding)
  (let ((initargs (list ':name (compiler-binding-name binding)
                        ':options (compiler-binding-options binding))))
    (etypecase binding
      (wildcard-binding
       (apply #'make-wildcard-binding (append initargs-override initargs)))
      (gate-binding
       (setf initargs (list* ':operator (gate-binding-operator binding)
                             ':parameters (gate-binding-parameters binding)
                             ':arguments (gate-binding-arguments binding)
                             initargs))
       (apply #'make-gate-binding (append initargs-override initargs)))
      (measure-binding
       (setf initargs (list* ':qubit (measure-binding-qubit binding)
                             ':target (measure-binding-target binding)
                             initargs))
       (apply #'make-measure-binding (append initargs-override initargs))))))

(defun binding-fmt (stream obj &optional colon-modifier at-modifier)
  (declare (ignore colon-modifier at-modifier))
  (typecase obj
    (measure-binding
     (format stream "MEASURE ~A" (measure-binding-qubit obj))
     (when (measure-binding-target obj)
       (format stream " ~A" (measure-binding-target obj))))
    (gate-binding
     (typecase (gate-binding-operator obj)
       (operator-description
        (print-operator-description (gate-binding-operator obj) stream))
       (otherwise
        (print (gate-binding-operator obj) stream)))
     (a:when-let ((params (gate-binding-parameters obj)))
       (typecase params
         (symbol
          (format stream " ~A" params))
         (list
          (format stream " (~{~A~^ ~})" params))))
     (a:when-let ((arguments (gate-binding-arguments obj)))
       (format stream "~{ ~A~}" arguments)))))

(defun get-binding-from-instr (instr)
  "Constructs a COMPILER-BINDING object from an INSTRUCTION object, in such a way that if some auxiliary COMPILER-BINDING subsumes the output of this routine, then it will match when applied to the original INSTRUCTION object."
  (typecase instr
    (measure-discard
     (make-measure-binding :qubit '_))
    (measurement
     (make-measure-binding :qubit '_ :target '_))
    (application
     (let ((parameters (loop :for p :in (application-parameters instr)
                             :when (typep p 'constant)
                               :collect (constant-value p)
                             :when (symbolp p)
                               :collect '_))
           (arguments (loop :for q :in (application-arguments instr)
                            :when (typep q 'qubit)
                              :collect (qubit-index q)
                            :when (symbolp q)
                              :collect '_)))
       (make-gate-binding :operator (application-operator instr)
                          :parameters parameters
                          :arguments arguments)))))

(defun generate-blank-binding (qubit-count)
  "Constructs a wildcard COMPILER-BINDING of a fixed QUBIT-COUNT."
  (make-gate-binding :operator '_
                     :parameters nil
                     :arguments (make-list qubit-count :initial-element '_)))

(defun binding-subsumes-p (big-binding small-binding)
  "If this routine returns T and BIG-BINDING matches on an INSTRUCTION, then SMALL-BINDING will necessarily also match on it."
  (flet ((wildcard-pattern-p (x)
           (or (symbolp x)
               (and (consp x) (every #'symbolp x)))))
    (cond
      ((and (typep big-binding 'wildcard-binding)
            (or (endp (compiler-binding-options big-binding))
                (equalp (compiler-binding-options big-binding)
                        (compiler-binding-options small-binding))))
       t)
      ((typep small-binding 'wildcard-binding)
       nil)
      ((and (compiler-binding-options big-binding)
            (compiler-binding-options small-binding))
       nil)
      ;; deal with the case of measure bindings
      ((or (and (typep big-binding 'measure-binding)
                (not (typep small-binding 'measure-binding)))
           (and (not (typep big-binding 'measure-binding))
                (typep small-binding 'measure-binding)))
       nil)
      ((and (typep big-binding 'measure-binding)
            (typep small-binding 'measure-binding))
       (or (symbolp (measure-binding-qubit big-binding))
           (equalp (measure-binding-qubit big-binding)
                   (measure-binding-qubit small-binding))))
      ;; big-binding and small-binding cannot either satisfy wildcard-binding-p.
      ;; check the operators for failing both _ <= _ and "literal" <= "literal"
      ((or (and (symbolp (gate-binding-operator small-binding))
                (not (symbolp (gate-binding-operator big-binding))))
           (and (not (symbolp (gate-binding-operator small-binding)))
                (not (symbolp (gate-binding-operator big-binding)))
                (not (equalp (gate-binding-operator big-binding)
                             (gate-binding-operator small-binding)))))
       nil)
      ;; check the parameters for failing both _ <= _ and literal <= literal
      ((or (and (wildcard-pattern-p (gate-binding-parameters small-binding))
                (not (wildcard-pattern-p (gate-binding-parameters big-binding))))
           (and (not (wildcard-pattern-p (gate-binding-parameters small-binding)))
                (not (wildcard-pattern-p (gate-binding-parameters big-binding)))
                (loop :for big-param :in (gate-binding-parameters big-binding)
                      :for small-param :in (gate-binding-parameters small-binding)
                        :thereis (or (and (symbolp small-param)
                                          (not (symbolp big-param)))
                                     (and (not (symbolp small-param))
                                          (not (symbolp big-param))
                                          (not (double= big-param small-param)))))))
       nil)
      ;; check the arguments for failing both _ <= _ and literal <= literal
      ((or (not (= (length (gate-binding-arguments big-binding))
                   (length (gate-binding-arguments small-binding))))
           (loop :for big-arg :in (gate-binding-arguments big-binding)
                 :for small-arg :in (gate-binding-arguments small-binding)
                   :thereis (or (and (symbolp small-arg)
                                     (not (symbolp big-arg)))
                                (and (not (symbolp small-arg))
                                     (not (symbolp big-arg))
                                     (not (= big-arg small-arg))))))
       nil)
      ;; otherwise, i guess everything checks out
      (t
       t))))

(defun binding-precedes-p (a-binding b-binding &optional a-options b-options)
  "If BINDING-PRECEDES-P and if A fails to match on an INSTRUCTION, then B fails to match later."
  (let* ((arg-count-a (if (typep a-binding 'gate-binding)
                          (length (gate-binding-arguments a-binding))
                          most-positive-fixnum))
         (arg-count-b (if (typep b-binding 'gate-binding)
                          (length (gate-binding-arguments b-binding))
                          most-positive-fixnum)))
    ;; fewer qubits means it comes earlier
    (cond
      ((< arg-count-a arg-count-b)
       t)
      ((> arg-count-a arg-count-b)
       nil)
      ;; from here on: (and (= arg-count-a arg-count b) ...)
      ;; cluster by operator
      ((and (typep a-binding 'wildcard-binding)
            (not (symbolp (gate-binding-operator b-binding))))
       nil)
      ((and (not (symbolp (gate-binding-operator a-binding)))
            (typep b-binding 'wildcard-binding))
       t)
      ((and (typep (gate-binding-operator a-binding) 'named-operator)
            (typep (gate-binding-operator b-binding) 'named-operator)
            (adt:with-data (named-operator a-str) (gate-binding-operator a-binding)
              (adt:with-data (named-operator b-str) (gate-binding-operator b-binding)
                (string< a-str b-str))))
       t)
      ;; from here on: either both wildcards or equal operators
      ;; option predicates make it come earlier
      ((and (or a-options (compiler-binding-options a-binding))
            (not (or b-options (compiler-binding-options b-binding))))
       t)
      ;; restrictive parameter matching makes it come earlier
      ((and (not (symbolp (gate-binding-parameters a-binding)))
            (symbolp (gate-binding-parameters b-binding)))
       t)
      ((and (symbolp (gate-binding-parameters a-binding))
            (not (symbolp (gate-binding-parameters b-binding))))
       nil)
      ;; does A need to specialize before B when matching parameters?
      ((and (not (symbolp (gate-binding-parameters a-binding)))
            (not (symbolp (gate-binding-parameters b-binding)))
            (loop :for param-a :in (gate-binding-parameters a-binding)
                  :for param-b :in (gate-binding-parameters b-binding)
                    :thereis (and (not (symbolp param-a))
                                  (symbolp param-b))))
       t)
      (t
       nil))))

(defgeneric binding-subsumes-gate-p (binding gate)
  (:documentation "Tests whether BINDING will match on GATE.")
  (:method ((binding gate-binding) gate)
    nil)
  (:method ((binding gate-binding) (gate gate-application))
    (and
     ;; binding operator is not a wildcard => binding operator equals gate operator
     (or (symbolp (gate-binding-operator binding))
         (equalp (gate-binding-operator binding) (application-operator gate)))
     ;; binding parameter is not a wildcard => each (binding parameter is not a wildcard => binding param = gate param)
     (or (symbolp (gate-binding-parameters binding))
         (every (lambda (b g) (or (symbolp b) (double= b (constant-value g))))
                (gate-binding-parameters binding)
                (application-parameters gate)))
     ;; each (binding argument is not a wildcard => binding arg = gate arg)
     (every (lambda (b g) (or (symbolp b) (equalp b (qubit-index g))))
            (gate-binding-arguments binding)
            (application-arguments gate))))
  (:method ((binding measure-binding) gate)
    nil)
  (:method ((binding measure-binding) (gate measurement))
    (and
     ;; binding qubit is not a wildcard => binding qubit equals gate qubit
     (or (symbolp (measure-binding-qubit binding))
         (= (measure-binding-qubit binding) (qubit-index (measurement-qubit gate))))
     ;; this binding isn't a measure-store
     (null (measure-binding-target binding))))
  (:method ((binding measure-binding) (gate measure))
    (and
     ;; binding qubit is not a wildcard => binding qubit equals gate qubit
     (or (symbolp (measure-binding-qubit binding))
         (= (measure-binding-qubit binding) (qubit-index (measurement-qubit gate))))
     ;; binding target not a wildcard => binding target equals gate target
     (or (symbolp (measure-binding-target binding))
         (equalp (measure-binding-target binding) (measure-address gate)))))
  (:method ((binding wildcard-binding) gate)
    (typep gate 'gate-application)))

(defun unguarded-binding-p (b)
  "Will the binding B match on any ol' gate?"
  (or (and (wildcard-binding-p b)
           (endp (wildcard-binding-options b)))
      (and (gate-binding-p b)
           (symbolp (gate-binding-operator b))
           (endp (gate-binding-options b)))))

(define-condition cannot-concretize-binding (serious-condition) ())
(define-condition no-binding-match (serious-condition) ())

(defun prefer-concrete-items (a b)
  "Return one of A or B, choosing the one that is more \"concrete\".

What \"concrete\" means depends on the types of A and B:

  * If A (B) is a SYMBOL, and B (A) is not, return B (A). In this case we are choosing a value that should not be a wildcard-type value.

  * If neither are symbols and they are equal to one another, return either. In this case both have a non-wildcard-type value.

  * If both are symbols, then return either. In this case both are wildcard symbols and neither is more \"concrete\" than the other."
  (cond
    ((and (symbolp a) (symbolp b))
     b)
    ((and (symbolp a) (not (symbolp b)))
     b)
    ((and (not (symbolp a)) (symbolp b))
     a)
    ((and (not (symbolp a)) (not (symbolp b)) (equalp a b))
     a)
    (t (error 'cannot-concretize-binding))))

(defun prefer-concrete-lists (a b)
  (cond
    ((and (symbolp a) (symbolp b))
     (error 'cannot-concretize-binding))
    ((and (symbolp a) (not (symbolp b)))
     (if (some #'symbolp b)
         (error 'cannot-concretize-binding)
         b))
    ((and (not (symbolp a)) (symbolp b))
     (if (some #'symbolp a)
         (error 'cannot-concretize-binding)
         a))
    (t
     (mapcar #'prefer-concrete-items a b))))

(defgeneric binding-meet (a b)
  (:documentation "Constructs a maximal binding subsumed by both A and B.")
  (:method (a b)
    (error 'cannot-concretize-binding))
  (:method ((a gate-binding) (b gate-binding))
    (make-gate-binding :operator (prefer-concrete-items (gate-binding-operator a) (gate-binding-operator b))
                       :parameters (prefer-concrete-lists (gate-binding-parameters a)
                                                          (gate-binding-parameters b))
                       :arguments (prefer-concrete-lists (gate-binding-arguments a)
                                                         (gate-binding-arguments b))))
  (:method ((a gate-binding) (b wildcard-binding))
    a)
  (:method ((a wildcard-binding) (b gate-binding))
    b))

(defgeneric instantiate-binding (binding)
  (:documentation "When possible, construct the unique instruction on which BINDING will match. If any of the binding's arguments are unspecified (i.e. match against any qubit), those arguments will be filled-in with unique qubit indices.")
  (:method (binding)
    (error 'cannot-concretize-binding))
  (:method ((binding gate-binding))
    (when (symbolp (gate-binding-operator binding))
      (error 'cannot-concretize-binding))
    (when (or (symbolp (gate-binding-parameters binding))
              (some #'symbolp (gate-binding-parameters binding)))
      (error 'cannot-concretize-binding))
    (when (some #'symbolp (gate-binding-arguments binding))
      (let* ((usedq (gate-binding-arguments binding))
             (availableq (set-difference (a:iota (length usedq)) usedq)))
        (setf binding (copy-binding binding
                                    :arguments (mapcar (lambda (q) (if (symbolp q) (pop availableq) q))
                                                       usedq)))))
    (apply #'build-gate
           (gate-binding-operator binding)
           (gate-binding-parameters binding)
           (gate-binding-arguments binding))))


;;; this data structure captures a compilation routine, annotated with various
;;; information about how and when to apply it.

(global-vars:define-global-var **compilers-available** nil)

(defclass compiler ()
  ((name
    :initarg :name
    :reader compiler-name
    :documentation "A human-readable name for the compiler.")
   (instruction-count
    :initarg :instruction-count
    :reader compiler-instruction-count
    :documentation "The number of instructions that this compiler consumes at a time.")
   (bindings
    :initarg :bindings
    :reader compiler-bindings
    :documentation "Raw list of bindings that the compiler matches against.")
   (options
    :initarg :options
    :reader compiler-options
    :documentation "plist of global options supplied to this compiler definition (cf. ENACT-COMPILER-OPTIONS).")
   (body
    :initarg :body
    :reader compiler-body
    :documentation "Raw source of the compiler.")
   (output-gates
    :initarg :output-gates
    :reader compiler-output-gates
    :documentation "Information automatically extracted about the target gate set.")
   (%function
    :initarg :function
    :reader compiler-%function
    :documentation "A funcallable that does the compilation. Signature (compilation-context &rest instructions) -> instructions."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "Represents a transformation that carries particular instruction sequences (e.g., CZ p q; CZ p q) into others (e.g., NOP), decorated with information about the transformation (e.g., the input and output gatesets)."))

(defmethod initialize-instance :after ((c compiler) &key)
  ;; N.B.: To achieve late binding, set this definition instead:
  ;;       (lambda (&rest args) (apply (compiler-%function x) args))
  (closer-mop:set-funcallable-instance-function c (compiler-%function c)))

(defmethod print-object ((obj compiler) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A" (compiler-name obj))))

(defun compiler-gateset-reducer-p (compiler)
  (getf (compiler-options compiler) ':gateset-reducer t))

(defun get-compilers (qubit-bound)
  "Returns a list of the available compilers which match on no more than QUBIT-BOUND qubits and which function as gateset reducers."
  (remove-if-not
   (lambda (compiler)
     (and (compiler-gateset-reducer-p compiler)
          (= 1 (length (compiler-bindings compiler)))
          (typep (first (compiler-bindings compiler)) 'gate-binding)
          (>= qubit-bound (length (gate-binding-arguments (first (compiler-bindings compiler)))))
          (endp (compiler-binding-options (first (compiler-bindings compiler))))))
   **compilers-available**))

(defmethod describe-object ((obj compiler) stream)
  (call-next-method)
  (format stream "~%  NAME: ~A~%~%  INPUT:~%~{    ~/cl-quil::binding-fmt/~%~}~%  OUTPUT FREQUENCY:~%"
          (compiler-name obj)
          (compiler-bindings obj))
  (dohash ((binding count) (compiler-output-gates obj))
    (format stream "    ~/cl-quil::binding-fmt/: ~A~%" binding count)))

(defun record-compiler (name)
  "Record (possibly overwriting) the existence of the compiler named by NAME."
  (check-type name symbol)
  (let ((c (fdefinition name)))
    (assert (typep c 'compiler) (name) "The name ~S doesn't name a known compiler." name)
    (let ((prev (member name **compilers-available** :key #'compiler-name)))
      (cond
        ((null prev)
         (push c **compilers-available**))
        (t
         (rplaca prev c)))))
  (values))

;;; in what follows, we're very interested in two kinds of maps keyed on bindings:
;;;  + an "occurrence table" is valued in integers, and it counts frequency.
;;;    this comes up when describing the output of a given compiler: in order to
;;;    discern whether it's making progress against some reference target gateset,
;;;    it's important to count what sorts of things it's emitting and how many.
;;;  + a "gate set" is valued in GATE-RECORD objects, and it tracks e.g. fidelity.
;;;    this comes up when trying to discern which of two possible "legal" outputs
;;;    is preferable: when two (chains of) compilers emit instruction sequences
;;;    that belong to some target gateset, we want to be able to tell, based on
;;;    a fine-grained understanding of the emitted sequences, which is more likely
;;;    to perform better on hardware. GATE-RECORDs track this performance info.

(defstruct (occurrence-table (:copier nil))
  "Wrapper for a hash map from bindings to integer frequencies."
  (map (make-hash-table :test #'equalp) :read-only t :type hash-table))

(defmethod print-object ((object occurrence-table) stream)
  (print-unreadable-object (object stream :type t)
    (dohash ((binding count) (occurrence-table-map object))
      (format stream "~&  ~/cl-quil::binding-fmt/ -> ~a" binding count))))

(defun add-entry-to-occurrence-table (table binding count &optional (scalar 1))
  "Destructively increment a binding's value in an occurrence table."
  (check-type table occurrence-table)
  (check-type binding compiler-binding)
  (incf (gethash binding (occurrence-table-map table) 0) (* count scalar)))

(defun copy-occurrence-table (table)
  "Create a shallow copy of an occurrence table."
  (check-type table occurrence-table)
  (let ((new-table (make-occurrence-table)))
    (dohash ((key val) (occurrence-table-map table))
      (add-entry-to-occurrence-table new-table key val))
    new-table))

(defun add-occurrence-tables (table1 table2 &optional (scalar2 1))
  "Construct a new occurrence table whose values are the sums of the values of the two input tables."
  (check-type table1 occurrence-table)
  (check-type table2 occurrence-table)
  (let ((new-table (copy-occurrence-table table1)))
    (dohash ((key val) (occurrence-table-map table2) new-table)
      (add-entry-to-occurrence-table new-table key val scalar2))))

(defun filter-occurrence-table-by-qubit-count (table qubit-count)
  (check-type table occurrence-table)
  (check-type qubit-count integer)
  (let ((new-table (make-occurrence-table)))
    (dohash ((key val) (occurrence-table-map table) new-table)
      (when (= qubit-count
               (length (gate-binding-arguments key)))
        (setf (gethash key (occurrence-table-map new-table)) val)))))

(defun update-occurrence-table (table compiler &optional qubit-count)
  "Treats a COMPILER as a replacement rule: each binding in the occurrence table TABLE on which COMPILER matches is replaced by the output occurrence table of COMPILER (appropriately scaled by the frequency of the binding in TABLE).

Optionally constrains the output to include only those bindings of a particular QUBIT-COUNT."
  (check-type table occurrence-table)
  (check-type compiler compiler)
  (assert (= 1 (length (compiler-bindings compiler))))
  (let ((binding (first (compiler-bindings compiler)))
        (replacement (if qubit-count
                         (filter-occurrence-table-by-qubit-count (compiler-output-gates compiler) qubit-count)
                         (compiler-output-gates compiler)))
        (new-table (make-occurrence-table))
        (binding-applied? nil))
    ;; for each existing entry in the table...
    (dohash ((key val) (occurrence-table-map table))
      ;; ... do we match the binding?
      (cond
        ;; if so, merge the replacement into the new table
        ((binding-subsumes-p binding key)
         (setf binding-applied? t)
         (setf new-table (add-occurrence-tables new-table replacement val)))
        ;; if not, retain the old entry
        (t
         (add-entry-to-occurrence-table new-table key val))))
    (values new-table binding-applied?)))

(defstruct occurrence-table-cost
  "Wrapper for deduced costs from occurrence tables married to gate sets."
  (fidelity 1d0 :type double-float)
  (unknowns 0 :type (integer 0)))

(defun occurrence-table-cost-> (x y)
  (cond
    ((> (occurrence-table-cost-unknowns x)
        (occurrence-table-cost-unknowns y))
     t)
    ((< (occurrence-table-cost-unknowns x)
        (occurrence-table-cost-unknowns y))
     nil)
    ((> (occurrence-table-cost-fidelity x)
        (occurrence-table-cost-fidelity y))
     t)
    (t
     nil)))

(defun occurrence-table-cost (occurrence-table gateset)
  "Given an OCCURRENCE-TABLE (i.e., a map from GATE-BINDINGs to frequencies) and a GATESET (i.e., a map from GATE-BINDINGs to GATE-RECORDs, which include fidelities), estimate the overall fidelity cost of all the gates in OCCURRENCE-TABLE."
  (check-type occurrence-table occurrence-table)
  (let ((ret (make-occurrence-table-cost)))
    (dohash ((key val) (occurrence-table-map occurrence-table) ret)
      (tagbody
         (dohash ((cost-key cost-val) gateset)
           (when (binding-subsumes-p cost-key key)
             (setf (occurrence-table-cost-fidelity ret)
                   (* (expt (gate-record-fidelity cost-val) val)
                      (occurrence-table-cost-fidelity ret)))
             (go :SKIP)))
         (incf (occurrence-table-cost-unknowns ret))
         :SKIP))))

(defun occurrence-table-in-gateset-p (occurrence-table gateset)
  "Are all of the entires in OCCURRENCE-TABLE subsumed by entries in GATESET?  This is the stopping condition for recognizing that a table consists of 'native gates'."
  (check-type occurrence-table occurrence-table)
  (loop :for table-key :being :the :hash-keys :of (occurrence-table-map occurrence-table)
        :always (loop :for gateset-key :being :the :hash-keys :of gateset
                        :thereis (and (or (gate-record-duration (gethash gateset-key gateset))
                                          (gate-record-fidelity (gethash gateset-key gateset)))
                                      (binding-subsumes-p gateset-key table-key)))))

(defun blank-out-qubits (gateset)
  (let ((new-gateset (make-hash-table :test #'equalp)))
    (dohash ((key val) gateset new-gateset)
      (let (new-key)
        (etypecase key
          (gate-binding
           (setf new-key
                 (copy-binding key :arguments (mapcar (constantly '_)
                                                      (gate-binding-arguments key)))))
          (measure-binding
           (setf new-key
                 (copy-binding key :qubit '_)))
          (wildcard-binding
           (setf new-key (copy-binding key))))
        (setf (gethash new-key new-gateset) val)))))


;;; a panoply of routines for dissecting compiler bodies and bindings.
;;; these are used, for instance, in WARM-HARDWARE-OBJECTs to automatically
;;; determine the relevant subset of compilers for nativizing gates for execution
;;; on that particular hardware device.

(define-condition no-compiler-path (serious-condition)
  ()
  (:documentation "We were asked to select a sequence of compilers from a collection which collectively carry one gate set to another, but we failed to find it."))

(defun find-shortest-compiler-path (compilers target-gateset occurrence-table &optional qubit-count)
  "Produces a path of compilers, drawn from COMPILERS, which convert all of the entries of OCCURRENCE-TABLE into \"native gates\" according to TARGET-GATESET.  Returns an alternating chain of occurrence tables and compilers: (On C(n-1) O(n-1) ... C1 O1), where O(j+1) is the result of substituting Cj through Oj and where On is subsumed by TARGET-GATESET.

Optionally restricts to considering only those gates and compilers which involve QUBIT-COUNT many qubits.

N.B.: The word \"shortest\" here is a bit fuzzy.  In practice it typically means \"of least length\", but in theory this invariant could be violated by pathological compiler output frequency + fidelity pairings that push the least-length path further down the priority queue."
  (let ((queue (make-instance 'cl-heap:priority-queue :sort-fun #'occurrence-table-cost->)))
    (flet ((collect-bindings (occurrence-table)
             (loop :for key :being :the :hash-keys :of (occurrence-table-map occurrence-table)
                   :collect key)))
      ;; initial contents: arbitrary gate, no history
      (loop :with visited-nodes := ()
            :for (task . history) := (list occurrence-table)
              :then (cl-heap:dequeue queue)
            :unless task
              :do (error 'no-compiler-path)
            :when (occurrence-table-in-gateset-p task target-gateset)
              :return (cons task history)
            :unless (member (sort (collect-bindings task) #'binding-precedes-p)
                            visited-nodes :test #'equalp)
              :do (push (sort (collect-bindings task) #'binding-precedes-p)
                        visited-nodes)
                  (dolist (compiler compilers)
                    (multiple-value-bind (new-table updated?)
                        (update-occurrence-table task compiler qubit-count)
                      (when updated?
                        (cl-heap:enqueue queue
                                         (list* new-table compiler task history)
                                         (occurrence-table-cost new-table target-gateset)))))))))

(defun print-compiler-path (path)
  "Pretty-prints the output of FIND-SHORTEST-COMPILER-PATH. Useful for debugging."
  (dolist (item path)
    (typecase item
      (compiler (format t "~&is the output from applying ~A, coming from...~%" item))
      (hash-table
       (dohash ((key val) item)
         (format t "~/cl-quil::binding-fmt/ -> ~A~%" key val))))))

(defun compute-applicable-compilers (target-gateset qubit-count) ; h/t lisp
  "Starting from all available compilers, constructs a precedence-sorted list of those compilers which help to convert from arbitrary inputs to a particular target gateset."
  (flet (;; Checks whether PATH doubles back through the occurrence table START.         
         (path-has-a-loop-p (path start)
           (and (< 1 (length path))
                (loop :for (table compiler) :on path :by #'cddr
                      :when (loop :for gate :being :the :hash-keys :of (occurrence-table-map table)
                                  :always (loop :for binding :in start
                                                  :thereis (binding-subsumes-p (first start) gate)))
                        :do (return-from path-has-a-loop-p t)
                      :finally (return nil)))))
    
    (let* ((target-gateset (blank-out-qubits target-gateset))
           (compilers (get-compilers qubit-count))
           (unconditional-compilers
             (remove-if (lambda (x)
                          (some (a:conjoin (complement #'symbolp) #'compiler-binding-options)
                                (compiler-bindings x)))
                        compilers))
           (candidate-special-compilers
             (remove-if (lambda (x)
                          (or (/= qubit-count (length (gate-binding-arguments (first (compiler-bindings x)))))
                              (loop :for b :being :the :hash-keys :of target-gateset
                                      :thereis (binding-subsumes-p b (first (compiler-bindings x))))))
                        compilers))
           ;; start by computing a fast route from the generic gate to the target gate set
           (generic-path (find-shortest-compiler-path unconditional-compilers
                                                      target-gateset
                                                      (make-occurrence-table
                                                       :map (alexandria:plist-hash-table
                                                             (list (generate-blank-binding qubit-count) 1)
                                                             :test #'equalp))
                                                      qubit-count))
           (generic-cost (occurrence-table-cost-fidelity
                          (occurrence-table-cost (first generic-path) target-gateset))))
      
      ;; it may be that non-generic gates have shorter routes to the target gate set.
      ;; each possible such route begins with a specialized compiler.
      ;; so, iterate over specialized compilers and see if they lead anywhere nice.
      (let ((compiler-hash (make-hash-table)))
        (dolist (compiler generic-path)
          (when (typep compiler 'compiler)
            (setf (gethash compiler compiler-hash) generic-cost)))
        (dolist (compiler candidate-special-compilers)
          (let* ((special-path
                   (find-shortest-compiler-path
                    unconditional-compilers target-gateset
                    (filter-occurrence-table-by-qubit-count (compiler-output-gates compiler) qubit-count)
                    qubit-count))
                 (special-cost (occurrence-table-cost-fidelity
                                (occurrence-table-cost (first special-path) target-gateset))))
            ;; did we in fact beat out the generic machinery?
            (when (and (not (path-has-a-loop-p special-path (compiler-bindings compiler)))
                       (> special-cost generic-cost))
              ;; then store it!
              (setf (gethash compiler compiler-hash) special-cost))))
        
        ;; these are basically all the compilers we care to use; now we need to
        ;; sort them into preference order.
        (let* ((sorted-compilers
                 (stable-sort (remove-duplicates
                               (append (loop :for compiler :being :the :hash-keys :of compiler-hash
                                             :collect compiler)
                                       (loop :for compiler :in generic-path
                                             :when (typep compiler 'compiler)
                                               :collect compiler)))
                              #'>
                              :key (lambda (x) (gethash x compiler-hash))))
               ;; additionally, we install a couple extra compilers as hax to make
               ;; the whole machine work. each such hak comes with an explanation,
               ;; and it would be preferable to work to make each hak unnecessary.
               (compilers-with-features
                 (append
                  ;; STATE-PREP-APPLICATION doesn't have a GATE-MATRIX, which causes
                  ;; some havoc with all this new automation. so, instead, we prefix
                  ;; with a compiler that catches S-P-As early.
                  (cond
                    ((= 1 qubit-count)
                     (list #'state-prep-1q-compiler))
                    ((= 2 qubit-count)
                     (list #'state-prep-2q-compiler)))
                  sorted-compilers)))
          compilers-with-features)))))

(defun gates-that-match-binding (binding gateset)
  (loop :for gate-binding :being :the :hash-keys :of gateset
        :for gate := (handler-case (instantiate-binding (binding-meet gate-binding binding))
                       (cannot-concretize-binding () nil)
                       (no-binding-match () nil))
        :when gate
          :collect gate))

(defun compute-applicable-reducers (gateset &key (compilers **compilers-available**))
  "Returns all those compilers (including those which match on sequences of instructions rather than single instructions) which improve the brevity of a gate sequence without exiting a particular GATESET. The optional COMPILERS specifies the list of compilers from which the applicable reducers should be selected. By default COMPILERS is the globally-defined compilers list."
  (labels
      ((calculate-fidelity-of-concrete-gates (gates)
         (loop :with fidelity := 1d0
               :for gate :in gates
               :do (loop :for binding :being :the :hash-keys :of gateset
                         :for gate-info := (gethash binding gateset)
                         :when (binding-subsumes-gate-p binding gate)
                           :return (setf fidelity (* fidelity (gate-record-fidelity gate-info)))
                         ;; only executes if no binding subsumes the gate
                         :finally (error 'no-binding-match))
               :finally (return fidelity)))
       (consider-input-test-case (compiler &rest gates)
         (handler-case
             (let* ((input-fidelity (calculate-fidelity-of-concrete-gates gates))
                    (output (apply compiler gates))
                    (output-fidelity (calculate-fidelity-of-concrete-gates output)))
               (>= output-fidelity input-fidelity))
           (no-binding-match () nil)
           (compiler-does-not-apply () nil)
           (cannot-concretize-binding () nil)))
       (consider-compiler (compiler)
         (and
          ;; certain things we will never tolerate:
          ;; (1) unguarded matches will always result in infinite loops.
          (notany #'unguarded-binding-p (compiler-bindings compiler))
          ;; (2) unguarded gatesets don't benefit from reduction
          (loop :for binding :being :the :hash-keys :of gateset
                :never (wildcard-binding-p binding))
          (or
           ;; having cleared these checks, we support two kinds of matches:
           ;; (1) a "blind" match, where we can tell just by considering bindings
           ;;     that the compiler will always have reasonable input and output
           ;; (2) an "exhaustive" match, where we instantiate different test cases
           ;;     to pump through the compiler, convincing ourselves that in all
           ;;     applicable situations the compiler gives good output.
           (let* ((gateset (blank-out-qubits gateset))
                  (in-fidelity 1d0)
                  ;; REM: the OCCURRENCE-TABLE-IN-GATESET-P check below lets us skip checking the UNKNOWNS slot
                  (out-fidelity (occurrence-table-cost-fidelity
                                 (occurrence-table-cost (compiler-output-gates compiler) gateset))))
             (and (occurrence-table-in-gateset-p (compiler-output-gates compiler) gateset)
                  (loop :for b :in (compiler-bindings compiler)
                        :always (loop :for g :being :the :hash-keys :of gateset
                                      :for gate-fidelity := (gate-record-fidelity (gethash g gateset))
                                      ;; TODO: revisit this predicate.
                                        :thereis (and (or (binding-subsumes-p g b)
                                                          (when (typep b 'gate-binding)
                                                            (binding-subsumes-p (copy-binding b :options nil) g)))
                                                      (setf in-fidelity (* in-fidelity gate-fidelity)))))
                  (>= out-fidelity in-fidelity)))
           ;; if that falls through, try the exhaustive match
           (let ((matches (loop :for binding :in (compiler-bindings compiler)
                                :for match := (gates-that-match-binding binding gateset)
                                :when (null match)
                                  :do (return-from consider-compiler nil)
                                :collect match)))
             ;; TODO: this MAP-PRODUCT could be better: unknown argument list size,
             ;;       generates a whole big list and then does EVERY afterward, ...
             (every #'identity (apply #'a:map-product (a:curry #'consider-input-test-case compiler) matches)))))))
    
    (let (applicable-compilers)
      (dolist (compiler compilers applicable-compilers)
        (when (consider-compiler compiler)
          (push compiler applicable-compilers))))))


;;; we now turn attention to the macro DEFINE-COMPILER, which constructs instances
;;; of the above class COMPILER and installs them into the appropriate namespaces.
;;; this relies on a bunch of routines to dissect the compiler source apart into:
;;;  + BINDINGs which encode the pattern matched arguments
;;;  + OCCURRENCE-TABLEs which record the expected output of the compiler
;;;  + a COMPILER object that stores all this stuff

(defun cleave-options (bindings-and-options &optional backlog)
  "Separates a list of the form (binding1 binding2 ... bindingn :option1 val1 ... :optionm valm) into the values pair (binding1 ... bindingn) (:option1 ... valm)."
  (cond
    ((or (endp bindings-and-options)
         (keywordp (first bindings-and-options)))
     (values (nreverse backlog) bindings-and-options))
    (t
     (cleave-options (rest bindings-and-options)
                     (cons (first bindings-and-options) backlog)))))

(defun estimate-output-gates-from-raw-code (body)
  "Walks the body of a compiler definition and extracts (or, at least attempts to extract) frequency data about the gates emitted by the compiler.  Results in an occurrence table.

N.B.: This routine is somewhat fragile, and highly creative compiler authors will want to supply a manually-constructed frequency table as an option to DEFINE-COMPILER rather than rely on the success of this helper."
  (unless (typep body 'cons)
    (return-from estimate-output-gates-from-raw-code (make-occurrence-table)))
  (case (first body)
    (inst
     (when (= 2 (length body))
       (return-from estimate-output-gates-from-raw-code
         (make-occurrence-table)))
     (destructuring-bind (head name param-list &rest qubit-list) body
       (declare (ignore head))
       (let ((table (make-occurrence-table))
             (operator (typecase name
                         (string (named-operator name))
                         (symbol name)
                         (otherwise (return-from estimate-output-gates-from-raw-code
                                      (make-occurrence-table)))))
             (param-list (cond
                           ((typep param-list 'list)
                            (loop :for item :in (case (first param-list)
                                                  ((quote
                                                    ;; generic attempt to detect quasiquotes
                                                    #+(or sbcl ecl ccl clisp) #.(first (quote `(t)))
                                                    #-(or sbcl ecl ccl clisp) #.(cerror "Bravely, boldly, stupidly continue." "I don't know how to detect quasiquotes."))
                                                   (second param-list))
                                                  ((list)
                                                   (rest param-list))
                                                  (otherwise param-list))
                                  :if (typep item 'number)
                                    :collect item
                                  :else
                                    :collect '_))
                           (t
                            '_)))
             (qubit-list (mapcar (lambda (x)
                                   (typecase x
                                     (number x)
                                     (otherwise '_)))
                                 qubit-list)))
         (setf (gethash (make-gate-binding :operator operator
                                           :parameters param-list
                                           :arguments qubit-list)
                        (occurrence-table-map table))
               1)
         table)))
    (otherwise
     (let ((table (make-occurrence-table)))
       (dolist (subbody body table)
         (let ((incoming-table (estimate-output-gates-from-raw-code subbody)))
           (setf table (add-occurrence-tables table incoming-table))))))))

(defun make-binding-from-source (source)
  (when (symbolp source)
    (return-from make-binding-from-source
      (make-wildcard-binding :name source)))
  (assert (listp source))
  (multiple-value-bind (source options) (cleave-options source)
    (cond
      ((or (endp (cdr source))
           (wildcard-pattern-p (second source)))
       (make-wildcard-binding :name (first source)
                              :options options))
      (t
       (make-gate-binding :name (first source)
                          :options options
                          :operator (let ((op (first (second source))))
                                      (etypecase op
                                        (symbol
                                         op)
                                        (string
                                         (named-operator op))
                                        (operator-description
                                         op)))
                          :parameters (second (second source))
                          :arguments (rest (rest (second source))))))))

;;; here are the forms directly related to DEFINE-COMPILER, whether its
;;; destructuring or utilities available with its body.

;; within the body of a DEFINE-COMPILER, we publish instructions to output
;; using a DEFINE-VOP-like INST macro, and we quit a compiler early using
;; FINISH-COMPILER.

;; first, prohibit the use of these items outside of a compiler body
(defun inst (&rest xs)
  "Emit the instructions XS in the current compiler context.

INST is a local function usable within the dynamic extent of a compiler body."
  (declare (ignore xs))
  (error "INST can only be used in the body of a compiler."))

(define-compiler-macro inst (&rest xs)
  (declare (ignore xs))
  (error "INST can only be used in the body of a compiler."))

(defun inst* (&rest xs)
  "Emit the instructions XS in the current compiler context. Treat the last argument of XS as a list of arguments, as if using APPLY #'BUILD-GATE.

INST* is a local function usable within the dynamic extent of a compiler body."
  (declare (ignore xs))
  (error "INST* can only be used in the body of a compiler."))

(define-compiler-macro inst* (&rest xs)
  (declare (ignore xs))
  (error "INST* can only be used in the body of a compiler."))

(defmacro finish-compiler ()
  "Finish compiling, retuning all instructed emitted with INST.

FINISH-COMPILER is a local macro usable within a compiler body."
  (error "FINISH-COMPILER can only be used in the body of a compiler."))

;;; The empty list is so that options can be provided in the future.
(defmacro with-inst (() &body body)
  "Define INST, INST*, and FINISH-COMPILER handlers extending over BODY."
  (a:with-gensyms (list tail compiler-context x xs retval-p)
    `(block ,compiler-context
       (let* ((,list (cons nil nil))
              (,tail ,list))
         (declare (dynamic-extent ,list)
                  (type cons ,list ,tail))
         (labels ((inst (&rest ,xs)
                    (declare (optimize speed (safety 0) (debug 0) (space 0)))
                    (let (,x)
                      (cond
                        ;; check for a raw gate object
                        ((and (= 1 (length ,xs))
                              (typep (first ,xs) 'gate-application))
                         (setf ,x (first ,xs)))
                        ;; check for an anon-gate signature
                        ((and (<= 3 (length ,xs))
                              (or (typep (cadr ,xs) 'magicl:matrix)
                                  (typep (car ,xs) 'gate)))
                         (setf ,x (apply #'anon-gate ,xs)))
                        ;; check for a build-gate signature
                        ((and (<= 3 (length ,xs))
                              (typep (cadr ,xs) 'list))
                         (setf ,x (apply #'build-gate ,xs)))
                        (t
                         (error "INST argument pattern not recognized: ~A" ,xs)))
                      (rplacd ,tail (cons ,x nil))
                      (setf ,tail (cdr ,tail))
                      (values)))
                  (inst* (&rest ,xs)
                    (apply #'inst (apply #'list* ,xs))))
           (declare (dynamic-extent #'inst))
           (macrolet ((finish-compiler (&optional (retval nil ,retval-p))
                        (if ,retval-p
                            `(return-from ,',compiler-context ,retval)
                            '(return-from ,compiler-context (cdr (the cons ,list))))))
             ,@body
             ;; Implicitly return the collected instructions.
             (finish-compiler)))))))

;; if we ever want to expand the list of "reserved symbols", this is the place
;; to do it. previously, `'pi` lived here, but it seems better to require users
;; to write `#.pi`, since it's more visually offsetting.
(defun match-symbol-p (x)
  (and (symbolp x)
       (not (wildcard-pattern-p x))))

(defun wildcard-pattern-p (x)
  (and (symbolp x)
       (string= "_" x)))

(defun binding-environment (binding)
  "Return an alist of (name . t) for each variable element in BINDING."
  (when (typep binding 'wildcard-binding)
    (return-from binding-environment nil))
  (nconc (when (match-symbol-p (gate-binding-operator binding))
           (list (cons (gate-binding-operator binding) t)))
         (unless (symbolp (gate-binding-parameters binding))
           (mapcan (lambda (param)
                     (when (match-symbol-p param)
                       (list (cons param t))))
                   (gate-binding-parameters binding)))
         (mapcan (lambda (arg)
                   (when (match-symbol-p arg)
                     (list (cons arg t))))
                 (gate-binding-arguments binding))))

(defun extend-environment (binding environment)
  (setf environment
        (append (binding-environment binding) environment)))

(defun lookup (name env)
  (cdr (assoc name env)))

(defun define-compiler-form (bindings body &key permit-binding-mismatches-when)
  (labels ((expand-bindings (bindings env)
             (cond
               ((endp bindings)
                `(values (with-inst () ,@body)
                         t))
               ((typep (first bindings) 'symbol)
                (expand-bindings (rest bindings)
                                 env))
               (t
                (let ((binding (first bindings)))
                  (expand-binding binding
                                  env
                                  (expand-bindings (rest bindings)
                                                   (extend-environment binding env)))))))
           
           (expand-binding (binding env body)
             (cond
               ((typep binding 'wildcard-binding)
                (if (compiler-binding-options binding)
                    (expand-options binding env body)
                    body))
               ((typep binding 'gate-binding)
                (expand-op binding env
                           (expand-parameters binding env
                                              (expand-arguments binding env
                                                                (expand-options binding env body)))))
               (t
                (error "Malformed binding in compiler form: ~A." binding))))
           
           (expand-sequence (seq env rest &key gensym-name seq-accessor gate-name elt-accessor elt-type test)
             (let* ((target-length (length seq))
                    (seq-var (gensym (format nil "~AS" gensym-name)))
                    (elt-vars (loop :repeat target-length
                                    :collect (gensym gensym-name)))
                    (dead-vars (loop :for v :in elt-vars
                                     :for p :in seq
                                     :when (wildcard-pattern-p p)
                                       :collect v)))
               `(let ((,seq-var (,seq-accessor ,gate-name)))
                  (when (= (length ,seq-var) ,target-length)
                    (destructuring-bind ,elt-vars
                        ,seq-var
                      ,@(when dead-vars
                          (list `(declare (ignore ,@dead-vars))))
                      ,(expand-each-element seq
                                            elt-vars
                                            env
                                            rest
                                            :elt-accessor elt-accessor
                                            :elt-type elt-type
                                            :test test))))))
           
           (expand-each-element (seq elt-vars env rest &key elt-accessor elt-type test)
             (when (endp seq)
               (return-from expand-each-element rest))
             (let ((ele (first seq))
                   (var (first elt-vars)))
               (cond
                 ((wildcard-pattern-p ele)
                  (expand-each-element (rest seq)
                                       (rest elt-vars)
                                       env
                                       rest
                                       :elt-accessor elt-accessor
                                       :elt-type elt-type
                                       :test test))
                 ((and (match-symbol-p ele)
                       (not (lookup ele env)))
                  ;; fresh binding
                  `(let ((,ele (if (typep ,var ',elt-type)
                                   (,elt-accessor ,var)
                                   ,var)))
                     ,(expand-each-element (rest seq)
                                           (rest elt-vars)
                                           (list* (cons ele t) env)
                                           rest
                                           :elt-accessor elt-accessor
                                           :elt-type elt-type
                                           :test test)))
                 (t
                  ;; existing binding / data. insert test check
                  `(when (or ,permit-binding-mismatches-when
                             (and (typep ,var ',elt-type)
                                  (,test ,ele (,elt-accessor ,var))))
                     ,(expand-each-element (rest seq)
                                           (rest elt-vars)
                                           env
                                           rest
                                           :elt-accessor elt-accessor
                                           :elt-type elt-type
                                           :test test))))))
           
           (expand-parameters (binding env rest)
             (cond
               ((wildcard-pattern-p (gate-binding-parameters binding))
                rest)
               ((and (gate-binding-parameters binding) ; no NILs please
                     (symbolp (gate-binding-parameters binding)))
                `(let ((,(gate-binding-parameters binding)
                         (mapcar #'constant-value (application-parameters ,(compiler-binding-name binding)))))
                   ,rest))
               (t
                (expand-sequence (gate-binding-parameters binding)
                                 env
                                 rest
                                 :gensym-name "PARAM"
                                 :seq-accessor 'application-parameters
                                 :gate-name (compiler-binding-name binding)
                                 :elt-accessor 'constant-value
                                 :elt-type 'constant
                                 :test 'double=))))
           
           (expand-arguments (binding env rest)
             (expand-sequence (gate-binding-arguments binding)
                              env
                              rest
                              :gensym-name "ARG"
                              :seq-accessor 'application-arguments
                              :gate-name (compiler-binding-name binding)
                              :elt-accessor 'qubit-index
                              :elt-type 'qubit
                              :test '=))
           
           (expand-options (binding env rest)
             (declare (ignore env))
             (let ((option-plist (compiler-binding-options binding)))
               (loop :for (val key) :on (reverse option-plist) :by #'cddr
                     :do (setf rest
                               (case key
                                 (:where
                                  `(when ,val
                                     ,rest))
                                 (:acting-on
                                  (destructuring-bind (wf qc) val
                                    `(unpack-wf ,(compiler-binding-name binding) context (,wf ,qc)
                                       ,rest)))
                                 (otherwise
                                  (error "Illegal OPERATOR-BIND option: ~A" key)))))
               rest))
           
           (expand-op (binding env body)
             (let ((op (gate-binding-operator binding)))
               (cond
                 ((wildcard-pattern-p op)
                  ;; ignore
                  body)
                 ((and (match-symbol-p op)
                       (not (lookup op env)))
                  ;; fresh binding
                  `(let ((,op ,(compiler-binding-name binding)))
                     ,body))
                 (t
                  ;; existing binding / data. insert string check
                  `(when (equalp ,op (application-operator ,(compiler-binding-name binding)))
                     ,body))))))
    (expand-bindings bindings nil)))

(defun enact-compiler-options (options body)
  "Produces code used to enact options passed to DEFINE-COMPILER.  BODY is a lump of compiler code (typically generated by DEFINE-COMPILER-FORM), and OPTIONS is a plist with the following keys:

FULL-CONTEXT: Names a symbol to which the active COMPILATION-CONTEXT will be bound.

CHIP-SPECIFICATION: Names a symbol to which the active CHIP-SPECIFICATION will be bound.

GATESET-REDUCER: When NIL, this compiler is filtered from participation in compression. Defaults to T.

CLASS: The compiler constructed by DEFINE-COMPILER will be of the specified subclass of COMPILER.

PERMIT-BINDING-MISMATCHES-WHEN: A predicate that, when true, permits the compiler to skip equality matching during gate destructuring.  For instance, this can be used to allow approximate matches.

OUTPUT-GATESET: A plist keyed on arguments to MAKE-GATE-BINDING with values in frequency count.  If specified supplants DEFINE-COMPILER's automatic estimation of the gate production of the compiler routine."
  (when (endp options)
    (return-from enact-compiler-options body))
  (destructuring-bind (key val &rest remaining-options) options
    (case key
      (:full-context
       (enact-compiler-options remaining-options
                               `(let ((,val context))
                                  ,body)))
      (:chip-specification
       (enact-compiler-options remaining-options
                               `(progn
                                  (unless context (give-up-compilation))
                                  (let ((,val (compilation-context-chip-specification context)))
                                    (unless ,val (give-up-compilation))
                                    ,body))))
      ((:gateset-reducer :class :permit-binding-mismatches-when :output-gateset)
       (enact-compiler-options remaining-options body))
      (otherwise
       (error "Unknown compiler option: ~A." (first options))))))

(defmacro define-compiler (name (&rest bindings) &body body)
  "Defines and registers a COMPILER object.  For detailed options information, see ENACT-COMPILER-OPTIONS."
  (multiple-value-bind (body decls docstring) (alexandria:parse-body body :documentation t)
    (multiple-value-bind (bindings options) (cleave-options bindings)
      (let* ((parsed-bindings (mapcar #'make-binding-from-source bindings))
             (variable-names (mapcar #'compiler-binding-name parsed-bindings)))
        (alexandria:when-let (pos (position "CONTEXT" variable-names :test #'string=))
          (warn "DEFINE-COMPILER reserves the variable name CONTEXT, but the ~Dth binding of ~A has that name."
                (1+ pos) (string name)))
        (alexandria:with-gensyms (ret-val ret-bool)
          ;; TODO: do the alexandria destructuring to catch the docstring or whatever
          `(progn
             ;; Let the world know about the existence of this function.
             (declaim (ftype function ,name))
             (setf (fdefinition ',name)
                   (make-instance ',(getf options ':class 'compiler)
                                  :name ',name
                                  :instruction-count ,(length variable-names)
                                  :bindings (mapcar #'make-binding-from-source ',bindings)
                                  :options ',options
                                  :body '(locally ,@decls ,@body)
                                  :output-gates ,(if (getf options ':output-gateset)
                                                     `(make-occurrence-table
                                                       :map (a:plist-hash-table
                                                             (loop :for (args count) :on ,(getf options ':output-gateset) :by #'cddr
                                                                   :nconc (list (apply #'make-gate-binding args) count))
                                                             :test #'equalp))
                                                     `(estimate-output-gates-from-raw-code (quote (progn ,@body))))
                                  :function (a:named-lambda ,name (,@variable-names &key context)
                                              (declare (ignorable context))
                                              (multiple-value-bind (,ret-val ,ret-bool)
                                                  ,(enact-compiler-options
                                                    options
                                                    (define-compiler-form parsed-bindings (append decls body)
                                                      :permit-binding-mismatches-when (getf options ':permit-binding-mismatches-when)))
                                                (if ,ret-bool ,ret-val (give-up-compilation))))))
             (setf (documentation ',name 'function) ,docstring)
             (record-compiler ',name)
             ',name))))))
