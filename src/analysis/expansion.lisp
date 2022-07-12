;;;; analysis/expansion.lisp
;;;;
;;;; Authors: Robert Smith
;;;;          Erik Davis

(in-package #:cl-quil/frontend)

;;; Since both circuit and calibration expansion rely on some common code, but
;;; we would like error messages to be as specific as possible, we choose to
;;; explicitly track an "expansion context".
;;;
;;; This should be bound to a token type (e.g. :DEFCIRCUIT) representing the AST
;;; object being expanded, before entry to RELABEL-BLOCK-LABELS-UNIQUELY or
;;; INSTANTIATE-INSTRUCTION. If it is not bound, error messages from
;;; QUIL-EXPANSION-ERROR will be less descriptive.
(defvar *expansion-context*)
(setf (documentation '*expansion-context* 'variable)
      "The the context for expansion, mainly used for error messages (cf. QUIL-EXPANSION-ERROR).")

(define-condition quil-expansion-error (simple-error)
  ()
  (:documentation "Representation of an error arising in Quil circuit or calibration expansion."))

(defun quil-expansion-error (format-control &rest format-args)
  "Signal a QUIL-EXPANSION-ERROR, incorporating information about the expansion context."
  (error 'quil-expansion-error
         :format-control "While expanding ~:[circuit or calibration~;~:*~A~]: ~?"
         :format-arguments (list (and (boundp '*expansion-context*)
                                      *expansion-context*)
                                 format-control
                                 format-args)))

(defvar *expansion-limit* 256
  "Limit the number of recursive circuit or calibration expansions that can happen before erroring. Intended to avoid infinite loops.")

(defvar *expansion-depth*)

(defun relabel-block-labels-uniquely (quil-block)
  "Non-destructively relabel all jump targets and associated JUMP-like instructions in the list of instructions QUIL-BLOCK."
  ;; Compute an alist mapping label name (string) to fresh LABEL
  ;; objects with unique names.
  (let ((relabelings
          (loop :for instr :in quil-block
                :when (jump-target-p instr)
                  :collect (let ((name (label-name (jump-target-label instr))))
                             (when (assoc name alist :test #'string=)
                               (quil-expansion-error "Duplicate label ~S" name))
                             (cons name (genlabel name)))
                    :into alist
                :finally (return alist))))
    ;; REPLACEMENT is a way to access RELABELINGS, and
    ;; RELABEL-INSTRUCTION will actually relabel the relevant
    ;; (pseudo-)instructions.
    (labels ((replacement (label)
               (cdr (assoc (label-name label)
                           relabelings
                           :test #'string=)))

             (relabel-instruction (instr)
               (cond
                 ((typep instr 'jump)
                  (let ((replacement (replacement (jump-label instr))))
                    (cond
                      ((null replacement)
                       instr)
                      (t
                       (let ((copy (copy-instance instr)))
                         (setf (jump-label copy) replacement)
                         copy)))))

                 ((jump-target-p instr)
                  (make-instance 'jump-target :label (replacement (jump-target-label instr))))

                 (t
                  instr))))
      (mapcar #'relabel-instruction quil-block))))

(defun instantiate-definition-body (defn body defn-params params defn-args args)
  "Returns a list of instructions which are the instructions in the BODY of DEFN instantiated with PARAMS and ARGS for DEFN-PARAMs and DEFN-ARGS respectively. Does not mutate its input."
  (assert (= (length params)
             (length defn-params)))
  (assert (= (length args)
             (length defn-args)))

  (let ((*expansion-depth* (1+ *expansion-depth*))
        (body (mapcar #'copy-instance body)))
    (unless (<= *expansion-depth* *expansion-limit*)
      (quil-expansion-error
       "Exceeded recursion limit of ~D. Current object being expanded is ~A."
       *expansion-limit*
       defn))
    (labels
        ((param-value (param)
           (if (not (is-param param))
               param
               (let ((pos (position (param-name param) defn-params
                                    :key #'param-name
                                    :test #'string-equal)))
                 (when (null pos)
                   (quil-expansion-error "No defined parameter named ~S" param))
                 (elt params pos))))
         (arg-value (arg)
           (if (not (is-formal arg))
               arg
               (let ((pos (position (formal-name arg) defn-args
                                    :key #'formal-name
                                    :test #'string-equal)))
                 (when (null pos)
                   (quil-expansion-error "No argument named ~S" (formal-name arg)))
                 (elt args pos))))
         (instantiate (instr)
           (a:ensure-list
            (instantiate-instruction instr #'param-value #'arg-value))))
      (mapcan #'instantiate
              (relabel-block-labels-uniquely body)))))

(defun substitute-parameter (param-value)
  "Given a function PARAM-VALUE to compute the value of a PARAM object, return a function which takes either a PARAM, DELAYED-EXPRESSION, or CONSTANT and computes it's numerical value."
  (lambda (param)
    (cond
      ((is-param param)
       (funcall param-value param))
      ((delayed-expression-p param)
       (map-de-params (substitute-parameter param-value) param))
      ((is-constant param)
       param)
      (t
       (error 'type-error :datum param
                          :expected-type '(or param delayed-expression constant))))))

(defun unitary-instruction-p (instr)
  "Is the instruction INSTR unitary?

An instruction is unitary if it is of type APPLICATION, whether that be INSTR itself or, if INSTR is a CIRCUIT-APPLICATION, the body of the circuit. With the caveat that UNRESOLVED-APPLICATIONS must be explicitly allowed by setting *ALLOW-UNRESOLVED-APPLICATIONS* to T."
  (etypecase instr
    (gate-application
     t)
    (circuit-application
     (every #'unitary-instruction-p
            (circuit-definition-body (circuit-application-definition instr))))
    (unresolved-application
     (unless *allow-unresolved-applications*
       (error "Unable to determine if the unresolved application is unitary:~%    ~/cl-quil:instruction-fmt/"
              instr))
     t)))

(defun transform-if (test transform)
  "Given a unary function TRANSFORM, return a new function which either applies TRANSFORM or does not, depending on whether TEST passes."
  (lambda (obj)
    (if (funcall test obj)
        (funcall transform obj)
        obj)))

;;; To REMAKE or not to REMAKE
;;; that is the question....
;;;
;;; Basically, in circuit or calibration bodies some instructions might depend
;;; on circuit parameters or arguments. For those, instantiation necessarily
;;; must make a new instruction (since this may happen more than once). However,
;;; in the absence of this we can avoid allocation and just return the original.
;;; Thus in INSTANTIATE-INSTRUCTION below that we keep careful track of when
;;; substition has actually occurred.

;;; The macro FLAG-ON-UPDATE is just a helper for the kind of bookkeeping involved.
(defmacro flag-on-update (flag op)
  "Given a unary function OP, lift to a function which applies OP and sets FLAG to T if the result differs from the input."
  (a:with-gensyms (x new-x)
    `(lambda (,x)
       (let ((,new-x (funcall ,op ,x)))
         (unless (eq ,x ,new-x)
           (setf ,flag t))
         ,new-x))))

(defgeneric instantiate-instruction (instr param-value arg-value)
  (:documentation "Given an instruction INSTR possibly with formal parameters/variables, instantiate it with the proper parameter/argument values provided by the unary functions PARAM-VALUE and ARG-VALUE, which take PARAM and FORMAL objects respectively as arguments. Return the instruction or a list of instructions as a result.")
  (:method ((instr circuit-application) param-value arg-value)
    (let ((params (mapcar (substitute-parameter param-value)
                          (application-parameters instr)))
          (args (mapcar (transform-if #'is-formal arg-value)
                        (application-arguments instr))))
      (cond
        ((simple-dagger-operator-p (application-operator instr))
         (let ((instrs (instantiate-instruction (circuit-application-definition instr)
                                                params
                                                args)))
           (dolist (instr instrs)
             (unless (unitary-instruction-p instr)
               (error "DAGGER cannot be applied to the impure instruction ~/cl-quil:instruction-fmt/"
                      instr))
             (setf (application-operator instr)
                   (involutive-dagger-operator (application-operator instr))))
           ;; The Hermitian transpose reverses the order of operator
           ;; applications
           (setf instrs (nreverse instrs))))
        ((simple-controlled-operator-p (application-operator instr))
         (let ((arguments (arguments instr)))
           (when (endp arguments)
             (error "CONTROLLED requires at least one control qubit in ~/cl-quil:instruction-fmt/." instr))

           (destructuring-bind (control-qubit . target-qubits) arguments
             (when (member control-qubit target-qubits :test #'argument=)
               (error "CONTROLLED cannot be applied when the control qubit is among the target qubits ~
                       in ~/cl-quil:instruction-fmt/" instr))

             (let ((definition (circuit-application-definition instr)))
               ;; Check that the instructions in the body of a given circuit
               ;; do not involve the control qubit.
               (dolist (x (circuit-definition-body definition))
                 (when (member control-qubit (arguments x) :test #'argument=)
                   (error "CONTROLLED circuit cannot have the control qubit used as a target qubit ~
                           within the circuit named ~A." (circuit-definition-name definition))))

               ;; The effect of controlling a 2^{n-1} unitary matrix U with
               ;; the most significant qubit being the control qubit is the
               ;; same as applying the matrix I ⊕ U, where I is the 2^{n-1} ×
               ;; 2^{n-1} identity matrix. Thus, if U = Uₖ ... U₂ U₁,
               ;; then it follows that
               ;;         I ⊕ U = (I ⊕ Uₖ) ... (I ⊕ U₂) (I ⊕ U₁).
               ;; If the control qubit is not the most significant one, we can
               ;; reduce this to the previous case by applying suitable
               ;; permutation matrices.
               (let ((instrs (instantiate-instruction definition params target-qubits)))
                 (dolist (instr instrs instrs)
                   (unless (unitary-instruction-p instr)
                     (error "CONTROLLED cannot be applied to the impure instruction ~
                            ~/cl-quil:instruction-fmt/" instr))
                   (setf (application-operator instr) (controlled-operator (application-operator instr))
                         (application-arguments instr) (cons control-qubit (arguments instr)))))))))

        ((plain-operator-p (application-operator instr))
         (instantiate-instruction (circuit-application-definition instr)
                                  params
                                  args))
        (t
         (error "Unable to instantiate the modifiers in the complex instruction ~/cl-quil:instruction-fmt/."
                instr)))))

  (:method ((instr application) param-value arg-value)
    (let ((remake nil))
      (let ((params (mapcar (flag-on-update remake
                                            (substitute-parameter param-value))
                            (application-parameters instr)))
            (args (mapcar (flag-on-update remake
                                          (transform-if #'is-formal arg-value))
                          (application-arguments instr))))
        (map-into params
                  (transform-if #'delayed-expression-p #'evaluate-delayed-expression)
                  params)
        (assert (notany (a:conjoin #'is-param #'delayed-expression-p) params))
        (assert (notany #'is-formal args))
        (if (not remake)
            instr
            (let ((copy (copy-instance instr)))
              (setf (application-parameters copy) params)
              (setf (application-arguments copy) args)
              copy)))))

  (:method ((instr jump-target) param-value arg-value)
    instr)

  (:method ((instr no-operation) param-value arg-value)
    instr)

  (:method ((instr halt) param-value arg-value)
    instr)

  (:method ((instr reset) param-value arg-value)
    instr)

  (:method ((instr reset-qubit) param-value arg-value)
    (let ((q (reset-qubit-target instr)))
      (if (not (is-formal q))
          instr
          (let ((new-q (funcall arg-value q)))
            (assert (qubit-p new-q) ()
                    "The formal argument ~A must be substituted with a qubit."
                    (formal-name q))
            (make-instance 'reset-qubit :target new-q)))))

  (:method ((instr wait) param-value arg-value)
    instr)

  (:method ((instr pragma) param-value arg-value)
    instr)

  (:method ((instr unary-classical-instruction) param-value arg-value)
    (let ((addr (classical-target instr)))
      (if (not (is-formal addr))
          instr
          (let ((new-addr (funcall arg-value addr)))
            (assert (or (is-mref addr) (is-formal addr)))
            (unless (< 1 *expansion-depth*)
              (assert (is-mref new-addr) ()
                      "The formal argument ~A must be substituted with an address."
                      (formal-name addr)))
            (make-instance (class-of instr) :target new-addr)))))

  (:method ((instr binary-classical-instruction) param-value arg-value)
    (flet ((transform-address (addr)
             (if (not (is-formal addr))
                 addr
                 (let ((new-addr (funcall arg-value addr)))
                   (assert (or (is-mref addr) (is-formal addr)))
                   (unless (< 1 *expansion-depth*)
                     (assert (is-mref new-addr) ()
                             "The formal argument ~A must be substituted with an address."
                             (formal-name addr)))
                   new-addr))))
      (let ((left (classical-left-operand instr))
            (right (classical-right-operand instr)))
        (if (or (is-formal left) (is-formal right))
            (make-instance (class-of instr) :left (transform-address left)
                                            :right (transform-address right))
            instr))))

  (:method ((instr trinary-classical-instruction) param-value arg-value)
    (flet ((transform-address (addr)
             (if (not (is-formal addr))
                 addr
                 (let ((new-addr (funcall arg-value addr)))
                   (assert (or (is-mref addr) (is-formal addr)))
                   (unless (< 1 *expansion-depth*)
                     (assert (is-mref new-addr) ()
                             "The formal argument ~A must be substituted with an address."
                             (formal-name addr)))
                   new-addr))))
      (let ((addr (classical-target instr))
            (left (classical-left-operand instr))
            (right (classical-right-operand instr)))
        (if (or (is-formal addr) (is-formal left) (is-formal right))
            (make-instance (class-of instr) :target (transform-address addr)
                                            :left (transform-address left)
                                            :right (transform-address right))
            instr))))

  (:method ((instr unconditional-jump) param-value arg-value)
    instr)

  (:method ((instr conditional-jump) param-value arg-value)
    (let ((addr (conditional-jump-address instr)))
      (cond
        ((is-formal addr)
         (make-instance (class-of instr)
                        :label (jump-label instr)
                        :address (funcall arg-value addr)))
        (t
         instr))))

  (:method ((instr measure-discard) param-value arg-value)
    (let ((q (measurement-qubit instr)))
      (if (not (is-formal q))
          instr
          (let ((new-q (funcall arg-value q)))
            (assert (qubit-p new-q) ()
                    "The formal argument ~A must be substituted with a qubit."
                    (formal-name q))
            (make-instance 'measure-discard :qubit new-q)))))

  (:method ((instr measure) param-value arg-value)
    (let ((q (measurement-qubit instr))
          (addr (measure-address instr))
          (remake nil))
      (when (is-formal q)
        (setf remake t)
        (let ((new-q (funcall arg-value q)))
          (assert (qubit-p new-q) ()
                  "The formal argument ~A must be substituted with a qubit."
                  (formal-name q))
          (setf q new-q)))

      (when (is-formal addr)
        (setf remake t)
        (let ((new-addr (funcall arg-value addr)))
          (check-mref new-addr)
          (setf addr new-addr)))

      (if (not remake)
          instr
          (make-instance 'measure :address addr :qubit q))))

  ;; Returns the definition body with parameters and arguments applied.
  (:method ((instr circuit-definition) param-value arg-value)
    (instantiate-definition-body instr
                                 (circuit-definition-body instr)
                                 (circuit-definition-parameters instr)
                                 param-value
                                 (circuit-definition-arguments instr)
                                 arg-value)))
