;;;; analysis/expand-circuits.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

(define-transform expand-circuits (expand-circuits)
  "This transform expands all circuits to create a circuit-free program."
  process-includes
  resolve-applications)

(defun relabel-circuit-labels-uniquely (circuit-body)
  "Non-destructively relabel all jump targets and associated JUMP-like instructions in the list of instructions CIRCUIT-BODY."
  ;; Compute an alist mapping label name (string) to fresh LABEL
  ;; objects with unique names.
  (let ((relabelings
          (loop :for instr :in circuit-body
                :when (jump-target-p instr)
                  :collect (let ((name (label-name (jump-target-label instr))))
                             (when (assoc name alist :test #'string=)
                               (quil-parse-error "Duplicate label ~S in DEFCIRCUIT"
                                                 name))
                             (list name (genlabel name)))
                    :into alist
                :finally (return alist))))
    ;; REPLACEMENT is a way to access RELABELINGS, and
    ;; RELABEL-INSTRUCTION will actually relabel the relevant
    ;; (pseudo-)instructions.
    (labels ((replacement (label)
               ;; ASSOC may return NIL. Using SECOND is intentional
               ;; since (SECOND NIL) => NIL.
               (second (assoc (label-name label)
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
      (mapcar #'relabel-instruction circuit-body))))

(defparameter *circuit-expansion-limit* 256
  "Limit on the number of recursive circuit expansions that can happen before erroring. Intended to avoid infinite loops.")
(defvar *circuit-expansion-depth*)

(defun instantiate-circuit (circdef params args)
  "Fill in the CIRCUIT-DEFINITION CIRCDEF with the list of parameter and argument values PARAMS and ARGS."
  (assert (= (length params)
             (length (circuit-definition-parameters circdef))))
  (assert (= (length args)
             (length (circuit-definition-arguments circdef))))

  (let ((*circuit-expansion-depth* (if (boundp '*circuit-expansion-depth*)
                                       (1+ *circuit-expansion-depth*)
                                       1)))
    (assert (<= *circuit-expansion-depth* *circuit-expansion-limit*)
            ()
            "Exceeded recursion limit of ~D for circuit expansion. ~
             Current circuit being expanded is ~A."
            *circuit-expansion-limit*
            circdef)
    (labels
        ((param-value (param)
           (if (not (is-param param))
               param
               (let ((pos (position (param-name param) (circuit-definition-parameters circdef)
                                    :key #'param-name
                                    :test #'string-equal)))
                 (when (null pos)
                   (error "No defined parameter named ~S" param))
                 (elt params pos))))
         (arg-value (arg)
           (if (not (is-formal arg))
               arg
               (let ((pos (position (formal-name arg) (circuit-definition-arguments circdef)
                                    :key #'formal-name
                                    :test #'string-equal)))
                 (when (null pos)
                   (error "No argument named ~S" (formal-name arg)))
                 (elt args pos))))
         (instantiate (instr)
           (let ((x (instantiate-instruction instr #'param-value #'arg-value)))
             (if (listp x)
                 x
                 (list x)))))
      (mapcan #'instantiate
              (relabel-circuit-labels-uniquely
               (circuit-definition-body circdef))))))

(defun substitute-parameter (param-value)
  "Given a function PARAM-VALUE to compute the value of a PARAM object, return a function which takes either a PARAM, DELAYED-EXPRESSION, or CONSTANT and computes it's numerical value. Should always"
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

An instruction is unitary if it is of type APPLICATION, whether that
be INSTR itself or, if INSTR is a CIRCUIT-APPLICATION, the body of the
circuit. With the caveat that UNRESOLVED-APPLICATIONS must be
explicitly allowed by setting *ALLOW-UNRESOLVED-APPLICATIONS* to T."
  (etypecase instr
    (gate-application
     t)
    (circuit-application
     (every #'unitary-instruction-p
            (circuit-definition-body (circuit-application-definition instr))))
    (unresolved-application
     (unless *allow-unresolved-applications*
       (error "Unable to determine if the unresolved application is unitary:~%    ~A"
              (print-instruction instr nil)))
     t)))

(defgeneric instantiate-instruction (instr param-value arg-value)
  (:documentation "Given an instruction INSTR possibly with formal parameters/variables, instantiate it with the proper parameter/argument values provided by the unary functions PARAM-VALUE and ARG-VALUE, which take PARAM and FORMAL objects respectively as arguments. Return the instruction or a list of instructions as a result.")
  (:method ((instr circuit-application) param-value arg-value)
    (labels ((sub (test lookup things)
               (loop :for thing :in things
                     :if (funcall test thing)
                       :collect (funcall lookup thing)
                     :else
                       :collect thing)))
      (let ((params (sub (constantly t) (substitute-parameter param-value) (application-parameters instr)))
            (args   (sub #'is-formal arg-value (application-arguments instr))))
        (cond
          ((simple-dagger-operator-p (application-operator instr))
           (let ((instrs (instantiate-circuit (circuit-application-definition instr)
                                              params
                                              args)))
             (dolist (instr instrs)
               (unless (unitary-instruction-p instr)
                 (error "DAGGER cannot be applied to the impure instruction ~S"
                        (print-instruction instr nil)))
               (setf (application-operator instr)
                     (involutive-dagger-operator (application-operator instr))))
             ;; The Hermitian transpose reverses the order of operator
             ;; applications
             (setf instrs (reverse instrs))))
          ((plain-operator-p (application-operator instr))
           (instantiate-circuit (circuit-application-definition instr)
                                params
                                args))
          (t
           (error "Unable to instantiate the modifiers in the complex instruction ~S."
                  (print-instruction instr nil)))))))

  (:method ((instr application) param-value arg-value)
    (let ((remake nil))
      (flet ((sub (test lookup things)
               (loop :for thing :in things
                     :if (funcall test thing)
                       :collect (progn (setf remake t) (funcall lookup thing))
                     :else
                       :collect thing)))
        (let ((params (sub (constantly t) (substitute-parameter param-value) (application-parameters instr)))
              (args (sub #'is-formal arg-value (application-arguments instr))))
          (setf remake t)
          (map-into params (lambda (p)
                             (if (delayed-expression-p p)
                                 (evaluate-delayed-expression p)
                                 p))
                    params)
          (assert (notany (a:conjoin #'is-param #'delayed-expression-p) params))
          (assert (notany #'is-formal args))
          (if (not remake)
              instr
              (let ((copy (copy-instance instr)))
                (setf (application-parameters copy) params)
                (setf (application-arguments copy) args)
                copy))))))

  (:method ((instr jump-target) param-value arg-value)
    (declare (ignore param-value arg-value))
    instr)

  (:method ((instr no-operation) param-value arg-value)
    (declare (ignore param-value arg-value))
    instr)

  (:method ((instr halt) param-value arg-value)
    (declare (ignore param-value arg-value))
    instr)

  (:method ((instr reset) param-value arg-value)
    (declare (ignore param-value arg-value))
    instr)

  (:method ((instr reset-qubit) param-value arg-value)
    (declare (ignore param-value))
    (let ((q (reset-qubit-target instr)))
      (if (not (is-formal q))
          instr
          (let ((new-q (funcall arg-value q)))
            (assert (qubit-p new-q) ()
                    "The formal argument ~A must be substituted with a qubit."
                    (formal-name q))
            (make-instance 'reset-qubit :target new-q)))))

  (:method ((instr wait) param-value arg-value)
    (declare (ignore param-value arg-value))
    instr)

  (:method ((instr pragma) param-value arg-value)
    (declare (ignore param-value arg-value))
    instr)

  (:method ((instr unary-classical-instruction) param-value arg-value)
    (declare (ignore param-value))
    (let ((addr (classical-target instr)))
      (if (not (is-formal addr))
          instr
          (let ((new-addr (funcall arg-value addr)))
            (assert (or (is-mref addr) (is-formal addr)))
            (unless (< 1 *circuit-expansion-depth*)
              (assert (is-mref new-addr) ()
                      "The formal argument ~A must be substituted with an address."
                      (formal-name addr)))
            (make-instance (class-of instr) :target new-addr)))))

  (:method ((instr binary-classical-instruction) param-value arg-value)
    (declare (ignore param-value))
    (flet ((transform-address (addr)
             (if (not (is-formal addr))
                 addr
                 (let ((new-addr (funcall arg-value addr)))
                   (assert (or (is-mref addr) (is-formal addr)))
                   (unless (< 1 *circuit-expansion-depth*)
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
    (declare (ignore param-value))
    (flet ((transform-address (addr)
             (if (not (is-formal addr))
                 addr
                 (let ((new-addr (funcall arg-value addr)))
                   (assert (or (is-mref addr) (is-formal addr)))
                   (unless (< 1 *circuit-expansion-depth*)
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
    (declare (ignore param-value arg-value))
    instr)

  (:method ((instr conditional-jump) param-value arg-value)
    (declare (ignore param-value))
    (let ((addr (conditional-jump-address instr)))
      (cond
        ((is-formal addr)
         (make-instance (class-of instr)
                        :label (jump-label instr)
                        :address (funcall arg-value addr)))
        (t
         instr))))

  (:method ((instr measure-discard) param-value arg-value)
    (declare (ignore param-value))
    (let ((q (measurement-qubit instr)))
      (if (not (is-formal q))
          instr
          (let ((new-q (funcall arg-value q)))
            (assert (qubit-p new-q) ()
                    "The formal argument ~A must be substituted with a qubit."
                    (formal-name q))
            (make-instance 'measure-discard :qubit new-q)))))

  (:method ((instr measure) param-value arg-value)
    (declare (ignore param-value))
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
          (make-instance 'measure :address addr :qubit q)))))

(defun expand-circuits (parsed-program)
  "Mutate the code slot of PARSED-PROGRAM with all known circuits recursively expanded."
  (flet ((always-error (x)
           (declare (ignore x))
           (error "Not in a circuit expansion context.")))
    (let* ((flat-instrs
             (loop :for instr :across (parsed-program-executable-code parsed-program)
                   :for instantiated := (instantiate-instruction instr #'always-error #'always-error)
                   :if (listp instantiated)
                     :append instantiated
                   :else
                     :collect instantiated)))
      (setf (parsed-program-executable-code parsed-program)
            (coerce flat-instrs 'vector))
      parsed-program)))
