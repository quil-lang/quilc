(in-package #:cl-quil)

;;; Since both circuit and calibration expansion rely on some common code, but
;;; we would like error messages to be as specific as possible, we choose to
;;; explicitly track an "expansion context".

(deftype expansion-context ()
  '(member nil :DEFCIRCUIT :DEFCAL))

(defvar *expansion-context*)
(setf (documentation '*expansion-context* 'variable)
      "The context for expansion, mainly used for error messages.")

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

(defun destructure-definition (defn)
  "Given a quilt definition DEFN, return three values: a list of the parameters, a list of the formal arguments, and the definition body."
  (etypecase defn
    (circuit-definition (values (circuit-definition-parameters defn)
                                (circuit-definition-arguments defn)
                                (circuit-definition-body defn)))
    (gate-calibration-definition (values (calibration-definition-parameters defn)
                                         (calibration-definition-arguments defn)
                                         (calibration-definition-body defn)))
    (measure-calibration-definition (values nil
                                            (list (measurement-calibration-qubit defn)
                                                  (measure-calibration-address defn))
                                            (calibration-definition-body defn)))
    (measure-discard-calibration-definition (values nil
                                                    (list (measurement-calibration-qubit defn))1
                                                    (calibration-definition-body defn)))))

(defun instantiate-definition (defn params args)
  "Fill in the given definition DEFN with the list of parameter and argument values PARAMS and ARGS."
  (multiple-value-bind (defn-params defn-args defn-body)
      (destructure-definition defn)
    (assert (= (length params)
               (length defn-params)))
    (assert (= (length args)
               (length defn-args)))

    (let ((*expansion-depth* (1+ *expansion-depth*)))
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
                (relabel-block-labels-uniquely defn-body))))))

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
  "Given a unary function TRANSFORM, return a new function which either applies TRANSFORM or does not,
depending on whether TEST passes."
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
  (let ((x (gensym))
        (new-x (gensym)))
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
         (let ((instrs (instantiate-definition (circuit-application-definition instr)
                                               params
                                               args)))
           (dolist (instr instrs)
             (unless (unitary-instruction-p instr)
               (error "DAGGER cannot be applied to the impure instruction ~/quil:instruction-fmt/"
                      instr))
             (setf (application-operator instr)
                   (involutive-dagger-operator (application-operator instr))))
           ;; The Hermitian transpose reverses the order of operator
           ;; applications
           (setf instrs (reverse instrs))))
        ((plain-operator-p (application-operator instr))
         (instantiate-definition (circuit-application-definition instr)
                                 params
                                 args))
        (t
         (error "Unable to instantiate the modifiers in the complex instruction ~/quil:instruction-fmt/."
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

  (:method ((instr simple-frame-mutation) param-value arg-value)
    (let ((frame (instantiate-frame (frame-mutation-target-frame instr)
                                    arg-value))
          (value (ensure-instantiated (frame-mutation-value instr)
                                      arg-value)))
      (if (and (frame= frame (frame-mutation-target-frame instr))
               (constant= value (frame-mutation-value instr)))
          instr
          (make-instance (class-of instr)
                         :frame frame
                         :value value))))

  (:method ((instr pulse) param-value arg-value)
    (let ((frame (instantiate-frame (pulse-frame instr)
                                    arg-value)))
      (if (frame= frame (pulse-frame instr))
          instr
          (make-instance 'pulse
                         :frame frame
                         :waveform (pulse-waveform instr)
                         :nonblocking (nonblocking-p instr)))))

  (:method ((instr capture) param-value arg-value)
    (let ((frame (instantiate-frame (capture-frame instr)
                                    arg-value))
          (memory-ref (ensure-instantiated (capture-memory-ref instr)
                                           arg-value)))
      (check-mref memory-ref)
      (if (and (frame= frame (capture-frame instr))
               (memory-ref= memory-ref (capture-memory-ref instr)))
          instr
          (make-instance 'capture
                         :frame frame
                         :waveform (capture-waveform instr)
                         :memory-ref memory-ref
                         :nonblocking (nonblocking-p instr)))))

  (:method ((instr raw-capture) param-value arg-value)
    (let ((frame (instantiate-frame (raw-capture-frame instr)
                                    arg-value))
          (memory-ref (ensure-instantiated (raw-capture-memory-ref instr)
                                           arg-value))
          (duration (ensure-instantiated (raw-capture-duration instr)
                                         arg-value)))
      (check-mref memory-ref)
      (if (and (frame= frame (raw-capture-frame instr))
               (memory-ref= memory-ref (raw-capture-memory-ref instr))
               (constant= duration (raw-capture-duration instr)))
          instr
          (make-instance 'raw-capture
                         :frame frame
                         :duration duration
                         :memory-ref memory-ref
                         :nonblocking (nonblocking-p instr)))))

  (:method ((instr delay-on-qubits) param-value arg-value)
    (let ((duration (ensure-instantiated (delay-duration instr)
                                         arg-value)))
      (if (and (constant= duration (delay-duration instr))
               (not (some #'is-formal (delay-qubits instr))))
          instr
          (make-instance 'delay-on-qubits
                         :duration duration
                         :qubits (mapcar (transform-if #'is-formal arg-value)
                                         (delay-qubits instr))))))

  (:method ((instr delay-on-frames) param-value arg-value)
    (let* ((remake nil)
           (duration (ensure-instantiated (delay-duration instr)
                                          arg-value))
           (frames (mapcar (flag-on-update remake
                                           (lambda (f) (instantiate-frame f arg-value)))
                           (delay-frames instr))))
      (if (and (constant= duration (delay-duration instr))
               (not remake))
          instr
          (make-instance 'delay-on-frames
                         :duration duration
                         :frames frames))))

  (:method ((instr fence) param-value arg-value)
    (let* ((remake nil)
           (qubits (mapcar (flag-on-update remake
                                           (lambda (q) (ensure-instantiated q arg-value))) 
                           (fence-qubits instr))))
      (if remake
          (make-instance 'fence :qubits qubits)
          instr))))

(defun ensure-instantiated (obj arg-value)
  (if (is-formal obj)
      (funcall arg-value obj)
      obj))

(defun instantiate-frame (frame arg-value)
  "Instantiate FRAME with respect to the unary function ARG-VALUE, constructing a new frame if needed."
  (let* ((remake nil)
         (qubits (mapcar (flag-on-update remake
                                         (lambda (q) (ensure-instantiated q arg-value)))
                         (frame-qubits frame))))
    (if remake
        (let ((instantiated (frame qubits (frame-name frame))))
          (setf (frame-name-resolution instantiated)
                (frame-name-resolution frame))
          instantiated)
        frame)))
