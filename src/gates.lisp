;;;; gates.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil/frontend)

;;;;;;;;;;;;;;;;;;;;;;;;;;; Gate Protocol ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Every gate represents a unitary operation. As such, every gate
;;; should be representable as a fully dense matrix acting on a
;;; particular qubit subsystem. This function accesses/constructs that
;;; representation.
;;;
;;; We don't implement the gate protocol on GATE-DEFINITION instances
;;; to avoid errors and costly performance penalties.

(defgeneric gate-matrix (gate &rest parameters)
  (:documentation "Get a MAGICL matrix representation of the gate, given the set of parameters PARAMETERS.")
  (:method ((gate magicl:matrix) &rest parameters)
    (assert (endp parameters))
    gate))

(defgeneric gate-dimension (gate)
  (:documentation "The dimension of the space that the gate acts on.")
  (:method ((gate magicl:matrix))
    (assert (magicl:square-matrix-p gate))
    (magicl:nrows gate)))


(define-condition unknown-gate-parameter (error)
  ((gate :initarg :gate))
  (:documentation "The value of the gate parameter could not be eagerly resolved."))

;;; N.B. Since this uses GATE-DEFINITION-TO-GATE, it may be
;;; inefficient. For simulation, it's preferable to do a different
;;; computation rather than relying on this functionality.
(defmethod gate-matrix ((gate gate-application) &rest parameters)
  (assert (endp parameters))
  (labels ((check-parameters (gate)
             (unless (every #'is-constant (application-parameters gate))
               (error 'unknown-gate-parameter :gate gate)))
           (recurse (od parameters)
             (adt:match operator-description od
               ((named-operator _)
                ;; We've already resolved the gate name.
                (check-parameters gate)
                (apply #'gate-matrix
                       (gate-definition-to-gate (gate-application-resolution gate))
                       (mapcar #'constant-value (application-parameters gate))))
               ((controlled-operator o)
                (let ((summand (recurse o parameters)))
                  (magicl:direct-sum
                   (eye (gate-dimension summand) :type 'double-float)
                   summand)))
               ((dagger-operator o)
                (magicl:dagger (recurse o parameters)))
               ((forked-operator o)
                (let* ((parameter-count (length parameters))
                       (left-summand (recurse o (subseq parameters 0 (/ parameter-count 2))))
                       (right-summand (recurse o (subseq parameters (/ parameter-count 2)))))
                  (magicl:direct-sum left-summand right-summand))))))
    (cond ((slot-boundp gate 'gate)
           (check-parameters gate)
           (apply #'gate-matrix (gate-application-gate gate)
                  (mapcar #'constant-value (application-parameters gate))))
          (t
           (recurse (application-operator gate) (application-parameters gate))))))


;;;;;;;;;;;;;;;;;;;;;; Gate Object Definitions ;;;;;;;;;;;;;;;;;;;;;;;

;;; These are alternative algebraic representations of gates that
;;; might make analysis more suitable. Each of these can always be
;;; made into a matrix with GATE-MATRIX.

(defclass gate ()
  (;; TODO move name into the gate-definition class?
   ;;
   ;; Eric sez that the name is useful for tracking anonymous gates.
   (name :initarg :name
         :reader gate-name
         :type (or null string)
         :documentation "The name of the gate."))
  (:metaclass abstract-class)
  (:default-initargs :name nil)
  (:documentation "Abstract class for gates."))

(defclass static-gate (gate)
  ()
  (:metaclass abstract-class)
  (:documentation "An abstract class representing gates which could be represented as a static matrix."))

(defclass dynamic-gate (gate)
  ((arity :initarg :arity
          :reader dynamic-gate-arity
          :documentation "The number of parameters the gate requires."))
  (:metaclass abstract-class)
  (:documentation "An abstract class representing gates which are a function of some collection of numerical parameters."))

(defmethod dynamic-gate-arity ((gate static-gate))
  0)

(defclass simple-gate (static-gate)
  ((matrix :initarg :matrix
           :reader simple-gate-matrix
           :documentation "The matrix of the gate."))
  (:documentation "Non-parameterized gate represented as a dense matrix."))

(defmethod initialize-instance :after ((gate simple-gate) &key)
  (with-slots (matrix) gate
    (assert (and (magicl:square-matrix-p matrix)
                 (positive-power-of-two-p (magicl:nrows matrix)))
            (matrix)
            "MATRIX must be square whose dimension is a positive power of two.~@
             Actual dimensions are ~D x ~D."
            (magicl:nrows matrix)
            (magicl:ncols matrix))))

(defmethod gate-dimension ((gate simple-gate))
  (magicl:nrows (simple-gate-matrix gate)))

(defmethod gate-matrix ((gate simple-gate) &rest parameters)
  (assert (null parameters))
  (simple-gate-matrix gate))

(defclass permutation-gate (static-gate)
  ((permutation :initarg :permutation
                :reader permutation-gate-permutation
                :documentation "The permutation representation of the gate."))
  (:documentation "A gate which could be realized as a permutation matrix."))

(defmethod initialize-instance :after ((gate permutation-gate) &key)
  (with-slots (permutation) gate
    (assert (positive-power-of-two-p (length permutation))
            (permutation)
            "PERMUTATION length must be a positive power of two. ~@
             PERMUTATION ~A has length ~D which is not a power of two."
            permutation (length permutation))
    (check-permutation permutation)))

(defmethod gate-dimension ((gate permutation-gate))
  (length (permutation-gate-permutation gate)))

(defmethod gate-matrix ((gate permutation-gate) &rest parameters)
  (assert (null parameters))
  (let* ((d (gate-dimension gate))
         (m (zeros (list d d))))
    (map nil (let ((row -1))
               (lambda (col)
                 (setf (magicl:tref m (incf row) col) #C(1.0d0 0.0d0))))
         (permutation-gate-permutation gate))
    m))

(defun check-permutation (perm)
  "Check that PERM is a valid permutation. Error if it's not."
  (let* ((n (length perm))
         (bits (make-array n :element-type 'bit :initial-element 0)))
    (flet ((mark-bit (i)
             (cond
               ((not (<= 0 i (1- n)))
                (error "Invalid permutation ~A. Entry out of range [0, ~A): ~A" perm n i))
               ((zerop (sbit bits i)) (setf (sbit bits i) 1))
               (t (error "Invalid permutation ~A. Found duplicate entry: ~A" perm i)))))
      (mapc #'mark-bit perm))))

(defun make-permutation-gate (name &rest permutation)
  "Make a permutation gate from the given PERMUTATION."
  (make-instance
   'permutation-gate
   :name name
   :permutation permutation))

(defclass parameterized-gate (dynamic-gate)
  ((dimension :initarg :dimension
              :reader gate-dimension
              :documentation "The minimal dimension of the space the gate acts on.")
   (matrix-function :initarg :matrix-function
                    :reader parameterized-gate-matrix-function
                    :writer (setf %parameterized-gate-matrix-function)
                    :documentation "Function mapping ARITY complex numbers to a DIMENSION x DIMENSION MAGICL matrix."))
  (:documentation "A gate parameterized by complex numbers."))

(defmethod initialize-instance :after ((gate parameterized-gate) &key)
  (with-slots (dimension) gate
    (assert (positive-power-of-two-p dimension)
            (dimension)
            "DIMENSION ~D must be a positive power of two."
            dimension)))

(defmethod gate-matrix ((gate parameterized-gate) &rest parameters)
  (apply (parameterized-gate-matrix-function gate) parameters))




;;; SEQUENCE GATE
;;;  
(defclass sequence-gate (gate)
    ((definition :initarg :definition
                 :reader sequence-gate-gate-definition))
  (:documentation "A gate that is a sequence of other gates, note that there's no practical difference between a sequence-gate and a sequence-gate-definition."))


(defun instantiate-sequence-gate (seq-gate parameters arguments)
  "Expands a sequence gate, parameters, and arguments into a list of gate applications."
  (let ((seq-gate-def (sequence-gate-gate-definition seq-gate)))
    (let* ((lookup-arg (make-map-list-to-list (sequence-gate-definition-arguments seq-gate-def) arguments #'formal-name))
           (lookup-param (make-map-list-to-list (sequence-gate-definition-parameters seq-gate-def) parameters #'param-name))
           (instantiated-list nil))
      (loop :for gate-app :in (reverse (sequence-gate-definition-sequence seq-gate-def))
            :do (setf instantiated-list (cons (instantiate-gate-application gate-app lookup-arg lookup-param) instantiated-list)))
      instantiated-list)))

(defun instantiate-gate-sequence-application (seq-gate-application)
  "Expands an instantiation of a sequence gate into a list of gate applications."
  (instantiate-sequence-gate  (gate-application-resolution seq-gate-application)
                              (application-parameters seq-gate-application)
                              (application-arguments seq-gate-application)))

(defun instantiate-gate-application (gate-app lut-args lut-params)
  (setf gate-app (copy-instance gate-app)
        (application-parameters gate-app) (instantiate-from-param-name (application-parameters gate-app) lut-params)
        (application-arguments gate-app) (instantiate-from-formal-name (application-arguments gate-app) lut-args))
  gate-app)

(defun instantiate-from-formal-name (list-names lut-names)
  (mapcar (lambda (name) (gethash (formal-name name) lut-names)) list-names))

(defun instantiate-from-param-name (list-names lut-names)
  (mapcar (lambda (name) (gethash (param-name name) lut-names)) list-names))

(defun make-map-list-to-list (lista listb apply-to-key)
  "evaluates apply-to-key on elements of lista and maps them to listb by index, result of apply-to-key must be string"
  (let ((return-table (make-hash-table :test 'equal)))
    (loop :for key :in lista
          :for val :in listb
          :do (setf (gethash (funcall apply-to-key key) return-table) val))
    return-table))

(defmethod gate-matrix ((gate sequence-gate) &rest parameters)
  (let ((n (gate-definition-qubits-needed (sequence-gate-gate-definition gate))))
    (simple-gate-matrix
     (gate-application-gate
      (premultiply-gates
       (append
        ;; premultiply gates will not be aware of unused qubits, which
        ;; will cause the dimension of the resulting gate-application
        ;; to not contain qubits that might be arguments of the
        ;; sequence gate
        (loop :for i :below n :collect (build-gate "I" () i))
        (instantiate-sequence-gate
         gate
         (mapcar #'constant parameters)
         (loop :for i :from (1- n) :downto 0 :collect (qubit i)))))))))

(defmethod gate-dimension ((gate sequence-gate))
  (expt 2 (length (sequence-gate-definition-arguments (sequence-gate-gate-definition gate)))))

;;; END SEQUENCE GATE

(defclass exp-pauli-sum-gate (parameterized-gate)
  ((parameters :initarg :parameters
               :reader exp-pauli-sum-gate-parameters)
   (arguments :initarg :arguments
              :reader exp-pauli-sum-gate-arguments)
   (terms :initarg :terms
          :reader exp-pauli-sum-gate-terms))
  (:documentation "A gate specified by the exponentiation of a weighted sum of Paulis.

The Pauli sum is recorded as a list of PAULI-TERM objects, stored in the TERMS slot, each of which is made up of a phase factor, a string of Pauli symbols (I, X, Y, Z), and an ordered list of qubit formals to which these symbols are applied.  The qubit formals are those appearing in the ARGUMENTS slot, which ultimately get substituted with the arguments appearing in a GATE-APPLICATION tagged with this gate definition.  Similarly, PARAMETERS is populated with a list of formals on which the Pauli phases can depend, and these are ultimately substituted with the parameters appearing in a GATE-APPLICATION tagged with this gate definition."))

(defmethod initialize-instance :after ((gate exp-pauli-sum-gate) &key)
  (with-slots (parameters arguments terms) gate
    (let ((size (expt 2 (length arguments))))
      (flet ((matrix-function (&rest params)
               (assert (= (length parameters) (length params)))
               (magicl:expih (magicl:scale
                              (reduce (lambda (m term)
                                        (magicl:.+ m (pauli-term->matrix term arguments params parameters)))
                                      terms
                                      :initial-value (zeros (list size size)))
                              -1))))
        (setf (%parameterized-gate-matrix-function gate) #'matrix-function)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Gate Operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Controlled Gates

(defclass controlled-gate ()
  ((target :initarg :target
           :reader target
           :documentation "The targeted gate of a single qubit control.")))

(defmethod gate-matrix ((gate controlled-gate) &rest parameters)
  (let ((target (target gate)))
    (magicl:direct-sum (eye (gate-dimension target))
                       (apply #'gate-matrix target parameters))))

(defmethod gate-dimension ((gate controlled-gate))
  (* 2 (gate-dimension (target gate))))

(defgeneric control-gate (target)
  (:documentation "Construct a controlled gate out of TARGET. (This representation may be more efficient than an instance of CONTROLLED-GATE.")
  (:method ((target t))
    (make-instance 'controlled-gate :target target))
  (:method ((target permutation-gate))
    (let* ((permutation (permutation-gate-permutation target))
           (size (length permutation)))
      (make-instance 'permutation-gate
                     :permutation (append (a:iota size)
                                          (mapcar (lambda (j) (+ j size)) permutation))))))

;;; Forked Gates

(defclass forked-gate ()
  ((target :initarg :target
           :reader target
           :documentation "The targeted gate of a single qubit \"fork\", where a \"fork\" is a special case of multiplexing where the same gate is used to form both blocks, with different parameters passed to the gate constructor.")))

(defmethod gate-matrix ((gate forked-gate) &rest parameters)
  (let* ((target (target gate))
         (length (/ (length parameters) 2))
         (first-half (subseq parameters 0 length))
         (second-half (subseq parameters length)))
    (magicl:direct-sum (apply #'gate-matrix target first-half)
                       (apply #'gate-matrix target second-half))))

(defmethod gate-dimension ((gate forked-gate))
  (* 2 (gate-dimension (target gate))))

(defgeneric fork-gate (target)
  (:documentation "Construct a forked gate out of TARGET.")
  (:method ((target t))
    (make-instance 'forked-gate :target target)))

;;; Daggered Gates

(defclass dagger-gate ()
  ((target :initarg :target
           :reader target
           :documentation "The gate being daggered.")))

(defmethod gate-matrix ((gate dagger-gate) &rest parameters)
  (let ((target (target gate)))
    (magicl:dagger (apply #'gate-matrix target parameters))))

(defmethod gate-dimension ((gate dagger-gate))
  (gate-dimension (target gate)))

(defgeneric dagger-gate (target)
  (:documentation "Construct a daggered gate out of TARGET. (This representation may be more efficient than an instance of DAGGER-GATE.")
  (:method ((target t))
    (make-instance 'dagger-gate :target target))
  (:method ((target permutation-gate))
    (let ((permutation (permutation-gate-permutation target)))
      (make-instance
       'permutation-gate
       :permutation (loop :for i :below (length permutation)
                          ;; Simple inversion. Could be done in linear
                          ;; time with an array or CL-PERMUTATION.
                          :for inv-i := (position i permutation :test #'=)
                          :collect inv-i)))))

;;; Taking gates and lifting them to CONTROLLED/DAGGER gates

(defun operator-description-gate-lifter (descr)
  "Given an OPERATOR-DESCRIPTION DESCR, return a unary function that takes a gate, and returns said gate lifted according to the description (i.e., has all proper gate modifiers applied). This may return any manner of GATE-like object, including ones that are optimized in their representation."
  (adt:match operator-description descr
    ((named-operator _)
     (lambda (gate) gate))
    ((controlled-operator od)
     (a:compose #'control-gate (operator-description-gate-lifter od)))
    ((dagger-operator od)
     (a:compose #'dagger-gate (operator-description-gate-lifter od)))
    ((forked-operator od)
     (a:compose #'fork-gate (operator-description-gate-lifter od)))))

(defmethod gate-application-gate ((app gate-application))
  (funcall (operator-description-gate-lifter (application-operator app))
           (gate-definition-to-gate
            (gate-application-resolution app))))

;;;;;;;;;;;;;;;;;;;;;; Default Gate Definitions ;;;;;;;;;;;;;;;;;;;;;;

;;; This will be initialized to a table of default gate definitions in
;;; initialize-standard-gates.lisp
(defvar **default-gate-definitions**)

(defun standard-gate-names ()
  "Query for the list of standard Quil gate names."
  (loop :for k :being :the :hash-keys :of **default-gate-definitions**
        :collect k))

(defun lookup-standard-gate (gate-name)
  "Lookup the gate named by GATE-NAME in the collection of *standard* Quil gates. Return NIL if non-standard."
  (check-type gate-name a:string-designator)
  (values (gethash (string gate-name) **default-gate-definitions**)))

;;;;;;;;;;;;;; Conversion of GATE-DEFINITIONs to GATEs ;;;;;;;;;;;;;;;

;;; These are convenient default translations from GATE-DEFINITIONs to
;;; GATEs. However, something like the QVM might prefer to create a
;;; more advanced representation of a gate, or perform further
;;; analysis on the gate.

(defgeneric gate-definition-to-gate (gate-def)
  (:documentation "Convert a parsed Quil gate definition to a usable, executable gate.")
  (:method :around ((gate-def gate-definition))
    (a:if-let ((gate (%gate-definition-cached-gate gate-def)))
      gate
      (setf (%gate-definition-cached-gate gate-def) (call-next-method)))))

(defmethod gate-definition-to-gate ((gate-def static-gate-definition))
  (let* ((name (gate-definition-name gate-def))
         (entries (gate-definition-entries gate-def))
         (dim (isqrt (length entries))))
    (make-instance 'simple-gate
                   :name name
                   :matrix (from-list entries (list dim dim)))))

(defmethod gate-definition-to-gate ((gate-def permutation-gate-definition))
  (let ((name (gate-definition-name gate-def))
        (entries (permutation-gate-definition-permutation gate-def)))
    (make-instance 'permutation-gate
                   :name name
                   :permutation entries)))

(defmethod gate-definition-to-gate ((gate-def parameterized-gate-definition))
  (flet ((lambda-form (params dimension entries)
           `(lambda ,params
              (declare (ignorable ,@params))
              (from-list (list ,@entries)
                         (list ,dimension ,dimension)))))
    (let* ((name (gate-definition-name gate-def))
           (entries (gate-definition-entries gate-def))
           (params (gate-definition-parameters gate-def))
           (dim (isqrt (length entries))))
      (check-type params symbol-list)
      (make-instance 'parameterized-gate
                     :name name
                     :dimension dim
                     :arity (length params)
                     :matrix-function (compile nil (lambda-form params dim entries))))))

(defmethod gate-definition-to-gate ((gate-def exp-pauli-sum-gate-definition))
  (with-slots (arguments parameters terms) gate-def
    (make-instance 'exp-pauli-sum-gate
                   :arguments arguments
                   :parameters parameters
                   :terms terms
                   :dimension (expt 2 (length arguments))
                   :arity (length arguments))))

(defmethod gate-definition-to-gate ((gate-def sequence-gate-definition))
  ;;hack, should change as given new opimizations
  (make-instance 'sequence-gate
    :name (gate-definition-name gate-def)
    :definition gate-def))



