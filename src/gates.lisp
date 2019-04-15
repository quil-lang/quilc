;;;; gates.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

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
    (magicl:matrix-rows gate)))


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
           (recurse (od)
             (adt:match operator-description od
               ((named-operator _)
                ;; We've already resolved the gate name.
                (check-parameters gate)
                (apply #'gate-matrix
                       (gate-definition-to-gate (gate-application-resolution gate))
                       (mapcar #'constant-value (application-parameters gate))))
               ((controlled-operator o)
                (let ((summand (recurse o)))
                  (magicl:direct-sum
                   (magicl:make-identity-matrix (gate-dimension summand))
                   summand)))
               ((dagger-operator o)
                (magicl:dagger (recurse o))))))
    (cond ((slot-boundp gate 'gate)
           (check-parameters gate)
           (apply #'gate-matrix (gate-application-gate gate)
                  (mapcar #'constant-value (application-parameters gate))))
          (t
           (recurse (application-operator gate))))))


;;;;;;;;;;;;;;;;;;;;;; Gate Object Definitions ;;;;;;;;;;;;;;;;;;;;;;;

;;; These are alternative algebraic representations of gates that
;;; might make analysis more suitable. Each of these can always be
;;; made into a matrix with GATE-MATRIX.

(defclass gate ()
  ((dimension :initarg :dimension
              :reader gate-dimension
              :documentation "The minimal dimension of the space the gate acts on.")
   ;; TODO move name into the gate-definition class?
   ;;
   ;; Eric sez that the name is useful for tracking anonymous gates.
   (name :initarg :name
         :reader gate-name
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

(defmethod gate-matrix ((gate simple-gate) &rest parameters)
  (assert (null parameters))
  (simple-gate-matrix gate))

(defclass permutation-gate (static-gate)
  ((permutation :initarg :permutation
                :reader permutation-gate-permutation
                :documentation "The permutation representation of the gate."))
  (:documentation "A gate which could be realized as a permutation matrix."))

(defmethod gate-matrix ((gate permutation-gate) &rest parameters)
  (assert (null parameters))
  (let* ((d (gate-dimension gate))
         (m (magicl:make-zero-matrix d d)))
    (map nil (let ((row -1))
               (lambda (col)
                 (setf (magicl:ref m (incf row) col) #C(1.0d0 0.0d0))))
         (permutation-gate-permutation gate))
    m))

(defun check-permutation (perm)
  "Check that PERM is a valid permutation. Error if it's not."
  (let* ((n (length perm))
         (bits (make-array n :element-type 'bit :initial-element 0)))
    (flet ((mark-bit (i)
             (cond
               ((zerop (sbit bits i)) (setf (sbit bits i) 1))
               (t (error "Invalid permutation ~A. Found duplicate entry: ~A" perm i)))))
      ;; Mark all of the bits.
      (mapc #'mark-bit perm)
      ;; Check that they're all marked.
      (dotimes (i n t)
        (when (zerop (sbit bits i))
          (error "Invalid permutation ~A. Missing entry: ~A" perm i))))))

(defun make-permutation-gate (name documentation &rest permutation)
  "Make a permutation gate with the permut"
  (check-type name (or nil string))
  (check-type documentation (or null string))
  (assert (zerop (logand (length permutation)
                         (1- (length permutation))))
          (permutation)
          "Permutation ~A must be of a power-of-two number of elements."
          permutation)
  (check-permutation permutation)
  (make-instance
   'permutation-gate
   :name name
   :documentation documentation
   :dimension (length permutation)
   :permutation permutation))

(defclass parameterized-gate (dynamic-gate)
  ((matrix-function :initarg :matrix-function
                    :reader parameterized-gate-matrix-function
                    :documentation "Function mapping ARITY complex numbers to a DIMENSION x DIMENSION MAGICL matrix."))
  (:documentation "A gate parameterized by complex numbers."))

(defmethod gate-matrix ((gate parameterized-gate) &rest parameters)
  (apply (parameterized-gate-matrix-function gate) parameters))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Gate Operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Controlled Gates

(defclass controlled-gate ()
  ((target :initarg :target
           :reader target
           :documentation "The targeted gate of a single qubit control.")))

(defmethod gate-matrix ((gate controlled-gate) &rest parameters)
  (let ((target (target gate)))
    (magicl:direct-sum (magicl:make-identity-matrix (gate-dimension target))
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
                     :permutation (append (alexandria:iota size)
                                          (mapcar (lambda (j) (+ j size)) permutation))))))

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
     (alexandria:compose #'control-gate (operator-description-gate-lifter od)))
    ((dagger-operator od)
     (alexandria:compose #'dagger-gate (operator-description-gate-lifter od)))))

(defmethod gate-application-gate ((app gate-application))
  (funcall (operator-description-gate-lifter (application-operator app))
           (gate-definition-to-gate
            (gate-application-resolution app))))

;;;;;;;;;;;;;;;;;;;;;; Default Gate Definitions ;;;;;;;;;;;;;;;;;;;;;;

;;; Load all of the standard gates from src/quil/stdgates.quil
(global-vars:define-global-var **default-gate-definitions**
    (let ((table (make-hash-table :test 'equal))
          (gate-defs (parsed-program-gate-definitions
                      (parse-quil-into-raw-program
                       (alexandria:read-file-into-string
                        (asdf:system-relative-pathname
                         "cl-quil" "src/quil/stdgates.quil"))))))
      (dolist (gate-def gate-defs table)
        (setf (gethash (gate-definition-name gate-def) table)
              gate-def)))
  "A table of default gate definitions, mapping string name to a GATE-DEFINITION object.")

(defun standard-gate-names ()
  "Query for the list of standard Quil gate names."
  (loop :for k :being :the :hash-keys :of **default-gate-definitions**
        :collect k))

(defun lookup-standard-gate (gate-name)
  "Lookup the gate named by GATE-NAME in the collection of *standard* Quil gates. Return NIL if non-standard."
  (check-type gate-name alexandria:string-designator)
  (values (gethash (string gate-name) **default-gate-definitions**)))


;;;;;;;;;;;;;; Conversion of GATE-DEFINITIONs to GATEs ;;;;;;;;;;;;;;;

;;; These are convenient default translations from GATE-DEFINITIONs to
;;; GATEs. However, something like the QVM might prefer to create a
;;; more advanced representation of a gate, or perform further
;;; analysis on the gate.

(defgeneric gate-definition-to-gate (gate-def)
  (:documentation "Convert a parsed Quil gate definition to a usable, executable gate.")
  (:method :around ((gate-def gate-definition))
    (alexandria:if-let ((gate (%gate-definition-cached-gate gate-def)))
      gate
      (setf (%gate-definition-cached-gate gate-def) (call-next-method)))))

(defmethod gate-definition-to-gate ((gate-def static-gate-definition))
  (let* ((name (gate-definition-name gate-def))
         (entries (gate-definition-entries gate-def))
         (dim (isqrt (length entries))))
    (make-instance 'simple-gate
                   :name name
                   :dimension dim
                   :matrix (make-row-major-matrix dim dim entries))))

(defmethod gate-definition-to-gate ((gate-def permutation-gate-definition))
  (let* ((name (gate-definition-name gate-def))
         (entries (permutation-gate-definition-permutation gate-def))
         (dim (length entries)))
    (make-instance 'permutation-gate
                   :name name
                   :dimension dim
                   :permutation entries)))

(defmethod gate-definition-to-gate ((gate-def parameterized-gate-definition))
  (flet ((lambda-form (params dimension entries)
           `(lambda ,params
              (declare (ignorable ,@params))
              (make-row-major-matrix ,dimension ,dimension (list ,@entries)))))
    (let* ((name (gate-definition-name gate-def))
           (entries (gate-definition-entries gate-def))
           (params (gate-definition-parameters gate-def))
           (dim (isqrt (length entries))))
      (assert (every #'symbolp params))
      (make-instance 'parameterized-gate
                     :name name
                     :dimension dim
                     :arity (length params)
                     :matrix-function (compile nil (lambda-form params dim entries))))))

;;;; some leftover stuff from standard-gates.lisp and elsewhere

(defun apply-gate (m instr)
  "Constructs the matrix representation associated to an instruction list consisting of gates. Suitable for testing the output of compilation routines."
  (check-type m magicl:matrix)
  (check-type instr application)
  (alexandria:when-let ((defn (gate-matrix instr)))
    (let* ((mat-size (ilog2 (magicl:matrix-rows m)))
           (size (max mat-size
                      (apply #'max
                             (map 'list (lambda (x) (1+ (qubit-index x)))
                                  (application-arguments instr)))))
           (mat (kq-gate-on-lines defn
                                  size
                                  (mapcar #'qubit-index (application-arguments instr))))
           ;; resize m if necessary
           (m (if (< mat-size size)
                  (kq-gate-on-lines m
                                    size
                                    (alexandria:iota mat-size :start (1- mat-size) :step -1))
                  m)))
      (magicl:multiply-complex-matrices mat m))))

(defun make-matrix-from-quil (instruction-list &key (relabeling #'identity))
  "If possible, create a matrix out of the instructions INSTRUCTION-LIST using the optional function RELABELING that maps an input qubit index to an output qubit index. If one can't be created, then return NIL.

Instructions are multiplied out in \"Quil\" order, that is, the instruction list (A B C) will be multiplied as if by the Quil program

    A
    B
    C

or equivalently as

    C * B * A

as matrices."
  (let ((u (magicl:diag 1 1 '(1d0))))
    (dolist (instr instruction-list u)
      (assert (not (null u)))
      ;; TODO: What to do about other quantum-state-transitioning
      ;; instructions?
      (when (typep instr 'application)
        ;; We can't make a matrix if we don't know the gate
        ;; parameters.
        (unless (every #'is-constant (application-parameters instr))
          (return-from make-matrix-from-quil nil))
        (let ((new-instr (copy-instance instr)))
          (setf (application-arguments new-instr)
                (mapcar (lambda (a)
                          (if (typep a 'formal)
                              (qubit (parse-integer (formal-name a) :start 1))
                              (funcall relabeling a)))
                        (application-arguments new-instr)))
          (setf u (apply-gate u new-instr)))))))

(defun kq-gate-on-lines (gate-mat n lines)
  "Writes the gate GATE-MAT as an N-qubit gate by applying it to the qubit lines in LINES."
  (check-type gate-mat magicl:matrix)
  (check-type n integer)
  (let* ((width (expt 2 n))
         (mask (- -1 (loop :for l :in lines :sum (expt 2 l))))
         (out-mat (magicl:make-zero-matrix width width)))
    (dotimes (i width)
      (dotimes (j width)
        (if (= (logand mask i) (logand mask j))
            (setf (magicl:ref out-mat i j)
                  (magicl:ref gate-mat
                              (loop :for r :below (length lines) :sum
                                 (if (logbitp (nth r lines) i)
                                     (ash 1 (- (length lines) 1 r))
                                     0))
                              (loop :for s :below (length lines) :sum
                                 (if (logbitp (nth s lines) j)
                                     (ash 1 (- (length lines) 1 s))
                                     0)))))))
    out-mat))

(defun su2-on-line (line m)
  "Treats m in SU(2) as either m (x) Id or Id (x) m."
  (kq-gate-on-lines m 2 (list line)))

(define-global-counter **premultiplied-gate-count** incf-premultiplied-gate-count)

(defun premultiply-gates (instructions)
  "Given a list of (gate) applications INSTRUCTIONS, construct a new gate application which is their product.

Instructions are multiplied out in \"Quil\" order, that is, the instruction list (A B C) will be multiplied as if by the Quil program

    A
    B
    C

or equivalently as

    C * B * A

as matrices."
  (let ((u (magicl:diag 1 1 '(1d0)))
        (qubits (list)))
    (dolist (instr instructions)
      (let ((new-qubits (set-difference (mapcar #'qubit-index (application-arguments instr))
                                        qubits)))
        (unless (endp new-qubits)
          (setf u (kq-gate-on-lines u
                                    (+ (length qubits) (length new-qubits))
                                    (alexandria:iota (length qubits)
                                                     :start (1- (length qubits))
                                                     :step -1)))
          (setf qubits (append new-qubits qubits)))
        (setf u (magicl:multiply-complex-matrices
                 (kq-gate-on-lines (gate-matrix instr)
                                   (length qubits)
                                   (mapcar (lambda (q)
                                             (- (length qubits) 1 (position (qubit-index q) qubits)))
                                           (application-arguments instr)))
                 u))))
    (make-instance 'gate-application
                   :gate (make-instance 'simple-gate
                                        :dimension (expt 2 (length qubits))
                                        :matrix u)
                   :operator (named-operator (format nil "FUSED-GATE-~D"
                                                     (incf-premultiplied-gate-count)))
                   :arguments (mapcar #'qubit qubits))))
