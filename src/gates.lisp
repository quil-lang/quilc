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
   ;; TODO move name+documentation into the gate-definition class?
   (name :initarg :name
         :reader gate-name
         :documentation "The name of the gate.")
   (documentation :initarg :documentation
                  :reader gate-documentation
                  :documentation "Documentation about the gate."))
  (:metaclass abstract-class)
  (:default-initargs :name nil
                     :documentation nil)
  (:documentation "Abstract class for gates."))

(defmethod documentation ((object gate) symbol)
  (declare (ignore symbol))
  (gate-documentation object))

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

(defun make-permutation-gate (name documentation &rest permutation)
  "Make a permutation gate with the permut"
  (check-type name (or nil string))
  (check-type documentation (or null string))
  (assert (zerop (logand (length permutation)
                         (1- (length permutation))))
          (permutation)
          "Permutation ~A must be of a power-of-two number of elements."
          permutation)
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

(defclass dagger-gate ()
  ((target :initarg :target
           :reader target
           :documentation "The gate being daggered.")))

(defmethod gate-matrix ((gate dagger-gate) &rest parameters)
  (let ((target (target gate)))
    (magicl:dagger (apply #'gate-matrix target parameters))))

(defmethod gate-dimension ((gate dagger-gate))
  (gate-dimension (target gate)))

(defun operator-description-gate-lifter (descr)
  "Given an OPERATOR-DESCRIPTION DESCR, return a unary function that takes a gate, and returns said gate lifted according to the description (i.e., has all proper gate modifiers applied)."
  (adt:match operator-description descr
    ((named-operator _)
     (lambda (gate) gate))
    ((controlled-operator od)
     (alexandria:compose
      (lambda (gate) (make-instance 'controlled-gate :target gate))
      (operator-description-gate-lifter od)))
    ((dagger-operator od)
     (alexandria:compose
      (lambda (gate) (make-instance 'dagger-gate :target gate))
      (operator-description-gate-lifter od)))))


;;;;;;;;;;;;;;;;;;;;;; Default Gate Definitions ;;;;;;;;;;;;;;;;;;;;;;

(global-vars:define-global-var **default-gate-definitions** (make-hash-table :test 'equal)
  "A table of default gate definitions, mapping string name to a gate object.")

(defun standard-gate-names ()
  "Query for the list of standard Quil gate names."
  (loop :for k :being :the :hash-keys :of **default-gate-definitions**
        :collect k))

(defun lookup-standard-gate (gate-name)
  "Lookup the gate named by GATE-NAME in the collection of *standard* Quil gates. Return NIL if non-standard."
  (check-type gate-name alexandria:string-designator)
  (values (gethash (string gate-name) **default-gate-definitions**)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun record-standard-gate (name gate)
    "Record the standard gate GATE under the name NAME in the default collection of Quil standard gates."
    (check-type name alexandria:string-designator)
    (check-type gate gate)
    (setf (gethash (string name) **default-gate-definitions**) gate)
    (values)))

(defmacro define-default-gate (name num-qubits (&rest params) &body matrix-code)
  "Defines a gate and adds it to the default-provided gate table.

    NAME: The name of the gate (symbol)
    NUM-QUBITS: The number of qubits the gate intends to act on.
    PARAMS: Parameter list for the gate (may be empty)
    MATRIX-CODE: The code, which may depend on PARAMS, to generate the gate's operator.
"
  (check-type name symbol)
  (check-type num-qubits (integer 1))
  (let ((name-string (symbol-name name))
        (dimension (expt 2 num-qubits)))
    (multiple-value-bind (forms decls doc-string)
        (alexandria:parse-body matrix-code :documentation t)
      (cond
        ;; Simple gate.
        ((null params)
         (let ((matrix (gensym "MATRIX-")))
           `(record-standard-gate
             ',name-string
             (let ((,matrix (locally ,@decls ,@forms)))
               (assert (= (magicl:matrix-rows ,matrix)
                          (magicl:matrix-cols ,matrix)
                          ,dimension))
               (make-instance 'simple-gate
                              :name ',name-string
                              :documentation ',doc-string
                              :dimension ',dimension
                              :matrix ,matrix)))))
        ;; Parameterized gate.
        (t
         (let ((arity (length params))
               (matrix-fn (gensym (format nil "~A-MATRIX-FN-" name-string))))
           `(record-standard-gate
             ',name-string
             (flet ((,matrix-fn ,params
                      ,@decls
                      ;; TODO: Check that DIMENSION matches the generated matrix.
                      ,@forms))
               (make-instance 'parameterized-gate
                              :name ',name-string
                              :documentation ',doc-string
                              :dimension ,dimension
                              :arity ,arity
                              :matrix-function #',matrix-fn)))))))))

(defun operator-matrix-from-truth-table (truth-table-outputs)
    "Return an appropriate matrix which can act as an operator for the truth table outputs TRUTH-TABLE-OUTPUTS (represented as a list) encoded in binary lexicographic order.

Example: A NAND gate can be made with

    (operator-matrix-from-truth-table '(1 1 1 0))
"
    (let* ((column-length (length truth-table-outputs))
           (operator-size (* 2 column-length))
           (matrix (magicl:make-zero-matrix operator-size operator-size)))
      (loop :for i :below column-length
            :for x :in truth-table-outputs
            :for offset := (* 2 i)
            :do (if (zerop x)
                    (setf (magicl:ref matrix offset offset)           #C(1.0d0 0.0d0)
                          (magicl:ref matrix (1+ offset) (1+ offset)) #C(1.0d0 0.0d0))
                    (setf (magicl:ref matrix offset (1+ offset))      #C(1.0d0 0.0d0)
                          (magicl:ref matrix (1+ offset) offset)      #C(1.0d0 0.0d0))))
      matrix))

;; Evaluate this stuff earlier so we can have access during compile
;; time.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun controlled (U)
    "Construct a controlled version of the one-qubit matrix operator U."
    (assert (= 2
               (magicl:matrix-rows U)
               (magicl:matrix-cols U)))
    (let ((u00 (magicl:ref U 0 0))
          (u01 (magicl:ref U 0 1))
          (u10 (magicl:ref U 1 0))
          (u11 (magicl:ref U 1 1)))
      (make-row-major-matrix 4 4
                             (list 1 0 0 0
                                   0 1 0 0
                                   0 0 u00 u01
                                   0 0 u10 u11)))))

;;; The default gate set

(define-default-gate H 1 ()
  "The Hadamard gate."
  '#.(let ((1/sqrt2 (/ (sqrt 2.0d0))))
       (make-row-major-matrix 2 2
                              (list 1/sqrt2 1/sqrt2
                                    1/sqrt2 (- 1/sqrt2)))))

(record-standard-gate "I" (make-permutation-gate
                           "I"
                           "The identity gate."
                           0 1))

(record-standard-gate "X" (make-permutation-gate
                           "X"
                           "The Pauli-X gate."
                           1 0))

(define-default-gate Y 1 ()
  "The Pauli-Y gate."
  '#.(make-row-major-matrix 2 2
                            '(0       #C(0 -1)
                              #C(0 1) 0)))

(define-default-gate Z 1 ()
  "The Pauli-Z gate."
  '#.(make-row-major-matrix 2 2
                            '(1 0
                              0 -1)))

(record-standard-gate "CNOT" (make-permutation-gate
                              "CNOT"
                              "The controlled-X or controlled-NOT gate."
                              0 1 3 2))

(record-standard-gate "CCNOT" (make-permutation-gate
                               "CCNOT"
                               "The Toffoli gate, AKA the CCNOT gate."
                               0 1 2 3
                               4 5 7 6))

(define-default-gate RX 1 (theta)
  "The R_x(theta) gate."
  (assert (realp theta)
          nil
          "RX(~a) has nonreal argument."
          theta)
  (let* ((theta/2 (/ theta 2))
         (cos (cos theta/2))
         (isin (complex 0.0d0 (- (sin theta/2)))))
    (make-row-major-matrix 2 2
                           (list cos isin
                                 isin cos))))

(define-default-gate RY 1 (theta)
  "The R_y(theta) gate."
  (assert (realp theta)
          nil
          "RY(~a) has nonreal argument."
          theta)
  (let* ((theta/2 (/ theta 2))
         (cos (cos theta/2))
         (sin (sin theta/2)))
    (make-row-major-matrix 2 2
                           (list cos (- sin)
                                 sin cos))))

(define-default-gate RZ 1 (theta)
  "The R_z(theta) gate."
  (assert (realp theta)
          nil
          "RZ(~a) has nonreal argument."
          theta)
  (let ((theta/2 (/ theta 2)))
    (make-row-major-matrix 2 2
                           (list (cis (- theta/2)) 0
                                 0                 (cis theta/2)))))

(define-default-gate phase 1 (alpha)
  "A regular phase gate. Equivalent to R_z multiplied by a phase."
  (assert (realp alpha)
          nil
          "PHASE(~a) has nonreal argument."
          alpha)
  (make-row-major-matrix 2 2
                         (list 1 0
                               0 (cis alpha))))

(define-default-gate S 1 ()
  "The S gate."
  '#.(make-row-major-matrix 2 2
                            '(1 0
                              0 #C(0 1))))

(define-default-gate T 1 ()
  "The T gate."
  '#.(make-row-major-matrix 2 2
                            (list 1 0
                                  0 (cis (/ pi 4)))))

(define-default-gate CZ 2 ()
  "The controlled-Z gate."
  '#.(make-row-major-matrix 4 4
                            '(1 0 0  0
                              0 1 0  0
                              0 0 1  0
                              0 0 0 -1)))

(define-default-gate CPHASE00 2 (alpha)
  "The controlled phase gate (00-variant)."
  (assert (realp alpha)
          nil
          "CPHASE00(~a) has nonreal argument."
          alpha)
  (make-row-major-matrix 4 4
                         (list (cis alpha) 0 0 0
                               0           1 0 0
                               0           0 1 0
                               0           0 0 1)))

(define-default-gate CPHASE01 2 (alpha)
  "The controlled phase gate (01-variant)."
  (assert (realp alpha)
          nil
          "CPHASE01(~a) has nonreal argument."
          alpha)
  (make-row-major-matrix 4 4
                         (list 1 0           0 0
                               0 (cis alpha) 0 0
                               0 0           1 0
                               0 0           0 1)))

(define-default-gate CPHASE10 2 (alpha)
  "The controlled phase gate (10-variant)."
  (assert (realp alpha)
          nil
          "CPHASE10(~a) has nonreal argument."
          alpha)
  (make-row-major-matrix 4 4
                         (list 1 0 0           0
                               0 1 0           0
                               0 0 (cis alpha) 0
                               0 0 0           1)))

(define-default-gate CPHASE 2 (alpha)
  "The controlled phase gate (11-variant).

Note that this is a controlled version of a R_z gate multiplied by a phase."
  (assert (realp alpha)
          nil
          "CPHASE(~a) has nonreal argument."
          alpha)
  (make-row-major-matrix 4 4
                         (list 1 0 0 0
                               0 1 0 0
                               0 0 1 0
                               0 0 0 (cis alpha))))

(define-default-gate SWAP 2 ()
  "A quantum gate that swaps the relevant amplitudes of two qubits."
  '#.(make-row-major-matrix 4 4
                            (list 1 0 0 0
                                  0 0 1 0
                                  0 1 0 0
                                  0 0 0 1)))

(record-standard-gate "CSWAP" (make-permutation-gate
                               "CSWAP"
                               "The Fredkin gate, AKA the CSWAP gate."
                               0 1 2 3
                               4 6 5 7))

(define-default-gate ISWAP 2 ()
  "The ISWAP gate, for superconducting quantum computers."
  '#.(make-row-major-matrix 4 4
                            (list 1 0       0       0
                                  0 0       #C(0 1) 0
                                  0 #C(0 1) 0       0
                                  0 0       0       1)))

(define-default-gate PSWAP 2 (theta)
  "The parametric SWAP gate, for superconducting quantum computers."
  (assert (realp theta)
          nil
          "PSWAP(~a) has nonreal argument."
          theta)
  (make-row-major-matrix 4 4
                         (list 1 0           0           0
                               0 0           (cis theta) 0
                               0 (cis theta) 0           0
                               0 0           0           1)))

(define-default-gate PISWAP 2 (theta)
  "The parametric ISWAP gate, for superconducting quantum computers. NOTE: This definition is 4 PI PERIODIC rather than 2 PI PERIODIC, but theta = 2 Pi is the local gate Z (x) Z."
  (assert (realp theta)
          nil
          "PISWAP(~a) has nonreal argument."
          theta)
  (let ((cos (cos (/ theta 2)))
        (isin (* #C(0 1)
                 (sin (/ theta 2)))))
    (make-row-major-matrix 4 4
                           (list 1 0    0    0
                                 0 cos  isin 0
                                 0 isin cos  0
                                 0 0    0    1))))

;;;;;;;;;;;;;; Conversion of GATE-DEFINITIONs to GATEs ;;;;;;;;;;;;;;;

;;; These are convenient default translations from GATE-DEFINITIONs to
;;; GATEs. However, something like the QVM might prefer to create a
;;; more advanced representation of a gate, or perform further
;;; analysis on the gate.

(defgeneric gate-definition-to-gate (gate-def)
  (:documentation "Convert a parsed Quil gate definition to a usable, executable gate."))

(defmethod gate-definition-to-gate ((gate-def static-gate-definition))
  (let* ((name (gate-definition-name gate-def))
         (entries (gate-definition-entries gate-def))
         (dim (isqrt (length entries))))
    (make-instance 'simple-gate
                   :name name
                   :dimension dim
                   :matrix (make-row-major-matrix dim dim entries))))

(defun permutation-from-gate-entries (entries)
  (loop :with size := (isqrt (length entries))
        :for i :below size
        :collect (position 1 (loop :for j :below size :collect (nth (+ i (* j size)) entries))
                           :test #'=)))

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

(defun make-matrix-from-quil (instruction-list &key relabeling)
  "If possible, create a matrix out of the instructions INSTRUCTION-LIST, within the context of the environment ENVIRONS. If one can't be created, then return NIL.

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
                          (cond
                            ((typep a 'formal)
                             (qubit (parse-integer (formal-name a) :start 1)))
                            ((and (typep a 'qubit)
                                  (assoc (qubit-index a) relabeling))
                             (qubit (cdr (assoc (qubit-index a) relabeling))))
                            (t
                             a)))
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

(defun make-gate-matrix-from-gate-string (qubit-list gate-string)
  "Produces a gate matrix from the product of the gate matrices for the gates in GATE-STRING, where each has its qubits remapped onto the positions in QUBIT-LIST.  (This is a kind of inverse to KQ-GATE-ON-LINES.)"
  (make-matrix-from-quil
   (loop :for i :in gate-string
         :for j := (copy-instance i)
         :do (setf (application-arguments j)
                   (mapcar (lambda (q)
                             (qubit (- (length qubit-list)
                                       1
                                       (position (qubit-index q) qubit-list
                                                 :key #'qubit-index))))
                           (application-arguments j)))
         :collect j)))

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
