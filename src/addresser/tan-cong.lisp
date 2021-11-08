(in-package :cl-quil)

(named-readtables:in-readtable :cl-smt-lib)


;;; variables

(defclass smt-variable ()
  ((name :initarg :name
         :accessor smt-variable-name))
  (:metaclass abstract-class)
  (:documentation "A variable for usage in a SMT program."))

(defmethod print-object ((obj smt-variable) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (smt-variable-name obj))))

(defclass smt-integer (smt-variable)
  ()
  (:documentation "An integer variable."))

(defun smt-integer (fmt &rest fmt-args)
  (let ((name
          (intern (apply #'format nil fmt fmt-args))))
    (make-instance 'smt-integer :name name)))

;;; constraints

(defclass smt-constraint ()
  ((term :initarg :term
         :accessor smt-constraint-term
         :documentation "The SMT term defining the constraint."))
  (:documentation "A constraint."))

(defmethod print-object ((obj smt-constraint) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (smt-constraint-term obj))))

(defun make-term (op &rest args)
  (let ((op (etypecase op
              (symbol op)
              (string (intern op)))))
    (cons op
          (mapcar (lambda (arg)
                    (typecase arg
                      (smt-variable (smt-variable-name arg))
                      (otherwise arg)))
                  args))))

(defun constraint (term)
  (make-instance 'smt-constraint :term term))

(defun bound-int (lower-bound var &optional upper-bound)
  (let ((name (smt-variable-name var)))
    (make-instance 'smt-constraint
      :term
      (if upper-bound
          #!`(and (<= ,LOWER-BOUND ,NAME) (< ,NAME ,UPPER-BOUND))
          #!`(<= ,LOWER-BOUND ,NAME)))))

(defun pairwise-distinct (vars)
  (let ((pairs-constraints
          (loop :for (var-a . rest) :on vars
                :for a := (smt-variable-name var-a)
                :append (loop :for var-b :in rest
                              :for b := (smt-variable-name var-b)
                              :collect #!`(distinct ,A ,B)))))
    (make-instance 'smt-constraint
      :term
      #!`(and ,@PAIRS-CONSTRAINTS))))


;;; programs

(defclass constraint-program ()
  ((variables :initarg :variables
              :initform nil
              :accessor constraint-program-variables)
   (constraints :initarg :constraints
                :initform nil
                :accessor constraint-program-constraints))
  (:documentation "A representation of a simple SMT script, with variables to declare and constraints to assert."))

(defgeneric translate-to-smt-command (obj)
  (:documentation "Translate OBJ to a corresponding SMT command.")
  (:method ((obj t))
    obj)
  (:method ((obj (eql nil)))
    "()")
  (:method ((obj list))
    (mapcar #'translate-to-smt-command obj))
  (:method ((var smt-integer))
    #!`(declare-fun ,(SMT-VARIABLE-NAME VAR) () Int))
  (:method ((constraint smt-constraint))
    (translate-to-smt-command (smt-constraint-term constraint))))

;;; Encoding
;;;
;;; The circuit is a rectangular grid, indexed horizontally by time
;;; slices, and vertically by edges of the chip. Every gate L gets
;;; 'space-time' coordinates (XS[L], BS[L]), placing it on the grid.

(defclass tan-cong-encoding ()
  ((num-qubits :initarg :num-qubits
               :reader encoding-num-qubits)   
   (num-links :initarg :num-links
              :reader encoding-num-links)
   (num-gates :initarg :num-gates
              :reader encoding-num-gates)
   (num-blocks :initarg :num-blocks
               :reader encoding-num-blocks)
   (l2p :initarg :l2p
        :reader %encoding-l2p
        :documentation "A 2D array with entry L2P[B,Q] denoting the physical qubit assigned to logical Q in gate block B.")
   (bs :initarg :bs
       :reader %encoding-bs
       :documentation "An array, with entry BS[G] denoting the block in which gate G is scheduled.")
   (xs :initarg :xs
       :reader %encoding-xs
       :documentation "An array, with entry XS[G] denoting the edge on which gate G is scheduled.")
   (sigma :initarg :sigma
          :reader %encoding-sigma
          :documentation "A 2D array, with entry SIGMA[B,K] equal to 1 if there is a swap on edge K at time S, and 0 otherwise.")))

(defun encoding-l2p (encoding block-idx logical-qubit)
  (aref (%encoding-l2p encoding) block-idx logical-qubit))

(defun encoding-bs (encoding gate-idx)
  (aref (%encoding-bs encoding) gate-idx))

(defun encoding-xs (encoding gate-idx)
  (aref (%encoding-xs encoding) gate-idx))

(defun encoding-sigma (encoding block-idx edge)
  (aref (%encoding-sigma encoding) block-idx edge))

(defun encoding-all-variables (encoding)
  (flet ((array-to-list (array)           
           (map 'list #'identity
                (make-array (array-total-size array) :displaced-to array))))
    (append
     (array-to-list (%encoding-l2p encoding))
     (array-to-list (%encoding-bs encoding))
     (array-to-list (%encoding-xs encoding))
     (array-to-list (%encoding-sigma encoding)))))

(defun make-tan-cong-encoding (instructions chip-spec num-blocks)
  (flet ((array-from-list (list)
           (let ((dims 
                   (if (listp (first list))
                       (list (length list) (length (first list)))
                       (length list))))
             (make-array dims :initial-contents list))))
    (let* ((num-qubits (chip-spec-n-qubits chip-spec))
           (num-links (chip-spec-n-links chip-spec))
           (num-gates (length instructions))
           (l2p (array-from-list
                 (loop :for b :to num-blocks ; NOTE: we add a block at the end for final swaps!
                       :collect (loop :for q :below num-qubits
                                      :collect (smt-integer "l2p[~D,~D]" b q)))))
           (bs (array-from-list               
                (loop :for l :below num-gates
                      :collect (smt-integer "bs[~D,]" l))))
           ;; comma is forcing this to print with bars |bs[0,]|
           ;; todo: do something better
           (xs (array-from-list
                (loop :for l :below num-gates
                      :collect (smt-integer "xs[~D,]" l))))
           (sigma (array-from-list
                   (loop :for b :below num-blocks
                         :collect (loop :for l :below num-links
                                        :collect (smt-integer "sigma[~D,~D]" b l))))))
      (make-instance 'tan-cong-encoding
        :num-qubits num-qubits :num-links num-links :num-gates num-gates :num-blocks num-blocks
        :l2p l2p :bs bs :xs xs :sigma sigma ))))

;;; TODO: assume by this point that instructions are just gate applications
(defun initial-gate-dependencies (instructions)
  (loop :for i :from 0
        :for (gi . rest) :on instructions
        :for ri := (instruction-resources gi)
        :append (loop :for j :from (1+ i)
                      :for gj :in rest
                      :for rj := (instruction-resources gj)
                      :when (resource-intersection ri rj)
                        :collect (cons i j))))

(defun generate-constraints (instrs encoding chip-spec &key initial-l2p final-l2p)
  (let ((nb (encoding-num-blocks encoding))
        (nq (encoding-num-qubits encoding))
        (nl (encoding-num-links encoding))
        (ng (encoding-num-gates encoding))
        (constraints nil)
        (dependencies (initial-gate-dependencies instrs)))
    (labels ((bs (g)
               (encoding-bs encoding g))
             (xs (g)
               (encoding-xs encoding g))
             (l2p (b q)
               (encoding-l2p encoding b q))
             (sigma (b k)
               (encoding-sigma encoding b k))
             (add-constraint (c)
               (push (etypecase c
                       (list (constraint c))
                       (smt-constraint c))
                     constraints))
             (qubits-on-link (l)
               (coerce (chip-spec-qubits-on-link chip-spec l) 'list))
             (links-on-qubit (q)
               (coerce (chip-spec-links-on-qubit chip-spec q) 'list))
             (constrain-l2p-equals (b target-l2p)
               (cons '|and|
                     (loop :for q :from 0
                           :for p :in (coerce target-l2p 'list)
                           :collect #!`(== ,(L2P B Q) ,P)))))
      ;; l2p mapping
      ;; 1. uses valid physical qubits: 0 <= l2p[b][q] < num_qubits
      ;; 2. is injective on every block b: l2p[b][q] != l2p[b][q0] for q != q0
      (loop :for b :to nb
            :for l2p := (loop :for q :below nq :collect (l2p b q))
            :do (add-constraint (pairwise-distinct l2p))
                (dolist (p l2p)
                  (add-constraint (bound-int 0 p))))

      ;; gate times
      ;; 1. are well defined (and not on last time slice)
      ;; 2. dependencies are weakly satisfied
      (loop :for g :below ng
            :do (add-constraint (bound-int 0 (bs g))))
      (loop :for (i . j) :in dependencies
            :do (add-constraint (constraint `(<= ,(bs i) ,(bs j)))))

      ;; gate positions
      ;; 1. are well defined
      (loop :for g :below ng
            :do (add-constraint (bound-int 0 (xs g) nl)))

      ;; swap placements
      ;; 1. are well defined (i.e. 0 or 1)
      ;; 2. if two edges overlap in space, then they occur in different blocks
      (dotimes (b nb)
        (dotimes (k nl)
          (add-constraint (bound-int 0 (sigma b k) 2)))
        (dotimes (l nl)              ; TODO: edge -> link, ne -> nl
          (dolist (lp (chip-spec-adj-links chip-spec l))
            (add-constraint #!`(=> (< 0 ,(SIGMA B L))
                                   (== 0 ,(SIGMA B LP)))))))

      ;; consistency between gate assignment and qubit assignment
      ;; 1. if a gate is on an edge e, then the logical endpoints of
      ;;    this gate map to the physical endpoints of e
      (dotimes (b nb)                   ; TODO: we assume 2Q gates only here
        (loop :for l :below nl
              :for (p0 p1) := (qubits-on-link l)
              :do (loop :for i :from 0
                        :for instr :in instrs
                        :for xi := (xs i)
                        :for bi := (bs i)
                        :for (q0 q1) := (mapcar #'qubit-index (application-arguments instr))
                        :for pibq0 := (l2p b q0)
                        :for pibq1 := (l2p b q1)
                        :do (add-constraint
                             #!`(=> (and (== ,BI ,B) (== ,XI ,L))
                                    (or (and (== ,PIBQ0 ,P0) (== ,PIBQ1 ,P1))
                                        (and (== ,PIBQ0 ,P1) (== ,PIBQ1 ,P0))))))))
      
      ;; L2P mapping is updated by swaps
      ;; 1. if there are no swaps touching a qubit q at block b, l2p[b,q] == l2p[b+1,q]
      ;; 2. if there is a swap on an edge, l2p[b] is related to l2p[b+1] by
      ;;    a swap on the assigned physical qubits
      (dotimes (b nb)
        (dotimes (p nq)
          (dotimes (q nq)
            (let ((no-swaps-on-p
                    `(|and| ,@(loop :for l :in (links-on-qubit p)
                                    :collect `(== 0 ,(sigma b l))))))
              (add-constraint
               #!`(=> (and (== ,(L2P B Q) ,P) ,NO-SWAPS-ON-P)
                      (== ,(L2P (1+ B) Q) ,P))))))
        (loop :for l :below nl
              :for (p0 p1) := (qubits-on-link l)
              :do (dotimes (q nq)
                    (add-constraint
                      #!`(=> (and (< 0 ,(SIGMA B L))
                                  (== P0 ,(L2P B Q)))
                             (== P1 ,(L2P (1+ B) Q))))
                    (add-constraint
                     #!`(=> (and (< 0 ,(SIGMA B L))
                                  (== P1 ,(L2P B Q)))
                            (== P0 ,(L2P (1+ B) Q))))))
        ;; Pin down initial // final L2P
        (when initial-l2p
          (add-constraint (constrain-l2p-equals 0 initial-l2p)))
        (when final-l2p
          (add-constraint (constrain-l2p-equals nb final-l2p)))))
        
    (nreverse constraints)))

(defun tan-cong-constraint-program (instrs chip-spec num-blocks &key
                                                                  initial-l2p
                                                                  final-l2p)
  (let ((cp (make-instance 'constraint-program))
        (encoding (make-tan-cong-encoding instrs chip-spec num-blocks)))
    (setf (constraint-program-variables cp) (encoding-all-variables encoding)
          (constraint-program-constraints cp) (generate-constraints instrs encoding chip-spec
                                                                    :initial-l2p initial-l2p
                                                                    :final-l2p final-l2p))
    (values cp encoding)))

(defun write-constraint-program (cp smt)
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'null (lambda (stream obj)
                                 (declare (ignore obj))
                                 (format stream "()")))
    (dolist (var (constraint-program-variables cp))
      (cl-smt-lib:write-to-smt smt (list (translate-to-smt-command var))))
    (dolist (constraint (constraint-program-constraints cp))
      (cl-smt-lib:write-to-smt smt (list (translate-to-smt-command constraint))))
    (cl-smt-lib:write-to-smt smt #!`((check-sat) (get-model)))))

(defun tb-olsq (instructions chip-spec &key
                                         initial-rewiring
                                         final-rewiring)
  "Address INSTRUCTONS to be compatible with CHIP-SPEC, using a transition-based constraint system.

Returns three values: (ADDRESSED-INSTRUCTIONS, INITIAL-REWIRING, FINAL-REWIRING)."
  ;; check whether instructions are addressable by these means
  (assert (every (lambda (i) (typep i 'application)) instrs))
  (multiple-value-bind (cp encoding)
      (tan-cong-constraint-program instrs chip-spec 2 ; TODO: num blocks
                                   :initial-l2p initial-rewiring
                                   :final-l2p final-rewiring)
    (declare (ignore encoding))
    (break)
    (let ((smt (cl-smt-lib:make-smt "z3" "-in" "-smt2")))
      ;; TODO: set options
      (write-constraint-program cp t)
      (return-from tb-olsq smt)))
  ;; run
  ;; recover model
  ;; unpack results
  ;; ASAP scheduling for blocks
  ;; return results
  (values instructions initial-rewiring final-rewiring))

;;; TODO: maybe just stash constraints as lists...
