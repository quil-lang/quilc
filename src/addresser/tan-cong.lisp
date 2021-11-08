(in-package :cl-quil)

(named-readtables:in-readtable :cl-smt-lib)


;;; smt constructors

(defun smt-integer (fmt &rest fmt-args)
  "Construct a SMT integer variable with the indicated name."
  (intern (apply #'format nil fmt fmt-args)))

;;; constraints

(defun bound-int (lower-bound var &optional upper-bound)
  "Constrain VAR to satisfy LOWER-BOUND <= VAR < UPPER-BOUND."
  (if upper-bound
      #!`(and (<= ,LOWER-BOUND ,VAR) (< ,VAR ,UPPER-BOUND))
      #!`(<= ,LOWER-BOUND ,VAR)))

(defun distinct (vars)
  "Constrain VARS to be distinct."
  (cons '|distinct| vars))

(defun declare-int (var)
  "Declare an integer variable VAR."
  #!`(declare-fun ,VAR () Int))


;;; programs

(defclass constraint-program ()
  ((variables :initarg :variables
              :initform nil
              :accessor constraint-program-variables)
   (constraints :initarg :constraints
                :initform nil
                :accessor constraint-program-constraints))
  (:documentation "A representation of a simple SMT script, with variables to declare and constraints to assert."))

;;; Encodings

(defclass encoding ()
  ((gates :initarg :gates
          :reader encoding-gates)
   (chip :initarg :chip
         :reader encoding-chip-spec))
  (:metaclass abstract-class:abstract-class)
  (:documentation "A base class for encodings which represent the problem of addressing GATES to CHIP."))

(defun encoding-num-qubits (encoding)
  (chip-spec-n-qubits (encoding-chip-spec encoding)))

(defun encoding-num-links (encoding)
  (chip-spec-n-links (encoding-chip-spec encoding)))

(defun encoding-num-gates (encoding)
  (length (encoding-gates encoding)))

;;; The circuit is a rectangular grid, indexed horizontally by time
;;; slices, and vertically by edges of the chip. Every gate L gets
;;; 'space-time' coordinates (XS[L], BS[L]), placing it on the grid.

(defclass tan-cong-encoding (encoding)
  ((num-blocks :initarg :num-blocks
               :reader tan-cong-encoding-num-blocks)
   (l2p :initarg :l2p
        :reader %tan-cong-encoding-l2p
        :documentation "A 2D array with entry L2P[B,Q] denoting the physical qubit assigned to logical Q in gate block B.")
   (bs :initarg :bs
       :reader %tan-cong-encoding-bs
       :documentation "An array, with entry BS[G] denoting the block in which gate G is scheduled.")
   (xs :initarg :xs
       :reader %tan-cong-encoding-xs
       :documentation "An array, with entry XS[G] denoting the edge on which gate G is scheduled.")
   (sigma :initarg :sigma
          :reader %tan-cong-encoding-sigma
          :documentation "A 2D array, with entry SIGMA[B,K] equal to 1 if there is a swap on edge K at time S, and 0 otherwise.")))

(defun tan-cong-encoding-l2p (encoding block-idx logical-qubit)
  (aref (%tan-cong-encoding-l2p encoding) block-idx logical-qubit))

(defun tan-cong-encoding-bs (encoding gate-idx)
  (aref (%tan-cong-encoding-bs encoding) gate-idx))

(defun tan-cong-encoding-xs (encoding gate-idx)
  (aref (%tan-cong-encoding-xs encoding) gate-idx))

(defun tan-cong-encoding-sigma (encoding block-idx edge)
  (aref (%tan-cong-encoding-sigma encoding) block-idx edge))

(defun tan-cong-encoding-variables (encoding)
  (flet ((array-to-list (array)           
           (map 'list #'identity
                (make-array (array-total-size array) :displaced-to array))))
    (append
     (array-to-list (%tan-cong-encoding-l2p encoding))
     (array-to-list (%tan-cong-encoding-bs encoding))
     (array-to-list (%tan-cong-encoding-xs encoding))
     (array-to-list (%tan-cong-encoding-sigma encoding)))))

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
        :gates (map 'vector #'identity instrs)
        :chip chip-spec
        :num-blocks num-blocks
        :l2p l2p :bs bs :xs xs :sigma sigma))))

(defun initial-gate-dependencies (gate-vec)
  (loop :for i :from 0 :below (length gate-vec)
        :for gi := (aref gate-vec i)
        :for ri := (instruction-resources gi)
        :append (loop :for j :from (1+ i) :below (length gate-vec)
                      :for gj := (aref gate-vec j)
                      :for rj := (instruction-resources gj)
                      :when (resource-intersection ri rj)
                        :collect (cons i j))))

(defun tan-cong-constraints (encoding chip-spec &key initial-l2p final-l2p)
  (let ((nb (tan-cong-encoding-num-blocks encoding))
        (nq (encoding-num-qubits encoding))
        (nl (encoding-num-links encoding))
        (ng (encoding-num-gates encoding))
        (constraints nil)
        (dependencies (initial-gate-dependencies (encoding-gates encoding))))
    (labels ((bs (g)
               (tan-cong-encoding-bs encoding g))
             (xs (g)
               (tan-cong-encoding-xs encoding g))
             (l2p (b q)
               (tan-cong-encoding-l2p encoding b q))
             (sigma (b k)
               (tan-cong-encoding-sigma encoding b k))
             (add-constraint (c)
               (push c constraints))
             (qubits-on-link (l)
               (coerce (chip-spec-qubits-on-link chip-spec l) 'list))
             (links-on-qubit (q)
               (coerce (chip-spec-links-on-qubit chip-spec q) 'list))
             (constrain-l2p-equals (b target-l2p)
               (cons '|and|
                     (loop :for q :from 0
                           :for p :in (coerce target-l2p 'list)
                           :collect #!`(= ,(L2P B Q) ,P)))))
      ;; l2p mapping
      ;; 1. uses valid physical qubits: 0 <= l2p[b][q] < num_qubits
      ;; 2. is injective on every block b: l2p[b][q] != l2p[b][q0] for q != q0
      (loop :for b :to nb
            :for l2p := (loop :for q :below nq :collect (l2p b q))
            :do (add-constraint (distinct l2p))
                (dolist (p l2p)
                  (add-constraint (bound-int 0 p))))

      ;; gate times
      ;; 1. are well defined (and not on last time slice)
      ;; 2. dependencies are weakly satisfied
      (loop :for g :below ng
            :do (add-constraint (bound-int 0 (bs g) (tan-cong-encoding-num-blocks encoding))))
      (loop :for (i . j) :in dependencies
            :do (add-constraint `(<= ,(bs i) ,(bs j))))

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
        (dotimes (l nl)
          (dolist (lp (chip-spec-adj-links chip-spec l))
            (add-constraint #!`(=> (< 0 ,(SIGMA B L))
                                   (= 0 ,(SIGMA B LP)))))))

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
                             #!`(=> (and (= ,BI ,B) (= ,XI ,L))
                                    (or (and (= ,PIBQ0 ,P0) (= ,PIBQ1 ,P1))
                                        (and (= ,PIBQ0 ,P1) (= ,PIBQ1 ,P0))))))))
      
      ;; L2P mapping is updated by swaps
      ;; 1. if there are no swaps touching a qubit q at block b, l2p[b,q] = l2p[b+1,q]
      ;; 2. if there is a swap on an edge, l2p[b] is related to l2p[b+1] by
      ;;    a swap on the assigned physical qubits
      (dotimes (b nb)
        (dotimes (p nq)
          (dotimes (q nq)
            (let ((no-swaps-on-p
                    `(|and| ,@(loop :for l :in (links-on-qubit p)
                                    :collect `(= 0 ,(sigma b l))))))
              (add-constraint
               #!`(=> (and (= ,(L2P B Q) ,P) ,NO-SWAPS-ON-P)
                      (= ,(L2P (1+ B) Q) ,P))))))
        (loop :for l :below nl
              :for (p0 p1) := (qubits-on-link l)
              :do (dotimes (q nq)
                    (add-constraint
                      #!`(=> (and (< 0 ,(SIGMA B L))
                                  (= ,P0 ,(L2P B Q)))
                             (= ,P1 ,(L2P (1+ B) Q))))
                    (add-constraint
                     #!`(=> (and (< 0 ,(SIGMA B L))
                                  (= ,P1 ,(L2P B Q)))
                            (= ,P0 ,(L2P (1+ B) Q))))))
        ;; Pin down initial // final L2P
        (when initial-l2p
          (add-constraint (constrain-l2p-equals 0 initial-l2p)))
        (when final-l2p
          (add-constraint (constrain-l2p-equals nb final-l2p)))))
        
    (nreverse constraints)))

(defgeneric encode-constraint-program (scheme instrs chip-spec &rest args &key initial-l2p final-l2p &allow-other-keys)
  (:method ((scheme (eql ':tb-olsq)) instr chip-spec &key initial-l2p final-l2p num-blocks)
    (let ((cp (make-instance 'constraint-program))
          (encoding (make-tan-cong-encoding instrs chip-spec num-blocks)))
      (setf (constraint-program-variables cp) (tan-cong-encoding-variables encoding)
            (constraint-program-constraints cp) (tan-cong-constraints encoding chip-spec
                                                                      :initial-l2p initial-l2p
                                                                      :final-l2p final-l2p))
      (values cp encoding))))


(defvar *smt-debug-stream* nil)

(defun write-constraint-program (cp smt)
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'null (lambda (stream obj)
                                 (declare (ignore obj))
                                 (format stream "()")))
    (let* ((declarations (mapcar #'declare-int (constraint-program-variables cp)))
           (assertions (mapcar (lambda (c) #!`(assert ,C))
                               (constraint-program-constraints cp)))
           (full-program (append declarations
                                 assertions
                                 #!`((check-sat) (get-model)))))
      (smt-debug-line 'write-constraint-program "~%~{    ~A~%~}" full-program)
      (cl-smt-lib:write-to-smt smt full-program))))

(defun smt-debug-line (ctxt fmt-msg &rest fmt-args)
  (when *smt-debug-stream*
    (apply #'format *smt-debug-stream* (format nil "~A: ~A~%" ctxt fmt-msg) fmt-args)))

(defgeneric attempt-to-recover-model (encoding smt)
  (:documentation "Attempt a model from the given ENCODING and the smt stream SMT.

Returns a hash table mapping variable names to values, or NIL on failure.")
  (:method ((encoding tan-cong-encoding) smt)
    (declare (ignore encoding))
    (let ((output (read smt))
          (model (make-hash-table)))
      (case output
        ((SAT)
         (let ((raw-model (read smt)))
           (smt-debug-line 'attempt-to-recover-model "~A" raw-model)
           (loop :for defn :in raw-model
                 ;; (DEFINE-FUN <var> () INT <val>)
                 ;; TODO: use pattern matching for this
                 :for var := (second defn)
                 :for val := (car (last defn))
                 :do (setf (gethash var model) val)
                 :finally (return model))))
        (t
         (warn "Unable to recover model: expected SAT but got ~A" output)
         nil)))))

(define-condition addresser-failed-condition (simple-error)
  ((instrs :initarg :instrs)
   (chip-spec :initarg :chip-spec)
   (message :initarg :message)))

(defgeneric unpack-model (encoding model)
  (:documentation "Unpack the given MODEL, defined with respect to ENCODING.

Returns a triple (INSTRS, INITIAL-L2P, FINAL-L2P).")
  (:method ((encoding tan-cong-encoding) model)
    (let ((unsorted-instrs nil))
      (labels ((model (var)
                 (gethash var model))
               (bs (l) (tan-cong-encoding-bs encoding l))
               (xs (l) (tan-cong-encoding-xs encoding l))
               (l2p (b q) (tan-cong-encoding-l2p encoding b q))
               (sigma (b k) (tan-cong-encoding-sigma encoding b k))
               (qubits-on-link (k)
                 (map 'list #'identity
                      (chip-spec-qubits-on-link (encoding-chip-spec encoding) k)))
               (wiring-at-block (b)
                 (map 'vector #'identity
                      (loop :for q :below (encoding-num-qubits encoding)
                            :for var := (tan-cong-encoding-l2p encoding b q)
                            :collect (model var))))
               (schedule-instr (b l instr)
                 (push (list b l instr) unsorted-instrs)))
        ;; get gates. those in a common block are not scheduled relative
        ;; to eachother; we adopt the ordering in the initial program
        (loop :for b :below (tan-cong-encoding-num-blocks encoding)
              :for l2p := (loop :for q :below (encoding-num-qubits encoding)
                                :collect (model (l2p b q)))
              :do (smt-debug-line 'unpack-model "l2p at block ~D = ~A" b l2p))
        (loop :for l :from 0
              :for gate :across (encoding-gates encoding)
              :for (q0 q1) := (mapcar #'qubit-index (application-arguments gate))
              :for b := (model (bs l))
              :for link := (model (xs l))
              :for p0 := (model (l2p b q0))
              :for p1 := (model (l2p b q1))
              :for physical-gate := (build-gate (application-operator gate) (application-parameters gate) p0 p1)
              :do (smt-debug-line 'unpack-model
                                  "scheduling ~/quil:instruction-fmt/ to ~/quil:instruction-fmt/ (block ~A)"
                                  gate physical-gate b)
                  (schedule-instr b l physical-gate))

        ;; get swaps
        (loop :for b :below (tan-cong-encoding-num-blocks encoding)
              :do (loop :for k :below (encoding-num-links encoding)
                        :for (p0 p1) := (qubits-on-link k)
                        :when (plusp (model (sigma b k)))
                          :do (schedule-instr (+ b 0.5) k
                                              (build-gate 'swap () p0 p1))))

        (let ((sorted-instrs
                (mapcar #'third
                        (sort unsorted-instrs (lambda (a b)
                                                (or (< (first a) (first b))
                                                    (and (= (first a) (first b))
                                                         (< (second a) (second b))))))))
              (initial-l2p (wiring-at-block 0))
              (final-l2p (wiring-at-block (tan-cong-encoding-num-blocks encoding))))
          (values sorted-instrs initial-l2p final-l2p))))))


(defvar *default-constraint-encoding* ':tb-olsq)

(defun constraint-based-addresser (instrs chip-spec &rest args
                                   &key
                                     initial-rewiring
                                     final-rewiring
                                     (scheme *default-constraint-encoding*)
                                   &allow-other-keys)
  "Address INSTRUCTONS to be compatible with CHIP-SPEC, using the encoding indicated by SCHEME.

Returns three values: (ADDRESSED-INSTRUCTIONS, INITIAL-REWIRING, FINAL-REWIRING)."
  ;; check whether instructions are addressable by these means
  (assert (every (lambda (i) (typep i 'application)) instrs))
  (multiple-value-bind (cp encoding)
      (apply #'encode-constraint-program
             scheme instrs chip-spec
             :initial-l2p initial-rewiring
             :final-l2p final-rewiring
             args)
    (let ((smt (cl-smt-lib:make-smt "z3" "-in" "-smt2")))
      ;; TODO: set options
      (write-constraint-program cp smt)
      (let ((model (attempt-to-recover-model encoding smt)))
        (unless model
          (error 'addresser-failed-condition :instrs instrs :chip-spec chip-spec
                                             :message "Unable to recover model from Z3."))
        (unpack-model encoding model)))))
