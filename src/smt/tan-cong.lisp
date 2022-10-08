;;; tan-cong.lisp
;;;
;;; Author: Erik Davis
;;;
;;; This implements the encoding described in "Optimal Layout
;;; Synthesis for Quantum Computing" by Tan & Cong (arXiv:2007.15671).
;;; In particular, we implement their proposed "transition based
;;; optimal layout synthesis for quantum computing" aka TB-OLSQ.
;;;
;;; At a high level, this involves the following assumption:
;;; instructions can be addressed by a scheme where we alternate
;;; between sections of gate applications and sections with swaps to
;;; update the logical-to-physical mapping (the 'transitions' in
;;; TB-OLSQ). Addressing is therefore a two stage process:
;;;   1. Identify which section each instruction goes in, and what
;;;      swaps are needed to transition between sections.
;;;   2. Solve the easy task of doing addressing within sections (where
;;;      no swaps should be needed).
;;;
;;; The constraint program we produce is used to solve (1), and then (2)
;;; gets handled in UNPACK-MODEL.
;;;
;;; Finally, it's worth noting that since qubit allocation is trivial
;;; for 1Q gates, we rely on a segmentation of the initial
;;; instructions into blocks of 2Q pseudoinstructions, representing
;;; (in general) a 2Q gate along with some adjacent 1Q operators.

(in-package :cl-quil.smt)

;; The following readtable gives access to #! which lets us
;; do case sensitive reading. This is useful because SMT-LIB
;; programs are case sensitive.
(named-readtables:in-readtable :cl-smt-lib)

;;; smt constructors

(defun smt-integer (fmt &rest fmt-args)
  "Construct a SMT integer variable with the indicated name."
  (intern (apply #'format nil fmt fmt-args) #.*package*))

;;; constraints

(defun bound-int (lower-bound var &optional upper-bound)
  "Constrain VAR to satisfy LOWER-BOUND <= VAR < UPPER-BOUND."
  (if upper-bound
      `(|and| (<= ,lower-bound ,var) (< ,var ,upper-bound))
      `(<= ,lower-bound ,var)))

(defun distinct (vars)
  "Constrain VARS to be distinct."
  (cons '|distinct| vars))

(defun declare-int (var)
  "Declare an integer variable VAR."
  `(|declare-fun| ,var () |Int|))

(defun smt-assert (constraint)
  "Construct an assertion from a constraint."
  `(|assert| ,constraint))

;;; The circuit is a rectangular grid, indexed horizontally by time
;;; slices, and vertically by edges of the chip. Every gate L gets
;;; 'space-time' coordinates (XS[L], BS[L]), placing it on the grid.

(defclass tan-cong-encoding (encoding)
  ((num-blocks :initarg :num-blocks
               :reader tan-cong-encoding-num-blocks)
   (free-1qs :initarg :free-1qs
             :reader tan-cong-encoding-free-1qs)
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

(defun make-tan-cong-encoding (instrs chip-spec num-blocks)
  (flet ((array-from-list (list)
           (let ((dims
                   (if (listp (first list))
                       (list (length list) (length (first list)))
                       (length list))))
             (make-array dims :initial-contents list))))
    (multiple-value-bind (segments free-1qs qubits-used) (segment-instructions instrs)
      (let ((num-qubits (cl-quil::chip-spec-n-qubits chip-spec))
            (num-links (cl-quil::chip-spec-n-links chip-spec))
            (num-gates (length segments)))
	(unless (<= (length qubits-used) num-qubits)
	  (addressing-failed "At least ~D qubits are required, but only ~D are available on the chip."
			     (length qubits-used)
			     num-qubits))
	(let ((l2p (array-from-list
                   (loop :for b :to num-blocks ; NOTE: we add a block at the end for final swaps!
                         :collect (loop :for q :below num-qubits
                                        :collect (smt-integer "l2p[~D,~D]" b q)))))
              (bs (array-from-list
                   (loop :for l :below num-gates
                         :collect (smt-integer "bs[~D,]" l))))
              (xs (array-from-list
                   (loop :for l :below num-gates
                         :collect (smt-integer "xs[~D,]" l))))
              (sigma (array-from-list
                      (loop :for b :below num-blocks
                            :collect (loop :for l :below num-links
                                           :collect (smt-integer "sigma[~D,~D]" b l))))))
          (make-instance 'tan-cong-encoding
			 :gates (map 'vector #'identity segments)
			 :free-1qs free-1qs
			 :chip chip-spec
			 :num-blocks num-blocks
			 :l2p l2p :bs bs :xs xs :sigma sigma))))))

(defun initial-gate-dependencies (gate-vec)
  "A graphical representation of logical gate dependencies. 

Returns a list of pairs, where (i, j) indicates that the ith gate should precede the jth gate."
  ;; Note: Technically we could take advantage of commutativity here
  ;; and get a smaller dependency graph. In Tan & Cong they consider a
  ;; variant for addressing QAOA programs where the phase separation
  ;; gates (ZZs) within a round are not constrained in relation to
  ;; eachother.  But at present, we don't do anything of the sort (and
  ;; things are also somewhat complicated by our use of 2Q segments,
  ;; since these are less likely to commute than their constituent
  ;; gates.
  (loop :for i :from 0 :below (length gate-vec)
        :for gi := (aref gate-vec i)
        :for ri := (cl-quil::instruction-resources gi)
        :append (loop :for j :from (1+ i) :below (length gate-vec)
                      :for gj := (aref gate-vec j)
                      :for rj := (cl-quil::instruction-resources gj)
                      :when (resource-intersection ri rj)
                        :collect (cons i j))))

(defun tan-cong-constraints (encoding chip-spec &key initial-l2p final-l2p)
  ;; All of the constraints we generate are described in Tan & Cong. It's probably
  ;; a good idea to have a copy of that at hand if you're trying to make sense of
  ;; this....
  (let ((nb (tan-cong-encoding-num-blocks encoding))
        (nq (encoding-num-qubits encoding))
        (nl (encoding-num-links encoding))
        (ng (encoding-num-gates encoding))
        (dependencies (initial-gate-dependencies (encoding-gates encoding)))
        (constraints nil))
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
               (coerce (cl-quil::chip-spec-qubits-on-link chip-spec l) 'list))
             (links-on-qubit (q)
               (coerce (cl-quil::chip-spec-links-on-qubit chip-spec q) 'list))
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
                  (add-constraint (bound-int 0 p nq))))

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
          (dolist (lp (cl-quil::chip-spec-adj-links chip-spec l))
            (add-constraint #!`(=> (< 0 ,(SIGMA B L))
                                   (= 0 ,(SIGMA B LP)))))))

      ;; consistency between gate assignment and qubit assignment
      ;; 1. if a gate is on an edge e, then the logical endpoints of
      ;;    this gate map to the physical endpoints of e
      (dotimes (b nb)
        (loop :for l :below nl
              :for (p0 p1) := (qubits-on-link l)
              :do (loop :for i :from 0
                        :for segment :across (encoding-gates encoding)
                        :for xi := (xs i)
                        :for bi := (bs i)
                        :for (q0 q1) := (2q-segment-qubits segment)
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

(defmethod encode-constraint-program ((scheme (eql ':tb-olsq)) instrs chip-spec &key
                                                                                  initial-l2p
                                                                                  final-l2p
                                                                                  num-blocks)
  (unless num-blocks
    (addressing-failed "TB-OLSQ requires :NUM-BLOCKS, but none was provided."))
  (let ((cp (make-instance 'constraint-program))
        (encoding (make-tan-cong-encoding instrs chip-spec num-blocks)))
    (setf (constraint-program-declarations cp)
          (mapcar #'declare-int (tan-cong-encoding-variables encoding))
          (constraint-program-assertions cp)
          (mapcar #'smt-assert
                  (tan-cong-constraints encoding chip-spec
                                        :initial-l2p initial-l2p
                                        :final-l2p final-l2p)))
    (values cp encoding)))

(defmethod attempt-to-recover-model ((encoding tan-cong-encoding) smt)
  (declare (ignore encoding))
  (let ((output (read-smt-form smt))
        (model (make-hash-table)))
    (case output
      ((SAT)
       (let ((raw-model (read-smt-form smt)))
         (smt-debug-line 'attempt-to-recover-model "~A" raw-model)
         (loop :for defn :in raw-model 	; syntax is (DEFINE-FUN <var> () INT <val>)
               :for var := (second defn)
               :for val := (car (last defn))
               :do (setf (gethash var model) val)
               :finally (return model))))
      (t
       (warn "Unable to recover model: expected SAT but got ~A" output)
       nil))))

(defmethod unpack-model ((encoding tan-cong-encoding) model)
  (let ((unsorted-instrs nil))
    (labels ((model (var)
               (gethash var model))
             (bs (l) (tan-cong-encoding-bs encoding l))
             (xs (l) (tan-cong-encoding-xs encoding l))
             (l2p (b q) (tan-cong-encoding-l2p encoding b q))
             (sigma (b k) (tan-cong-encoding-sigma encoding b k))
             (qubits-on-link (k)
               (map 'list #'identity
                    (cl-quil::chip-spec-qubits-on-link (encoding-chip-spec encoding) k)))
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
            :for segment :across (encoding-gates encoding)
            :for (q0 q1) := (2q-segment-qubits segment)
            :for b := (model (bs l))
            :for link := (model (xs l))
            :for p0 := (model (l2p b q0))
            :for p1 := (model (l2p b q1))
            :do (smt-debug-line 'unpack-model
				"scheduling ~/cl-quil:instruction-fmt/ to ~D ~D (block ~A)"
				segment p0 p1 b)
                (schedule-instr b l (rewire-2q-segment segment p0 p1)))

      ;; get swaps
      (loop :for b :below (tan-cong-encoding-num-blocks encoding)
            :do (loop :for k :below (encoding-num-links encoding)
                      :for (p0 p1) := (qubits-on-link k)
                      :when (plusp (model (sigma b k)))
                        :do (schedule-instr (+ b 0.5) k
                                            (build-gate 'swap () p0 p1))))

      (let* ((sorted-instrs
              (alexandria:mappend
               (lambda (weighted-instr)
                 (let ((instr (third weighted-instr)))
                   (typecase instr
                     (2q-segment (2q-segment-instrs instr))
                     (otherwise (list instr)))))
               (stable-sort unsorted-instrs
                            (lambda (a b)
                              (or (< (first a) (first b))
                                  (and (= (first a) (first b))
                                       (< (second a) (second b))))))))
             (initial-l2p (wiring-at-block 0))
             (final-l2p (wiring-at-block (tan-cong-encoding-num-blocks encoding)))
	     (rewired-1qs
	       (map 'list (lambda (instr)
			    (build-gate (application-operator instr)
					(application-parameters instr)
					(aref initial-l2p (qubit-index (first (application-arguments instr))))))
		    (tan-cong-encoding-free-1qs encoding))))
        (values (append rewired-1qs sorted-instrs)
                initial-l2p
                final-l2p)))))
