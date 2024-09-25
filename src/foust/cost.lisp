;;;; cost.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/cost
  (:documentation "This package defines costs for various elements involved in the circuit->graph->circuit compilation pipeline.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:use
   #:cl-quil.foust/pauli
   #:cl-quil.foust/frame
   #:cl-quil.foust/node
   #:cl-quil.foust/gate
   #:cl-quil.foust/graph)
  (:local-nicknames
   (#:bits #:coalton-library/bits)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:map #:coalton-library/ord-map))
  (:export
   #:row-cost
   #:get-row-costs
   #:frame-cost
   #:singlet-cost
   #:factor-cost
   #:node-cost
   #:get-node-costs
   #:get-entry-node-costs
   #:delta-singlet-cost
   #:delta-row-cost
   #:delta-frame-cost
   #:delta-node-cost
   #:gate-cost))

(in-package #:cl-quil.foust/cost)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare singlet-cost (Pauli -> UFix))
  (define (singlet-cost p)
    "Return the cost of a singlet `Node`, given it's `Pauli`."
    ;; The cost of a singlet `Node`, one defined by a single `Pauli`, is
    ;; just one less than the number of qubits on which it operates.
    ;; This is because a node operating on a single qubit is directly
    ;; convertible to a `Gate`, and for any other `Node`, one TQE is
    ;; required to reduce it from operating on n to n-1 qubits.
    (let ((support (iter:count! (map:keys (get-pauli-operator-map p)))))
      (if (== 0 support)
          (error "`singlet-cost` encountered the Identity `Pauli`.")
          (1- support))))

  (declare factor-support (Pauli -> Pauli -> (Tuple UFix UFix)))
  (define (factor-support p q)
    "Return the number of qubits on which P or Q operate, and the

number of qubits to which P and Q assign anti-commuting Pauli operators."
    (let ((operator-pairs (map (fn (qubit)
                                       (Tuple (get-pauli-operator-at qubit p)
                                              (get-pauli-operator-at qubit q)))
                                     (list:union (get-pauli-support p)
                                                 (get-pauli-support q)))))
      (Tuple
       ;; weak support = number of qubits on which either P or Q
       ;; operates.
       (length operator-pairs)
       ;; strong support = number of qubits for which P and Q assign
       ;; anti-commuting Pauli operators.
       (list:countby (complement (uncurry commute?)) operator-pairs))))

  (declare factor-cost (Pauli -> Pauli -> UFix))
  (define (factor-cost p q)
    "Return the cost of a factor `Node`, given as a pair of anti-commuting `Pauli`s."
    ;; The cost of a factor `Node`, one defined by anti-commuting
    ;; `Pauli`s is one less than the number of qubits on which it
    ;; operates, plus and additional unit of cost for every extra pair
    ;; of qubits at which the `Pauli`s anti-commute. See
    ;; `foust/reduce` and the README for more details.
    (match (factor-support p q)
      ((Tuple weak-support strong-support)
       (+ (1- weak-support) (bits:shift -1 (1- strong-support)))))))

(coalton-toplevel

  (declare row-cost ((Tuple UFix (Tuple Pauli Pauli)) -> UFix))
  (define (row-cost (Tuple row-index (Tuple p q)))
    "Return the cost of a row of a `Frame`."
    ;; In addition to the cost of a row treated as a factor `Node`
    ;; (see above), the cost of a row depends on its index being the
    ;; same as that of the qubit on which its defining `Pauli`s
    ;; operate. See `foust/reduce` and the README for more details.
    (+ (factor-cost p q)
       (let ((operator-p (get-pauli-operator-at row-index p))
             (operator-q (get-pauli-operator-at row-index q)))
         (cond
           ;; This case requires a SWAP = 3 TQEs
           ((conjoin (== operator-p) (== operator-q) I) 3)
           ;; This case requires a single TQE
           ((commute? operator-p operator-q) 1)
           (True 0)))))

  (declare get-row-costs (Frame -> (map:Map UFix UFix)))
  (define (get-row-costs f)
    "Return the costs of all of the rows in a `Frame` as a `Map`."
    (map:collect! (map (compose (map-fst fst) (pair-with row-cost))
                       (map:entries (get-frame-row-map f)))))

  (declare frame-cost (Frame -> UFix))
  (define (frame-cost f)
    "Return the cost of a `Frame`."
    (iter:sum! (map (compose 1+ row-cost) (map:entries (get-frame-row-map f))))))

(coalton-toplevel

  (declare node-cost (Node -> UFix))
  (define (node-cost n)
    "Return the cost to implement `n` as a single-qubit `Gate` a circuit."
    (match n
      ((FrameNode f) (frame-cost f))
      ((AssignmentsNode _) 0)
      ((RotationNode (Rotation p _)) (singlet-cost p))
      ((Rotation2Node (Rotation2 p q _ _)) (factor-cost p q))
      ((PreparationNode (Preparation p q)) (factor-cost p q))
      ((MeasurementNode (Measurement p _)) (singlet-cost p)))))

(coalton-toplevel

  (declare delta-singlet-cost (Gate -> Pauli -> IFix))
  (define (delta-singlet-cost g p)
    "Compute the change in singlet `Node` cost upon the action of a TQE `Gate`."
    (match g
      ((TQE t-one t-two qubit-one qubit-two _)
       (let ((p-one (get-pauli-operator-at qubit-one p))
             (p-two (get-pauli-operator-at qubit-two p)))
         (+ (cond
              ((commute? p-one t-one) 0) ;; Here, p2 remains unchanged.
              ((== p-two I) +1)          ;; Here, p2 = I goes to P2 in {X, Y, Z}
              ((== p-two t-two) -1)      ;; Here, p2 in {X, Y, Z} goes to p2 = I
              (True 0))                  ;; Here, p2 stays in {X, Y, Z}
             (cond
               ;; See above; cases apply symmetrically.
               ((commute? p-two t-two) 0)
               ((== p-one I) +1)
               ((== p-one t-one) -1)
               (True 0)))))
      (_ (error "Expected TQE Gate for delta-singlet-costs.")))))

(coalton-toplevel

  (declare delta-factor-cost (Gate -> Pauli -> Pauli -> IFix))
  (define (delta-factor-cost g p q)
    "Compute the change in factor `Node` cost upon the action of a TQE `Gate`."
    (match g
      ((TQE _ _ qubit-one qubit-two _)
       (let ((f (gate->frame (dagger-tqe g)))
             ;; Here, rather than matching many cases, we simply
             ;; consider a "subnode", looking only at the qubits affected
             ;; by the TQE, and compute the change in cost directly.
             ;; While it may be more efficient to match cases, as with
             ;; `delta-singlet-cost`, this operation still terminates
             ;; in constant time.
             (old-subpaulis (bimap (subpauli (make-list qubit-one qubit-two))
                                   (subpauli (make-list qubit-one qubit-two))
                                   (Tuple p q)))
             (new-subpaulis (bimap (frame-> f) (frame-> f) old-subpaulis)))
         (match (Tuple (uncurry factor-support new-subpaulis)
                       (uncurry factor-support old-subpaulis))
           ((Tuple (Tuple new-weak-support new-strong-support)
                   (Tuple old-weak-support old-strong-support))
            (+ (- (into new-weak-support) (into old-weak-support))
               (bits:shift -1 (- (into new-strong-support) (into old-strong-support))))))))
      (_ (error "Expected TQE Gate for delta-factor-cost.")))))

(coalton-toplevel

  (declare delta-row-cost (Gate -> (Tuple UFix (Tuple Pauli Pauli)) -> IFix))
  (define (delta-row-cost g (Tuple row-index (Tuple p q)))
    "Compute the change in row cost upon the action of a TQE `Gate`."
    (match g
      ((TQE _ _ qubit-one qubit-two _)
       (if (disjoin (== qubit-one) (== qubit-two) row-index)
           (- (let ((f (gate->frame (dagger-tqe g))))
                (into (row-cost (Tuple row-index (Tuple (frame-> f p) (frame-> f q))))))
               (into (row-cost (Tuple row-index (Tuple p q)))))
           (delta-factor-cost g p q)))
      (_ (error "Expected TQE `Gate` for delta-row-cost."))))

  (declare delta-frame-cost (Gate -> Frame -> IFix))
  (define (delta-frame-cost g f)
    "Compute the change in `Frame` cost upon action of a TQE `gate`."
    (iter:sum! (map (delta-row-cost g) (map:entries (get-frame-row-map f))))))

(coalton-toplevel

  (declare delta-node-cost (Gate -> Node -> IFix))
  (define (delta-node-cost g n)
    "Compute the change in `Node` cost upon action of a TQE `Gate`."
    (match n
      ((FrameNode f) (delta-frame-cost g f))
      ((AssignmentsNode _) (error "`AssignmentsNode` has no cost."))
      ((RotationNode (Rotation p _)) (delta-singlet-cost g p))
      ((Rotation2Node (Rotation2 p q _ _)) (delta-factor-cost g p q))
      ((PreparationNode (Preparation p q)) (delta-factor-cost g p q))
      ((MeasurementNode (Measurement p _)) (delta-singlet-cost g p)))))

(coalton-toplevel

  (declare gate-cost (Boolean -> Graph -> Gate -> IFix))
  (define (gate-cost preserve-state? (Graph _ node-map f _) g)
    "Calculate the cost of adding a TQE `Gate`, by pushing the associated `Frame` through the `Graph`."
    ;; The frame cost only matters if it will be synthesized in the preserve-state case.
    (pipe (iter:sum! (map (compose (delta-node-cost g) fst) (map:values node-map)))
          (if preserve-state? (+ (delta-frame-cost g f)) id))))
