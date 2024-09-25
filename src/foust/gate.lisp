;;;; gate.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/gate
  (:documentation
   "This package represents `Gate`s and defines their interactions and compilations with/into/from `Node`s and `Frame`s.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:use
   #:cl-quil.foust/pauli
   #:cl-quil.foust/frame
   #:cl-quil.foust/assignments
   #:cl-quil.foust/node)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:map #:coalton-library/ord-map))
  (:export
   #:Gate
   #:PauliGate
   #:S
   #:SDag
   #:H
   #:Permute
   #:TQE
   #:iSwap
   #:Swap
   #:Controlled
   #:R
   #:RR
   #:RMult
   #:R2
   #:R2Mult
   #:Prep
   #:PrepMult
   #:Meas
   #:MeasMult
   #:get-gate-qubits
   #:make-tqe
   #:tqe?
   #:dagger-tqe
   #:gate->node
   #:gate->frame
   #:node->gate
   #:row->gate))

(in-package #:cl-quil.foust/gate)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type Gate

    ;; A standard single-qubit Pauli gate.
    (PauliGate PauliOperator     ;; this operator.
               Ufix              ;; qubit
               )

    ;; A gate equivalent to exp(-i(π/2)P)
    (S PauliOperator             ;; SQRT of this operator.
       Ufix                      ;; qubit
       )

    ;; A gate equivalent to exp(i(π/2)P)
    (SDag PauliOperator          ;; SQRT^dag of this operator.
          UFix                   ;; qubit
          )

    ;; A generalized Hadamard gate which can be conceptualized as a
    ;; 180° rotation about a diagonal axis.
    (H Sign                      ;; the other operators get this sign change
       PauliOperator             ;; this operator gets a sign flip
       UFix                      ;; qubit
       )

    ;; A right-handed permutation of the axes ±x, ±y, and ±z.
    (Permute Sign                ;; + or - x axis
             Sign                ;; + or - y axis
             Sign                ;; + or - z axis
             UFix                ;; qubit
             )

    ;; A gate which performs a phase flip which each qubit is in the
    ;; negative eigenstate of its respective Pauli operator.
    (TQE PauliOperator           ;; control on first qubit
         PauliOperator           ;; control on second qubit
         UFix                    ;; first qubit
         UFix                    ;; second qubit
         Boolean                 ;; follow with a Swap?
         )

    ;; A gate which swaps the indexed qubits and adds a phase of i if
    ;; they are in opposite eigenstates of Z.
    (iSwap UFix                  ;; first qubit
           UFix                  ;; second qubit
           )

    ;; A gate which swaps the indexed qubits.
    (Swap UFix                   ;; first qubit
          UFix                   ;; second qubit
          )

    ;; A subset of the TQE gates which performs the Pauli operator on
    ;; the target (second) qubit, conditioned on the control (first)
    ;; qubit being in the (-1) eigenstate of Z.
    (Controlled PauliOperator    ;; applied to target qubit conditionally
                UFix             ;; control qubit
                UFix             ;; target qubit
                )

    ;; A single-qubit rotation gate equivalent to exp(-i(θ/2)P)
    (R PauliOperator             ;; axis
       Angle                     ;; angle
       UFix                      ;; qubit
       )

    ;; A two-qubit rotation gate equivalent to exp(-i(θ/2)Pᵢ⊗Qⱼ)
    (RR PauliOperator            ;; axis defined by this on qubit one
        PauliOperator            ;; and this on qubit two
        Angle                    ;; angle of rotation
        UFix                     ;; qubit one
        UFix                     ;; qubit two
        )

    ;; A multi-qubit rotation about a Pauli axis
    (RMult Pauli                 ;; axis
           Angle                 ;; angle of rotation
           )

    ;; A single-qubit rotation gate equivalent to
    ;; exp[-i(θ/2)(cos(φ)P+sin(φ)Q)
    (R2 PauliOperator            ;; P
        PauliOperator            ;; Q
        Angle                    ;; θ
        Angle                    ;; φ
        UFix                     ;; qubit
        )

    ;; A multi-qubit rotation along an axis within the span of two
    ;; orthogonal Paulis.
    (R2Mult Pauli                ;; P
            Pauli                ;; Q
            Angle                ;; θ
            Angle                ;; φ
            )

    ;; A preparation of a single qubit in the eigenstate of P
    ;; specified by the sign, and corrected by Q.
    (Prep Sign                   ;; Prepare positive or negative eigenstate
          PauliOperator          ;; of this operator
          PauliOperator          ;; corrected by this operator.
          UFix                   ;; qubit
          )

    ;; A preparation of multiple qubits in the positive eigenstate of
    ;; P and corrected by Q.
    (PrepMult Pauli              ;; Prepare + eigenstate of this Pauli
              Pauli              ;; corrected by this Pauli.
              )

    ;; A measurement of ±Pᵢ stored in the variable specified by the
    ;; second index.
    (Meas Sign                   ;; measure 0 if this (+ or -) eigenstate is found.
          PauliOperator          ;; which operator to measure
          UFix                   ;; qubit
          UFix                   ;; classical-variable ID.
          )

    ;; A measurement of P stored in the variable specified.
    (MeasMult Pauli              ;; Measure this Pauli
              UFix               ;; classical-variable ID.
              ))

  (declare get-gate-qubits (Gate -> (List UFix)))
  (define (get-gate-qubits g)
    "Get a `List` of the qubits on which `g` operates."
    (match g
      ((PauliGate _ qubit)
       (singleton qubit))
      ((S _ qubit)
       (singleton qubit))
      ((SDag _ qubit)
       (singleton qubit))
      ((H _ _ qubit)
       (singleton qubit))
      ((Permute _ _ _ qubit)
       (singleton qubit))
      ((TQE _ _ qubit-one qubit-two _)
       (make-list qubit-one qubit-two))
      ((iSwap qubit-one qubit-two)
       (make-list qubit-one qubit-two))
      ((Controlled _ qubit-one qubit-two)
       (make-list qubit-one qubit-two))
      ((Swap qubit-one qubit-two)
       (make-list qubit-one qubit-two))
      ((R _ _ qubit)
       (singleton qubit))
      ((RR _ _ _ qubit-one qubit-two)
       (make-list qubit-one qubit-two))
      ((RMult pauli-p _)
       (get-pauli-support pauli-p))
      ((R2 _ _ _ _ qubit)
       (singleton qubit))
      ((R2Mult pauli-p pauli-q _ _)
       (list:union (get-pauli-support pauli-p)
                   (get-pauli-support pauli-q)))
      ((Prep _ _ _ qubit)
       (singleton qubit))
      ((PrepMult pauli-p pauli-q)
       (list:union (get-pauli-support pauli-p)
                   (get-pauli-support pauli-q)))
      ((Meas _ _ qubit _)
       (singleton qubit))
      ((MeasMult pauli-p _)
       (get-pauli-support pauli-p))))

  (define-instance (Into Gate String)
    (define (into g)
      (match g
        ((PauliGate p qubit)
         (mconcat (make-list (into p) (into qubit))))
        ((S p qubit)
         (mconcat (make-list "S" (into p) (into qubit))))
        ((SDag p qubit)
         (mconcat (make-list "Sdg" (into p) (into qubit))))
        ((H sign p qubit)
         (mconcat (make-list "H " (into sign) (into p) (into qubit))))
        ((Permute sign-x sign-y sign-z qubit)
         (mconcat (make-list "Permute "
                             (into sign-x) "x, "
                             (into sign-y) "y, "
                             (into sign-z) "z "
                             (into qubit))))
        ((TQE operator-one operator-two qubit-one qubit-two then-swap?)
         (mconcat (make-list "TQE "
                             (into operator-one) (into qubit-one) " "
                             (into operator-two) (into qubit-two)
                             (if then-swap? " ; Swap" ""))))
        ((iSwap qubit-one qubit-two)
         (mconcat (make-list "iSwap " (into qubit-one) ", " (into qubit-two))))
        ((Controlled p control-qubit target-qubit)
         (mconcat (make-list "C" (into p) " " (into control-qubit) ", " (into target-qubit))))
        ((Swap qubit-one qubit-two)
         (mconcat (make-list "Swap " (into qubit-one) ", " (into qubit-two))))
        ((R p theta qubit)
         (mconcat (make-list "R" (into p) " " (into theta) " " (into qubit))))
        ((RR operator-one operator-two theta qubit-one qubit-two)
         (mconcat (make-list "RR "
                             (into operator-one) (into qubit-one) " "
                             (into operator-two) (into qubit-two) " "
                             (into theta))))
        ((RMult p theta)
         (mconcat (make-list "RMult " (into p) " " (into theta))))
        ((R2 p q theta phi qubit)
         (mconcat (make-list "R2 "
                             (into p) (into qubit) " "
                             (into q) (into qubit) " "
                             (into theta) " " (into phi))))
        ((R2Mult p q theta phi)
         (mconcat (make-list "R2Mult " (into p) " " (into q) " " (into theta) " " (into phi))))
        ((Prep sign p q qubit)
         (mconcat (make-list "Prep " (into sign) (into p) " " (into q) " " (into qubit))))
        ((PrepMult p q)
         (mconcat (make-list "Prep " (into p) ", " (into q))))
        ((Meas sign p qubit v)
         (mconcat (make-list "Meas " (into sign) (into p) (into qubit) " -> c" (into v))))
        ((MeasMult p v)
         (mconcat (make-list "Meas " (into p) " -> c" (into v)))))))

  (declare make-tqe (PauliOperator -> PauliOperator -> UFix -> UFix -> Boolean -> Gate))
  (define (make-tqe operator-one operator-two qubit-one qubit-two then-swap?)
    "Make a TQE `Gate` with sorted indices."
    (if (< qubit-one qubit-two)
        (TQE operator-one operator-two qubit-one qubit-two then-swap?)
        (TQE operator-two operator-one qubit-two qubit-one then-swap?)))

  (declare tqe? (Gate -> Boolean))
  (define (tqe? g)
    "Is `g` a two-qubit entangling (TQE) `Gate`?"
    (match g
      ((TQE _ _ _ _ _) True)
      ((Controlled _ _ _) True)
      ((iSwap _ _) True)
      (_ False)))

  (declare dagger-tqe (Gate -> Gate))
  (define (dagger-tqe g)
    "Given a two-qubit entangling (TQE) `gate`, return it's inverse."
    (match g
      ((TQE operator-one operator-two qubit-one qubit-two then-swap?)
       (if then-swap?
           (TQE operator-two operator-one qubit-one qubit-two then-swap?)
           g))
      (_ (error "Expected TQE gate.")))))

(coalton-toplevel

  (declare gate->node (Gate -> Node))
  (define (gate->node g)
    "Cast a Gate to a Node"
    (match g
      ;; Clifford Gates
      ((PauliGate p qubit)
       (FrameNode (frame-from-pauli-gate p qubit)))
      ((S p qubit)
       (FrameNode (frame-from-s False p qubit)))
      ((SDag p qubit)
       (FrameNode (frame-from-s True p qubit)))
      ((H sign p qubit)
       (FrameNode (frame-from-h sign p qubit)))
      ((Permute sign-x sign-y sign-z qubit)
       (FrameNode (frame-from-permute sign-x sign-y sign-z qubit)))
      ((TQE operator-one operator-two qubit-one qubit-two then-swap?)
       (FrameNode (frame-from-tqe operator-one operator-two qubit-one qubit-two then-swap?)))
      ((iSwap qubit-one qubit-two)
       (FrameNode (msum (make-list (frame-from-tqe Z Z qubit-one qubit-two True)
                                   (frame-from-s False Z qubit-one)
                                   (frame-from-s False Z qubit-two)))))
      ((Controlled p control-qubit target-qubit)
       (FrameNode (frame-from-controlled p control-qubit target-qubit)))
      ((Swap qubit-one qubit-two)
       (FrameNode (frame-from-swap qubit-one qubit-two)))

      ;; Rotation Gates
      ((R operator theta qubit)
       (let ((p (make-pauli-one operator qubit Plus)))
         (match (angle-order theta)
           ((Some order) (FrameNode (frame-from-npi2-rotation p order)))
           ((None) (RotationNode (Rotation p theta))))))
      ((RR operator-one operator-two theta qubit-one qubit-two)
       (let ((p (make-pauli-two operator-one operator-two qubit-one qubit-two Plus)))
         (match (angle-order theta)
           ((Some order) (FrameNode (frame-from-npi2-rotation p order)))
           ((None) (RotationNode (Rotation p theta))))))
      ((RMult p theta)
       (match (angle-order theta)
         ((Some order) (FrameNode (frame-from-npi2-rotation p order)))
         ((None) (RotationNode (Rotation p theta)))))
      ((R2 p q theta phi qubit)
       (match (angle-order phi)
         ((Some order)
          (gate->node (R (if (even? order) p q)
                         (if (< 2 order) theta (negate theta))
                         qubit)))
         ((None)
          (Rotation2Node (Rotation2 (make-pauli-one p qubit Plus)
                                    (make-pauli-one q qubit Plus)
                                    theta phi)))))
      ((R2Mult p q theta phi)
       (match (angle-order phi)
         ((Some order)
          (gate->node (RMult (if (even? order) p q)
                             (if (< 2 order) theta (negate theta)))))
         ((None) (Rotation2Node (Rotation2 p q theta phi)))))

      ;; Preparation and Measurement Gates
      ((Prep sign p q qubit)
       (PreparationNode (Preparation (make-pauli-one p qubit sign)
                                     (make-pauli-one q qubit Plus))))
      ((PrepMult p q) (PreparationNode (Preparation p q)))
      ((Meas sign p qubit v) (MeasurementNode (Measurement (make-pauli-one p qubit sign) v)))
      ((MeasMult p v) (MeasurementNode (Measurement p v)))))

  (declare gate->frame (Gate -> Frame))
  (define (gate->frame g)
    "Extract a `Frame` from `g` if it is a `FrameNode`."
    (match (gate->node g)
      ((FrameNode f) f)
      (_ (error "Cannot make Frame from Gate.")))))

(coalton-toplevel

  (declare get-single-qubit-pair (Pauli -> (Tuple UFix PauliOperator)))
  (define (get-single-qubit-pair p)
    "If `p` acts on a single qubit, return it's index and associated operator, otherwise error."
    (match (iter:collect! (map:entries (get-pauli-operator-map p)))
      ((Cons pair (Nil)) pair)
      (_ (error "Pauli does not act on exactly one qubit"))))

  (declare rotation->gate (Rotation -> Gate))
  (define (rotation->gate (Rotation p theta))
    "Wrap a single-qubit `Rotation` into a `Gate`."
    (match (get-single-qubit-pair p)
      ((Tuple qubit operator)
       (R operator (if (== Minus (get-pauli-sign p)) (negate theta) theta) qubit))))

  (declare rotation2->gate (Rotation2 -> Gate))
  (define (rotation2->gate (Rotation2 p q theta phi))
    "Wrap a single-qubit `Rotation2` into a `Gate`."
    (match (Tuple (get-single-qubit-pair p)
                  (get-single-qubit-pair q))
      ((Tuple (Tuple qubit operator-p)
              (Tuple %qubit operator-q))
       (if (/= qubit %qubit)
           (error "`Rotation2` `Pauli`s do not have the same support.")
           (R2 operator-p operator-q
               (if (== Minus (get-pauli-sign p)) (negate theta) theta)
               (if (xor (== Minus (get-pauli-sign p))
                        (== Minus (get-pauli-sign q)))
                   (negate phi) phi)
               qubit)))))

  (declare preparation->gate (Preparation -> Gate))
  (define (preparation->gate (Preparation p q))
    "Wrap a single-qubit `Preparation` into a `Gate`."
    (match (Tuple (get-single-qubit-pair p)
                  (get-single-qubit-pair q))
      ((Tuple (Tuple qubit operator-p)
              (Tuple %qubit operator-q))
       (if (/= qubit %qubit)
           (error "`Preparation` `Pauli`s do not have the same support.")
           (Prep (get-pauli-sign p) operator-p operator-q qubit)))))

  (declare measurement->gate (Measurement -> Gate))
  (define (measurement->gate (Measurement p v))
    "Wrap a single-qubit `Measurement` into a `Gate`."
    (match (get-single-qubit-pair p)
      ((Tuple qubit operator)
       (Meas (get-pauli-sign p) operator qubit v))))

  (declare node->gate (Node -> Gate))
  (define (node->gate n)
    "Wrap a single-qubit non-Clifford `Node` into a `Gate`."
    (match n
      ((RotationNode rotation-r)
       (rotation->gate rotation-r))
      ((Rotation2Node rotation2-r)
       (rotation2->gate rotation2-r))
      ((PreparationNode preparation-p)
       (preparation->gate preparation-p))
      ((MeasurementNode measurement-m)
       (measurement->gate measurement-m))
      ((FrameNode _) (error "Do not use node->gate for FrameNode."))
      ((AssignmentsNode _) (error "Cannot make gate from Assignments.")))))

(coalton-toplevel

  ;; The following helpers were designed to map the patterns in
  ;; single-qubit Frames. There are 24 possible frames:
  ;; 1 identity frame.
  ;; 3 frames which correspond to Paulis: X, Y, and Z.
  ;; 6 frames which correspond to 90° rotations: SX, SY, SZ, and their daggers.
  ;; 6 frames which correspond to Hadamards: H±X H±Y H±Z.
  ;; 8 frames which correspond to each possible right-handed permutation.

  ;; All Pauli Gate frames are (±Zᵢ ±Xᵢ)
  (declare pauli-gate-helper (Sign -> Sign -> UFix -> Gate))
  (define (pauli-gate-helper sign-p sign-q qubit)
    "Helper for casting row to `Gate` when row corresponds to `PauliGate`."
    (PauliGate (pauli-operator-* (if (== Minus sign-p) X I)
                                 (if (== Minus sign-q) Z I))
               qubit))

  (declare permute-helper (Boolean -> Sign -> Sign -> PauliOperator -> PauliOperator -> UFix -> Gate))
  (define (permute-helper dag? sign-p sign-q p q qubit)
    "Helper for casting row to `Gate` when row corresponds to `PauliGate`."
    (Permute (sign-* (if dag? Plus Minus)
                     (if (== p X)
                         (msum (make-list Minus sign-p sign-q))
                         sign-p))
             (sign-* (if dag? Minus Plus)
                     (if (== X p)
                         sign-p
                         (sign-* Minus sign-q)))
             (sign-* (if dag? Minus Plus)
                     (if (== q Z)
                         (msum (make-list Minus sign-p sign-q))
                         sign-q))
             qubit))

  (declare h-helper (Sign -> Sign -> PauliOperator -> PauliOperator -> UFix -> Gate))
  (define (h-helper sign-p sign-q p _ qubit)
    "Helper for casting row to `Gate` when row corresponds to `H`."
    (H (if (== Minus sign-p) sign-q Plus)
       (if (== p Z) Z (pauli-operator-* Z p))
       qubit))

  (declare s-helper (Boolean -> Sign -> Sign -> PauliOperator -> PauliOperator -> UFix -> Gate))
  (define (s-helper dag? sign-p sign-q p q qubit)
    "Helper for casting row to `Gate` when row corresponds to `S` or `SDag`."
    ((if (== dag?
             (cond
               ((== p Z) (== Plus sign-q))
               ((== q X) (== Minus sign-p))
               (True (== Plus sign-p))))
         S SDag)
     (if (== p Z) Z (pauli-operator-* Z p))
     qubit))

  (declare row->gate (Boolean -> (Tuple UFix (Tuple Pauli Pauli)) -> Gate))
  (define (row->gate dag? (Tuple row-index (Tuple p q)))
    "Cast a `Frame` row to a `Gate`."
    (if (or (/= 1 (length (get-pauli-support p)))
            (/= 1 (length (get-pauli-support q))))
        (error "Expected single-qubit Paulis for gate-from-row.")
        (let ((operator-p (get-pauli-operator-at row-index p))
              (operator-q (get-pauli-operator-at row-index q))
              (sign-p (get-pauli-sign p))
              (sign-q (get-pauli-sign q)))
          (if (commute? operator-p operator-q)
              (error "Expected row with anti-commuting `Paulis`.")
              (if (== 1 (levi-civita operator-p operator-q))
                  (if (== Y (pauli-operator-* operator-p operator-q))
                      (pauli-gate-helper sign-p sign-q row-index)
                      (permute-helper dag? sign-p sign-q operator-p operator-q row-index))
                  (if (== (if (== Z operator-p) Minus sign-q)
                          (if (== X operator-q) Minus sign-p))
                      (h-helper sign-p sign-q operator-p operator-q row-index)
                      (s-helper dag? sign-p sign-q operator-p operator-q row-index))))))))
