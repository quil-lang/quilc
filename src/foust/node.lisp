;;;; node.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/node
  (:documentation
   "This package represents `Node`s in a Foust `Graph`, which can be Rotations, Rotation2s,

Preparations, Measurements, Frames, or Assignments.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:use
   #:cl-quil.foust/pauli
   #:cl-quil.foust/frame
   #:cl-quil.foust/assignments
   #:cl-quil.foust/angle)
  (:export
   #:Angle
   #:Angle
   #:angle-order
   #:angle->radians
   #:Singlet
   #:singlet->pauli
   #:Factor
   #:factor->pair
   #:Rotation
   #:Rotation2
   #:Preparation
   #:Measurement
   #:Node
   #:FrameNode
   #:AssignmentsNode
   #:RotationNode
   #:Rotation2Node
   #:PreparationNode
   #:MeasurementNode
   #:clifford-node?
   #:measurement-node?
   #:frame-node->
   #:merge-nodes))

(in-package #:cl-quil.foust/node)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-class (Singlet :a)
    "A class for representing node types which are defined by a single `Pauli`."
    (singlet->pauli (:a -> Pauli)))

  (define-class (Factor :a)
    "A class for representing node types which are defined by a pair of anticommuting `Pauli`s."
    (factor->pair (:a -> (Tuple Pauli Pauli)))))

(coalton-toplevel

  (define-type Rotation
    "A `Rotation` represents `exp(-i(θ/2)*P)` where `θ` is the `Angle`, and `P` is the `Pauli`."
    (Rotation Pauli Angle))

  (define-instance (Singlet Rotation)
    (define (singlet->pauli (Rotation p _)) p))

  (define-instance (Into Rotation String)
    (define (into (Rotation p theta))
      (mconcat (make-list "Rot[" (into p) ", " (into theta) "]"))))

  (define-type Rotation2
    "A `Rotation2` represents `exp[-i(θ/2)(cos(φ)P+sin(φ)Q)]` where `θ` and `φ` are the `Angle`s and `P` and `Q` are the `Pauli`s."
    (Rotation2 Pauli Pauli Angle Angle))

  (define-instance (Factor Rotation2)
    (define (factor->pair (Rotation2 p q _ _)) (Tuple p q)))

  (define-instance (Into Rotation2 String)
    (define (into (Rotation2 p q theta phi))
      (mconcat
       (make-list
        "Rot2[" (into p) ", " (into q) ", θ = " (into theta) ", φ = " (into phi) "]")))))

(coalton-toplevel

  (define-type Preparation
    "A `Preparation` takes a state to an arbitary (+1 or -1) eigenstate of a `Pauli` and then applies the

second `Pauli` as an operation, if necessary, to bring the state to the (+1) eigenstate of the first `Pauli`.

This flexible representation of Preparations is convenient for stabilizer-based error correction."
    (Preparation Pauli Pauli))

  (define-instance (Factor Preparation)
    (define (factor->pair (Preparation p q)) (Tuple p q)))

  (define-instance (Into Preparation String)
    (define (into (Preparation  p q))
      (mconcat (make-list "Prep[" (into p) ", " (into q) "]"))))

  (define-type Measurement
    "A `Measurement` collapes the quantum state into an eigenstate of a `Pauli`, and stores 0 or 1 if the

resulting state is the (+1) or (-1) eigenstate, respectively."
    (Measurement Pauli UFix))

  (define-instance (Singlet Measurement)
    (define (singlet->pauli (Measurement p _)) p))

  (define-instance (Into Measurement String)
    (define (into (Measurement p v))
      (mconcat (make-list "Meas[" (into p) " -> c" (into v) "]")))))

(coalton-toplevel

  (define-type Node
    (FrameNode Frame)
    (AssignmentsNode Assignments)
    (RotationNode Rotation)
    (Rotation2Node Rotation2)
    (PreparationNode Preparation)
    (MeasurementNode Measurement))

  (define-instance (Into Node String)
  (define (into n)
    (match n
      ((FrameNode f) (into f))
      ((AssignmentsNode a) (into a))
      ((RotationNode rot) (into rot))
      ((Rotation2Node rot2) (into rot2))
      ((PreparationNode p) (into p))
      ((MeasurementNode m) (into m)))))

  (declare clifford-node? (Node -> Boolean))
  (define (clifford-node? n)
    "Is `n` is a `FrameNode`?"
    (match n
      ((FrameNode _) True)
      (_ False)))

  (declare measurement-node? (Node -> Boolean))
  (define (measurement-node? n)
    "Is `n` is a `MeasurementNode`?"
    (match n
      ((MeasurementNode _) True)
      (_ False))))

(coalton-toplevel

  (declare frame-node-> (Frame -> Node -> Node))
  (define (frame-node-> f n)
    "Conjugate `n` by pushing a `f` downstream."
    (match n
      ((FrameNode f-prime)
       ;; If `f` is U and `f-prime` is V, then we take (V)(U) ->
       ;; (U)(V') = (U)[(U+)(V)(U)] so we return the equivalent of
       ;; (U+)(V)(U).
       (FrameNode (msum (make-list (frame-inverse f) f-prime f))))
      ((AssignmentsNode _) n)
      ((RotationNode (Rotation p theta))
       (RotationNode (Rotation (frame-> f p) theta)))
      ((Rotation2Node (Rotation2 p q theta phi))
       (Rotation2Node (Rotation2 (frame-> f p) (frame-> f q) theta phi)))
      ((PreparationNode (Preparation p q))
       (PreparationNode (Preparation (frame-> f p) (frame-> f q))))
      ((MeasurementNode (Measurement p v))
       (MeasurementNode (Measurement (frame-> f p) v))))))

(coalton-toplevel

  (declare node-pauli-commute? (Node -> Pauli -> Boolean))
  (define (node-pauli-commute? n p)
    "Does `n` commute with `p`?"
    (match n
      ((FrameNode f)
       (== p (frame-> f p)))
      ((AssignmentsNode _) True)
      ((RotationNode (Rotation p-one _))
       (commute? p p-one))
      ((Rotation2Node (Rotation2 p-one q-one _ _))
       (all (commute? p) (make-list p-one q-one)))
      ((PreparationNode (Preparation p-one q-one))
       (all (commute? p) (make-list p-one q-one)))
      ((MeasurementNode (Measurement p-one _))
       (commute? p p-one))))

  (define-instance (Commute Node)
    (define (commute? n-one n-two)
      (match (Tuple n-one n-two)
        ;; Frame Nodes
        ((Tuple (FrameNode f-one) (FrameNode f-two))
         (mcommute? f-one f-two))
        ((Tuple (FrameNode _) _)
         (commute? n-two n-one))
        ;; Assignments Nodes
        ((Tuple (AssignmentsNode a-one) (AssignmentsNode a-two))
         (mcommute? a-one a-two))
        ((Tuple (AssignmentsNode _) _) True)
        ;; Pauli Nodes
        ((Tuple (RotationNode (Rotation p _)) _)
         (node-pauli-commute? n-two p))
        ((Tuple (Rotation2Node (Rotation2 p q _ _)) _)
         (all (node-pauli-commute? n-two) (make-list p q)))
        ((Tuple (PreparationNode (Preparation p q)) _)
         (all (node-pauli-commute? n-two) (make-list p q)))
        ((Tuple (MeasurementNode (Measurement p _)) _)
         (node-pauli-commute? n-two p))))))

(coalton-toplevel

  (declare merge-nodes (Node -> Node -> (Optional (Tuple Node Assignments))))
  (define (merge-nodes n-one n-two)
    "If `n-one` and `n-two` can be merged, merge them into a new `Node` and `Assignments`, otherwise return `None`."
    (match (Tuple n-one n-two)

      ;; Compose frames.
      ((Tuple (FrameNode f-one) (FrameNode f-two))
       (Some (Tuple (FrameNode (<> f-two f-one)) (default))))
      ;; Compose assignments.
      ((Tuple (AssignmentsNode a-one) (AssignmentsNode a-two))
       (Some (Tuple (AssignmentsNode (<> a-two a-one)) (default))))
      ;; Compose rotations whose axes are the same.
      ((Tuple (RotationNode (Rotation p-one theta-one))
              (RotationNode (Rotation p-two theta-two)))
       (if (== (get-pauli-operator-map p-one)
               (get-pauli-operator-map p-two))
           (let ((theta ((if (== (get-pauli-sign p-one) (get-pauli-sign p-two)) + -) theta-one theta-two)))
             (Some (Tuple (match (angle-order theta)
                            ((Some order) (FrameNode (frame-from-npi2-rotation p-one order)))
                            ((None) (RotationNode (Rotation p-one theta))))
                          (default))))
           None))
      ;; If the destabilizers are the same and the stabilizers comprise
      ;; the same `PauliOperator`s then remove the first preparation.
      ((Tuple (PreparationNode (Preparation p-one q-one))
              (PreparationNode (Preparation p-two q-two)))
       (if (and (== q-one q-two)
                (== (get-pauli-operator-map p-one)
                    (get-pauli-operator-map p-two)))
           (Some (Tuple n-two (default)))
           None))
      ;; Measuring the same stabilizer? remove the second measurement
      ;; and resolve any sign discrepancy with an Assignments.
      ((Tuple (MeasurementNode (Measurement p-one v-one))
              (MeasurementNode (Measurement p-two v-two)))
       (if (== (get-pauli-operator-map p-one)
               (get-pauli-operator-map p-two))
           (let ((e (ClassicalExpression (singleton v-one) (/= (get-pauli-sign p-one)
                                                               (get-pauli-sign p-two)))))
             (Some (Tuple n-one (add-instruction (default) (Tuple v-two e)))))
           None))
      ;; Compose Rotation2s which operate on the same axis.
      ((Tuple (Rotation2Node (Rotation2 p-one q-one theta-one phi-one))
              (Rotation2Node (Rotation2 p-two q-two theta-two phi-two)))
       (if (and (== (get-pauli-operator-map p-one)
                    (get-pauli-operator-map p-two))
                (== (get-pauli-operator-map q-one)
                    (get-pauli-operator-map q-two)))
           (match (Tuple (== (get-pauli-sign p-one) (get-pauli-sign p-two))
                         (== (get-pauli-sign q-one) (get-pauli-sign q-two)))
             ((Tuple (True) (True))
              (cond
                ((== phi-one phi-two)
                 (Some (Tuple (Rotation2Node (Rotation2 p-one q-one (+ theta-one theta-two) phi-one)) (default))))
                ((== phi-one (+ (Angle 1/2) phi-two))
                 (Some (Tuple (Rotation2Node (Rotation2 p-one q-one (- theta-one theta-two) phi-one)) (default))))
                (True None)))
             ((Tuple (True) (False))
              (cond
                ((== phi-one (negate phi-two))
                 (Some (Tuple (Rotation2Node (Rotation2 p-one q-one (+ theta-one theta-two) phi-one)) (default))))
                ((== phi-one (- (Angle 1/2) phi-two))
                 (Some (Tuple (Rotation2Node (Rotation2 p-one q-one (- theta-one theta-two) phi-one)) (default))))
                (True None)))
             ((Tuple (False) (True))
              (cond
                ((== phi-one (negate phi-two))
                 (Some (Tuple (Rotation2Node (Rotation2 p-one q-one (- theta-one theta-two) phi-one)) (default))))
                ((== phi-one (- (Angle 1/2) phi-two))
                 (Some (Tuple (Rotation2Node (Rotation2 p-one q-one (+ theta-one theta-two) phi-one)) (default))))
                (True None)))
             ((Tuple (False) (False))
              (cond
                ((== phi-one phi-two)
                 (Some (Tuple (Rotation2Node (Rotation2 p-one q-one (- theta-one theta-two) phi-one)) (default))))
                ((== phi-one (+ (Angle 1/2) phi-two))
                 (Some (Tuple (Rotation2Node (Rotation2 p-one q-one (+ theta-one theta-two) phi-one)) (default))))
                (True None))))
           None))

      ;; A Rotation after a Preparation along the same axis will have
      ;; no effect, so remove it.
      ((Tuple (PreparationNode (Preparation p-one _))
              (RotationNode (Rotation p-two _)))
       (if (== (get-pauli-operator-map p-one) (get-pauli-operator-map p-two))
           (Some (Tuple n-one (default)))
           None))
      ;; A Measurement after a Preparation can be handled directly
      ;; with an Assignments.
      ((Tuple (PreparationNode (Preparation p-one _))
              (MeasurementNode (Measurement p-two v)))
       (if (== (get-pauli-operator-map p-one) (get-pauli-operator-map p-two))
           (let ((b (ClassicalBit (/= (get-pauli-sign p-one) (get-pauli-sign p-two)))))
             (Some (Tuple n-one (add-instruction (default) (Tuple v b)))))
           None))
      ;; A rotation before a measurement will have no effect.
      ((Tuple (RotationNode (Rotation p-one _))
              (MeasurementNode (Measurement p-two _)))
       (if (== (get-pauli-operator-map p-one) (get-pauli-operator-map p-two))
           (Some (Tuple n-two (default)))
           None))
      (_ None))))
