;;;; frame.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/frame
  (:documentation
   "This package represents Clifford operators as stabilizer or Pauli tableaus, hereafter referred to as frames.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:use
   #:cl-quil.foust/pauli)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:map #:coalton-library/ord-map))
  (:export
   #:Frame
   #:Frame
   #:get-frame-row-map
   #:get-frame-support
   #:get-pair-at
   #:frame->
   #:frame<-
   #:frame-inverse
   #:frame-compose
   #:frame-from-pauli-gate
   #:frame-from-s
   #:frame-from-h
   #:frame-from-permute
   #:frame-from-tqe
   #:frame-from-controlled
   #:frame-from-swap
   #:frame-from-npi2-rotation))

(in-package #:cl-quil.foust/frame)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type Frame
    "A `Frame` represents the Clifford operator (U) as a sparse map from index i to a `Tuple`,

storing (Udg)(Zi)(U) and (Udg)(Xi)(U), where Udg is the Hermitian conjugate of U,

and Zi and Xi represent the Pauli operators Z and X acting on the qubit i."
    (%Frame (map:Map UFix (Tuple Pauli Pauli))))

  (declare Frame ((iter:IntoIterator :collection (Tuple UFix (Tuple Pauli Pauli))) => :collection -> Frame))
  (define (Frame row-map)
    "Construct a `Frame` from a collection of row-index <-> Pauli-pair pairs."
    (%Frame (iter:collect! (iter:filter! (complement default-row?) (iter:into-iter row-map)))))

  (declare get-frame-row-map (Frame -> (map:Map UFix (Tuple Pauli Pauli))))
  (define (get-frame-row-map (%Frame row-map)) row-map)

  (define-instance (Eq Frame)
    (define (== (%Frame row-map-one) (%Frame row-map-two))
      (== row-map-one row-map-two)))

  (define-instance (Default Frame)
    (define (default) (Frame map:Empty)))

  (declare get-frame-support (Frame -> (List UFix)))
  (define (get-frame-support (%Frame row-map))
    "Return the nontrivial row indices from a `Frame` as a `List`."
    (iter:collect! (map:keys row-map)))

  (declare default-stabilizer (UFix -> Pauli))
  (define (default-stabilizer row-index)
    "Return the default stabilizer `Pauli` for row row-index"
    (make-pauli-one Z row-index Plus))

  (declare default-destabilizer (UFix -> Pauli))
  (define (default-destabilizer row-index)
    "Return the default destabilizer `Pauli` for row row-index"
    (make-pauli-one X row-index Plus))

  (declare default-pair (UFix -> (Tuple Pauli Pauli)))
  (define (default-pair row-index)
    "Return the default stabilizer <-> destabilizer pair for row row-index"
    (bimap default-stabilizer default-destabilizer (Tuple row-index row-index)))

  (declare default-row? ((Tuple UFix (Tuple Pauli Pauli)) -> Boolean))
  (define (default-row? (Tuple row-index pauli-pair))
    "Is `pauli-pair` the default pair for the row at `row-index`?"
    (== pauli-pair (default-pair row-index)))

  (declare get-pair-at (Frame -> UFix -> (Tuple Pauli Pauli)))
  (define (get-pair-at (%Frame row-map) row-index)
    "Return the stabilizer <-> destabilizer pair associated with `row-index`."
    (with-default (default-pair row-index) (map:lookup row-map row-index)))

  (declare get-stabilizer-at (Frame -> UFix -> Pauli))
  (define (get-stabilizer-at (%Frame row-map) row-index)
    "Return the stabilizer associated with `row-index`."
    (with-default (default-stabilizer row-index) (map fst (map:lookup row-map row-index))))

  (declare get-destabilizer-at (Frame -> UFix -> Pauli))
  (define (get-destabilizer-at (%Frame row-map) row-index)
    "Return the stabilizer associated with `row-index`."
    (with-default (default-destabilizer row-index) (map snd (map:lookup row-map row-index))))

  (define-instance (Into Frame String)
    (define (into f)
      (mconcat
       (list:intersperse
        (into #\newline)
        (map (compose
              (fn ((Tuple left right))
                (mconcat (make-list "(" (into left) " " (into right) ")")))
              (get-pair-at f))
             (range 0 (reduce max 0 (get-frame-support f)))))))))

(coalton-toplevel

  (declare frame-> (Frame -> Pauli -> Pauli))
  (define (frame-> f p)
    "Push a `Frame` downstream, conjugating a `Pauli`. If F represents U, then (P)(U) -> (U)[(U+)P(U)]."
    (iter:fold! pauli-*
                (make-pauli-i (get-pauli-sign p))
                (map (fn ((Tuple qubit operator))
                       (match operator
                         ((I) (make-pauli-i Plus))
                         ((X) (get-destabilizer-at f qubit))
                         ((Y) (uncurry pauli-* (get-pair-at f qubit)))
                         ((Z) (get-stabilizer-at f qubit))))
                     (map:entries (get-pauli-operator-map p)))))

  (declare frame<- (Frame -> Pauli -> Pauli))
  (define (frame<- f p)
    "Push a `Frame` upstream and, conjugating a `Pauli`. If F represents U, then (U)(P) -> [(U)P(U+)](U)."
    (pipe
     (iter:fold! (fn ((Tuple p-prime q) (Tuple row-index (Tuple stabilizer-z destabilizer-x)))
                   (match (bimap (complement (commute? p-prime))
                                 (complement (commute? p-prime))
                                 (Tuple stabilizer-z destabilizer-x))
                     ((Tuple (True) (False))
                      (Tuple (pauli-* p-prime destabilizer-x)
                             (Cons (Tuple row-index X) q)))
                     ((Tuple (True) (True))
                      (Tuple (msum (make-list p-prime stabilizer-z destabilizer-x))
                             (Cons (Tuple row-index Y) q)))
                     ((Tuple (False) (True))
                      (Tuple (pauli-* p-prime stabilizer-z)
                             (Cons (Tuple row-index Z) q)))
                     ((Tuple (False) (False)) (Tuple p-prime q))))
                 (Tuple p Nil)
                 (map:entries (get-frame-row-map f)))
     (fn ((Tuple (cl-quil.foust/pauli::%Pauli sign operator-map) q))
       (cl-quil.foust/pauli::%Pauli sign (map:merge operator-map (map:collect q))))))

  (declare frame-inverse (Frame -> Frame))
  (define (frame-inverse f)
    "Invert, or dagger, a `Frame`."
    (Frame
     (map (pair-with (compose
                      ;; [(U+)(Zi)(U)]+ = ((U+)+)(Zi)(U+) = (U)(Zi)(U+).
                      (bimap (frame<- f) (frame<- f))
                      default-pair))
          (map:keys (get-frame-row-map f)))))

  (declare frame-compose (Frame -> Frame -> Frame))
  (define (frame-compose f-two f-one)
    "Compose two `Frame`s such that the equivalent operator is (U2)(U1)."
    (Frame
     (map (pair-with (compose
                      ;; Since U2 is pushed downstream before U1,
                      ;; conjugation by U2 is prior to U1, and we can
                      ;; compose them by conjugating the pairs in U2
                      ;; by U1.
                      (bimap (frame-> f-one) (frame-> f-one))
                      (get-pair-at f-two)))
          (list:union (get-frame-support f-one)
                      (get-frame-support f-two)))))

  (define-instance (Semigroup Frame)
    (define <> frame-compose))

  (define-instance (Monoid Frame)
    (define mempty (default))))

(coalton-toplevel

  (declare map-pair ((PauliOperator -> Pauli) -> (Tuple Pauli Pauli)))
  (define (map-pair f)
    "Map the canonical stabilizer <-> destabilizer pair."
    (bimap f f (Tuple Z X)))

  (declare frame-from-pauli-gate (PauliOperator -> UFix -> Frame))
  (define (frame-from-pauli-gate p qubit)
    "Make a `Frame` corresponding to a Pauli gate."
    (Frame
     (iter:once
      (Tuple qubit
             (map-pair (fn (q)
                         ;; A Pauli gate is a 180deg rotation, so its
                         ;; action is to flip the sign of any Pauli
                         ;; string which anticommutes with it.
                         (make-pauli-one q qubit (if (commute? p q) Plus Minus))))))))

  (declare frame-from-s (Boolean -> PauliOperator -> UFix -> Frame))
  (define (frame-from-s dag? p qubit)
    "Make a `Frame` corresponding to sqrt(p) on `qubit`, daggered if `dag?`."
    (Frame
     (iter:once
      (Tuple qubit
             (map-pair (fn (q)
                         ;; An S (SDag) gate is a 90deg (270deg) rotation.
                         (make-pauli-one (if (commute? p q) q (pauli-operator-* p q))
                                         qubit
                                         (if (== (levi-civita p q) (if dag? -1 +1)) Minus Plus))))))))

  (declare frame-from-h (Sign -> PauliOperator -> UFix -> Frame))
  (define (frame-from-h sign p qubit)
    "Make a `Frame` corresponding to a Hadamard: take p -> -p and flip other axis with `sign`."
    (Frame
     (iter:once
      (Tuple qubit
             (map-pair (fn (q)
                         ;; A Hadamard gate will interchange two axes.
                         (if (commute? p q)
                             (make-pauli-one q qubit Minus)
                             (make-pauli-one (pauli-operator-* p q) qubit sign))))))))

  (declare frame-from-permute (Sign -> Sign -> Sign -> UFix -> Frame))
  (define (frame-from-permute sign-x sign-y sign-z qubit)
    "Make a `Frame` corresponding to the right-handed permutation of axes specified by the `Signs`."
    (let permuter-f = (if (== Plus (msum (make-list sign-x sign-y sign-z)))
                          next-pauli-operator prev-pauli-operator))
    (let signer-f = (fn (q)
                      (if (commute? (permuter-f q) (msum (make-list (if (== sign-x Plus) I X)
                                                                    (if (== sign-y Plus) I Y)
                                                                    (if (== sign-z Plus) I Z))))
                          Plus Minus)))
    (Frame
     (iter:once
      (Tuple qubit (map-pair (fn (q) (let ((p (permuter-f q)))
                                       (make-pauli-one p qubit (signer-f p)))))))))

  (declare frame-from-tqe (PauliOperator -> PauliOperator -> UFix -> UFix -> Boolean -> Frame))
  (define (frame-from-tqe operator-one operator-two qubit-one qubit-two then-swap?)
    "Make a `Frame` corresponding to a TQE, flipping the sign of a state where both qubits are in the corresponding (-) eigenstates."
    (Frame
     ;; The resulting frame will be
     ;;
     ;; row i : (if [P, Z], then Zi, else ZiQj) (if [P, X], then Xi, else XiQj)
     ;; row j : (if [Q, Z], then Zj, else PiZj) (if [Q, X], then Xj, else PiXj)
     ;;
     ;; with the rows swapped if `then-swap?`
     ;;
     ;; e.g. CNOT i j = TQE Z X i j False
     ;;
     ;; row i : Zi   XiXj
     ;; row j : ZiZj   Xj
     (make-list (Tuple (if then-swap? qubit-two qubit-one)
                       (map-pair (fn (operator)
                                   (make-pauli-two operator
                                                   (if (commute? operator operator-one) I operator-two)
                                                   qubit-one qubit-two Plus))))
                (Tuple (if then-swap? qubit-one qubit-two)
                       (map-pair (fn (operator)
                                   (make-pauli-two (if (commute? operator operator-two) I operator-one)
                                                   operator qubit-one qubit-two Plus)))))))

  (declare frame-from-controlled (PauliOperator -> UFix -> UFix -> Frame))
  (define (frame-from-controlled p control-qubit target-qubit)
    "Make a `Frame` from a controlled Pauli gate."
    (frame-from-tqe Z p control-qubit target-qubit False))

  (declare frame-from-swap (UFix -> UFix -> Frame))
  (define (frame-from-swap qubit-one qubit-two)
    "Make a `Frame` for a SWAP operation."
    (Frame (make-list (Tuple qubit-one (default-pair qubit-two))
                      (Tuple qubit-two (default-pair qubit-one)))))

  (declare frame-from-npi2-rotation (Pauli -> UFix -> Frame))
  (define (frame-from-npi2-rotation p order)
    "Make a `Frame` from an integral rotation about a `Pauli` p. Order 0 -> 0, 1 -> π/2, 2 -> π, 3 -> -π/2."
    (cond
      ((== 0 order) (default))
      ((== 2 order)
       ;; These are 180deg rotations, implemented similarly to PauliGate Frames.
       (Frame
        (map (fn ((Tuple qubit operator-one))
               (Tuple qubit
                      (map-pair (fn (operator-two)
                                  (make-pauli-one operator-two qubit
                                                  (if (commute? operator-one operator-two) Plus Minus))))))
             (map:entries (get-pauli-operator-map p)))))
      ((disjoin (== 1) (== 3) order)
       ;; These are 90deg and 270deg rotations, implemented similarly to S and SDag Frames.
       (let ((sign (if (== 1 order) Plus Minus)))
         (Frame
          (map (fn ((Tuple qubit operator-one))
                 (Tuple qubit
                        (map-pair (fn (operator-two)
                                    (if (commute? operator-one operator-two)
                                        (make-pauli-one operator-two qubit Plus)
                                        (pauli-* (make-pauli-one operator-two qubit sign) p))))))
               (map:entries (get-pauli-operator-map p))))))
      (True (error "Invalid `order`. Must be 0, 1, 2, or 3.")))))
