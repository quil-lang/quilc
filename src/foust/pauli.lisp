;;;; pauli.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/pauli
  (:documentation
   "This package represents Hermitian Pauli Strings sparsely, as `Map`s with qubit <-> `PauliOperator`

key <-> value pairs,  and a `Sign` to indicate a phase of +1 or -1.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:use
   #:cl-quil.foust/sign
   #:cl-quil.foust/pauli-operator)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:map #:coalton-library/ord-map))
  (:export
   #:Commute
   #:commute?
   #:mcommute?
   #:PauliOperator
   #:I
   #:X
   #:Y
   #:Z
   #:pauli-operator-*
   #:next-pauli-operator
   #:prev-pauli-operator
   #:levi-civita
   #:Sign
   #:Plus
   #:Minus
   #:sign-*
   #:Pauli
   #:Pauli
   #:get-pauli-sign
   #:get-pauli-operator-map
   #:get-pauli-operator-at
   #:get-pauli-support
   #:subpauli
   #:make-pauli-i
   #:make-pauli-one
   #:make-pauli-two
   #:pauli-*))

(in-package #:cl-quil.foust/pauli)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type Pauli (%Pauli Sign (map:Map UFix PauliOperator)))

  (declare Pauli ((iter:IntoIterator :collection (Tuple UFix PauliOperator)) => Sign -> :collection -> Pauli))
  (define (Pauli sign operator-map)
    "Construct a `Pauli` from a `Sign` and a collection of qubit <-> `PauliOperator` pairs."
    (%Pauli sign (iter:collect! (iter:filter! (compose (complement default?) snd) (iter:into-iter operator-map)))))

  (declare get-pauli-sign (Pauli -> Sign))
  (define (get-pauli-sign (%Pauli sign _)) sign)

  (declare get-pauli-operator-map (Pauli -> (map:Map UFix PauliOperator)))
  (define (get-pauli-operator-map (%Pauli _ operator-map)) operator-map)

  (define-instance (Eq Pauli)
    (define (== (%Pauli sign-one operator-map-one) (%Pauli sign-two operator-map-two))
      (and (== sign-one sign-two) (== operator-map-one operator-map-two))))

  (define-instance (Default Pauli)
    (define (default) (%Pauli Plus map:Empty))))

(coalton-toplevel

  (declare get-pauli-operator-at (UFix -> Pauli -> PauliOperator))
  (define (get-pauli-operator-at qubit (%Pauli _ operator-map))
    "Return the `PauliOperator` assigned to the qubit specified."
    (defaulting-unwrap (map:lookup operator-map qubit)))

  (declare get-pauli-support (Pauli -> (List UFix)))
  (define (get-pauli-support (%Pauli _ operator-map))
    "Return a `List` of the qubits to which a `Pauli` assigns non-identity operators."
    (iter:collect! (map:keys operator-map)))

  (declare subpauli ((List UFix) -> Pauli -> Pauli))
  (define (subpauli qubits (%Pauli sign operator-map))
    "Create a `Pauli` which operates only on the qubits in `qubits`,

with the `Sign` and `PauliOperator`s from the given `Pauli`."
    (%Pauli sign (iter:collect!
                  (iter:filter!
                   (compose (flip list:member qubits) fst)
                   (map:entries operator-map))))))

(coalton-toplevel

  (define-instance (Commute Pauli)
    (define (commute? p q)
      ;; Two `Pauli`s commute if their respective `PauliOperator`s
      ;; anti-commute at an even number of qubits. By folding with
      ;; `boolean-xor`, the `Boolean` `True` will be flipped once for
      ;; each anti-commuting pair of `PauliOperator`s.
      (fold boolean-xor
            True
            (map (fn (qubit)
                   (not (commute? (get-pauli-operator-at qubit p)
                                  (get-pauli-operator-at qubit q))))
                 ;; We take the intersection of the supports because,
                 ;; to all other qubits, at least one of the two
                 ;; `Pauli`s necessarily associates the identity
                 ;; `PauliOperator`.
                 (list:intersection (get-pauli-support p)
                                    (get-pauli-support q)))))))

(coalton-toplevel

  (declare make-pauli-i (Sign -> Pauli))
  (define (make-pauli-i sign)
    "If `sign` is `Plus`, make the universal stabilizer +I, otherwise the universal destabilizer -I."
    (%Pauli sign map:Empty))

  (declare make-pauli-one (PauliOperator -> UFix -> Sign -> Pauli))
  (define (make-pauli-one operator qubit sign)
    "Make a `Pauli` that acts on a single qubit."
    (Pauli sign (singleton (Tuple qubit operator))))

  (declare make-pauli-two (PauliOperator -> PauliOperator -> UFix -> UFix -> Sign -> Pauli))
  (define (make-pauli-two operator-one operator-two qubit-one qubit-two sign)
    "Make a `Pauli` that acts on two qubits."
    (Pauli sign (make-list (Tuple qubit-one operator-one) (Tuple qubit-two operator-two)))))

(coalton-toplevel

  (declare pauli-product-phase (Pauli -> Pauli -> IFix))
  (define (pauli-product-phase p q)
    "Returns the phase of the product of two `Pauli`s. 1 -> 0, i -> 1, -1 -> 2, -i -> 3."
    ((flip mod 4)
     (sum
      (cons (if (== (get-pauli-sign p) (get-pauli-sign q)) 0 2)
            ;; Every anti-cyclic pair of `PauliOperator`s contributes
            ;; -i to the phase, and every cyclic pair contributes i to
            ;; the phase.
            (map (fn (qubit)
                   (levi-civita (get-pauli-operator-at qubit p)
                                (get-pauli-operator-at qubit q)))
                 ;; Here, we take the intersection because at any
                 ;; qubit to which only either `p` or `q` assigns a
                 ;; `PauliOperator`, we are guaranteed at least one of
                 ;; the operands are identity, so the contribution to
                 ;; the product phase is 0.
                 (list:intersection (get-pauli-support p)
                                    (get-pauli-support q)))))))

  (declare pauli-* (Pauli -> Pauli -> Pauli))
  (define (pauli-* p q)
    "If Q and Q commute, return the PQ, otherwise, return -iPQ.

This is the _Hermitian product_ which closes the group of Hermitian paulis."
    (Pauli
     ;; If P and Q commute, then the phase of PQ will be either +1 (0)
     ;; or -1 (2). Otherwise, the phase will be either i (1) or -i
     ;; (3). In the case that P and Q anticommute and PQ has a phase
     ;; of i, the Hermitian product -iPQ will have a phase of -i*i=1.
     (if (> 2 (pauli-product-phase p q)) Plus Minus)
     (map (pair-with (fn (qubit)
                       (pauli-operator-* (get-pauli-operator-at qubit p)
                                         (get-pauli-operator-at qubit q))))
          (list:union (get-pauli-support p)
                      (get-pauli-support q)))))

  (define-instance (Semigroup Pauli)
    (define <> pauli-*))

  (define-instance (Monoid Pauli)
    (define mempty (default))))

(coalton-toplevel

  (define-instance (Into Pauli String)
  (define (into (%Pauli sign operator-map))
    (if (== map:Empty operator-map)
        (mconcat (make-list "(" (into sign) " I)"))
        (mconcat (make-list
                  "("
                  (into sign)
                  ((iter:fold! <> mempty)
                   (map (fn ((Tuple qubit operator))
                          (mconcat (make-list " " (into operator) (into qubit))))
                        (map:entries operator-map)))
                  ")")))))

  (declare enumerate ((List :a) -> (List (Tuple UFix :a))))
  (define (enumerate xs)
    (iter:collect! (iter:enumerate! (iter:into-iter xs))))

  (define-instance (TryInto String Pauli String)
    (define (tryinto string-p)
      (let ((cs (iter:collect! (coalton-library/string:chars string-p)))
            (bad-input-err (Err "Only `String`s in the form \"_sign_PPPPP..PPPP\" can be represented as `Pauli`s,

e.g., \"+IIXZZIY\" for (+ X2 Z3 Z4 Y6), or \"-ZIX\" for (- Z0 X2).")))
        (match cs
          ((Nil) bad-input-err)
          ((Cons sign-c pauli-operators-cs)
           (match (tryinto sign-c)
             ((Err sign-err) (Err sign-err))
             ((Ok sign-s)
              (Ok (Pauli sign-s (enumerate (map (compose unwrap tryinto) pauli-operators-cs))))))))))))
