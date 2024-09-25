;;;; reduce.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/reduce
  (:documentation
   "This package defines functions for producing `Iterator`s of `TQE` gates which, by conjugation,

reduce the costs of various elements involved in a Foust.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:use
   #:cl-quil.foust/pauli
   #:cl-quil.foust/frame
   #:cl-quil.foust/node
   #:cl-quil.foust/gate)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:map #:coalton-library/ord-map))
  (:export
   #:reduce-singlet-node
   #:reduce-node
   #:reduce-row))

(in-package #:cl-quil.foust/reduce)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare permsof2 ((List :a) -> (List (Tuple :a :a))))
  (define (permsof2 l)
    "Create a `List` of pairs of elements in `l`."
    (match l
      ((Nil) Nil)
      ((Cons e es)
       (mconcat
        (make-list (map (Tuple e) es)
                   (map (flip Tuple e) es)
                   (permsof2 es))))))

  (declare find-singlet-tqe ((UFix -> UFix -> Boolean) -> (Tuple UFix PauliOperator) -> (Tuple UFix PauliOperator) -> (Iterator Gate)))
  (define (find-singlet-tqe then-swap?? (Tuple qubit-one operator-one) (Tuple qubit-two operator-two))
    "Given two non-identity `PauliOperator`s, return an `Iterator` of `TQE`s which will take the first `PauliOperator` to identity."
    (let ((then-swap? (then-swap?? qubit-one qubit-two)))
      (if (== qubit-one qubit-two)
          iter:Empty
          (map (fn (operator) (make-tqe operator-one operator qubit-one qubit-two then-swap?))
               (iter:into-iter (list:remove operator-two (make-list X Y Z)))))))

  (declare reduce-singlet-node ((UFix -> UFix -> Boolean) -> Pauli -> (Iterator Gate)))
  (define (reduce-singlet-node then-swap?? p)
    "Return `TQE`s which will reduce the support of a given `Pauli` by one."
    (iter:flat-map! (uncurry (find-singlet-tqe then-swap??))
                    (iter:into-iter
                     (permsof2
                      (iter:collect! (map:entries (get-pauli-operator-map p))))))))

(coalton-toplevel

  (declare find-factor-tqe ((UFix -> UFix -> Boolean)
                            -> (Tuple UFix (Tuple PauliOperator PauliOperator))
                            -> (Tuple UFix (Tuple PauliOperator PauliOperator))
                            -> (Iterator Gate)))
  (define (find-factor-tqe then-swap??
                           (Tuple qubit-one (Tuple p-one q-one))
                           (Tuple qubit-two (Tuple p-two q-two)))
    "Given two pairs of `PauliOperator`s, return an iterator of `TQE`s which will reduce the cost contributed by the pairs."
    (if (== qubit-one qubit-two)
        iter:Empty
        (let ((then-swap? (then-swap?? qubit-one qubit-two))
              (r-one (pauli-operator-* p-one q-one))
              (r-two (pauli-operator-* p-two q-two)))
          (match (Tuple (not (commute? p-one q-one)) (not (commute? p-two q-two)))
            ((Tuple (True) (True))
             ;; Case: {p1,q1}={p2,q2}=0 can be reduced to [p1,q1]=[p2,q2]=0
             (iter:into-iter
              (make-list
               (make-tqe p-one r-two qubit-one qubit-two then-swap?)
               (make-tqe q-one r-two qubit-one qubit-two then-swap?)
               (make-tqe p-one q-two qubit-one qubit-two then-swap?))))
            ((Tuple (True) (False))
             ;; Case: {p1,q1}=[p2,q2]=0 will be handled when i <-> j below.
             iter:Empty)
            ((Tuple (False) (True))
             ;; Case: [p1,q1]={p2,q2}=0 can be reduced to p1=q1=I, {p2,q2}=0
             (iter:once
              (if (== I p-one)
                  (make-tqe q-one p-two qubit-one qubit-two then-swap?)
                  (make-tqe p-one (if (== I q-one) q-two r-two) qubit-one qubit-two then-swap?))))
            ((Tuple (False) (False))
             ;; Case: [p1,q1]=[p2,q2]=0 can be reduced to p1=q1=I, [p2,q2]=0
             ;; only if (p1,p2)=(q1,q2), or if either (p1,p2) or (q1,q2) = (I,I)
             (cond
               ((== (Tuple p-one p-two) (Tuple q-one q-two))
                (find-singlet-tqe then-swap?? (Tuple qubit-one p-one) (Tuple qubit-two p-two)))
               ((disjoin (== (Tuple p-one p-two)) (== (Tuple q-one q-two)) (Tuple I I))
                (find-singlet-tqe then-swap?? (Tuple qubit-one r-one) (Tuple qubit-two r-two)))
               (True iter:empty)))))))

  (declare reduce-factor-node ((UFix -> UFix -> Boolean) -> Pauli -> Pauli -> (Iterator Gate)))
  (define (reduce-factor-node then-swap?? p q)
    "Return `TQE`s which will reduce the cost of two anti-commuting `Pauli`s by one."
    (iter:flat-map! (uncurry (find-factor-tqe then-swap??))
                    (iter:into-iter
                     (permsof2 (map (pair-with (fn (qubit)
                                                 (Tuple (get-pauli-operator-at qubit p)
                                                        (get-pauli-operator-at qubit q))))
                                    (list:union (get-pauli-support p)
                                                (get-pauli-support q))))))))

(coalton-toplevel

  (declare reduce-node ((UFix -> UFix -> Boolean) -> Node -> (Iterator Gate)))
  (define (reduce-node then-swap?? n)
    "Return `TQE`s which reduce the cost of a non-Clifford `node`."
    (match n
      ((FrameNode _) (error "Cannot reduce `FrameNode`."))
      ((AssignmentsNode _) (error "Cannot reduce `AssignmentsNode`."))
      ((RotationNode (Rotation p _)) (reduce-singlet-node then-swap?? p))
      ((Rotation2Node (Rotation2 p q _ _)) (reduce-factor-node then-swap?? p q))
      ((PreparationNode (Preparation p q)) (reduce-factor-node then-swap?? p q))
      ((MeasurementNode (Measurement p _)) (reduce-singlet-node then-swap?? p)))))

(coalton-toplevel

  (declare reduce-canonical-entry-tqe ((UFix -> UFix -> Boolean)
                                       -> (Tuple UFix (Tuple PauliOperator PauliOperator))
                                       -> (Tuple UFix (Tuple PauliOperator PauliOperator))
                                       -> (Iterator Gate)))
  (define (reduce-canonical-entry-tqe then-swap?? canonical-entry extraneous-entry)
    "Given a canonical entry, `PauliOperator` pair corresponding to qubit index = row index, and an extraneous entry,

return `TQE`s which increases the support of the canonical entry and/or reduces the support of the extraneous entry."
    (let ((operators (make-list X Y Z)))
      (match (if (then-swap?? (fst canonical-entry) (fst extraneous-entry))
                 (Tuple True (Tuple extraneous-entry canonical-entry))
                 (Tuple False (Tuple canonical-entry extraneous-entry)))
        ((Tuple then-swap? (Tuple (Tuple qubit-one (Tuple p-one q-one))
                                  (Tuple qubit-two (Tuple p-two q-two))))
         (cond
           ((or (conjoin (== p-one) (== q-one) I)
                (conjoin (== p-two) (== q-two) I))
            ;; Case: p1=q1=I can be raised to [p1,q1]=0
            (iter:into-iter
             (do
              (r-one <- operators)
              (r-two <- (pipe operators (if (not (commute? p-two q-two)) id
                                            (list:remove-if (disjoin (== p-two) (== q-two))))))
               (pure (make-tqe r-one r-two qubit-one qubit-two then-swap?)))))
           ((or (conjoin (== p-one) (== p-two) I)
                (conjoin (== q-one) (== q-two) I)
                (== (Tuple p-one p-two) (Tuple q-one q-two)))
            ;; Case: p1=p2=I or q1,q2=I or (p1p2)=(q1q2) can be reduced to p2=q2=I
            (find-singlet-tqe (fn (_ _) then-swap?)
                              (Tuple qubit-two (if (== p-two I) q-two p-two))
                              (Tuple qubit-one (if (== p-one I) q-one p-one))))
           ((and (commute? p-one q-one) (not (commute? p-two q-two)))
            (if then-swap?
                iter:Empty
                (iter:into-iter
                 (do
                  (r-one <- (list:remove-if (disjoin (== p-one) (== q-one)) operators))
                  (r-two <- (list:remove (pauli-operator-* (if (== p-one I) I q-two)
                                                           (if (== q-one I) I p-two))
                                         operators))
                   (pure (make-tqe r-one r-two qubit-one qubit-two then-swap?))))))
           ((and (not (commute? p-one q-one)) (commute? p-two q-two))
            (if then-swap?
                (iter:into-iter
                 (append (map (fn (r-two)
                                (make-tqe (pauli-operator-* (if (== p-two I) I q-one)
                                                            (if (== q-two I) I p-one))
                                          r-two qubit-one qubit-two then-swap?))
                              operators)
                         (map (fn (r-one)
                                (make-tqe r-one (if (== p-two I) q-two p-two) qubit-one qubit-two then-swap?))
                              (list:remove (pauli-operator-* (if (== p-two I) I q-one)
                                                             (if (== q-two I) I p-one))
                                           operators))))
                (iter:chain!
                 (find-factor-tqe (fn (_ _) then-swap?)
                                  (Tuple qubit-one (Tuple p-one q-one))
                                  (Tuple qubit-two (Tuple p-two q-two)))
                 (find-factor-tqe (fn (_ _) then-swap?)
                                  (Tuple qubit-two (Tuple p-two q-two))
                                  (Tuple qubit-one (Tuple p-one q-one))))))
           (True iter:Empty))))))

  (declare reduce-row ((UFix -> UFix -> Boolean) -> (Tuple UFix (Tuple Pauli Pauli)) -> (Iterator Gate)))
  (define (reduce-row then-swap?? (Tuple row-index (Tuple p q)))
    "Return `TQE`s which will reduce the cost of a `Frame` row."
    (let ((canonical-entry (Tuple row-index (Tuple (get-pauli-operator-at row-index p)
                                                   (get-pauli-operator-at row-index q))))
          (extraneous-entries (map (pair-with (fn (qubit)
                                                (Tuple (get-pauli-operator-at qubit p)
                                                       (get-pauli-operator-at qubit q))))
                                   (list:remove row-index (list:union (get-pauli-support p)
                                                                      (get-pauli-support q))))))
      (iter:chain! (iter:flat-map! (uncurry (find-factor-tqe then-swap??))
                                   (iter:into-iter (permsof2 extraneous-entries)))
                   (iter:flat-map! (reduce-canonical-entry-tqe then-swap?? canonical-entry)
                                   (iter:into-iter extraneous-entries))))))
