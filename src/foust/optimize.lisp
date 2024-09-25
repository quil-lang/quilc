;;;; optimize.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/optimize
  (:documentation
   "This package defines functions for compiling commuting multi-qubit measurements into single-qubit measurements,

and (TODO:) for preparation-based optimizations applied before compiling a Graph to a Circuit.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:use
   #:cl-quil.foust/pauli
   #:cl-quil.foust/frame
   #:cl-quil.foust/assignments
   #:cl-quil.foust/gate
   #:cl-quil.foust/node
   #:cl-quil.foust/circuit
   #:cl-quil.foust/graph
   #:cl-quil.foust/cost
   #:cl-quil.foust/reduce)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list))
  (:export
   #:search-measurements
   #:map-measurements
   #:optimize-graph))

(in-package #:cl-quil.foust/optimize)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare pauli-agreement-at ((iter:IntoIterator :a Pauli) => UFix -> :a -> (Optional PauliOperator)))
  (define (pauli-agreement-at qubit ps)
    "Check if all `Pauli`s in a collection commute at a particular index, and return the nontrivial `PauliOperator` if so."
    ;; Take all of the pauli operators at some index.
    (let ((operators (map (get-pauli-operator-at qubit) (iter:into-iter ps))))
      ;; find the first non-identity operator.
      (match (iter:next! (iter:filter! (/= I) operators))
        ;; if all remaining operators commute with the found operator
        ;; then each is either identity or the same as that operator
        ;; so that operator can be returned as the agreement of the
        ;; `Pauli`s at the specified index. Otherwise, they do not
        ;; agree.
        ((Some p) (if (iter:every! (commute? p) operators) (Some p) None))
        ;; Otherwise, all `Pauli`s are identity on this qubit, so no
        ;; non-trivial operator is agreed on.
        ((None) None))))

  (declare mask-support ((List UFix) -> Pauli -> (List UFix)))
  (define (mask-support support p)
    "Return the support of `p` which is also included in `support`."
    (list:intersection support (get-pauli-support p))))

(coalton-toplevel

  (declare process-zero-cost-measurements ((Tuple4 (List Pauli) (List UFix) Frame Circuit) ->
                                           (Tuple4 (List Pauli) (List UFix) Frame Circuit)))
  (define (process-zero-cost-measurements tuple4-sqfc)
    "Take all free `Measurement`s to `Meas`s in the `Circuit`."
    (fold (fn ((Tuple4 ps support f c) p)
            (match (mask-support support p)
              ((Cons qubit (Nil))
               (Tuple4 (list:remove p ps) (list:remove qubit support) f
                       (pipe (Meas Plus (get-pauli-operator-at qubit p) qubit (get-circuit-next-fresh-index c))
                             (add-gate c)
                             (map-circuit-assignments assignments-increment))))
              ((Cons _ _) (Tuple4 ps support f c))
              ((Nil) (Tuple4 (list:remove p ps) support f c))))
          tuple4-sqfc
          (.first tuple4-sqfc)))

  (declare process-min-cost-measurement ((Gate -> IFix)
                                         -> (UFix -> UFix -> Boolean)
                                         -> (Tuple4 (List Pauli) (List UFix) Frame Circuit)
                                         -> (Tuple4 (List Pauli) (List UFix) Frame Circuit)))
  (define (process-min-cost-measurement clifford-costs then-swap?? (Tuple4 ps support f c))
    "Find the cheapest `Measurement` and reduce it's cost."
    (match (iter:minimize-by! (compose length (mask-support support)) (iter:into-iter ps))
      ((Some p)
       (if (== 0 (singlet-cost (subpauli support p)))
           (process-zero-cost-measurements (Tuple4 ps support f c))
           (match (iter:minimize-by! (fn (g) (+ (clifford-costs g) (sum (map (delta-singlet-cost g) ps))))
                                     (reduce-singlet-node then-swap?? p))
             ((Some g)
              (let ((f-prime (gate->frame (dagger-tqe g))))
                (Tuple4 (map (frame-> f-prime) ps) support (<> f-prime f) (add-gate c g))))
             ((None) (error "Unable to reduce Pauli.")))))
      ((None) (Tuple4 ps support f c))))

  (declare fold-measurements ((Gate -> IFix)
                              -> (UFix -> UFix -> Boolean)
                              -> (Tuple4 (List Pauli) (List UFix) Frame Circuit)
                              -> (Tuple4 (List Pauli) (List UFix) Frame Circuit)))
  (define (fold-measurements clifford-costs then-swap?? tuple4-sqfc)
    "Process all free `Measurement`s and then one cheap `Measurement`, and repeat until all `Measurement`s

are processed into the `Circuit`."
    (if (list:null? (.first tuple4-sqfc))
        tuple4-sqfc
        (pipe tuple4-sqfc
              process-zero-cost-measurements
              (process-min-cost-measurement clifford-costs then-swap??)
              (fold-measurements clifford-costs then-swap??))))

  (declare fold-general ((Tuple4 (List Pauli) (List UFix) Frame Circuit) ->
                         (Tuple4 (List Pauli) (List UFix) Frame Circuit)))
  (define (fold-general tuple4-sqfc)
    "Implement all `PauliOperator`s which are agreed upon as single-qubit `Measurement`s."
    (fold (fn ((Tuple4 ps support f c) qubit)
            (match (pauli-agreement-at qubit ps)
              ((Some p)
               (Tuple4 ps (list:remove qubit support) f
                       (map-circuit-assignments
                        assignments-increment
                        (add-gate c (Meas Plus p qubit (get-circuit-next-fresh-index c))))))
              ((None) (Tuple4 ps support f c))))
          tuple4-sqfc
          (.second tuple4-sqfc)))

  (declare search-measurements (Boolean -> (Gate -> IFix) -> (UFix -> UFix -> Boolean)
                                        -> UFix -> (List Measurement) -> (Tuple Circuit Frame)))
  (define (search-measurements general? clifford-costs then-swap?? fresh-index measurements)
    "Collect a `List` of `Measurement`s as single-qubit `Measurement`s and `TQE`s in a `Circuit` and a `Frame` which

encodes the `Assignments`."
    (let ((ps (remove-duplicates (map singlet->pauli measurements))))
      (match (pipe (Tuple4 ps
                           (remove-duplicates (concatmap get-pauli-support ps))
                           (default)
                           (set-circuit-assignments (default) (null-assignments fresh-index)))
                   (if general? fold-general id)
                   (fold-measurements clifford-costs then-swap??))
        ((Tuple4 _ _ f c)
         (Tuple c (frame-inverse f)))))))

(coalton-toplevel

  (declare get-measurement-pairs-from-measurements ((List Measurement) -> (List (Tuple Pauli UFix))))
  (define (get-measurement-pairs-from-measurements measurements)
    "Get the `Measurement`s in a `List` as an `Iterator` of `Pauli`s and classical variable indices."
    (map (fn ((Measurement p v)) (Tuple p v)) measurements))

  (declare get-measurement-pairs-from-circuit (Circuit -> (List (Tuple Pauli UFix))))
  (define (get-measurement-pairs-from-circuit (Circuit gs _))
    "Get the `Meas` `Gate`s in a `Circuit` as an `Iterator` of `Pauli`s and classical variable indices."
    (map (fn (measurement)
           (match measurement
             ((MeasurementNode (Measurement p v)) (Tuple p v))
             (_ (error "A `Node` that is not a `MeasurementNode` was missed by the filter."))))
         (filter measurement-node? (map gate->node gs))))

  (declare map-single-measurement ((Tuple Pauli UFix) -> (List (Tuple Pauli UFix)) -> Frame ->
                                   (Tuple UFix ClassicalExpression)))
  (define (map-single-measurement (Tuple measurement-pauli v) measurement-pairs f)
    "Recover a `Measurement` via a classical instruction, given a `Frame` from the `measurement-search` function."
    (match (fold (fn (expression-assignment measurement-pair)
                   (match (get-pauli-support (fst measurement-pair))
                     ((Cons qubit (Nil))
                      (if (== (get-pauli-operator-at qubit measurement-pauli)
                              (get-pauli-operator-at qubit (frame-> f (fst measurement-pair))))
                          (bimap (pauli-* (frame-> f (fst measurement-pair)))
                                 (classical-xor (ClassicalVariable (snd measurement-pair)))
                                 expression-assignment)
                          expression-assignment))
                     (_ (error "Encountered non-single-qubit `Measurement`."))))
                 (Tuple (default) (default))
                 measurement-pairs)
      ((Tuple p e)
       (if (== (get-pauli-sign measurement-pauli) (get-pauli-sign p))
           (Tuple v e)
           (Tuple v (classical-bit-flip e))))))

  (declare map-measurements ((List Measurement) -> Circuit -> Frame -> Assignments))
  (define (map-measurements measurements c f)
    "Given the output of the `measurement-search` function, provide the `Assignments` object that recovers

the intended measurement statistics."
    (let ((measurement-pairs (get-measurement-pairs-from-circuit c)))
      (fold (fn (a measurement-pair)
              (add-instruction a (map-single-measurement measurement-pair measurement-pairs f)))
            (default)
            (get-measurement-pairs-from-measurements measurements)))))

(coalton-toplevel

  (declare optimize-graph (Boolean -> (Gate -> IFix) -> Graph -> Graph))
  (define (optimize-graph _preserve-state? _clifford-costs g)
    "TODO: Add on-preparation intermediary optimization steps."
    g))
