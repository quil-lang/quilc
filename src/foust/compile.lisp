;;;; compile.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/compile
  (:documentation
   "This package defines the functions that compile `Circuit`s to `Graph`s and `Graph`s to `Circuit`s,

including the compilation of `Frame`s into single-qubit Clifford `Gate`s and `TQE`s.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:use
   #:cl-quil.foust/pauli
   #:cl-quil.foust/frame
   #:cl-quil.foust/assignments
   #:cl-quil.foust/node
   #:cl-quil.foust/gate
   #:cl-quil.foust/circuit
   #:cl-quil.foust/graph
   #:cl-quil.foust/cost
   #:cl-quil.foust/reduce
   #:cl-quil.foust/optimize)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:map #:coalton-library/ord-map)
   (#:tree #:coalton-library/ord-tree))
  (:local-nicknames
   (#:fraction #:coalton-library/math/fraction))
  (:export
   #:circuit->graph
   #:graph->circuit))

(in-package #:cl-quil.foust/compile)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare add-node (Node -> Graph -> Graph))
  (define (add-node n g)
    "Add any `Node` to the end of a `Graph`."
    (match n
      ((FrameNode f) (map-graph-frame (flip <> f) g))
      ((AssignmentsNode a) (map-graph-assignments (<> a) g))
      (_
       ;; If the new `Node` merges with a previously-terminating `Node`,
       ;; remove the old `Node`, and re-add the merged `Node`.
       (for (Tuple n0-index n0) in (get-terminal-tuples g)
         (match (merge-nodes n0 n)
           ((Some (Tuple n0-prime a0))
            (return (pipe (remove-vertex g n0-index)
                          (map-graph-assignments (<> a0))
                          (add-node n0-prime))))
           ((None) (continue))))
       ;;
       ;; Draw edges to new node from all nodes which do not commute
       ;; with it.
       ;;
       ;; TODO: Efficiently reduce the number of edges drawn. This is
       ;; a nontrivial problem. Consider the following graph.
       ;;
       ;; ->-1->-2->-3->-4
       ;;         \   \       <- 7
       ;;          5->-6
       ;;
       ;; It may be the case that 7 commutes with 3, 4 and 6, but not
       ;; with 1, 2, or 5. In this case, we need to draw an edge from
       ;; 5 to 7, but we don't want to draw it twice. The result that
       ;; we want is
       ;;
       ;; ->-1->-2->-3->-4
       ;;         \   \
       ;;          5->-6
       ;;           \
       ;;            7
       ;;
       ;; Currently, the algorithm *will* draw the edge from 1 and 2 to 7.
       ;;
       (let ((node-index (get-graph-fresh-node-index g)))
         (iter:fold! (fn (g-prime (Tuple n0-index (Tuple n0 _)))
                       (if (commute? n n0) g-prime (add-edge g-prime n0-index node-index)))
                     (add-vertex g n)
                     (map:entries (get-graph-node-map g)))))))

  (declare circuit->graph (Circuit -> Graph))
  (define (circuit->graph c)
    "Compile a `Circuit` to a `Graph`."
    (fold (fn (g n)
            (match n
              ((FrameNode f) (map-graph-frame (<> f) g))
              ((MeasurementNode (Measurement p v))
               (add-node (frame-node->
                          (get-graph-frame g)
                          (MeasurementNode (Measurement p (get-assignments-next-fresh-index (get-graph-assignments g)))))
                         (map-graph-assignments (flip assign-fresh v) g)))
              (_ (add-node (frame-node-> (get-graph-frame g) n) g))))
          (map-graph-assignments (<> (get-circuit-assignments c)) (default))
          (map gate->node (get-circuit-gates c)))))

(coalton-toplevel

  ;; The following protocol is used to reconstruct a `Graph` as a
  ;; `Circuit`, except the terminal `Frame` in the case where the quantum
  ;; state is to be preserved, or the terminal `Measurement`s in the
  ;; case where the quantum state is to be released.

  (declare keep-if-not-preserve? (Graph -> (Tuple UFix Node) -> Boolean))
  (define (keep-if-not-preserve? g (Tuple node-index n))
    "Should `n` be processed in the case where `preserve-state? = False`?"
    (not (and (measurement-node? n) (is-terminal-at? g node-index))))

  (declare continue-search? (Boolean -> Graph -> Boolean))
  (define (continue-search? preserve-state? g)
    "Should we continue compiling non-Clifford `Node`s?"
    (not (if preserve-state? (graph-empty? g) (graph-empty-except-terminal-measurements? g))))

  (declare process-zero-cost ((Gate -> (Tuple (List Gate) Frame))
                              -> (Tuple Circuit Graph)
                              -> (Tuple UFix Node)
                              -> (Tuple Circuit Graph)))
  (define (process-zero-cost corrections (Tuple c g) (Tuple node-index n))
    "Remove a free `Node` from the `Graph` and add it as a `Gate` to the `Circuit`."
    (let ((correct-and-add-gate
            (compose (bimap (fold add-gate c) (push-frame (remove-vertex g node-index))) corrections)))
      (match n
        ((MeasurementNode (Measurement p v))
         (if (== Minus (get-pauli-sign p))
             (process-zero-cost corrections
                                (Tuple c (map-graph-assignments
                                          (flip add-instruction (Tuple v (classical-bit-flip (ClassicalVariable v))))
                                          g))
                                (Tuple node-index (MeasurementNode (Measurement (pauli-* p (make-pauli-i Minus)) v))))
             (correct-and-add-gate (node->gate n))))
        (_ (correct-and-add-gate (node->gate n))))))

  (declare fold-zero-cost (Boolean
                           -> (Gate -> (Tuple (List Gate) Frame))
                           -> (Tuple Circuit Graph) -> (Tuple Circuit Graph)))
  (define (fold-zero-cost preserve-state? corrections (Tuple c g))
    "Transfer all free entry `Node`s from the `Graph` to the `Circuit`."
    (let ((zero-cost-tuples (iter:filter! (fn ((Tuple _ n)) (== 0 (node-cost n))) (get-entry-tuples g))))
      (iter:fold! (process-zero-cost corrections)
                  (Tuple c g)
                  ((if preserve-state? id (iter:filter! (keep-if-not-preserve? g))) zero-cost-tuples))))

  (declare process-min-cost (Boolean
                             -> (Gate -> (Tuple (List Gate) Frame))
                             -> (Gate -> IFix)
                             -> (UFix -> UFix -> Boolean)
                             -> (Tuple Circuit Graph)
                             -> (Tuple UFix Node)
                             -> (Tuple Circuit Graph)))
  (define (process-min-cost preserve-state? corrections clifford-costs then-swap?? (Tuple c gr) (Tuple node-index n))
    "Process the cheapest entry `Node` by adding a `TQE` to the `Circuit` and pushing it through the `Graph`."
    (if (== 0 (node-cost n))
        (process-zero-cost corrections (Tuple c gr) (Tuple node-index n))
        (match (iter:minimize-by! (fn (g)
                                    (+ (clifford-costs g) (gate-cost preserve-state? gr g)))
                                  (reduce-node then-swap?? n))
          ((Some g) (Tuple (add-gate c g) (push-frame gr (gate->frame (dagger-tqe g)))))
          ((None) (error "Unable to reduce `Node`!")))))

  (declare reduce-min-cost (Boolean
                            -> (Gate -> (Tuple (List Gate) Frame))
                            -> (Gate -> IFix)
                            -> (UFix -> UFix -> Boolean)
                            -> (Tuple Circuit Graph) -> (Tuple Circuit Graph)))
  (define (reduce-min-cost preserve-state? corrections clifford-costs then-swap?? (Tuple c g))
    "Reduce the cost of the cheapest entry `Node` of the `Graph` by adding a `TQE` to the `Circuit`."
    (pipe (get-entry-tuples g)
          (if preserve-state? id (iter:filter! (keep-if-not-preserve? g)))
          (iter:minimize-by! (compose node-cost snd))
          (unwrap-or-else (process-min-cost preserve-state? corrections clifford-costs then-swap?? (Tuple c g))
                          (fn () (Tuple c g)))))

  (declare search-non-clifford (Boolean
                                -> (Gate -> (Tuple (List Gate) Frame))
                                -> (Gate -> IFix)
                                -> (UFix -> UFix -> Boolean)
                                -> (Tuple Circuit Graph)
                                -> (Tuple Circuit Graph)))
  (define (search-non-clifford preserve-state? corrections clifford-costs then-swap?? (Tuple c g))
    "Transpile all `Node`s from the `Graph` to the `Circuit`, ignoring terminal `Measurement`s if `hold? = False`."
    (if (not (continue-search? preserve-state? g))
        (Tuple c g)
        (pipe (Tuple c g)
              (fold-zero-cost preserve-state? corrections)
              (reduce-min-cost preserve-state? corrections clifford-costs then-swap??)
              (search-non-clifford preserve-state? corrections clifford-costs then-swap??)))))

(coalton-toplevel

  ;; The following protocol is used to generate a sequences of `TQE`s
  ;; single-qubit Clifford `Gate`s that is equivalent to a particular
  ;; `Frame`. Within the Foust pipeline, it is used exclusively to
  ;; reconstruct the terminal `Frame` when the quantum state is to be
  ;; preserved.

  (declare process-zero-cost-row ((Tuple Circuit Frame) -> (Tuple UFix (Tuple Pauli Pauli)) -> (Tuple Circuit Frame)))
  (define (process-zero-cost-row (Tuple c f) row)
    "Remove a free row from the `Frame` and add it as a `Gate` to the `Circuit`."
    (Tuple (add-gate c (row->gate False row))
           (<> f (gate->frame (row->gate True row)))))

  (declare fold-zero-cost-rows ((map:Map UFix UFix) -> (Tuple Circuit Frame) -> (Tuple Circuit Frame)))
  (define (fold-zero-cost-rows row-costs (Tuple c f))
    "Compile all free rows into single-qubit Clifford `Gate`s."
    (iter:fold! process-zero-cost-row
                (Tuple c f)
                (iter:filter! (fn ((Tuple row _))
                                (== 0 (unwrap (map:lookup row-costs row))))
                              (map:entries (get-frame-row-map f)))))

  (declare process-min-cost-row ((Gate -> IFix)
                                 -> (UFix -> UFix -> Boolean)
                                 -> (Tuple Circuit Frame)
                                 -> (Tuple UFix (Tuple Pauli Pauli))
                                 -> (Tuple Circuit Frame)))
  (define (process-min-cost-row  clifford-costs then-swap?? (Tuple c f) row)
    "Process the cheapest row with a `TQE`."
    (match (iter:minimize-by! (fn (g) (+ (clifford-costs g) (delta-frame-cost g f)))
                              (reduce-row then-swap?? row))
      ((Some g) (Tuple (add-gate c g) (<> f (gate->frame (dagger-tqe g)))))
      ((None) (error "Unable to reduce row!"))))

  (declare reduce-min-cost-row ((Gate -> IFix)
                                -> (UFix -> UFix -> Boolean)
                                -> (map:Map UFix UFix)
                                -> (Tuple Circuit Frame)
                                -> (Tuple Circuit Frame)))
  (define (reduce-min-cost-row clifford-costs then-swap?? row-costs (Tuple c f))
    "Reduce the cost of the cheapest row."
    (match (iter:minimize-by! (.< unwrap (map:lookup row-costs) fst) (map:entries (get-frame-row-map f)))
      ((Some row)
       (if (== 0 (unwrap (map:lookup row-costs (fst row))))
           (process-zero-cost-row (Tuple c f) row)
           (process-min-cost-row clifford-costs then-swap?? (Tuple c f) row)))
      ((None) (Tuple c f))))

  (declare synthesize-frame ((Gate -> IFix)
                             -> (UFix -> UFix -> Boolean)
                             -> (Tuple Circuit Frame)
                             -> (Tuple Circuit Frame)))
  (define (synthesize-frame clifford-costs then-swap?? (Tuple c f))
    "Convert a `Frame` to a series of `TQE`s and single-qubit Clifford `Gate`s."
    (if (== f (default))
        (Tuple c f)
        (let ((row-costs (get-row-costs f)))
          (pipe (Tuple c f)
                (fold-zero-cost-rows row-costs)
                (reduce-min-cost-row clifford-costs then-swap?? row-costs)
                (synthesize-frame clifford-costs then-swap??))))))

(coalton-toplevel

  (declare add-measurement-with-corrections ((Gate -> (Tuple (List Gate) Frame))
                                             -> Circuit
                                             -> Gate
                                             -> Circuit))
  (define (add-measurement-with-corrections corrections c m)
    "Assuming `measurement-m` is a single-qubit `Measurement`, after which the qubit will not be operated on,

correct and add the `Measurement` to the `Circuit`."
    (pipe m corrections fst (fold add-gate c)))

  (declare synthesize-measurements ((Gate -> (Tuple (List Gate) Frame))
                                    -> (Gate -> IFix)
                                    -> (UFix -> UFix -> Boolean)
                                    -> (Tuple Circuit Graph)
                                    -> Circuit))
  (define (synthesize-measurements corrections clifford-costs then-swap?? (Tuple c g))
    "Synthesize terminal `Measurement`s as single-qubit `Measurement`s, and a correcting `Assignments`."
    (let ((terminal-measurements (map (fn ((Tuple node-index n))
                                        (if (is-terminal-at? g node-index)
                                            (match n
                                              ((MeasurementNode m) m)
                                              (_ (error "Expected only `MeasurementNodes`.")))
                                            (error "Expected only terminal `Nodes`.")))
                                      (iter:collect! (map:entries (map fst (get-graph-node-map g))))))
          (tuple-cf (search-measurements True
                                         clifford-costs
                                         then-swap??
                                         (get-circuit-next-fresh-index c)
                                         terminal-measurements))
          (assignments-mu-prime (uncurry (map-measurements terminal-measurements)
                                         tuple-cf)))
      (fold (add-measurement-with-corrections corrections)
            (map-circuit-assignments (assignments-compose assignments-mu-prime) c)
            (get-circuit-gates (fst tuple-cf))))))

(coalton-toplevel

  (declare graph->circuit (Boolean
                           -> (Gate -> (Tuple (List Gate) Frame))
                           -> (Gate -> IFix)
                           -> (UFix -> UFix -> Boolean)
                           -> Graph
                           -> Circuit))
  (define (graph->circuit preserve-state? corrections clifford-costs then-swap?? g)
    "Make a `Circuit` from a `Graph`."
    (match (search-non-clifford preserve-state? corrections clifford-costs then-swap?? (Tuple (default) g))
      ((Tuple c-prime g-prime)
       (let ((tuple-cg (pipe (Tuple c-prime g-prime)
                             (nest map-fst
                                   map-circuit-assignments
                                   assignments-compose
                                   get-graph-assignments
                                   g-prime))))
         (if preserve-state?
             (pipe tuple-cg
                   (map-snd get-graph-frame)
                   (synthesize-frame clifford-costs then-swap??)
                   fst
                   (map-circuit-assignments simplify-assignments))
             (pipe tuple-cg
                   (synthesize-measurements corrections clifford-costs then-swap??)
                   (map-circuit-assignments simplify-assignments))))))))
