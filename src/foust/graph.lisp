;;;; graph.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/graph
  (:documentation
   "This package represents Foust `Graph`s as `Map`s, storing incoming and outgoing `Node` indices,

with special treatment for the terminal `Frame` and `Assignments`.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:use
   #:cl-quil.foust/frame
   #:cl-quil.foust/assignments
   #:cl-quil.foust/node)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:map #:coalton-library/ord-map)
   (#:tree #:coalton-library/ord-tree))
  (:export
   #:Graph
   #:get-graph-fresh-node-index
   #:get-graph-node-map
   #:get-graph-frame
   #:get-graph-assignments
   #:map-graph-frame
   #:map-graph-assignments
   #:map-graph-nodes
   #:map-graph-edges
   #:remove-vertex
   #:add-vertex
   #:add-edge
   #:is-entry-at?
   #:is-terminal-at?
   #:get-entry-tuples
   #:get-terminal-tuples
   #:graph-any?
   #:graph-empty?
   #:graph-empty-except-terminal-measurements?
   #:push-frame))

(in-package #:cl-quil.foust/graph)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type Graph
    "A D.A.G., storing the next available `Node` index, a `Map` from `Node` indices to

The `Node`s themselves, paired with `Tree`s comprising the indices of the incoming

and outgoing `Node`s, respectively."
    (Graph UFix (map:Map UFix (Tuple Node (Tuple (tree:Tree UFix) (tree:Tree UFix)))) Frame Assignments))

  (define-instance (Default Graph)
    (define (default) (Graph 0 map:Empty (default) (default))))

  (declare get-graph-fresh-node-index (Graph -> UFix))
  (define (get-graph-fresh-node-index (Graph fresh-node-index _ _ _)) fresh-node-index)

  (declare get-graph-node-map (Graph -> (map:Map UFix (Tuple Node (Tuple (tree:Tree UFix) (tree:Tree UFix))))))
  (define (get-graph-node-map (Graph _ node-map _ _)) node-map)

  (declare get-graph-frame (Graph -> Frame))
  (define (get-graph-frame (Graph _ _ f _)) f)

  (declare map-graph-frame ((Frame -> Frame) -> Graph -> Graph))
  (define (map-graph-frame func (Graph fresh-node-index node-map f a))
    (Graph fresh-node-index node-map (func f) a))

  (declare get-graph-assignments (Graph -> Assignments))
  (define (get-graph-assignments (Graph _ _ _ a)) a)

  (declare map-graph-assignments ((Assignments -> assignments) -> Graph -> Graph))
  (define (map-graph-assignments func (Graph fresh-node-index node-map f a))
    (Graph fresh-node-index node-map f (func a))))

(coalton-toplevel

  (declare map-graph-nodes ((Node -> Node) -> Graph -> Graph))
  (define (map-graph-nodes func (Graph fresh-node-index node-map f a))
    "Map the `Node`s of a `Graph`."
    (Graph fresh-node-index (map (map-fst func) node-map) f a))

  (declare map-graph-edges (((tree:Tree UFix) -> (tree:Tree UFix)) -> Graph -> Graph))
  (define (map-graph-edges func (Graph fresh-node-index node-map f a))
    "Map the edges of a `Graph`."
    (Graph fresh-node-index (map (map-snd (bimap func func)) node-map) f a)))

(coalton-toplevel

  (declare remove-vertex (Graph -> UFix -> Graph))
  (define (remove-vertex (Graph fresh-node-index node-map f a) node-index)
    "Remove a `Node` from a `Graph`."
    (map-graph-edges (fn (edge) (with-default edge (tree:remove edge node-index)))
                     (Graph fresh-node-index (unwrap (map:remove node-map node-index)) f a)))

  (declare add-vertex (Graph -> Node -> Graph))
  (define (add-vertex (Graph fresh-node-index node-map f a) n)
    "Add a `n` to the `Graph`."
    (Graph (1+ fresh-node-index)
           (map:insert-or-replace node-map fresh-node-index (Tuple n (Tuple tree:Empty tree:Empty)))
           f a))

  (declare add-edge (Graph -> UFix -> UFix -> Graph))
  (define (add-edge (Graph fresh-node-index node-map f a) from-index to-index)
    "Add an edge from one `Node` to another."
    (Graph fresh-node-index
           (unwrap (map:update (map-snd (map-fst (flip tree:insert-or-replace from-index)))
                               (unwrap (map:update (map-snd (map-snd (flip tree:insert-or-replace to-index)))
                                                   node-map
                                                   from-index))
                               to-index))
           f a)))

(coalton-toplevel

  (declare is-entry-at? (Graph -> UFix -> Boolean))
  (define (is-entry-at? (Graph _ node-map _ _) node-index)
    "Is the `Node` indexed by `node-index` pointed to by no others?"
    (== tree:Empty (fst (snd (unwrap (map:lookup node-map node-index))))))

  (declare is-terminal-at? (Graph -> UFix -> Boolean))
  (define (is-terminal-at? (Graph _ node-map _ _) node-index)
    "Does the `Node` indexed by `node-index` point to no others?"
    (== tree:Empty (snd (snd (unwrap (map:lookup node-map node-index))))))

  (declare get-entry-tuples (Graph -> (iter:Iterator (Tuple UFix Node))))
  (define (get-entry-tuples (Graph _ node-map _ _))
    "Return entry `Node`s as an iterator of node index <-> `Node` pairs."
    (map (map-snd fst) (iter:filter! (fn ((Tuple _ (Tuple _ (Tuple ins _))))
                                       (== tree:Empty ins))
                                     (map:entries node-map))))

  (declare get-terminal-tuples (Graph -> (iter:Iterator (Tuple UFix Node))))
  (define (get-terminal-tuples (Graph _ node-map _ _))
    "Return terminal nodes as an iterator of (Node ID, Node)."
    (map (map-snd fst) (iter:filter! (fn ((Tuple _ (Tuple _ (Tuple _ outs))))
                                       (== tree:Empty outs))
                                     (map:entries node-map)))))

(coalton-toplevel

  (declare graph-empty? (Graph -> Boolean))
  (define (graph-empty? (Graph _ node-map _ _))
    "Is the `Graph` empty, except terminal `Frame` and `Assignments`?"
    (none? (iter:next! (map:entries node-map))))

  (declare graph-empty-except-terminal-measurements? (Graph -> Boolean))
  (define (graph-empty-except-terminal-measurements? (Graph _ node-map _ _))
    "Is the `Graph` empty, except terminal `MeasurementNode`s, `Frame`, and `Assignments`?"
    (iter:and! (map (fn ((Tuple n (Tuple _ outs)))
                      (and (measurement-node? n) (== tree:Empty outs)))
                    (map:values node-map))))

  (declare graph-any? ((Node -> Boolean) -> Graph -> Boolean))
  (define (graph-any? node-satisfies? (Graph _ node-map _ _))
    "Do any `Node`s in the `Graph` satisfy `node-satisfies?`"
    (iter:any! node-satisfies? (map fst (map:values node-map)))))

(coalton-toplevel

  (declare push-frame (Graph -> Frame -> Graph))
  (define (push-frame g f)
    "Push `f` through `g` and merge it into `g`'s terminating `Frame`."
    (pipe g (map-graph-nodes (frame-node-> f)) (map-graph-frame (flip <> f)))))
