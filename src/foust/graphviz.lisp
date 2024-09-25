;;;; graphviz.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/graphviz
  (:use
   #:coalton
   #:coalton-prelude)
  (:use
   #:cl-quil.foust/graph)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:map #:coalton-library/ord-map))
  (:export #:graphviz-command
           #:graphviz))

(in-package #:cl-quil.foust/graphviz)

(named-readtables:in-readtable coalton:coalton)

;; This package supports visualization of Foust Graphs via the `dot`
;; command.

(coalton-toplevel

  ;; Consider adding to Standard Library.

  (declare mconcat! ((Monoid :a) => (Iterator :a) -> :a))
  (define (mconcat! iter)
    (iter:fold! <> mempty iter))

  (declare mconcatmap ((Foldable :a) (Monoid :b) => (:c -> :b) -> (:a :c) -> :b))
  (define (mconcatmap f xs)
    (fold (fn (this that) (<> this (f that))) mempty xs))

  (declare mconcatmap! ((Monoid :a) => (:b -> :a) -> (Iterator :b) -> :a))
  (define (mconcatmap! f iter)
    (mconcat! (map f iter))))

(coalton-toplevel

  (declare graphviz-prelude (Graph -> String))
  (define (graphviz-prelude (Graph _ _ f a))
    "Populate the portion of the graphviz command which generates the terminating Frame and Assignment table."
    (mconcat (make-list "F[label=\"" (into f) "\"] ; mu[label=\"" (into a) "\"]")))

  (declare graphviz-define-nodes (Graph -> String))
  (define (graphviz-define-nodes (Graph _ node-map _ _))
    "Populate the portion of the graphviz command which generates the nodes."
    (mconcatmap!
     (fn ((Tuple node-index (Tuple n _)))
       (mconcat (make-list "; " (into node-index) "[label=\"" (into n) "\"]")))
     (map:entries node-map)))

  (declare graphviz-draw-edges (Graph -> String))
  (define (graphviz-draw-edges (Graph _ node-map _ _))
    "Populate the portion of the graphviz command which connects the nodes."
    (mconcatmap!
     (fn ((Tuple node-index (Tuple _ (Tuple _ outs))))
       (mconcat (make-list " ; " (into node-index) " -> {" (mconcatmap (compose (<> " ") into) outs) " }")))
     (map:entries node-map)))

  (declare graphviz-command (Graph -> String -> String))
  (define (graphviz-command g pathname)
    "Generate a command to use graphviz to display a graph."
    (mconcat (make-list "echo '"
                        "digraph { fontname=\"Monospace:matrix=1 0 0 1\" ; rankdir=\"LR\" ; TBbalance=\"min\" ; "
                        (graphviz-prelude g)
                        (graphviz-define-nodes g)
                        (graphviz-draw-edges g)
                        " }"
                        "' | dot -Tsvg -Nshape=box -Gfontnames=svg -o "
                        pathname)))

  (declare graphviz (Graph -> String -> Unit))
  (define (graphviz g pathname)
    "Given a graph and a pathname, store an .SVG graphviz of the graph."
    (let ((command (graphviz-command g pathname)))
      (lisp Unit (command)
        (cl:progn (uiop:run-program command :force-shell cl:t) Unit)))))
