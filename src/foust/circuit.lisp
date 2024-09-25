;;;; circuit.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/circuit
  (:documentation
   "This package represents `Circuit`s as `List`s of `Gate`s and an `Assignments`.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:use
   #:cl-quil.foust/pauli
   #:cl-quil.foust/assignments
   #:cl-quil.foust/gate)
  (:local-nicknames
   (#:list #:coalton-library/list))
  (:export
   #:Circuit
   #:get-circuit-gates
   #:set-circuit-gates
   #:map-circuit-gates
   #:get-circuit-assignments
   #:set-circuit-assignments
   #:map-circuit-assignments
   #:add-gate
   #:circuit-gate-count
   #:circuit-tqe-count
   #:get-circuit-next-fresh-index
   #:make-circuit))

(in-package #:cl-quil.foust/circuit)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type Circuit (Circuit (List Gate) Assignments))

  (define-instance (Default Circuit)
    (define (default) (Circuit Nil (default))))

  (declare get-circuit-gates (Circuit -> (List Gate)))
  (define (get-circuit-gates (Circuit gs _)) gs)

  (declare set-circuit-gates (Circuit -> (List Gate) -> Circuit))
  (define (set-circuit-gates (Circuit _ a) gs) (Circuit gs a))

  (declare map-circuit-gates (((List Gate) -> (List Gate)) -> Circuit -> Circuit))
  (define (map-circuit-gates f (Circuit gs a)) (Circuit (f gs) a))

  (declare get-circuit-assignments (Circuit -> Assignments))
  (define (get-circuit-assignments (Circuit _ a)) a)

  (declare set-circuit-assignments (Circuit -> Assignments -> Circuit))
  (define (set-circuit-assignments (Circuit gs _) a) (Circuit gs a))

  (declare map-circuit-assignments ((Assignments -> Assignments) -> Circuit  -> Circuit))
  (define (map-circuit-assignments f (Circuit gs a)) (Circuit gs (f a)))

  (declare add-gate (Circuit -> Gate -> Circuit))
  (define (add-gate (Circuit gs a) g)
    "Add a `Gate` to the end of a `Circuit`."
    (Circuit (list:append gs (singleton g)) a))

  (declare circuit-gate-count (Circuit -> UFix))
  (define (circuit-gate-count (Circuit gs _))
    "Get the number of `Gate`s in a `Circuit`."
    (length gs))

  (declare circuit-tqe-count (Circuit -> UFix))
  (define (circuit-tqe-count (Circuit gs _))
    "Get the number of TQE `Gate`s in a `Circuit`."
    (list:countby tqe? gs))

  (declare get-circuit-next-fresh-index (Circuit -> UFix))
  (define (get-circuit-next-fresh-index (Circuit _ a))
    "Get the next fresh index from the `Assignments` of the `Circuit`."
    (get-assignments-next-fresh-index a))

  (declare make-circuit ((List Gate) -> Circuit))
  (define (make-circuit gs)
    "Make a `Circuit` from a `List` of `Gate`s, setting the first fresh index of the `Assignments`."
    (Circuit gs
             (null-assignments
              (fold (fn (fresh-index g)
                      (match g
                        ((Meas _ _ _ v) (max (1+ v) fresh-index))
                        ((MeasMult _ v) (max (1+ v) fresh-index))
                        (_ fresh-index)))
                    0
                    gs))))

  (define-instance (Into Circuit String)
    (define (into (Circuit gs a))
      (mconcat
       (list:intersperse
        (into #\newline)
        (concat (make-list (make-list "┌───────────────────┐"
                                      "│      Circuit      │"
                                      "└───────────────────┘"
                                      "┌──────"
                                      (<> "│ Gates : " (into (length gs)))
                                      (<> "│ TQEs  : " (into (list:countby tqe? gs)))
                                      "└──────")
                           (map into gs)
                           (singleton (into a)))))))))
