;;;; assignments.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/assignments
  (:documentation
   "This package defines classical expressions and represents classical instructions as `List`s of assignments,

storing the first and the next fresh variables.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:map #:coalton-library/ord-map))
  (:export #:ClassicalExpression
           #:ClassicalBit
           #:ClassicalVariable
           #:get-classical-expression-variables
           #:get-classical-expression-bit
           #:classical-bit-flip
           #:classical-xor
           #:Assignments
           #:get-assignments-instructions
           #:get-assignments-first-fresh-index
           #:get-assignments-next-fresh-index
           #:null-assignments
           #:assignments-compose
           #:assignments-increment
           #:assign-fresh
           #:add-instruction
           #:simplify-assignments))

(in-package #:cl-quil.foust/assignments)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type ClassicalExpression
    "Represent a classical expression as a `List` of bit variables to be summed together with a constant bit.

A `ClassicalExpression` is designed to represent expressions which evaluate to one bit."
    (ClassicalExpression (List UFix) Boolean))

  (declare ClassicalBit (Boolean -> ClassicalExpression))
  (define (ClassicalBit b) (ClassicalExpression Nil b))

  (declare ClassicalVariable (UFix -> ClassicalExpression))
  (define (ClassicalVariable v) (ClassicalExpression (singleton v) False))

  (declare get-classical-expression-variables (ClassicalExpression -> (List UFix)))
  (define (get-classical-expression-variables (ClassicalExpression vs _ )) vs)

  (declare get-classical-expression-bit (ClassicalExpression -> Boolean))
  (define (get-classical-expression-bit (ClassicalExpression _ b)) b)

  (define-instance (Default ClassicalExpression)
    (define (default) (ClassicalExpression Nil False)))

  (define-instance (Eq ClassicalExpression)
    (define (== (ClassicalExpression vs-one b-one) (ClassicalExpression vs-two b-two))
      (and (== b-one b-two) (== (list:sort vs-one) (list:sort vs-two)))))

  (declare classical-bit-flip (ClassicalExpression -> ClassicalExpression))
  (define (classical-bit-flip (ClassicalExpression vs b))
    "Take a `ClassicalExpression` and return a new one with the bit flipped."
    (ClassicalExpression vs (not b)))

  (declare classical-xor (ClassicalExpression -> ClassicalExpression -> ClassicalExpression))
  (define (classical-xor (ClassicalExpression vs-one b-one) (ClassicalExpression vs-two b-two))
    "Take two `ClassicalExpression`s and return the result of combining them with XOR."
    (ClassicalExpression (list:difference (list:union vs-one vs-two)
                                          (list:intersection vs-one vs-two))
                         (boolean-xor b-one b-two)))

  (define-instance (Semigroup ClassicalExpression)
    (define <> classical-xor))

  (define-instance (Monoid ClassicalExpression)
    (define mempty (default)))

  (define-instance (Into ClassicalExpression String)
    (define (into (ClassicalExpression vs b))
      (if (list:null? vs)
          (if b "1" "0")
          ((if b (flip <> " + 1") id)
           (mconcat (Cons "c" (list:intersperse " + c" (map into (list:sort vs))))))))))

(coalton-toplevel

  (define-type Assignments
    "`Assignment`s store a list of classical instructions: var <- expression. The head of the `List`

is the last instruction. The two `UFix`s correspond to:

The first fresh index, i.e., the first index which is not user-defined.

All variable IDs below this value correspond to  user-defined variables

as provided in a circuit input to Foust.

The next fresh index, i.e., all values at or above this index are guaranteed

not to have been used in preceeding instructions."
    (Assignments (List (Tuple UFix ClassicalExpression)) UFix UFix))

  (declare get-assignments-instructions (Assignments -> (List (Tuple UFix ClassicalExpression))))
  (define (get-assignments-instructions (Assignments instructions _ _)) instructions)

  (declare get-assignments-first-fresh-index (Assignments -> UFix))
  (define (get-assignments-first-fresh-index (Assignments _ first-fresh-index _)) first-fresh-index)

  (declare get-assignments-next-fresh-index (Assignments -> UFix))
  (define (get-assignments-next-fresh-index (Assignments _ _ next-fresh-index)) next-fresh-index)

  (define-instance (Default Assignments)
    (define (default) (Assignments Nil 0 0)))

  (define-instance (Eq Assignments)
    (define (== (Assignments instructions-one first-fresh-index-one next-fresh-index-one)
                (Assignments instructions-two first-fresh-index-two next-fresh-index-two))
      (and (== instructions-one instructions-two)
           (== first-fresh-index-one first-fresh-index-two)
           (== next-fresh-index-one next-fresh-index-two))))

  (declare null-assignments (UFix -> Assignments))
  (define (null-assignments fresh-index)
    "Create an empty `Assignments` with the first and next fresh indices set."
    (Assignments Nil fresh-index fresh-index))


  (declare assignments-compose (Assignments -> Assignments -> Assignments))
  (define (Assignments-compose (Assignments instructions-two first-fresh-index-two next-fresh-index-two)
                               (Assignments instructions-one first-fresh-index-one next-fresh-index-one))
    "Concatenate two `Assignments`."
    (Assignments (list:append instructions-two instructions-one)
                 (max first-fresh-index-one first-fresh-index-two)
                 (max next-fresh-index-one next-fresh-index-two)))

  (define-instance (Semigroup Assignments)
    (define <> assignments-compose))

  (define-instance (Monoid Assignments)
    (define mempty (default)))

  (declare assignments-increment (Assignments -> Assignments))
  (define (assignments-increment (Assignments instructions first-fresh-index next-fresh-index))
    "Increment the next fresh index."
    (Assignments instructions first-fresh-index (1+ next-fresh-index)))

  (declare add-instruction (Assignments -> (Tuple UFix ClassicalExpression) -> Assignments))
  (define (add-instruction (Assignments instructions first-fresh-index next-fresh-index) instruction)
    "Add an instruction among existing variables to an `Assignments`."
    (Assignments (Cons instruction instructions) first-fresh-index next-fresh-index))

  (declare assign-fresh (Assignments -> UFix -> Assignments))
  (define (assign-fresh (Assignments instructions first-fresh-index next-fresh-index) v)
    "Assign a fresh variable to `v` and add the instruction to the `Assignments`."
    (Assignments (Cons (Tuple v (ClassicalVariable next-fresh-index)) instructions)
                 first-fresh-index (1+ next-fresh-index)))

  (define-instance (Into Assignments String)
    (define (into a)
      (mconcat
       (list:intersperse
        (into #\newline)
        (concat (make-list (make-list "┌───────────────────┐"
                                      "│    Assignments    │"
                                      "└───────────────────┘"
                                      "┌──────")
                           (map (fn ((Tuple v e))
                                  (mconcat (make-list "│ c" (into v) " <- " (into e))))
                                (get-assignments-instructions a))
                           (singleton "└──────"))))))))

(coalton-toplevel

  (declare fold-instructions ((List (Tuple UFix ClassicalExpression)) -> (map:Map UFix ClassicalExpression)))
  (define (fold-instructions instructions)
    "Take the `List` of instructions and perform all necessary simplifying substitutions."
    (fold (fn (instruction-map (Tuple v (ClassicalExpression vs b)))
            (map:insert-or-replace
             instruction-map
             v
             ;; while walking through the instructions, check if the
             ;; assigned expression contains any variables that have
             ;; already been assigned, and make the appropriate
             ;; substitutions.
             (msum (Cons (ClassicalBit b)
                         (map (fn (v-prime)
                                (with-default (ClassicalVariable v-prime)
                                  (map:lookup instruction-map v-prime)))
                              vs)))))
          map:Empty
          instructions))

  (declare simplify-assignments (Assignments -> Assignments))
  (define (simplify-assignments (Assignments instructions first-fresh-index next-fresh-index))
    "Simplify `Assignments` to a sequence of instructions assigning only variables below the first fresh index."
    (Assignments (iter:collect! (iter:filter! (compose (> first-fresh-index) fst)
                                              (map:entries (fold-instructions instructions))))
                 first-fresh-index
                 next-fresh-index)))
