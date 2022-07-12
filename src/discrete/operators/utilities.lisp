;;;; src/discrete/operators/utilities.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil/discrete/operators)

(coalton-toplevel
  ;; Helps conceptualize matrix multiplication in correct order
  (repr :transparent)
  (define-type (Backwards :f :a)
    "Implements Foldable in reverse order"
    (Backwards (:f :a)))

  (define (backwards-snoc rlist elem)
    "Append an element to the end of a (Backwards List)."
    (match rlist
      ((Backwards list) (Backwards (Cons elem list)))))

  (define (forwards rs)
    "Returns the the reverse of a Backwards (unwraps the functor)."
    (match rs
      ((Backwards xs) xs)))

  (define-instance ((Functor :f) => (Functor (Backwards :f)))
    (define (map f rs)
      ;; Does not preserve evaluation order
      (Backwards (map f (forwards rs)))))

  (define-instance ((Foldable :f) => (Foldable (Backwards :f)))
    (define (foldr f z rs)
      (match rs
        ((Backwards t) (fold (flip f) z t))))
    (define (fold f z rs)
      (match rs
        ((Backwards t) (foldr (flip f) z t)))))

  (define-instance ((Into :a :b) => (into (List :a) (Backwards List :b)))
    (define (into list)
      (let ((inner
              (fn (as bs)
                (match as
                  ((Nil) bs)
                  ((Cons a as)
                   (inner as (backwards-snoc bs (into a))))))))
        (inner list (Backwards Nil)))))

  (define-instance ((Into :a :b) => (into (Backwards List :a) (List :b)))
    (define (into rlist)
      (forwards (into (forwards rlist))))))

(cl:defun specify-generic-function (f args class)
  "Specifiy a CLOS method F dispatched on CLASS with ARGS (of the same coalton type)."
  (cl:funcall
   (closer-mop:method-function
    (cl:find-method
     f cl:nil
     (cl:map 'cl:list
             #'(cl:lambda (x) x (cl:find-class class))
             args)))
   args '()))
