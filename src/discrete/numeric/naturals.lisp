;;;; src/discrete/numeric/naturals.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil.discrete/numeric)

;;; Type-level programming using Peano natural numbers

;;; Usage (coalton (fromNat (the #.(nat-type 2) single)))

(coalton-toplevel
  (define-class (Singleton :a)
    "Type with a single value associated with it."
    (single :a))

  (define-class (Singleton :a => Nat :a)
    "Type level natural number singleton"
    (fromNat (:a -> UFix)))

  (define-instance (Singleton Unit)
    (define single Unit))

  (repr :enum)
  (define-type Z Z)
  (define-instance (Singleton Z)
    (define single Z))

  (define-instance (Nat Z)
    (define (fromNat _) 0))

  (repr :transparent)
  (define-type (S :a)
    (S :a))

  (define-instance (Singleton :a => (Singleton (S :a)))
    (define single
      (S single)))

  (define-instance (Nat :a => (Nat (S :a)))
    (define fromNat
      (progn
        (let nat = single)
        ;; Pre-calculate the return value.
        (let natural = (+ 1 (match nat ((S m) (fromNat m)))))
        ((the (:a -> :a -> UFix)
              (fn (_ _) natural)) nat))))

  (declare extract-single (Singleton :a => (:c :a) -> :a))
  (define (extract-single _)
    "Extracts the singleton from the right-most type."
    single)

  (declare with-singleton (Singleton :a => :a -> (:c :a) -> (:c :a)))
  (define (with-singleton _ p)
    "Carries over the singleton across constructors."
    p))

(cl:defun nat-type (n)
  (cl:if (cl:eql n 0)
         'Z
         `(S ,(nat-type (cl:- n 1)))))

(cl:defmacro has-singleton (type cl:&rest vars)
  (cl:let ((def (cl:intern (cl:concatenate
                            'cl:string "THE-" (cl:symbol-name type)))))
    `(cl:defmacro ,def (n cl:&optional m)
       (cl:if m
              `(the (,(cl:intern ,(cl:symbol-name type)) ,,@vars ,(nat-type n)) ,m)
              `(fn (x) (the (,(cl:intern ,(cl:symbol-name type))
                             ,,@vars
                             ,(nat-type n)) x))))))

;; (coalton (make-nat 2))
(cl:defmacro make-nat (n)
  `(the ,(nat-type n) ,(nat-type n)))
