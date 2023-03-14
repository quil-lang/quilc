;;;; src/discrete/operators/clifford.lisp
;;;;
;;;; Author: A.J. Nyquist

(in-package #:cl-quil.discrete/operators)

(named-readtables:in-readtable coalton:coalton)

;;; Wrapper around Quilc's Clifford groups

(has-singleton Clifford)

(cl:defmacro clifford-element (cl:&body body)
  (cl:let ((c (cl:eval `(cl-quil.clifford:clifford-element ,@body))))
    `(the-clifford
      ,(cl-quil.clifford::num-qubits c)
      (lisp (Clifford :a) () (cl-quil.clifford:clifford-element ,@body)))))

(coalton-toplevel
  (repr :native cl-quil.clifford:clifford)
  (define-type (Clifford :nat))

  (define (count-clifford n)
    (lisp Integer (n)
      (cl-quil.clifford:count-clifford n)))

  (declare pauli->clifford (Nat :a => (Pauli :a) -> (Clifford :a)))
  (define (pauli->clifford p)
    (lisp (Clifford :a) (p)
      (cl-quil.clifford:clifford-from-pauli p)))

  (declare random-clifford (Nat :a => Unit -> (Clifford :a)))
  (define (random-clifford)
    (let nat = single)
    (let n = (fromNat nat))
    (with-singleton nat
      (lisp (Clifford :a) (n)
        (cl-quil.clifford::random-clifford n))))

  (define-instance (Eq (Clifford :n))
    (define (== a b)
      (lisp Boolean (a b)
        (cl-quil.clifford:clifford= a b))))

  (define-instance (Hash (Clifford :n))
    (define (hash c)
      (lisp Hash (c)
        (cl-quil.clifford::clifford-hash c))))

  (define-instance (Composable (Clifford :n) (Clifford :n) (Clifford :n))
    (define (apply a b)
      (lisp (Clifford :a) (a b)
        (specify-generic-function #'cl-quil.clifford:group-mul
                                  (cl:list a b)
                                  'cl-quil.clifford:clifford))))

  (define-instance (Composable (Clifford :n) (Pauli :n) (Pauli :n))
    (define (apply c p)
      (lisp (Pauli :a) (c p)
        'cl-quil.clifford:apply-clifford c p)))

  (define-instance (Nat :a => Identity (Clifford :a))
    (define identity
      (progn
        (let nat = single)
        (let n = (fromNat nat))
        (with-singleton nat
          (lisp (Clifford :a) (n)
            (cl-quil.clifford:clifford-identity n))))))

  (define-instance (Nat :a => Inverse (Clifford :a))
    (define (inverse p)
      (lisp (Clifford :a) (p)
        (specify-generic-function #'cl-quil.clifford:group-inv
                                  (cl:list p)
                                  'cl-quil.clifford:clifford)))
    (define (safe-inverse p)
      (Some
       (lisp (Clifford :a) (p)
         (specify-generic-function #'cl-quil.clifford:group-inv
                                   (cl:list p)
                                   'cl-quil.clifford:clifford)))))

  (define (clifford-phase-identity p)
    (pauli->clifford (pauli-phase-identity p))))
