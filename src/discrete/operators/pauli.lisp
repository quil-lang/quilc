;;;; src/discrete/operators/pauli.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil.discrete/operators)

;;; Wrapper around Quilc's Pauli groups

(has-singleton Pauli)

(coalton-toplevel
  (repr :native cl-quil.clifford:pauli)
  (define-type (Pauli :a))

  (define (pauli-phase-factor p)
    (RootUnity4
     (modulo
      (lisp Integer (p)
        (cl-quil.clifford::phase-factor p)))))

  (define-instance (Eq (Pauli :a))
    (define (== a b)
      (lisp Boolean (a b)
        (cl-quil.clifford:pauli= a b))))

  (declare integer->pauli (Nat :a => Integer -> (Pauli :a)))
  (define (integer->pauli a)
    (let nat = single)
    (let n = (fromNat nat))
    (with-singleton nat
      (lisp (Pauli :nat) (a n)
        (cl-quil.clifford::integer-pauli a n))))

  (define (make-pauli bs p)
    (let bs-base4 = (map rootunity4->integer bs))
    (let p-base4 = (rootunity4->integer p))
    (let nat = single)
    (if (== (list:length bs) (fromNat nat))
        (with-singleton nat
          (lisp (Pauli :a) (bs-base4 p-base4)
            (cl-quil.clifford:make-pauli bs-base4 p-base4)))
        (error "Incorrect length list to `make-pauli'.")))

  (define-instance (Composable (Pauli :n) (Pauli :n) (Pauli :n))
    (define (apply a b)
      (lisp (Pauli :a) (a b)
        (specify-generic-function #'cl-quil.clifford:group-mul
                                  (cl:list a b)
                                  'cl-quil.clifford:pauli))))

  (define (pauli-phase-identity p)
    (let base4 = (rootunity4->integer p))
    (let nat = single)
    (let n = (fromNat nat))
    (with-singleton nat
      (lisp (Pauli :a) (n base4)
        (cl-quil.clifford:pauli-identity n base4))))

  (define-instance (Nat :a => Identity (Pauli :a))
    (define identity
      (progn
        (let nat = single)
        (let n = (fromNat nat))
        (with-singleton nat
          (lisp (Pauli :a) (n)
            (cl-quil.clifford:pauli-identity n))))))

  (define-instance (Nat :a => Inverse (Pauli :a))
    (define (inverse p)
      (lisp (Pauli :a) (p)
        (specify-generic-function #'cl-quil.clifford:group-inv
                                  (cl:list p)
                                  'cl-quil.clifford:pauli)))
    (define (safe-inverse p)
      (Some
       (lisp (Pauli :a) (p)
         (specify-generic-function #'cl-quil.clifford:group-inv
                                   (cl:list p)
                                   'cl-quil.clifford:pauli)))))

  (define pauli-x
    (the-pauli 1 (make-pauli (make-list rootunity4-plus-i) rootunity4-plus-1)))
  (define pauli-z
    (the-pauli 1 (make-pauli (make-list rootunity4-minus-1) rootunity4-plus-1)))
  (define pauli-y
    (the-pauli 1 (make-pauli (make-list rootunity4-minus-i) rootunity4-plus-1))))
