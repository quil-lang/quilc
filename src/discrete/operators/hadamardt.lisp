;;;; src/discrete/operators/hadamardt.lisp
;;;;
;;;; Author: A.J. Nyquist

(in-package #:cl-quil.discrete/operators)

(named-readtables:in-readtable coalton:coalton)

;;; Defines an exact synthsis algorithm and its output type

(coalton-toplevel
  (define-type HadamardT
    (HT-H)
    (HT-T^^ Integer))

  (define-instance (Into HadamardT (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)))
    (define (into x)
      (match x
        ((HT-T^^ n) (%Unitary2 (RootUnity8 (fromInt n)) 1 0))
        ((HT-H) (%Unitary2 (RootUnity8 (fromInt 4))
                           (Cyclotomic8 (Dyadic -1 1) 0 (Dyadic 1 1) 0)
                           (Cyclotomic8 (Dyadic -1 1) 0 (Dyadic 1 1) 0))))))

  (define-instance (Into HadamardT (Unitary2 RootUnity8 (Complex (Root2plex Dyadic))))
    (define (into x)
      (match x
        ((HT-T^^ n) (%Unitary2 (RootUnity8 (fromInt n)) 1 0))
        ((HT-H) (%Unitary2 (RootUnity8 (fromInt 4))
                           (Complex 0 (Root2plex 0 (Dyadic 1 1)))
                           (Complex 0 (Root2plex 0 (Dyadic 1 1))))))))

  (define-instance (Into (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)) (List HadamardT))
    (define into omega-unitary->ht-sequence))

  (declare omega-unitary->ht-sequence
           ((Unitary2 RootUnity8 (Cyclotomic8 Dyadic)) -> (List HadamardT)))
  (define (omega-unitary->ht-sequence x)
    "Takes an unitary matrix of ℤ[i,√2] and converts it to a list of H/Tⁿ gates.
(See arXiv:1206.5236)"
    ;; s = sde(|z_00^2)
    (let s-first =
      (smallest-dividing-exponent
       (map into (cyclotomic8-square-modulus (unitary2-topleft x)))
       (Root2plex 0 1)))
    (let find-z-prime =
      (fn (k u-current)
        (the (Unitary2 RootUnity8 (Cyclotomic8 Dyadic))
             (@ (@ (into HT-H) (into (HT-T^^ (negate k))))
                u-current))))
    (let ((gen-rec
            (fn (sequence u-current s k)
              (let z-prime = (find-z-prime k u-current))
              (let z-sde = (smallest-dividing-exponent
                            (map into
                                 (cyclotomic8-square-modulus (unitary2-topleft z-prime)))
                            (Root2plex 0 1)))
              (cond
                ((<= s 3)
                 (list:append
                  sequence
                  (match (hashtable:get %exact-synth-table (hash u-current))
                    ((Some l) l)
                    ((None) (error "No remainder in `%exact-synth-table'.")))))
                ((>= k 4)
                 (error "State remained unfound in `omega-unitary->ht-sequence'."))
                ((== z-sde (- s 1))
                 (progn
                   (gen-rec (list:append sequence (make-list (HT-T^^ k) HT-H))
                            z-prime
                            z-sde
                            0)))
                (True (gen-rec sequence u-current s (+ k 1)))))))
      (gen-rec Nil x s-first 0)))

  (define %exact-synth-table
    "Conversion table of unitaries z s.t. SDE(|z_00|^2, √2) to HT-H+T lists."
    (let
        ((table (hashtable:with-capacity 1664))
         (generate-ht-list
           (fn (ks)
             "For a given list of I = [a,b,c...] give a [T^a H T^b H T^c ...]"
             (match ks
               ((Cons k ks)
                (Cons (HT-T^^ k)
                      (if (== ks Nil)
                          Nil
                          (Cons HT-H (generate-ht-list ks)))))
               ((Nil) Nil))))
         (unitary-remainder?
           (fn (candidate)
             "Checks if a candidate satisfies constraints and is not in the table."
             (the (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)) candidate)
             (>= 3
                 (smallest-dividing-exponent
                  (map into (cyclotomic8-square-modulus (unitary2-topleft candidate)))
                  (Root2plex 0 1)))))
         (table<-candidate!
           (fn (ht-list candidate)
             "Adds the candidate to table (if it satisfies `unitary-remainder?')"
             (if (unitary-remainder? candidate)
                 (progn
                   (let candidate-hash = (hash candidate))
                   (if (none? (hashtable:get table candidate-hash))
                       (progn
                         (hashtable:set! table candidate-hash ht-list)
                         True)
                       False))
                 False)))
         (gen-table
           (fn (ks)
             "Generates an entry for the table."
             (let ht-list = (generate-ht-list ks))
             (let candidate =
               (the (Unitary2 RootUnity8 (Cyclotomic8 Dyadic))
                    (fold @ identity (map into ht-list))))
             ;; T^nHT^m ...
             (table<-candidate! ht-list candidate)))
         (increments
           (fn (as bs)
             "Returns lists where each element is incremented by 1 until it equals 7."
             (the (List Integer) as)
             (the (List Integer) bs)
             (match (the (List Integer) bs)
               ((Cons x xs)
                (if (< x 7)
                    (list:append (map (fn (y) (list:append as (Cons (+ y x) xs)))
                                      (list:range 1 (- 7 x)))
                                 (increments (list:append as (make-list x)) xs))
                    (increments (list:append as (make-list x)) xs)))
               (_ Nil))))
         (gen-table-rec
           (fn (depth kss)
             "Generates the entire table with elements of length l.e. DEPTH."
             (the (List (List Integer)) kss)
             (when (< depth 1) (return Nil))
             (gen-table-rec
              (- depth 1)
              (list:remove-duplicates
               (list:concatMap
                (increments Nil)
                (map (Cons 0)
                     (list:filter gen-table kss))))))))
      (progn
        (gen-table-rec 5 (map (fn (x) (Cons x Nil)) (make-list 0 1 2 3 4 5 6 7)))
        (the (Hashtable Hash (List HadamardT)) table)))))
