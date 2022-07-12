;;;; src/discrete/operators/ma-normal-form.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil/discrete/operators)

;;; Implements a canonical form for Clifford+T elements for 1 qubit

(coalton-toplevel

  (define-type MAForm
    "Matsumato Amano Normal Form of Clifford+T (arXiv:0806.3834)"
    (MAForm LeftMAForm (Backwards List MidMAForm) CliffordGates1))

  (repr :enum)
  (define-type LeftMAForm
    Left-T
    Left-I)

  (define-instance (Into LeftMAForm (List Universal1))
    (define (into c)
      (match c
        ((Left-I) Nil)
        ((Left-T) (make-list TGate)))))

  (define-instance (Into LeftMAForm (List OutputGate1))
    (define (into c)
      (match c
        ((Left-I) Nil)
        ((Left-T) (make-list Discrete-T)))))

  (define-instance (Into LeftMAForm (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)))
    (define (into c)
      (match c
        ((Left-I) identity)
        ((Left-T) (into TGate)))))

  (repr :enum)
  (define-type MidMAForm
    Mid-HT
    Mid-SHT)

  (define-instance (Into MidMAForm (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)))
    (define (into c)
      (match c
        ((Mid-HT) (@ (into HGate) (into TGate)))
        ((Mid-SHT) (@ (into SGate) (@ (into HGate) (into TGate)))))))

  (define-instance (Into MidMAForm (List Universal1))
    (define (into c)
      (match c
        ((Mid-HT) (make-list HGate TGate))
        ((Mid-SHT) (make-list SGate HGate TGate)))))

  (define-instance (Into MidMAForm (List OutputGate1))
    (define (into c)
      (match c
        ((Mid-HT) (make-list Discrete-H Discrete-T))
        ((Mid-SHT) (make-list Discrete-S Discrete-H Discrete-T)))))

  (define (right-apply-t mid)
    (match mid
      ;; H T T  = H S
      ((Mid-HT) (@ cliffordgates1-h cliffordgates1-s))
      ;; S H T T  = S H S
      ((Mid-SHT) (@ cliffordgates1-sh cliffordgates1-s))))

  (define-instance (into MAForm (List Universal1))
    (define (into ct)
      (match ct
        ((MAForm left rlist clifford)
         (<> (<> (into left) (list:concat (into rlist))) (into clifford))))))

(define-instance (into MAForm (List OutputGate1))
    (define (into ct)
      (match ct
        ((MAForm left rlist clifford)
         (<> (<> (into left) (list:concat (into rlist))) (into clifford))))))

  (define-instance (Composable MAForm CliffordGates1 MAForm)
    (define (apply ct c1)
      (match ct
        ((MAForm left rlist c2) (MAForm left rlist (@ c2 c1))))))

  (define-instance (Composable MAForm Universal1 MAForm)
    ;; arXiv:1312.6584
    (define (apply ct gate)
      (match (the (Optional CliffordGates1) (into gate))
        ;; Clifford element
        ((Some c) (apply ct c))
        ;; TGate
        ((None)
         (match ct
           ((MAForm left rlist c)
            ;; CT = KTC'
            (match (cliffordgates1-commute-t c)
              ;; _ CT = _ HTC'
              ((Tuple (Commute-H) c-prime)
               (MAForm left (backwards-snoc rlist Mid-HT) c-prime))
              ;; _ CT = _ SHTC'
              ((Tuple (Commute-SH) c-prime)
               (MAForm left (backwards-snoc rlist Mid-SHT) c-prime))
              ;; _ CT = _ ITC'
              ((Tuple (Commute-I) c-prime)
               (match rlist
                 ;; (_ H T) T C' = _ (H S C')
                 ((Backwards (Cons mid rlist))
                  (MAForm left (Backwards rlist) (@ (right-apply-t mid) c-prime)))
                 (_ ; Nil
                  (match left
                    ;; T T C' = S C'
                    ((Left-T) (MAForm Left-I rlist (@ cliffordgates1-s c-prime)))
                    ;; I T C' = T C'
                    ((Left-I) (MAForm Left-T rlist c-prime)))))))))))))

  (define-instance (Into (List Universal1) MAForm)
    (define into
      (fold apply identity)))

  (declare hadamardts->cliffordtgates ((List HadamardT) -> (List Universal1)))
  (define (hadamardts->cliffordtgates ht)
    (list:concat (map into ht)))

  (define-instance (Into (List HadamardT) (List Universal1))
    (define into hadamardts->cliffordtgates))

  (define-instance (Into (List HadamardT) MAForm)
    (define (into hts)
      (into (hadamardts->cliffordtgates hts))))

  (define-instance (Into (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)) MAForm)
    (define (into u)
      (into (omega-unitary->ht-sequence u))))

  (define-instance (Into MAForm (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)))
    (define (into u)
      (match u
        ((MAForm left mids cs)
         (@ (@ (into left) (foldable->action mids)) (into cs))))))

  (define-instance (Iso MAForm (Unitary2 RootUnity8 (Cyclotomic8 Dyadic))))

  (define-instance (Composable MAForm MAForm MAForm)
    (define (apply a b)
      (into (the (List Universal1) (<> (into a) (into b))))))

  (define-instance (Identity MAForm)
    (define identity
      (MAForm Left-I (Backwards Nil) identity))))
