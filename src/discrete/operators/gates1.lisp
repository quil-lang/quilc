;;;; src/discrete/operators/gates1.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil.discrete/operators)

(coalton-toplevel

  ;; Used for expanding MAForms
  (define-type Universal1
    "One qubit gates that make up Clifford+T
where X, Y, Z, H, S, T, E = HS³ω³, and W = ωI = exp(iπ/4)I"
    XGate YGate ZGate SGate HGate TGate EGate WGate)

  (define-instance (Eq Universal1)
    (define (== a b)
      (lisp Boolean (a b)
        (cl:eq a b))))

  (define-instance (Into HadamardT (List Universal1))
    (define (into ht)
      (match ht
        ((HT-H) (make-list HGate))
        ((HT-T^^ n)
         (match (mod n 8)
           (0 Nil)
           (1 (make-list TGate))
           (2 (make-list SGate))
           (3 (make-list TGate SGate))
           (4 (make-list ZGate))
           (5 (make-list TGate ZGate))
           (6 (make-list SGate ZGate))
           (7 (make-list TGate SGate ZGate)))))))

  (repr :enum)
  (define-type OutputGate1
    "Phase invariant universal gateset for 1 qubit."
    Discrete-S Discrete-H Discrete-T)

  (declare universal1->outputgate1 ((List Universal1) -> (List OutputGate1)))
  (define (universal1->outputgate1 gates)
    "Converts a list of Uniersal1 GATES to a list of Output1Gate. To get
simplification see (List Universal1) to MAForm to (List OutputGate1)."
    (match gates
      ((Cons gate gates)
       (list:append
        (match gate
          ((TGate) (make-list Discrete-T))
          ((HGate) (make-list Discrete-H))
          ((SGate) (make-list Discrete-S))
          ((ZGate) (make-list Discrete-S Discrete-S))
          ((XGate) (make-list Discrete-H Discrete-S Discrete-S Discrete-H))
          ((YGate) (<> (<> (make-list Discrete-H Discrete-S)
                           (universal1->outputgate1 (make-list ZGate XGate)))
                       (make-list Discrete-S Discrete-H)))
          ((EGate) (Cons Discrete-H (list:repeat 3 Discrete-S)))
          ((WGate) nil))
        (universal1->outputgate1 gates)))
      ((Nil) Nil)))

  (define-instance (Into OutputGate1 Universal1)
    (define (into g)
      (match g
        ((Discrete-S) SGate)
        ((Discrete-H) HGate)
        ((Discrete-T) TGate))))

  (define-type CliffordGates1
    "Clifford group for 1 qubit with the phase factor ω = exp(iπ/4)."
    (CliffordGates1
     (Modulo #.(nat-type 3))
     (Modulo #.(nat-type 2))
     (Modulo #.(nat-type 4))
     (Modulo #.(nat-type 8))))

  (declare cliffordgates1-get-encoding
           (CliffordGates1 -> (Tuple4 Integer Integer Integer Integer)))
  (define (cliffordgates1-get-encoding c)
    "For a CliffordGates1 element C = E^a X^b S^c ω^d return (Tuple4 a b c d)."
    (match c
      ((CliffordGates1 a b c d) (Tuple4 (modulo->integer a)
                                        (modulo->integer b)
                                        (modulo->integer c)
                                        (modulo->integer d)))))

  (define-instance (Eq CliffordGates1)
    (define (== a b)
      (== (cliffordgates1-get-encoding a) (cliffordgates1-get-encoding b))))

  (define (cliffordgates1-from-integers a b c d)
    (CliffordGates1 (fromInt a) (fromInt b) (fromInt c) (fromInt D)))

  (define cliffordgates1-X (CliffordGates1 0 1 0 0))
  (define cliffordgates1-Y (CliffordGates1 0 1 2 2))
  (define cliffordgates1-Z (CliffordGates1 0 0 2 0))
  (define cliffordgates1-H (CliffordGates1 1 0 1 5))
  (define cliffordgates1-S (CliffordGates1 0 0 1 0))
  (define cliffordgates1-E (CliffordGates1 1 0 0 0))
  (define cliffordgates1-W (CliffordGates1 0 0 0 1))
  (define cliffordgates1-SH (@ cliffordgates1-S cliffordgates1-H))

  (define-type CliffordTCommutator
    Commute-I Commute-H Commute-SH)

  (define-instance (Into CliffordTCommutator CliffordGates1)
    (define (into c)
      (match c
        ((Commute-I) identity)
        ((Commute-H) cliffordgates1-H)
        ((Commute-SH) cliffordgates1-SH))))

  (define-instance (Into CliffordTCommutator (List Universal1))
    (define (into c)
      (match c
        ((Commute-I) Nil)
        ((Commute-H) (make-list HGate))
        ((Commute-SH) (make-list SGate HGate)))))

  (declare cliffordgates1-commute-t
           (CliffordGates1 -> (Tuple CliffordTCommutator CliffordGates1)))
  (define (cliffordgates1-commute-t c)
    "For a given C return a (Tuple K C') such that CT = KTC."
    (match c
      ((CliffordGates1 a1 b1 c1 d1)
       (progn
         (let make-result =
           (fn (e c2 d2)
             (Tuple e (CliffordGates1 0 b1 (+ c1 c2) (+ d1 d2)))))
         (match (Tuple (modulo->integer a1) (modulo->integer b1))
           ((Tuple 0 0) (make-result Commute-I 0 0))
           ((Tuple 0 1) (make-result Commute-I 1 7))
           ((Tuple 1 0) (make-result Commute-H 3 3))
           ((Tuple 1 1) (make-result Commute-H 2 0))
           ((Tuple 2 0) (make-result Commute-SH 0 5))
           ((Tuple 2 1) (make-result Commute-SH 1 4)))))))

  (define-instance (Into CliffordGates1 (List Universal1))
    (define (into c)
      (match (cliffordgates1-get-encoding c)
        ((Tuple4 a b c d)
         (<> (<> (list:repeat a EGate)
                 (list:repeat b XGate))
             (<> (list:repeat c SGate)
                 (list:repeat d WGate)))))))

  (define-instance (Into CliffordGates1 (List OutputGate1))
    (define (into c)
      (match (cliffordgates1-get-encoding c)
        ((Tuple4 a b c _)
         (<> (list:concat
              (list:repeat a
                ;; E = HSSS (ɷ^3)
                (make-list Discrete-H Discrete-S Discrete-S Discrete-S)))
             (<> (list:concat
                  (list:repeat b
                    ;; X = HSSH
                    (make-list Discrete-H Discrete-S Discrete-S Discrete-H)))
                 (list:repeat c Discrete-S)))))))

  (define-instance (Into Universal1 (Optional CliffordGates1))
    (define (into ct)
      (match ct
        ((XGate) (Some cliffordgates1-X))
        ((YGate) (Some cliffordgates1-Y))
        ((ZGate) (Some cliffordgates1-Z))
        ((SGate) (Some cliffordgates1-S))
        ((HGate) (Some cliffordgates1-H))
        ((EGate) (Some cliffordgates1-E))
        ((WGate) (Some cliffordgates1-W))
        (_ None))))

  (define-instance (Into Universal1
                         (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)))
    (define (into ct)
      (match ct
        ((XGate) (%Unitary2 (RootUnity8 4) 0 1))
        ((YGate) (%Unitary2 mempty 0 (Cyclotomic8 0 1 0 0)))
        ((ZGate) (into (HT-T^^ 4)))
        ((SGate) (into (HT-T^^ 2)))
        ((HGate) (into HT-H))
        ;; E = H S^3 W^3
        ((EGate) (@ (into HGate) (@ (into (HT-T^^ 6)) (@^ (into WGate) 3))))
        ((WGate) (%Unitary2 (RootUnity8 2) (Cyclotomic8 0 0 1 0) 0))
        ((TGate) (into (HT-T^^ 1))))))

  (define-instance (Into CliffordGates1 (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)))
    (define (into c)
      (match (cliffordgates1-get-encoding c)
        ((Tuple4 a b c d)
         (@ (@ (@^ (into EGate) a )
               (@^ (into XGate) b))
            (@ (@^ (into SGate) c)
               (@^ (into WGate) d)))))))

  (define-instance (Into OutputGate1 (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)))
    (define (into c)
      (into (the Universal1 (into c)))))

  (define-instance (Into CliffordGates1 (Clifford #.(nat-type 1)))
    (define (into c)
      (match (cliffordgates1-get-encoding c)
        ((Tuple4 a b c d)
         ;;       E^a
         (@ (@ (@^ (clifford-element X -> Y Z -> X) a)
               ;; X^b
               (@^ (pauli->clifford pauli-X) b))
            ;;    S^c
            (@ (@^ (clifford-element Z -> Z X -> Y) c)
               ;; W^d = I
               (@^ (clifford-element X -> X Z -> Z) d)))))))

  (define-instance (Into (Clifford #.(nat-type 1)) CliffordGates1)
    (define (into c)
      (match (hashtable:get %clifford1-table (hash c))
        ((Some a) a)
        ((None) (error "Clifford element not found in `%clifford1-table'.")))))

  (define-instance (Composable CliffordGates1 CliffordGates1 CliffordGates1)
    (define (apply m1 m2)
      (match (Tuple m1 m2)
        ((Tuple
          ;; M1 = E^a1 X^b1 S^c1 W^d1
          (CliffordGates1 a1 b1 c1 d1)
          ;; M2 = E^a2 X^b2 S^c2 W^d2
          (CliffordGates1 a2 b2 c2 d2))
         ;; M1 M2 = (E^a1 X^b1 S^c1) W^d1 E^a2 (X^b2 S^c2 W^d2)
         ;; M1 M2 = E^a1 (X^b1 S^c1 E^a2) X^b2 S^c2 W^d2 W^d1
         (match (cliffordgates1-xse->exsw b1 c1 a2)
           ;; M3 = E^a3 X^b3 S^c3 W^d3
           ;; M1 M2 = (E^a1) M3 (X^b2 S^c2) W^d1 W^d2
           ((CliffordGates1 a3 b3 c3 d3)
            ;; M1 M2 = (E^a1) E^a3 X^b3 (S^c3 X^b2) S^c2 W^d3 W^d1 W^d2
            (match (cliffordgates1-sx->sw c3 b2)
              ;; M1 M2 = (E^a1) E^a3 X^b3 (X^b2 S^c4 W^d4) S^c2 W^d3 W^d1 W^d2
              ((Tuple c4 d4)
               (CliffordGates1
                ;; M1 M2 = (E^a1 E^a3) (X^b3 X^b2) (S^c4 S^c2) (W^d4 W^d3 W^d1 W^d2)
                (+ a1 a3) (+ b2 b3) (+ c2 c4) (+ (+ d1 d2) (+ d3 d4)))))))))))

  (define-instance (Identity CliffordGates1)
    (define identity (CliffordGates1 0 0 0 0)))

  (define-instance (Inverse CliffordGates1)
    (define (inverse m)
      (match m
        ((CliffordGates1 a b c d)
         (match (Tuple3 (modulo->integer a)
                        (modulo->integer b)
                        (modulo->integer c))
           ;;Tuple3 0 0 0) (CliffordGates1 0 0 0 (- 0 d)))
           ((Tuple3 0 0 1) (CliffordGates1 0 0 3 (- 0 d)))
           ;;Tuple3 0 0 2) (CliffordGates1 0 0 2 (- 0 d)))
           ((Tuple3 0 0 3) (CliffordGates1 0 0 1 (- 0 d)))
           ;;Tuple3 0 1 0) (CliffordGates1 0 1 0 (- 0 d)))
           ((Tuple3 0 1 1) (CliffordGates1 0 1 1 (- 6 d)))
           ((Tuple3 0 1 2) (CliffordGates1 0 1 2 (- 4 d)))
           ((Tuple3 0 1 3) (CliffordGates1 0 1 3 (- 2 d)))
           ((Tuple3 1 0 0) (CliffordGates1 2 0 0 (- 0 d)))
           ((Tuple3 1 0 1) (CliffordGates1 1 0 1 (- 2 d)))
           ((Tuple3 1 0 2) (CliffordGates1 2 1 0 (- 0 d)))
           ((Tuple3 1 0 3) (CliffordGates1 1 1 3 (- 4 d)))
           ((Tuple3 1 1 0) (CliffordGates1 2 1 2 (- 2 d)))
           ((Tuple3 1 1 1) (CliffordGates1 1 1 1 (- 6 d)))
           ((Tuple3 1 1 2) (CliffordGates1 2 0 2 (- 2 d)))
           ((Tuple3 1 1 3) (CliffordGates1 1 0 3 (- 4 d)))
           ((Tuple3 2 0 0) (CliffordGates1 1 0 0 (- 0 d)))
           ((Tuple3 2 0 1) (CliffordGates1 2 1 3 (- 6 d)))
           ((Tuple3 2 0 2) (CliffordGates1 1 1 2 (- 2 d)))
           ((Tuple3 2 0 3) (CliffordGates1 2 0 3 (- 6 d)))
           ((Tuple3 2 1 0) (CliffordGates1 1 0 2 (- 0 d)))
           ((Tuple3 2 1 1) (CliffordGates1 2 1 1 (- 6 d)))
           ((Tuple3 2 1 2) (CliffordGates1 1 1 0 (- 2 d)))
           ((Tuple3 2 1 3) (CliffordGates1 2 0 1 (- 6 d)))
           ((Tuple3 a b c)
            (CliffordGates1 (fromInt a) (fromInt b) (fromInt c) (- 0 d)))))))
    (define (safe-inverse m)
      (Some (inverse m))))

  (define %clifford1-table
    (progn
      (let table = (hashtable:with-capacity 24))
      (let increment-cliffordgates1 =
        (fn (c)
          "Returns the next non-identity element in the cliffordgates1."
          (match c
            ((CliffordGates1 a b c d)
             (cond
               ((and (and (== 0 (+ a 1)) (== 0 (+ b 1)))
                     (and (== 0 (+ c 1)) (== 0 (+ d 1))))
                None)
               ((and (== 0 (+ b 1)) (and (== 0 (+ c 1)) (== 0 (+ d 1))))
                (Some (CliffordGates1 (+ a 1) (+ b 1) (+ c 1) (+ d 1))))
               ((and (== 0 (+ c 1)) (== 0 (+ d 1)))
                (Some (CliffordGates1 a (+ b 1) (+ c 1) (+ d 1))))
               ((== 0 (+ d 1))
                (Some (CliffordGates1 a b (+ c 1) (+ d 1))))
               ;; We could actually ignore the d case.
               (True (Some (CliffordGates1 a b c (+ d 1)))))))))
      (let ((increment-all
              (fn (c)
                (hashtable:set! table
                                (hash (the-clifford 1 (into c))) c)
                (match (increment-cliffordgates1 c)
                  ((Some c-inc) (increment-all c-inc))
                  ((None) table)))))
        (increment-all (the CliffordGates1 identity)))))

  (define (cliffordgates1-sx->sw c b)
    "For a S^C X^B return an equivalent S^C' W^D'"
    (the-modulo 2 b)
    (the-modulo 4 c)
    (match (Tuple (modulo->integer b) (modulo->integer c))
      ;;Tuple 0 0) (Tuple 0 0))
      ((Tuple 1 0) (Tuple 0 0))
      ;;Tuple 0 1) (Tuple 1 0))
      ;;Tuple 0 2) (Tuple 2 0))
      ;;Tuple 0 3) (Tuple 3 0))
      ((Tuple 1 1) (Tuple 3 2))
      ((Tuple 1 2) (Tuple 2 4))
      ((Tuple 1 3) (Tuple 1 6))
      ((Tuple y x)
       (Tuple (the-modulo 4 (fromInt x)) (the-modulo 8 (fromInt y))))))

  (define (cliffordgates1-xse->exsw b c a)
    "For a X^B S^C E^A return an equivalent E^A' X^B' S^C' W^D'"
    (the-modulo 3 a)
    (the-modulo 2 b)
    (the-modulo 4 c)
    (match (Tuple3 (modulo->integer a)
                   (modulo->integer b)
                   (modulo->integer c))
      ;;Tuple3 0 0 0) (CliffordGates1 0 0 0 0))
      ;;Tuple3 1 0 0) (CliffordGates1 1 0 0 0))
      ;;Tuple3 2 0 0) (CliffordGates1 2 0 0 0))
      ;;Tuple3 0 0 1) (CliffordGates1 0 0 1 0))
      ((Tuple3 1 0 1) (CliffordGates1 2 0 3 6))
      ((Tuple3 2 0 1) (CliffordGates1 1 1 3 4))
      ;;Tuple3 0 0 2) (CliffordGates1 0 0 2 0))
      ((Tuple3 1 0 2) (CliffordGates1 1 1 2 2))
      ((Tuple3 2 0 2) (CliffordGates1 2 1 0 0))
      ;;Tuple3 0 0 3) (CliffordGates1 0 0 3 0))
      ((Tuple3 1 0 3) (CliffordGates1 2 1 3 6))
      ((Tuple3 2 0 3) (CliffordGates1 1 0 1 2))
      ;;Tuple3 0 1 0) (CliffordGates1 0 1 0 0))
      ((Tuple3 1 1 0) (CliffordGates1 1 0 2 0))
      ((Tuple3 2 1 0) (CliffordGates1 2 1 2 2))
      ;;Tuple3 0 1 1) (CliffordGates1 0 1 1 0))
      ((Tuple3 1 1 1) (CliffordGates1 2 1 1 0))
      ((Tuple3 2 1 1) (CliffordGates1 1 1 1 0))
      ;;Tuple3 0 1 2) (CliffordGates1 0 1 2 0))
      ((Tuple3 1 1 2) (CliffordGates1 1 1 0 6))
      ((Tuple3 2 1 2) (CliffordGates1 2 0 2 6))
      ;;Tuple3 0 1 3) (CliffordGates1 0 1 3 0))
      ((Tuple3 1 1 3) (CliffordGates1 2 0 1 4))
      ((Tuple3 2 1 3) (CliffordGates1 1 0 3 2))
      ((Tuple3 a b c) (cliffordgates1-from-integers a b c 0)))))
