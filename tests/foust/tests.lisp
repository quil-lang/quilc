;;;; Author: Yarin Heffes

(defpackage #:cl-quil-tests/foust-tests
  (:documentation
   "This package defines a series of test to validate the stability of Foust in the event of

any change to the repository.")
  (:use #:coalton
        #:coalton-prelude
        #:coalton-testing)
  (:use #:cl-quil.foust/pauli
        #:cl-quil.foust/frame
        #:cl-quil.foust/assignments
        #:cl-quil.foust/node
        #:cl-quil.foust/gate
        #:cl-quil.foust/circuit
        #:cl-quil.foust/graph
        #:cl-quil.foust/cost
        #:cl-quil.foust/reduce)
  (:import-from
   #:coalton-library/list
   #:nth
   #:combsof)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:map #:coalton-library/ord-map)
   (#:result #:coalton-library/result))
  (:export #:run-tests))

(in-package #:cl-quil-tests/foust-tests)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:cl-quil-tests.foust/fiasco-test-package)

(coalton-fiasco-init #:cl-quil-tests.foust/fiasco-test-package)

(cl:defun run-tests ()
  (fiasco:run-package-tests
   :packages '(#:cl-quil-tests.foust/fiasco-test-package)
   :interactive cl:t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/sign`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-sign-equals ()
  (is (/= Minus Plus))
  (is (/= Plus Minus))
  (is (== Plus Plus))
  (is (== Minus Minus))
  (is (== Plus (default))))

(define-test test-sign-* ()
  (is (== Plus (sign-* Minus Minus)))
  (is (== Minus (sign-* Minus Plus)))
  (is (== Minus (sign-* Plus Minus)))
  (is (== Plus (sign-* Plus Plus)))

  (is (== Plus (msum (make-list Plus Minus Minus Minus Plus Minus))))
  (is (== Minus (msum (make-list Minus Plus Minus Minus Plus Plus)))))

(define-test test-sign-into ()
  (is (== Plus (unwrap (tryinto #\+))))
  (is (== Minus (unwrap (tryinto #\-))))

  (is (== #\+ (into Plus)))
  (is (== #\- (into Minus)))

  (is (== "+" (into Plus)))
  (is (== "-" (into Minus))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/pauli-operator`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (define pauli-operators (make-list I X Y Z))

  (define (both condition? this that)
    (is (condition? this that))
    (is (condition? that this)))

  (define (bothf condition? func this that)
    (is (condition? (func this that)))
    (is (condition? (func that this)))))

(define-test test-pauli-operator-equals ()
  (for p in pauli-operators
    (is (== p p)))

  (for pq in (combsof 2 pauli-operators)
    (both /= (nth 0 pq) (nth 1 pq))))

(define-test test-pauli-operator-commute ()
  (is (commute? I I))
  (for p in (unwrap (tail pauli-operators))
    (both commute? I p)
    (is (commute? p p)))

  (for pq in (combsof 2 (list:cdr pauli-operators))
    (bothf not commute? (nth 0 pq) (nth 1 pq))))

(define-test test-pauli-operator-* ()
  (for p in pauli-operators
    (is (== I (pauli-operator-* p p))))
  (bothf (== X) pauli-operator-* Y Z)
  (bothf (== Y) pauli-operator-* Z X)
  (bothf (== Z) pauli-operator-* X Y)

  (is (== X (msum (make-list X Y Z X Z Y X)))))

(define-test test-pauli-operator-levi-civita ()
  (is (== Y (next-pauli-operator X)))
  (is (== Z (next-pauli-operator Y)))
  (is (== X (next-pauli-operator Z)))
  (is (== Z (prev-pauli-operator X)))
  (is (== X (prev-pauli-operator Y)))
  (is (== Y (prev-pauli-operator Z)))

  (is (== 0 (levi-civita I I)))
  (for p in (unwrap (tail pauli-operators))
    (bothf (== 0) levi-civita I p)
    (is (== 0 (levi-civita p p))))

  (is (== -1 (levi-civita Y X)))
  (is (== +1 (levi-civita X Y)))

  (is (== -1 (levi-civita Z Y)))
  (is (== +1 (levi-civita Y Z)))

  (is (== -1 (levi-civita X Z)))
  (is (== +1 (levi-civita Z X))))

(define-test test-pauli-operator-into ()
  (is (== i (unwrap (tryinto #\I))))
  (is (== X (unwrap (tryinto #\X))))
  (is (== Y (unwrap (tryinto #\Y))))
  (is (== Z (unwrap (tryinto #\Z))))

  (is (== #\I (into I)))
  (is (== #\X (into X)))
  (is (== #\Y (into Y)))
  (is (== #\Z (into Z)))

  (is (== "I" (into I)))
  (is (== "X" (into X)))
  (is (== "Y" (into Y)))
  (is (== "Z" (into Z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/pauli`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (define paulis (cons (Pauli Plus map:Empty)
                       (map (fn (p) (Pauli Plus (map:insert-or-replace map:Empty 0 p)))
                            (list:cdr pauli-operators)))))

(define-test test-pauli-make-pauli ()
  (for pairing in (zip paulis pauli-operators)
    (is (uncurry == (map-snd (fn (p)
                               (Pauli Plus (singleton (Tuple 0 p))))
                             pairing))))

  (is (== (Pauli Minus (make-list (Tuple 0 I)
                                       (Tuple 1 X)
                                       (Tuple 2 Z)))
          (Pauli Minus (map:insert-or-replace (map:insert-or-replace map:empty 1 X) 2 Z)))))

(define-test test-pauli-into ()
  (for pairing in (zip paulis (make-list "+I" "+X" "+Y" "+Z"))
    (is (uncurry == (map-snd (compose unwrap tryinto) pairing))))

  (is (== (Pauli Minus (map:insert-or-replace (map:insert-or-replace map:empty 1 X) 2 Z))
          (compose result:ok-or-error tryinto "-IXZ"))))

(define-test test-pauli-commute ()
  (for pairing-one in (zip paulis pauli-operators)
    (for pairing-two in (zip paulis pauli-operators)
      (is (== (commute? (fst pairing-one) (fst pairing-two))
              (commute? (snd pairing-one) (snd pairing-two))))))

  (is (commute? (Pauli Plus (make-list (Tuple 0 X)
                                             (Tuple 1 Y)
                                             (Tuple 2 X)
                                             (Tuple 3 Z)
                                             (Tuple 4 i)))
                (Pauli Minus (make-list (Tuple 0 Y)
                                             (Tuple 1 Y)
                                             (Tuple 2 X)
                                             (Tuple 3 X)
                                             (Tuple 4 Z)))))

  (is (not (commute? (Pauli Plus (make-list (Tuple 0 X)
                                                  (Tuple 1 Y)
                                                  (Tuple 2 X)
                                                  (Tuple 3 Z)
                                                  (Tuple 4 i)))
                     (Pauli Minus (make-list (Tuple 0 Y)
                                                  (Tuple 1 Y)
                                                  (Tuple 2 Z)
                                                  (Tuple 3 X)
                                                  (Tuple 4 Z)))))))

(define-test test-pauli-* ()
  (for p in paulis
    (is (== (pauli-* p p) (default))))

  (is (== (unwrap (tryinto "-YXYIIZZIZZY"))
          (pauli-* (unwrap (tryinto "+XYIIXYZZXIX"))
                   (unwrap (tryinto "+ZZYIXXIZYZZ"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/frame`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare permsof (UFix -> (List :a) -> (List (List :a))))
  (define (permsof n l)
    (concatmap list:perms (combsof n l)))

  (declare single-qubit-paulis (List Pauli))
  (define single-qubit-paulis (concatmap (fn (sign-s)
                                           (map (fn (p) (make-pauli-one p 0 sign-s))
                                                (list:cdr pauli-operators)))
                                         (make-list Minus Plus)))

  ;; 24 one-qubit Cliffords.
  (declare single-qubit-frames (List Frame))
  (define single-qubit-frames (concatmap
                               (fn (pq)
                                 (make-list (Frame (singleton (Tuple 0 (Tuple (nth 0 pq) (nth 1 pq)))))
                                            (Frame (singleton (Tuple 0 (Tuple (nth 1 pq) (nth 0 pq)))))))
                               (filter (fn (pq) (not (commute? (nth 0 pq) (nth 1 pq))))
                                       (combsof 2 single-qubit-paulis))))

  (declare two-qubit-paulis (List Pauli))
  (define two-qubit-paulis (concatmap
                            (fn (sign-s)
                              (concatmap
                               (fn (p)
                                 (map
                                  (fn (q)
                                    (make-pauli-two p q 0 1 sign-s))
                                  pauli-operators))
                               pauli-operators))
                            (make-list Minus Plus)))

  ;; 11520 two-qubit Cliffords.
  (declare two-qubit-frames (List Frame))
  (define two-qubit-frames (map (fn (pqpq)
                                  (match pqpq
                                    ((Cons p1 (Cons q1 (Cons p2 (Cons q2 (Nil)))))
                                     (Frame (make-list (Tuple 0 (Tuple p1 q1))
                                                            (Tuple 1 (Tuple p2 q2)))))
                                    (_ (error ""))))
                                (filter (fn (pqpq)
                                          (match pqpq
                                            ((Cons p1 (Cons q1 (Cons p2 (Cons q2 (Nil)))))
                                             (and (not (commute? p1 q1))
                                                  (not (commute? p2 q2))
                                                  (commute? p1 p2)
                                                  (commute? p1 q2)
                                                  (commute? q1 p2)
                                                  (commute? q1 q2)))
                                            (_ (error ""))))
                                        (permsof 4 two-qubit-paulis)))))

(coalton-toplevel

  (declare random-from (UFix -> (List :a) -> (List :a)))
  (define (random-from n xs)
    (let ((l (length xs)))
      (pipe (lisp (List UFix) (n l)
              (cl:loop :repeat n :collect (cl:random l)))
            (map (flip nth xs))))))

(define-test test-frame-><- ()
  (for frame-f in (random-from 288 two-qubit-frames)
    (for p in (random-from 8 two-qubit-paulis)
      (is (== p (frame-> frame-f (frame<- frame-f p))))
      (is (== p (frame<- frame-f (frame-> frame-f p))))
      (is (== (frame-> frame-f p) (frame<- (frame-inverse frame-f) p)))
      (is (== (frame<- frame-f p) (frame-> (frame-inverse frame-f) p))))
    (is (== (default) (frame-compose frame-f (frame-inverse frame-f))))
    (is (== (default) (frame-compose (frame-inverse frame-f) frame-f)))))

(define-test test-single-qubit-frames ()
  (for p in (list:cdr pauli-operators)
    (is (== (frame-from-pauli-gate p 0)
            (frame-inverse (frame-from-pauli-gate p 0))))
    (for sign-s in (make-list Minus Plus)
      (is (== (frame-from-h sign-s p 0) (frame-inverse (frame-from-h sign-s p 0)))))
    (is (== (frame-from-s False p 0)
            (frame-inverse (frame-from-s True p 0))))
    (is (== (frame-from-s True p 0)
            (frame-inverse (frame-from-s False p 0)))))
  (for s-one in (make-list Plus Minus)
    (for s-two in (make-list Plus Minus)
      (for s-three in (make-list Plus Minus)
        (is (== (frame-from-permute s-one s-two s-three 0)
                (frame-inverse (frame-from-permute (sign-* Minus s-one)
                                                   (sign-* Minus s-two)
                                                   (sign-* Minus s-three)
                                                   0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/assignments`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-classical-xor ()
  (let ((expr1 (ClassicalExpression (make-list 0 1 2 5   9 18    34) False))
        (expr2 (ClassicalExpression (make-list 0   2 5 7   18 22   ) True))
        (expr3 (classical-xor expr1 expr2)))
    (is (== (make-list 1 7 9 22 34) (list:sort (get-classical-expression-variables expr3))))
    (is (== True (get-classical-expression-bit expr3)))
    (is (== "c1 + c7 + c9 + c22 + c34 + 1" (into expr3)))))

(define-test test-simplify-assignments ()
  (let ((mu1 (fold add-instruction
                   (null-assignments 4)
                   (make-list (Tuple 0 (ClassicalVariable 5))
                              (Tuple 1 (ClassicalVariable 6))
                              (Tuple 2 (ClassicalVariable 4))
                              (Tuple 3 (ClassicalVariable 7))
                              (Tuple 6 (fold classical-xor
                                             (ClassicalBit True)
                                             (map ClassicalVariable (make-list 4 5 7))))
                              (Tuple 8 (classical-xor (ClassicalVariable 6)
                                                      (ClassicalVariable 5)))
                              (Tuple 7 (ClassicalBit True)))))
        (mu2 (simplify-assignments mu1)))
    (is (== (make-list (Tuple 0 (ClassicalVariable 5))
                       (Tuple 1 (classical-xor (ClassicalVariable 4)
                                               (ClassicalVariable 5)))
                       (Tuple 2 (ClassicalVariable 4))
                       (Tuple 3 (ClassicalBit True)))
            (get-assignments-instructions mu2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/angle`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-angle-equals ()
  (is (== (Angle 0/1) (default)))
  (is (== (Angle 0/1) (Angle 1/1))))

(define-test test-make-angle ()
  (is (== (Angle 0/1) (Angle 1/1)))
  (is (== (Angle 1/8) (Angle 9/8)))
  (is (== (Angle 1/8) (Angle -7/8))))

(define-test test-angle-ops ()
  (is (== (Angle 0/1) (+ (Angle 7/4) (Angle -6/8))))
  (is (== (Angle 1/2) (- (Angle 2/3) (Angle 2/12))))
  (is (== (Angle 7/8) (negate (Angle 9/8)))))

(define-test test-angle-order ()
  (is (== (Some 0) (angle-order (Angle 0/1))))
  (is (== (Some 0) (angle-order (Angle 7/7))))
  (is (== (Some 1) (angle-order (Angle 9/4))))
  (is (== (Some 2) (angle-order (Angle -4/8))))
  (is (== (Some 3) (angle-order (Angle 9/12))))
  (is (none? (angle-order (Angle 1/3))))
  (is (none? (angle-order (Angle 1/7)))))

(define-test test-angle-into ()
  (is (== "0" (into (Angle 1/1))))
  (is (== "2π/3" (into (Angle 1/3))))
  (is (== "π/2" (into (Angle -3/4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/node`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/gate`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  (define +x (the Pauli (unwrap (tryinto "+X"))))
  (define +y (the Pauli (unwrap (tryinto "+Y"))))
  (define +z (the Pauli (unwrap (tryinto "+Z"))))
  (define -x (the Pauli (unwrap (tryinto "-X"))))
  (define -y (the Pauli (unwrap (tryinto "-Y"))))
  (define -z (the Pauli (unwrap (tryinto "-Z")))))


(define-test test-gate->frame ()
  (for p in (unwrap (tail pauli-operators))
    (is (== (gate->frame (PauliGate p 0))
            (frame-from-pauli-gate p 0)))
    (for sign-s in (make-list Minus Plus)
      (is (== (gate->frame (H sign-s p 0)) (frame-from-h sign-s p 0))))
    (is (== (gate->frame (S p 0))
            (frame-from-s False p 0)))
    (is (== (gate->frame (Sdag p 0))
            (frame-from-s True p 0))))
  (for s-one in (make-list Plus Minus)
    (for s-two in (make-list Plus Minus)
      (for s-three in (make-list Plus Minus)
        (is (== (gate->frame (Permute s-one s-two s-three 0))
                (frame-from-permute s-one s-two s-three 0)))))))

(define-test test-row->gate ()
  (let ((test-row (fn (b p q gate-g)
                    (is (== (gate->frame gate-g) (gate->frame (row->gate b (Tuple 0 (Tuple p q)))))))))

    (test-row False +x +z (H Plus y 0))
    (test-row False -x -z (H Minus y 0))
    (test-row False +y -x (H Plus x 0))
    (test-row False -y -x (H Minus x 0))
    (test-row False -z +y (H Plus Z 0))
    (test-row False -z -y (H Minus Z 0))
    (test-row False +z -y (S Z 0))
    (test-row False +y +x (S x 0))
    (test-row False -x +z (S y 0))
    (test-row False +Z +y (SDag z 0))
    (test-row False -y +x (SDag x 0))
    (test-row False +x -z (SDag y 0))
    (test-row False -z +x (PauliGate x 0))
    (test-row False -z -x (PauliGate y 0))
    (test-row False +z -x (PauliGate Z 0))
    (test-row False +x +y (Permute Plus Plus Plus 0))
    (test-row False -y -z (Permute Plus Plus Minus 0))
    (test-row False -y +z (Permute Plus Minus Plus 0))
    (test-row False -x -y (Permute Plus Minus Minus 0))
    (test-row False +y -z (Permute Minus Plus Plus 0))
    (test-row False +x -y (Permute Minus Plus Minus 0))
    (test-row False -x +y (Permute Minus Minus Plus 0))
    (test-row False +y +z (Permute Minus Minus Minus 0))

    (test-row True +x +z (H Plus y 0))
    (test-row True -x -z (H Minus y 0))
    (test-row True +y -x (H Plus x 0))
    (test-row True -y -x (H Minus x 0))
    (test-row True -z +y (H Plus Z 0))
    (test-row True -z -y (H Minus Z 0))
    (test-row True +z -y (SDag Z 0))
    (test-row True +y +x (SDag x 0))
    (test-row True -x +z (SDag y 0))
    (test-row True +z +y (S Z 0))
    (test-row True -y +x (S x 0))
    (test-row True +x -z (S y 0))
    (test-row True -z +x (PauliGate x 0))
    (test-row True -z -x (PauliGate y 0))
    (test-row True +z -x (PauliGate Z 0))
    (test-row True +x +y (Permute Minus Minus Minus 0))
    (test-row True -y -z (Permute Minus Minus Plus 0))
    (test-row True -y +z (Permute Minus Plus Minus 0))
    (test-row True -x -y (Permute Minus Plus Plus 0))
    (test-row True +y -z (Permute Plus Minus Minus 0))
    (test-row True +x -y (Permute Plus Minus Plus 0))
    (test-row True -x +y (Permute Plus Plus Minus 0))
    (test-row True +y +z (Permute Plus Plus Plus 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/circuit`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-make-circuit ()

  (let ((circuit-c (make-circuit
                    (make-list (R Z (Angle 1/3) 0)
                               (Controlled x 0 1)
                               (Meas Plus x 0 0)
                               (MeasMult -x 1)
                               (Meas Plus z 1 3)))))
    (is (== 4 (get-assignments-next-fresh-index
               (get-circuit-assignments
                circuit-c))))
    (is (== 4 (get-assignments-first-fresh-index
               (get-circuit-assignments
                circuit-c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/graph`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/cost`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare single-qubit-cliffords (List (UFix -> Gate)))
  (define single-qubit-cliffords
    (make-list
     (PauliGate X)
     (PauliGate Y)
     (PauliGate Z)
     (S X)
     (S Y)
     (S Z)
     (SDag X)
     (SDag Y)
     (SDag Z)
     (H Plus X)
     (H Plus Y)
     (H Plus Z)
     (H Minus X)
     (H Minus Y)
     (H Minus Z)
     (Permute Plus Plus Plus)
     (Permute Plus Plus Minus)
     (Permute Plus Minus Plus)
     (Permute Plus Minus Minus)
     (Permute Minus Plus Plus)
     (Permute Minus Plus Minus)
     (Permute Minus Minus Plus)
     (Permute Minus Minus Minus)))

  (declare random-boolean (Unit -> Boolean))
  (define (random-boolean)
    (== 1 (lisp UFix () (cl:random 2))))

  (declare randn (UFix -> UFix))
  (define (randn n)
    (lisp UFix (n) (cl:random n)))

  (declare randt (UFix -> (Tuple UFix UFix)))
  (define (randt n)
    (match (Tuple (randn n) (randn n))
      ((Tuple n1 n2) (if (== n1 n2)
                         (randt n)
                         (Tuple n1 n2)))))

  (declare random-pauli-operator (Unit -> PauliOperator))
  (define (random-pauli-operator)
    (list:car (random-from 1 (list:cdr pauli-operators))))

  (declare tqes (UFix -> UFix -> Boolean -> (List Gate)))
  (define (tqes min-index max-index then-swap?)
    (do
     (p1 <- (make-list x y Z))
     (p2 <- (make-list x y Z))
     (index-one <- (range min-index max-index))
     (index-two <- (filter (< index-one) (range min-index max-index)))
      (pure (make-tqe p1 p2 index-one index-two then-swap?))))

  (declare random-two-qubit-clifford (UFix -> UFix -> Frame))
  (define (random-two-qubit-clifford index-one index-two)
    (match (random-from 2 single-qubit-cliffords)
      ((Cons s1 (Cons s2 (Nil)))
       (pipe (gate->frame (s1 index-one))
             (frame-compose (gate->frame (s2 index-two)))
             (if (random-boolean)
                 (frame-compose
                  (gate->frame (make-tqe (random-pauli-operator)
                                         (random-pauli-operator)
                                         index-one
                                         index-two
                                         False)))
                 id)
             (if (random-boolean)
                 (frame-compose (frame-from-swap index-one index-two))
                 id)))
      (_ (error "Unexpected error."))))

  (declare random-n-qubit-clifford (UFix -> UFix -> Frame))
  (define (random-n-qubit-clifford m n)
    (msum (map (fn (_) (uncurry random-two-qubit-clifford (randt n))) (range 0 (1- m)))))

  (declare random-npauli (UFix -> Pauli))
  (define (random-npauli n)
    (let ((p (Pauli (list:car (random-from 1 (make-list Plus Minus)))
                         (zip (range 0 (1- n))
                              (random-from n pauli-operators)))))
      (if (== 0 (length (get-pauli-support p)))
          (random-npauli n)
          p)))

  (declare random-anticommuting-npauli (UFix -> (Tuple Pauli Pauli)))
  (define (random-anticommuting-npauli n)
    (let ((pq (Tuple (random-npauli n) (random-npauli n))))
      (if (uncurry commute? pq)
          (random-anticommuting-npauli n)
          pq)))

  (declare random-singlet-nodes (UFix -> UFix -> (List Node)))
  (define (random-singlet-nodes num-nodes n)
    (map (fn (p)
           (if (list:car (random-from 1 (make-list True False)))
               (MeasurementNode (Measurement p 0))
               (RotationNode (Rotation p (Angle 1/3)))))
         (map (fn (_) (random-npauli n)) (range 0 (1- num-nodes)))))

  (declare random-factor-nodes (UFix -> UFix -> (List Node)))
  (define (random-factor-nodes num-nodes n)
    (map (fn (pq)
           (match pq
             ((Tuple p q)
              (if (list:car (random-from 1 (make-list True False)))
                  (PreparationNode (Preparation p q))
                  (Rotation2Node (Rotation2 p q (Angle 1/3) (Angle 2/3)))))))
         (map (fn (_) (random-anticommuting-npauli n)) (range 0 (1- num-nodes))))))

(define-test test-delta-singlet-cost-no-swap ()
  (for n in (random-singlet-nodes 30 5)
    (for t in (tqes 0 4 False)
      (is (== (delta-node-cost t n)
              (- (into (node-cost (frame-node-> (gate->frame (dagger-tqe t)) n)))
                  (into (node-cost n))))))))

(define-test test-delta-singlet-cost-with-swap ()
  (for n in (random-singlet-nodes 30 5)
    (for t in (tqes 0 4 True)
      (is (== (delta-node-cost t n)
              (- (into (node-cost (frame-node-> (gate->frame (dagger-tqe t)) n)))
                  (into (node-cost n))))))))

(define-test test-delta-factor-cost-no-swap ()
  (for n in (random-factor-nodes 30 5)
    (for t in (tqes 0 4 False)
      (is (== (delta-node-cost t n)
              (- (into (node-cost (frame-node-> (gate->frame (dagger-tqe t)) n)))
                  (into (node-cost n))))))))

(define-test test-delta-factor-cost-with-swap ()
  (for n in (random-factor-nodes 30 5)
    (for t in (tqes 0 4 True)
      (is (== (delta-node-cost t n)
              (- (into (node-cost (frame-node-> (gate->frame (dagger-tqe t)) n)))
                  (into (node-cost n))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/reduce`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-singlet-nodes-reduce-no-swap ()
  (for n in (random-singlet-nodes 30 5)
    (if (positive? (node-cost n))
        (let ((reductions (the (List Frame)
                               (iter:collect!
                                (map gate->frame (reduce-node (fn (_ _) False) n)))))
              (ts (tqes 0 4 False)))
          (for t in ts
            (is (== (list:member (gate->frame t) reductions)
                    (negative? (delta-node-cost t n))))))
        (continue))))

(define-test test-singlet-nodes-reduce-with-swap ()
  (for n in (random-singlet-nodes 30 5)
    (if (positive? (node-cost n))
        (let ((reductions (the (List Frame)
                               (iter:collect!
                                (map gate->frame (reduce-node (fn (_ _) True) n)))))
              (ts (tqes 0 4 True)))
          (for t in ts
            (is (== (list:member (gate->frame t) reductions)
                    (negative? (delta-node-cost t n))))))
        (continue))))

(define-test test-factor-nodes-reduce-no-swap ()
  (for n in (random-factor-nodes 30 5)
    (if (positive? (node-cost n))
        (let ((reductions (the (List Frame)
                               (iter:collect!
                                (map gate->frame (reduce-node (fn (_ _) False) n)))))
              (ts (tqes 0 4 False)))
          (for t in ts
            (is (== (list:member (gate->frame t) reductions)
                    (negative? (delta-node-cost t n))))))
        (continue))))

(define-test test-factor-nodes-reduce-with-swap ()
  (for n in (random-factor-nodes 30 5)
    (if (positive? (node-cost n))
        (let ((reductions (the (List Frame)
                               (iter:collect!
                                (map gate->frame (reduce-node (fn (_ _) True) n)))))
              (ts (tqes 0 4 True)))
          (for t in ts
            (is (== (list:member (gate->frame t) reductions)
                    (negative? (delta-node-cost t n))))))
        (continue))))

(define-test test-row-reduce-no-swap ()
  (for frame-f in (iter:take! 10 (iter:new (fn () (Some (random-n-qubit-clifford 40 4)))))
    (for row-r in (map:entries (get-frame-row-map frame-f))
      (if (positive? (row-cost row-r))
          (let ((reductions (iter:collect! (map gate->frame (reduce-row (fn (_ _) False) row-r))))
                (ts (tqes 0 3 False)))
            (for t in ts
              (is (== (negative? (delta-row-cost t row-r))
                      (list:member (gate->frame t) reductions)))))
          (continue)))))

(define-test test-row-reduce-with-swap ()
  (for frame-f in (iter:take! 10 (iter:new (fn () (Some (random-n-qubit-clifford 40 4)))))
    (for row-r in (map:entries (get-frame-row-map frame-f))
      (if (positive? (row-cost row-r))
          (let ((reductions (iter:collect! (map gate->frame (reduce-row (fn (_ _) True) row-r))))
                (ts (tqes 0 3 True)))
            (for t in ts
              (is (== (negative? (delta-row-cost t row-r))
                      (list:member (gate->frame t) reductions)))))
          (continue)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/optimize`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/compile`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  `foust/foust-quil`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
