;;;; initial-rewiring-tests.lisp
;;;;
;;;; Author: Corwin de Boor

(in-package #:cl-quil-tests)

(deftest test-queue ()
  (let ((q (quil::make-q)))
    (is (quil::q-empty q))
    (quil::q-enq 1 q)
    (is (not (quil::q-empty q)))
    (quil::q-enq 2 q)
    (quil::q-enq 3 q)
    (is (= 1 (quil::q-deq q)))
    (is (= 2 (quil::q-deq q)))
    (is (not (quil::q-empty q)))
    (quil::q-enq 4 q)
    (quil::q-enq 5 q)
    (is (= 3 (quil::q-deq q)))
    (is (= 4 (quil::q-deq q)))
    (is (= 5 (quil::q-deq q)))
    (is (quil::q-empty q))))

(deftest test-queue-make ()
  (let ((q (quil::make-q 1 2 3 4)))
    (is (not (quil::q-empty q)))
    (is (= 1 (quil::q-deq q)))
    (is (= 2 (quil::q-deq q)))
    (is (= 3 (quil::q-deq q)))
    (quil::q-enq 5 q)
    (is (= 4 (quil::q-deq q)))
    (is (= 5 (quil::q-deq q)))
    (is (quil::q-empty q))))
