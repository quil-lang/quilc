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

(deftest test-partial-rewiring-disconnected-components ()
  "Test that partial rewiring behaves sensibly when presented with a
chip-spec that has disconnected components.

Specifically, this is an issue then both PRAGMA INITIAL_REWIRING
\"PARTIAL\" and PRAGMA EXPECTED_REWIRING are provided. See
https://github.com/rigetti/quilc/pull/131 for colour."
  ;; This first case would cause an error due to the missing qubit 0.
  (let ((progm (quil::parse-quil "
PRAGMA INITIAL_REWIRING \"PARTIAL\"
PRAGMA EXPECTED_REWIRING \"#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)\"
RX(pi) 1
PRAGMA CURRENT_REWIRING \"#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)\"
HALT
"))
        (chip (quil::qpu-hash-table-to-chip-specification
               (yason:parse
                "{\"isa\":
        {\"1Q\": {\"1\": {}, \"2\": {}, \"3\": {}},
         \"2Q\": {\"1-2\": {}, \"2-3\": {}}}}"))))
    (not-signals error (compiler-hook progm chip)))
  ;; The second case would case an error due to the disconnected qubits 1 and 3
  (let ((progm (quil::parse-quil "
PRAGMA INITIAL_REWIRING \"PARTIAL\"
PRAGMA EXPECTED_REWIRING \"#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)\"
RX(pi) 1
PRAGMA CURRENT_REWIRING \"#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)\"
HALT
"))
        (chip (quil::qpu-hash-table-to-chip-specification
               (yason:parse
                "{\"isa\":
        {\"1Q\": {\"1\": {}, \"3\": {}},
         \"2Q\": {\"1-2\": {}, \"2-3\": {}}}}"))))
    (not-signals error (compiler-hook progm chip)))
  ;; Just to be sure, let's test that a 2Q gate fails since the
  ;; largest connected component of the chip is length-1.
  (let ((progm (quil::parse-quil "
PRAGMA INITIAL_REWIRING \"PARTIAL\"
PRAGMA EXPECTED_REWIRING \"#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)\"
CNOT 1 3
PRAGMA CURRENT_REWIRING \"#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)\"
HALT
"))
        (chip (quil::qpu-hash-table-to-chip-specification
               (yason:parse
                "{\"isa\":
        {\"1Q\": {\"1\": {}, \"3\": {}},
         \"2Q\": {\"1-2\": {}, \"2-3\": {}}}}"))))
    (signals error (compiler-hook progm chip))))
