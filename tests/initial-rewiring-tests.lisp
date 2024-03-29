;;;; initial-rewiring-tests.lisp
;;;;
;;;; Author: Corwin de Boor

(in-package #:cl-quil-tests)

(deftest test-queue ()
  (let ((q (quil:make-queue)))
    (is (quil:queue-empty-p q))
    (quil:enqueue q 1)
    (is (not (quil:queue-empty-p q)))
    (quil:enqueue q 2)
    (quil:enqueue q 3)
    (is (= 1 (quil:dequeue q)))
    (is (= 2 (quil:dequeue q)))
    (is (not (quil:queue-empty-p q)))
    (quil:enqueue q 4)
    (quil:enqueue q 5)
    (is (= 3 (quil:dequeue q)))
    (is (= 4 (quil:dequeue q)))
    (is (= 5 (quil:dequeue q)))
    (is (quil:queue-empty-p q))))

(deftest test-queue-make ()
  (let ((q (quil:make-queue)))
    (quil:enqueue q 1)
    (quil:enqueue q 2)
    (quil:enqueue q 3)
    (quil:enqueue q 4)
    (is (not (quil:queue-empty-p q)))
    (is (= 1 (quil:dequeue q)))
    (is (= 2 (quil:dequeue q)))
    (is (= 3 (quil:dequeue q)))
    (quil:enqueue q 5)
    (is (= 4 (quil:dequeue q)))
    (is (= 5 (quil:dequeue q)))
    (is (quil:queue-empty-p q))))

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
    (signals quil::connected-components-incompatible (compiler-hook progm chip))))

(deftest test-sohaib-gh-361-regression ()
  "Regression test for github issue #361."
  ;; https://github.com/rigetti/quilc/issues/361
  (let ((progm (with-output-to-quil
                 "CNOT 0 2"
                 "CNOT 0 7"
                 "RZ(-0.4693665293556365) 7"
                 "CNOT 0 7"
                 "CNOT 0 10"
                 "RZ(-0.439060348695375) 10"
                 "CNOT 0 10"
                 "CNOT 0 15"
                 "RZ(-0.1774648984274036) 15"
                 "CNOT 0 15"
                 "CNOT 0 16"
                 "RZ(-0.5283738721906502) 16"
                 "CNOT 0 16"
                 "CNOT 0 17"
                 "RZ(-0.5127678603188986) 17"
                 "CNOT 0 17"
                 "CNOT 1 2"
                 "RZ(-0.38636880263183865) 2"
                 "CNOT 1 2"
                 "CNOT 1 7"
                 "RZ(-0.23607918922895188) 7"
                 "CNOT 1 7"
                 "CNOT 1 14"))
        (chip (%read-test-chipspec "Aspen-4-10Q-A.qpu")))
    (not-signals error (compiler-hook progm chip))
    (not-signals error (compiler-hook progm chip :rewiring-type ':partial))
    (not-signals error (compiler-hook progm chip :rewiring-type ':greedy))
    ;; We used to get an error here when using NAIVE rewiring, but after the addresser refactor
    ;; it went away. That's good: we'd prefer that the addresser never error, and so we're going
    ;; to retain this test and change the signal expectation.
    ;;
    ;; In reality, the NAIVE addresser _does_ have a nonzero chance of erroring in practice, but
    ;; the input that triggers it is highly fragile under changes to the source.  So, for the
    ;; purposes of tracking these errors down and monitoring them, this test is no longer useful.
    (not-signals error (compiler-hook progm chip :rewiring-type ':naive))))

(deftest test-prog-initial-rewiring-heuristic ()
  ;; Any PRAGMA INITIAL_REWIRING is respected.
  (dolist (init-rewiring '("NAIVE" "PARTIAL" "GREEDY" "RANDOM"))
    (is (eq (a:make-keyword init-rewiring)
            (quil::prog-initial-rewiring-heuristic
             (with-output-to-quil
               (format t "PRAGMA INITIAL_REWIRING ~S~%" init-rewiring)
               "X 0"
               "CZ 0 2"
               "CNOT 2 3")
             (quil::build-nq-linear-chip 4)))))

  (let ((pp-a (with-output-to-quil "X 0" "CZ 0 2" "CNOT 2 3"))
        (pp-b (with-output-to-quil "X 0" "CZ 1 2" "CNOT 2 3")))
    (dolist (quil::*initial-rewiring-default-type* '(:naive :partial :greedy :random))
      ;; Contains CZ on non-adjancent qubits: *INITIAL-REWIRING-DEFAULT-TYPE*
      (is (eq quil::*initial-rewiring-default-type*
              (quil::prog-initial-rewiring-heuristic pp-a (quil::build-nq-linear-chip 4))))

      ;; Always :NAIVE for a fully-connected chip
      (is (eq ':naive
              (quil::prog-initial-rewiring-heuristic pp-a (quil::build-nq-fully-connected-chip 4))))

      ;; Always :NAIVE for PP-B on linear chip
      (is (eq ':naive
              (quil::prog-initial-rewiring-heuristic pp-b (quil::build-nq-linear-chip 4))))))

  ;; Test instructions operating on dead or non-existent qubit.
  (let ((pp-x       (with-output-to-quil "X 0"))
        (pp-reset   (with-output-to-quil "RESET 0"))
        (pp-measure (with-output-to-quil "DECLARE ro BIT" "MEASURE 0 ro"))
        (pp-r-and-m (with-output-to-quil "RESET" "DECLARE ro BIT" "MEASURE 0 ro"))
        (chip-dead  (quil::qpu-hash-table-to-chip-specification
                     (yason:parse "{\"isa\": {\"1Q\": {\"0\": {\"dead\": \"true\"}, \"1\": {}},
                                              \"2Q\": {}}}")))
        (chip-missing (quil::qpu-hash-table-to-chip-specification
                       (yason:parse "{\"isa\": {\"1Q\": {\"1\": {}}}}"))))
    (dolist (quil::*initial-rewiring-default-type* '(:naive :partial :greedy :random))
      (dolist (pp (list pp-x pp-reset pp-measure pp-r-and-m))
        (dolist (chip (list chip-dead chip-missing))
          (is (eq quil::*initial-rewiring-default-type*
                  (quil::prog-initial-rewiring-heuristic pp chip))))))))
