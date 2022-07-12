;;;; initial-rewiring-tests.lisp
;;;;
;;;; Author: Corwin de Boor

(in-package #:cl-quil-tests)

(deftest test-queue ()
  (let ((q (cl-quil:make-queue)))
    (is (cl-quil:queue-empty-p q))
    (cl-quil:enqueue q 1)
    (is (not (cl-quil:queue-empty-p q)))
    (cl-quil:enqueue q 2)
    (cl-quil:enqueue q 3)
    (is (= 1 (cl-quil:dequeue q)))
    (is (= 2 (cl-quil:dequeue q)))
    (is (not (cl-quil:queue-empty-p q)))
    (cl-quil:enqueue q 4)
    (cl-quil:enqueue q 5)
    (is (= 3 (cl-quil:dequeue q)))
    (is (= 4 (cl-quil:dequeue q)))
    (is (= 5 (cl-quil:dequeue q)))
    (is (cl-quil:queue-empty-p q))))

(deftest test-queue-make ()
  (let ((q (cl-quil:make-queue)))
    (cl-quil:enqueue q 1)
    (cl-quil:enqueue q 2)
    (cl-quil:enqueue q 3)
    (cl-quil:enqueue q 4)
    (is (not (cl-quil:queue-empty-p q)))
    (is (= 1 (cl-quil:dequeue q)))
    (is (= 2 (cl-quil:dequeue q)))
    (is (= 3 (cl-quil:dequeue q)))
    (cl-quil:enqueue q 5)
    (is (= 4 (cl-quil:dequeue q)))
    (is (= 5 (cl-quil:dequeue q)))
    (is (cl-quil:queue-empty-p q))))

(deftest test-partial-rewiring-disconnected-components ()
  "Test that partial rewiring behaves sensibly when presented with a
chip-spec that has disconnected components.

Specifically, this is an issue then both PRAGMA INITIAL_REWIRING
\"PARTIAL\" and PRAGMA EXPECTED_REWIRING are provided. See
https://github.com/rigetti/quilc/pull/131 for colour."
  ;; This first case would cause an error due to the missing qubit 0.
  (let ((progm (cl-quil::parse-quil "
PRAGMA INITIAL_REWIRING \"PARTIAL\"
PRAGMA EXPECTED_REWIRING \"#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)\"
RX(pi) 1
PRAGMA CURRENT_REWIRING \"#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)\"
HALT
"))
        (chip (cl-quil::qpu-hash-table-to-chip-specification
               (yason:parse
                "{\"isa\":
        {\"1Q\": {\"1\": {}, \"2\": {}, \"3\": {}},
         \"2Q\": {\"1-2\": {}, \"2-3\": {}}}}"))))
    (not-signals error (compiler-hook progm chip)))
  ;; The second case would case an error due to the disconnected qubits 1 and 3
  (let ((progm (cl-quil::parse-quil "
PRAGMA INITIAL_REWIRING \"PARTIAL\"
PRAGMA EXPECTED_REWIRING \"#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)\"
RX(pi) 1
PRAGMA CURRENT_REWIRING \"#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)\"
HALT
"))
        (chip (cl-quil::qpu-hash-table-to-chip-specification
               (yason:parse
                "{\"isa\":
        {\"1Q\": {\"1\": {}, \"3\": {}},
         \"2Q\": {\"1-2\": {}, \"2-3\": {}}}}"))))
    (not-signals error (compiler-hook progm chip)))
  ;; Just to be sure, let's test that a 2Q gate fails since the
  ;; largest connected component of the chip is length-1.
  (let ((progm (cl-quil::parse-quil "
PRAGMA INITIAL_REWIRING \"PARTIAL\"
PRAGMA EXPECTED_REWIRING \"#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)\"
CNOT 1 3
PRAGMA CURRENT_REWIRING \"#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)\"
HALT
"))
        (chip (cl-quil::qpu-hash-table-to-chip-specification
               (yason:parse
                "{\"isa\":
        {\"1Q\": {\"1\": {}, \"3\": {}},
         \"2Q\": {\"1-2\": {}, \"2-3\": {}}}}"))))
    (signals cl-quil::connected-components-incompatible (compiler-hook progm chip))))

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
            (cl-quil::prog-initial-rewiring-heuristic
             (with-output-to-quil
               (format t "PRAGMA INITIAL_REWIRING ~S~%" init-rewiring)
               "X 0"
               "CZ 0 2"
               "CNOT 2 3")
             (cl-quil::build-nq-linear-chip 4)))))

  (let ((pp-a (with-output-to-quil "X 0" "CZ 0 2" "CNOT 2 3"))
        (pp-b (with-output-to-quil "X 0" "CZ 1 2" "CNOT 2 3")))
    (dolist (cl-quil::*initial-rewiring-default-type* '(:naive :partial :greedy :random))
      ;; Contains CZ on non-adjancent qubits: *INITIAL-REWIRING-DEFAULT-TYPE*
      (is (eq cl-quil::*initial-rewiring-default-type*
              (cl-quil::prog-initial-rewiring-heuristic pp-a (cl-quil::build-nq-linear-chip 4))))

      ;; Always :NAIVE for a fully-connected chip
      (is (eq ':naive
              (cl-quil::prog-initial-rewiring-heuristic pp-a (cl-quil::build-nq-fully-connected-chip 4))))

      ;; Always :NAIVE for PP-B on linear chip
      (is (eq ':naive
              (cl-quil::prog-initial-rewiring-heuristic pp-b (cl-quil::build-nq-linear-chip 4))))))

  ;; Test instructions operating on dead or non-existent qubit.
  (let ((pp-x       (with-output-to-quil "X 0"))
        (pp-reset   (with-output-to-quil "RESET 0"))
        (pp-measure (with-output-to-quil "DECLARE ro BIT" "MEASURE 0 ro"))
        (pp-r-and-m (with-output-to-quil "RESET" "DECLARE ro BIT" "MEASURE 0 ro"))
        (chip-dead  (cl-quil::qpu-hash-table-to-chip-specification
                     (yason:parse "{\"isa\": {\"1Q\": {\"0\": {\"dead\": \"true\"}, \"1\": {}},
                                              \"2Q\": {}}}")))
        (chip-missing (cl-quil::qpu-hash-table-to-chip-specification
                       (yason:parse "{\"isa\": {\"1Q\": {\"1\": {}}}}"))))
    (dolist (cl-quil::*initial-rewiring-default-type* '(:naive :partial :greedy :random))
      (dolist (pp (list pp-x pp-reset pp-measure pp-r-and-m))
        (dolist (chip (list chip-dead chip-missing))
          (is (eq cl-quil::*initial-rewiring-default-type*
                  (cl-quil::prog-initial-rewiring-heuristic pp chip))))))))
