;;;; logical-matrix-sanity-tests.lisp
;;;;
;;;; Author: appleby

(in-package #:cl-quil-tests)

;;; The purpose of these tests are to act as a basic sanity-check on the combination of PARSE-QUIL
;;; and PARSED-PROGRAM-TO-LOGICAL-MATRIX. These tests are motivated by a change I made to the parser
;;; in which I reversed the order in which gate application modifiers were parsed, but neglected to
;;; also reverse the order of the qubit arguments. That change broke the parser and, subsequently,
;;; caused PARSED-PROGRAM-TO-LOGICAL-MATRIX to produce incorrect output, but did not result in any
;;; test failures. Making the same parser change will now cause the below tests (specifically ones
;;; that include both FORKED and CONTROLLED modifiers) to fail. So the purpose of these tests is to
;;; prevent appleby from foolishly breaking the parser again (at least not in the same way)!

(deftest test-parser->logical-matrix-sanity ()
  "Test that PARSE-QUIL -> PARSED-PROGRAM-TO-LOGICAL-MATRIX produces the expected matrix for a handful of simple programs."
  (mapc (lambda (testcase)
          (destructuring-bind (input entries) testcase
            (let* ((n (length entries))
                   (p (if (stringp input) (quil:parse-quil input) input))
                   (actual (quil::parsed-program-to-logical-matrix p))
                   ;; TRANSPOSE here to allow writing ENTRIES in row-major order, for readability.
                   (expected (magicl:transpose (magicl:make-complex-matrix n n (a:flatten entries)))))
              (is (quil::matrix-equality actual expected)
                  (format nil "Checking input: ~S~%~A"
                          input
                          (%matrix-mismatch-error-message actual expected))))))
        (let ((i+ #C(0.0 1.0))
              (i- #C(0.0 -1.0)))
          `(("I 0"
             ((1.0  0.0)
              (0.0  1.0)))
            ("X 0"
             ((0.0  1.0)
              (1.0  0.0)))
            ("Y 0"
             ((0.0  ,i-)
              (,i+  0.0)))
            ("Z 0"
             ((1.0  0.0)
              (0.0 -1.0)))
            (,(with-output-to-quil
                "X 0"
                "X 0")
             ((1.0  0.0)
              (0.0  1.0)))
            (,(with-output-to-quil
                "I 0"
                "I 1")
             ((1.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0)
              (0.0  0.0  1.0  0.0)
              (0.0  0.0  0.0  1.0)))
            ("CNOT 0 1"
             ((1.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  1.0)
              (0.0  0.0  1.0  0.0)
              (0.0  1.0  0.0  0.0)))
            ("CNOT 1 0"
             ((1.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0)
              (0.0  0.0  0.0  1.0)
              (0.0  0.0  1.0  0.0)))
            (,(with-output-to-quil
                "X 0"
                "CNOT 0 1")
             ((0.0  1.0  0.0  0.0)
              (0.0  0.0  1.0  0.0)
              (0.0  0.0  0.0  1.0)
              (1.0  0.0  0.0  0.0)))
            (,(with-output-to-quil
                "X 0"
                "CNOT 1 0")
             ((0.0  1.0  0.0  0.0)
              (1.0  0.0  0.0  0.0)
              (0.0  0.0  1.0  0.0)
              (0.0  0.0  0.0  1.0)))
            ("PHASE(pi/2) 0"
             ((1.0  0.0)
              (0.0  ,i+)))
            ("PHASE(-pi/2) 0"
             ((1.0  0.0)
              (0.0  ,i-)))
            (;; Same as above
             "DAGGER PHASE(pi/2) 0"
             ((1.0  0.0)
              (0.0  ,i-)))
            (;; Same as PHASE(pi/2) 0
             "DAGGER DAGGER PHASE(pi/2) 0"
             ((1.0  0.0)
              (0.0  ,i+)))
            (;; Should be the same as CNOT 0 1, above
             "CONTROLLED X 0 1"
             ((1.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  1.0)
              (0.0  0.0  1.0  0.0)
              (0.0  1.0  0.0  0.0)))
            (;; Should be the same as CNOT 1 0, above
             "CONTROLLED X 1 0"
             ((1.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0)
              (0.0  0.0  0.0  1.0)
              (0.0  0.0  1.0  0.0)))
            (;; Test CONTROLLED on non-permutation gate
             "CONTROLLED Y 0 1"
             ((1.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  ,i-)
              (0.0  0.0  1.0  0.0)
              (0.0  ,i+  0.0  0.0)))
            ("CONTROLLED Y 1 0"
             ((1.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0)
              (0.0  0.0  0.0  ,i-)
              (0.0  0.0  ,i+  0.0)))
            ("CONTROLLED DAGGER PHASE(pi/2) 1 0"
             ((1.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0)
              (0.0  0.0  1.0  0.0)
              (0.0  0.0  0.0  ,i-)))
            (;; Same as above
             "DAGGER CONTROLLED PHASE(pi/2) 1 0"
             ((1.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0)
              (0.0  0.0  1.0  0.0)
              (0.0  0.0  0.0  ,i-)))
            ("FORKED Y 0 1"
             ((0.0  0.0  ,i-  0.0)
              (0.0  0.0  0.0  ,i-)
              (,i+  0.0  0.0  0.0)
              (0.0  ,i+  0.0  0.0)))
            ("FORKED Y 1 0"
             ((0.0  ,i-  0.0  0.0)
              (,i+  0.0  0.0  0.0)
              (0.0  0.0  0.0  ,i-)
              (0.0  0.0  ,i+  0.0)))
            ("FORKED PHASE(0, pi/2) 1 0"
             ((1.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0)
              (0.0  0.0  1.0  0.0)
              (0.0  0.0  0.0  ,i+)))
            ("FORKED PHASE(pi/2, 0) 1 0"
             ((1.0  0.0  0.0  0.0)
              (0.0  ,i+  0.0  0.0)
              (0.0  0.0  1.0  0.0)
              (0.0  0.0  0.0  1.0)))
            ("CONTROLLED FORKED Y 2 1 0"
             ((1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  ,i-  0.0  0.0)
              (0.0  0.0  0.0  0.0  ,i+  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  0.0  0.0  ,i-)
              (0.0  0.0  0.0  0.0  0.0  0.0  ,i+  0.0)))
            (;; Same as above
             "FORKED CONTROLLED Y 1 2 0"
             ((1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  ,i-  0.0  0.0)
              (0.0  0.0  0.0  0.0  ,i+  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  0.0  0.0  ,i-)
              (0.0  0.0  0.0  0.0  0.0  0.0  ,i+  0.0)))
            ("FORKED CONTROLLED Y 2 1 0"
             ((1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  ,i-  0.0  0.0  0.0  0.0)
              (0.0  0.0  ,i+  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  0.0  0.0  ,i-)
              (0.0  0.0  0.0  0.0  0.0  0.0  ,i+  0.0)))
            (;; Same as previous
             "CONTROLLED FORKED Y 1 2 0"
             ((1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  ,i-  0.0  0.0  0.0  0.0)
              (0.0  0.0  ,i+  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  0.0  0.0  ,i-)
              (0.0  0.0  0.0  0.0  0.0  0.0  ,i+  0.0)))
            ("CONTROLLED FORKED DAGGER PHASE(pi, pi/2) 2 1 0"
             ((1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0 -1.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  0.0  0.0  ,i-)))
            (;; Same as previous
             "CONTROLLED DAGGER FORKED PHASE(pi, pi/2) 2 1 0"
             ((1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0 -1.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  0.0  0.0  ,i-)))
            (;; Same as previous
             "DAGGER CONTROLLED FORKED PHASE(pi, pi/2) 2 1 0"
             ((1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0 -1.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  0.0  0.0  ,i-)))
            ("FORKED CONTROLLED DAGGER PHASE(pi, pi/2) 2 1 0"
             ((1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0 -1.0  0.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0)
              (0.0  0.0  0.0  0.0  0.0  0.0  0.0  ,i-)))))))
