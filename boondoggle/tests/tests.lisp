;;;; tests.lisp
;;;;
;;;; Authors: Eric Peterson & Chris Osborn

(in-package #:boondoggle-tests)

(fiasco:deftest compiled/uncompiled-chi-squared-test ()
  "This script calculates the two-sample chi-squared statistic between QVM results from compiled and uncompiled programs, respectively, as a test for whether the two programs generate the same quantum state.

In this test we are creating quantum states `Psi` and `Psi'` from uncompiled and compiled programs `P` and `P'`, respectively. Measuring the resulting wavefunctions (with `N` trials each), we generate two bitstring histograms `H` and `H'`. We say that quilc works if we run the two-sample chi-squared test against `H` and `H'` and fail to reject the null hypothesis, and therefore inferring that the underlying 'distributions' are equal (`Psi = Psi'`).

Bitstrings for K-qubit systems are equivalent to multinomial distributions of categorical data for 2^K categories. The one-sample chi-squared test checks whether a sample was taken from a given distribution. The two-sample chi-squared test checks whether two samples are taken from the same underlying (and unspecified) distribution.

After calculating the chi-squared statistic for the two samples, we may compare that value against the output of the inverse-chi-squared function to determine the likelihood that the generated value does not reject the null hypothesis. Assuming that we would like tests to pass 99.9% when quilc is working, we give 0.999 to the quantile-of-chi-square-distribution function below, together with the number of categories (degrees of freedom). The final check in the tests will fail if the calculated chi-squared value is greater than the 0.999th quantile threshold. For more details on this implementation, see the processor-two-sample-chi-squared apply-process docstring and references in processors.lisp.

This test conforms to the boondoggle pattern of specifying producers, processors, consumers, and post-processors. In this case:

1. Producers generate quil programs,
2. Processors either compile or don't compile the generated program
   (perform the identity operation),
3. the consumer runs the program on the QVM, and
4. Post-processors calculate the chi-square statistic between the qvm results.

In the generated `pipeline`, the producers, processors, consumers and post-processors are first defined as the output from various make-instances.

Note that the processors are indexed by compiled/uncompiled in the variable i, and that although consumers could also be made plural with indexing, there is just one consumer (the QVM). The qvm-results are indexed by both the consumer and the processor.

The processor-two-sample-chi-squared post-process takes all consumers and processors as input (a.k.a. the qvm results and quil programs, respectively), and generates as output the chi-squared statistics (bundled with their categorical degrees of freedom) between all possible combinations.

"
  (let ((boondoggle::*debug-noise-stream* *standard-output*)
        (chip-spec (cl-quil::build-8q-chip)))
        (let ((chi-squared-results
                (boondoggle::pipeline
                  ((producer      ()        (make-instance 'boondoggle::producer-random
                                                           :program-volume-limit 20
                                                           :chip-specification chip-spec
                                                           :respect-topology t))      
                   (processors    (i)       (list (make-instance 'boondoggle::processor-identity)
                                                  (make-instance 'boondoggle::processor-quilc
                                                                 :executable-path (namestring #P"./quilc"))))
                   (consumer      ()        (make-instance 'boondoggle::consumer-local-qvm
                                                           :trials 1000))
                   (post-process  ()        (make-instance 'boondoggle::processor-two-sample-chi-squared)))
                  (produced-program () (boondoggle::produce-quil-program (producer)))
                  (compiled-program ((i processors))
                                    (progn
                                      (cl-quil::print-parsed-program (produced-program))
                                      (boondoggle::apply-process (processors i) (produced-program))))
                  (qvm-results ((i processors))
                               (progn
                                 (cl-quil::print-parsed-program (compiled-program i))
                                 (boondoggle::consume-quil (consumer) (compiled-program i))))
                  (chi-squared-result ((k processors) (i processors))
                                      (boondoggle::apply-process (post-process) (qvm-results i) (qvm-results k))))))
      (destructuring-bind (((chi-1 deg-1) (chi-2 deg-2)) ((chi-3 deg-3) (chi-4 deg-4))) chi-squared-results ; chi-squared-results are of the form e.g. ((0.0d0 0.5d0) (0.5d0 0.0d0))
        (format t "chi-squared-results: ~A" chi-squared-results)
        (fiasco:is (= deg-2 deg-3)) ; Off-diagonal degree-of-freedom elements must equal
        (fiasco:is (<= deg-1 deg-2)) ; Diagonal degree-of-freedom elements must be leq off-diagonal
        (fiasco:is (<= deg-4 deg-2)) ; Same as above
        (fiasco:is (= chi-1 chi-4)) ; Off-diagonal chi-square elements must equal
        (fiasco:is (= chi-2 chi-3)) ; Diagonal chi-square elements must equal
        (fiasco:is (= chi-1 0.0)) ; Diagonal chi-square elements must be zero
        (fiasco:is (<= chi-2 (sapa:quantile-of-chi-square-distribution deg-2 0.999))) ; Off-diagonal chi-square elements must be less than inverse-chi for 99.9% success rate.
))))
