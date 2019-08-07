;;;; examples.lisp
;;;; Authors: Eric Peterson & Chris Osborn

(in-package #:boondoggle-tests)

(fiasco:deftest compiled/uncompiled-l1-distance ()
  "This script measures the L1 distance between QVM results from compiled and uncompiled programs, respectively. It conforms to the boondoggle pattern of specifying producers, processors, consumers, and post-processors. In this case:

1. Producers generate quil programs,
2. Processors either compile or don't compile the generated program
   (perform the identity operation),
3. Consumers run the program on the QVM, and
4. Post-processors measure the L1 distance between the qvm results.

In the generated `pipeline`, the producers, processors, consumers and post-processors are first defined as the output from various make-instances.

Note that the processors are indexed by compiled/uncompiled in the variable i, and that although the consumers are indexed by j, there is just one (the QVM). The qvm-results are indexed by both the consumer and the processor.

The processor-L1-distance post-process takes all consumers and processors as input (a.k.a. the qvm results and quil programs, respectively), and generates as output the L1 distance between all possible combinations."
  (declare (optimize (debug 3)))
  (let ((*debug-noise-stream* *standard-output*)
        (chip-spec (quil::build-8q-chip)))
    (let ((l1-results (boondoggle::pipeline
                        ((producer      ()        (make-instance 'boondoggle::producer-random
                                                                 :program-volume-limit 20
                                                                 :chip-specification chip-spec
                                                                 :respect-topology t))      
                         (processors    (i)       (list (make-instance 'boondoggle::processor-identity)
                                                        (make-instance 'boondoggle::processor-quilc
                                                                       :executable-path "~/code/lisp/quilc/quilc")))
                         (consumers     (j)       (list (make-instance 'boondoggle::consumer-local-qvm
                                                                       :trials 1000)))
                         (post-process  ()        (make-instance 'boondoggle::processor-L1-distance)))
                        (produced-program ()
                                          (boondoggle::produce-quil-program (producer)))
                        (compiled-program ((i processors))
                                          (progn
                                            (quil::print-parsed-program (produced-program))
                                            (boondoggle::apply-process (processors i) (produced-program))))
                        (qvm-results ((j consumers) (i processors))
                                     (progn
                                       (quil::print-parsed-program (compiled-program i))
                                       (boondoggle::consume-quil (consumers j) (compiled-program i))))
                        (l1-distance ((l consumers) (k processors) (j consumers) (i processors))
                                     (boondoggle::apply-process (post-process) (qvm-results j i) (qvm-results l k))))))
      ;; Assert that off-diagonal elements of 2x2 matrix are equivalent
      ;; TODO: Get flatten nested l1-results
      (fiasco:is (=
                  (car (cdr (car (car (car l1-results)))))
                  (car (car (car (cdr (car l1-results)))))))
      )))
