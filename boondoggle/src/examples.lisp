;;;; examples.lisp
;;;; Authors: Eric Peterson & Chris Osborn

(in-package #:boondoggle)

(defun compiled/uncompiled-l1-distance ()
  "This script measures the L1 distance between QVM results from
compiled and uncompiled programs, respectively. It conforms to the
boondoggle pattern of specifying producers, processors, consumers,
and post-processors. In this case:

1. Producers generate quil programs,
2. Processors either compile or don't compile the generated program
   (perform the identity operation),
3. Consumers run the program on the QVM, and
4. Post-processors measure the L1 distance between the qvm results.

In the generated `pipeline`, the producers, processors, consumers and
post-processors are first defined as the output from various 
make-instances.

Note that the processors are indexed by compiled/uncompiled in the
variable i, and that although the consumers are indexed by j, there
is just one (the QVM). The qvm-results are indexed by both the
consumer and the processor.

The processor-L1-distance post-process takes all consumers and processors
as input (a.k.a. the qvm results and quil programs, respectively), and generates as
output the L1 distance between all possible combinations."
  (declare (optimize (debug 3)))
  (let ((*debug-noise-stream* *standard-output*)
        (chip-spec (quil::build-8q-chip)))
    (pipeline
      ((producer      ()        (make-instance 'producer-random
                                               :program-volume-limit 20
                                               :chip-specification chip-spec
                                               :respect-topology t))      
       (processors    (i)       (list (make-instance 'processor-identity)
                                      (make-instance 'processor-quilc
                                                     :executable-path "~/code/lisp/quilc/quilc")))
       (consumers     (j)       (list (make-instance 'consumer-local-qvm
                                                     :trials 1000)))
       (post-process  ()        (make-instance 'processor-L1-distance)))
      (produced-program ()
                        (produce-quil-program (producer)))
      (compiled-program ((i processors))
                        (progn
                          (quil::print-parsed-program (produced-program))
                          (apply-process (processors i) (produced-program))))
      (qvm-results ((j consumers) (i processors))
        (progn
          (quil::print-parsed-program (compiled-program i))
          (consume-quil (consumers j) (compiled-program i))))
      (l1-distance ((l consumers) (k processors) (j consumers) (i processors))
        (apply-process (post-process) (qvm-results j i) (qvm-results l k))))))
