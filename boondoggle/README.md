# Boondoggle

A quilc/QVM/QPU integration testing suite.

This library is in a very early stage of development.

## Example usage

Here is a Lisp snippet illustrating the usage of this library:

```
(let ((*debug-noise-stream* *standard-output*)
      (chip-spec ...))
  (pipeline
   ;; variables / data / definitions
   ((producer      ()        (make-instance 'producer-random
                                            :program-volume-limit 20
                                            :chip-specification chip-spec
                                            :respect-topology t))
    (preprocessors (i) (list (make-instance 'processor-identity)
                             (make-instance 'processor-quilc
                                            :executable-path "...")))
    (processors    (j) (list (make-instance 'processor-measures
                                            :chip-specification chip-spec)
                             (make-instance 'processor-identity)))
    (consumers     (j) (list (make-instance 'consumer-local-qvm
                                            :trials 10)))
    (post-process  ()        (make-instance 'processor-L1-distance)))
   ;; actual process
   (stepA ()
          (produce-quil-program (producer)))
   (stepB ((i preprocessors))
          (progn
            (quil::print-parsed-program (stepA))
            (apply-process (preprocessors i) (stepA))))
   (stepC ((j processors) (i preprocessors))
          (apply-process (processors j) (stepB i)))
   (stepD ((j consumers) (i preprocessors))
          (consume-quil (consumers j) (stepC j i)))
   (stepE ((l consumers) (k preprocessors)
           (j consumers) (i preprocessors))
          (apply-process (post-process) (stepD j i) (stepD l k)))))
```
