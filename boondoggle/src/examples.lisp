;;;; examples.lisp
;;;; Authors: Eric Peterson & Chris Osborn

(in-package #:boondoggle)

;;;; This script measures the L1 distance between 

(defun compiled/uncompiled-l1-distance ()
  ""
  (let ((*debug-noise-stream* *standard-output*)
        (chip-spec (quil:build-8q-chip)))
    (pipeline
     ;; variables / data / definitions
     ((producer      ()        (make-instance 'producer-random
                                              :program-volume-limit 20
                                              :chip-specification chip-spec
                                              :respect-topology t))
    
      (processors    (i)       (list (make-instance 'processor-identity)
                                     (make-instance 'processor-quilc
                                                    :executable-path "<path-to-quilc-executable>")))
      (consumers     (j)       (list (make-instance 'consumer-local-qvm
                                                    :trials 1000)))
      (post-process  ()        (make-instance 'processor-L1-distance)))
     ;;
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
     (l1-distance ((l consumers) (k processors)
                   (j consumers) (i processors))
       (apply-process (post-process) (qvm-results j i) (qvm-results l k))))))


