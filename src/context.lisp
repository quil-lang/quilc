;;;; context.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; This file contains the buckets of information carried around by the
;;;; compressor to track the state of the quantum machine and to feed into
;;;; stateful reduction routines.

(in-package #:cl-quil)

(defstruct compilation-context
  "COMPILATION-CONTEXT stores the information concerning the quantum device used by the compressor to perform state-aware reduction of instructions."
  (aqvm nil)
  (chip-specification nil))


(defun set-up-compilation-context (&key (qubit-count 0) (simulate nil)
                                        (chip-specification nil))
  "A helper function for instantiating a new COMPILATION-CONTEXT."
  (make-compilation-context :aqvm (build-aqvm qubit-count :simulate simulate)
                            :chip-specification chip-specification))

(defun update-compilation-context (context instr &key (destructive? nil))
  "This is called when the compressor walks over an instruction in its inner loops, which may in turn modify the active context."
  (let ((n-qubits (length (antisocial-qvm-wfs (compilation-context-aqvm context))))
        (context
          (if destructive?
              context
              (make-compilation-context :aqvm (aqvm-copy (compilation-context-aqvm context))))))
    (cond
      ((global-instruction-p instr)
       (etypecase instr
         (halt
          (setf context (set-up-compilation-context :qubit-count n-qubits)))
         (wait
          (setf context (set-up-compilation-context :qubit-count n-qubits)))
         (reset
          (setf context (set-up-compilation-context :qubit-count n-qubits
                                                    :simulate *enable-state-prep-compression*)))
         (jump
          nil)          ; do nothing, this global instr is non-quantum
         (pragma
          nil)))        ; do nothing, this global instr is non-quantum
      ((or (local-classical-quantum-instruction-p instr)
           (local-classical-instruction-p instr)
           (typep instr 'measure-discard)
           (typep instr 'reset-qubit))
       (let ((resources (instruction-resources instr)))
         (dotimes (qubit n-qubits)
           (when (resource-subsetp (make-qubit-resource qubit) resources)
             (aqvm-stop-simulating (compilation-context-aqvm context) qubit)))))
      (t
       (aqvm-apply-instruction (compilation-context-aqvm context) instr)))
    context))

(defun clean-up-compilation-context (context &key (destructive? nil))
  "This is called when the compressor finishes processing an instruction in the outer loop, which is an opportunity to do computation-intensive cleaning of the context."
  (let ((context
          (if destructive?
              context
              (make-compilation-context :aqvm (aqvm-copy (compilation-context-aqvm context))))))
    (aqvm-unlink (compilation-context-aqvm context))
    context))
