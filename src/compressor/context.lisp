;;;; compressor/context.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; This file contains the buckets of information carried around by the
;;;; compressor to track the state of the quantum machine and to feed into
;;;; stateful reduction routines.

(in-package #:cl-quil)

(defstruct compressor-context
  "COMPRESSOR-CONTEXT stores the information concerning the quantum device used by the compressor to perform state-aware reduction of instructions."
  (aqvm nil))


(defun set-up-compressor-context (&key (qubit-count 0) (simulate nil))
  "A helper function for instantiating a new COMPRESSOR-CONTEXT."
  (make-compressor-context :aqvm (build-aqvm qubit-count :simulate simulate)))

(defun update-compressor-context (context instr &key (destructive? nil))
  "This is called when the compressor walks over an instruction in its inner loops, which may in turn modify the active context."
  (let ((n-qubits (length (antisocial-qvm-wfs (compressor-context-aqvm context))))
        (context
          (if destructive?
              context
              (make-compressor-context :aqvm (aqvm-copy (compressor-context-aqvm context))))))
    (cond
      ((global-instruction-p instr)
       (etypecase instr
         (halt
          (setf context (set-up-compressor-context :qubit-count n-qubits)))
         (wait
          (setf context (set-up-compressor-context :qubit-count n-qubits)))
         (reset
          (setf context (set-up-compressor-context :qubit-count n-qubits
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
             (aqvm-stop-simulating (compressor-context-aqvm context) qubit)))))
      (t
       (aqvm-apply-instruction (compressor-context-aqvm context) instr)))
    context))

(defun clean-up-compressor-context (context &key (destructive? nil))
  "This is called when the compressor finishes processing an instruction in the outer loop, which is an opportunity to do computation-intensive cleaning of the context."
  (let ((context
          (if destructive?
              context
              (make-compressor-context :aqvm (aqvm-copy (compressor-context-aqvm context))))))
    (aqvm-unlink (compressor-context-aqvm context))
    context))
