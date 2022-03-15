;;;; validate-sequence-gate.lisp
;;;;
;;;; Author: Parker Williams

(in-package #:cl-quil.frontend)

(defun validate-resolved-seq-gate-definition (gate-def)
  (declare (type sequence-gate-definition gate-def))
  (let ((seq (sequence-gate-definition-sequence gate-def))
        (args (sequence-gate-definition-arguments gate-def)))
    (labels ((validate-argument (arg)
               (cond
                 ((qubit-p arg)
                  (quil-parse-error "argument cannot be qubit"))
                 ((not (is-formal arg))
                  (quil-parse-error "argument is not formal"))
                 ((not (find arg args :test #'formal=))
                  (quil-parse-error "argument ~a is not in gatedef arguments" arg))
                 (T T))))
      (unless (seq-contains-only-gate-definitions-p seq)
        (quil-parse-error (format nil "Sequence gate definition body (~a) contains non gate-application" (gate-definition-name gate-def) )))
      (loop :for isn :in seq
            :for isn-args := (application-arguments isn)
            :do (map nil #'validate-argument isn-args)))))

(defun seq-contains-only-gate-definitions-p (sequence)
  (every #'gate-application-p sequence))

(define-transform validate-defgate-loops (validate-defgate-loops)
  "This transform traverses all defgate as sequence objects to verify there are no circular references.")

(defun validate-defgate-loops (parsed-program)
  (verify-no-loops-in-gate-defs (parsed-program-gate-definitions parsed-program))
  parsed-program)

(defun verify-no-loops-in-gate-defs (gate-defs &optional (ancestors NIL))
  "Takes list of gate defs, verifys there are no circular dependencies between sequence gate defs."
  (dolist (gate-def gate-defs)
    (when (typep gate-def 'SEQUENCE-GATE-DEFINITION)
      (let ((gate-name (gate-definition-name gate-def)))
        (if (member gate-name ancestors)
            (quil-parse-error (format nil "Defgate sequence dependencies contains a loop: ~a " (reverse (cons gate-name ancestors))))
            (verify-no-loops-in-gate-defs (neighbors-of-sequence-gate-def gate-def) (cons gate-name ancestors)))))))

(defun neighbors-of-sequence-gate-def (gate-def)
  "Returns all sequence gate defs that another sequence gate def depends on."
  (mapcar #'gate-application-gate (sequence-gate-definition-sequence gate-def)))
