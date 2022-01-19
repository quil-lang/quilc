;;;; validate-seq-gate.lisp
;;;;
;;;; Author: Parker Williams

(in-package #:cl-quil.frontend)

(defun validate-resolved-seq-gate-definition (gate-def)
  (declare (type sequence-gate-definition gate-def))
  (let ((seq (sequence-gate-definition-sequence gate-def))
        (args (sequence-gate-definition-arguments gate-def)))
    (labels (
             (validate-argument (arg)
               (cond
                 ((qubit-p arg)
                  (error "argument cannot be qubit"))
                 ((not (is-formal arg))
                  (error "argument is not formal"))
                 ((not (find arg args :test #'formal=))
                  (error "argument ~a is not in gatedef arguments" arg))
                 (T T))))
      (unless (seq-contains-only-gate-definitions-p seq)
        (error "sequence gate definition body contains non gate-application"))
      (loop :for isn :in seq
            :for isn-args := (application-arguments isn)
            :do (map nil #'validate-argument isn-args)))))

(defun seq-contains-only-gate-definitions-p (sequence)
  (every #'gate-application-p sequence))

(defgeneric graph-of-seq-def (program)
  (:method ((program parsed-program))
    (graph-of-seq-def (parsed-program-gate-definitions program)))
  (:method ((program null))
    nil)
  (:method ((program sequence-gate-definition))
    (list (sequence-gate-def-to-graph-node program)))
  (:method ((program list))
    (append (graph-of-seq-def (first program)) (graph-of-seq-def (rest program))))
  (:method ((program t))
    nil)
  )

(defgeneric sequence-gate-def-to-graph-node (gate-def)
  (:method ((gate-def sequence-gate-definition))
    (cons (gate-definition-name gate-def) (sequence-gate-def-to-graph-node (sequence-gate-definition-sequence gate-def))))
  (:method ((gate-def list))
    (cons (operator-description-name (application-operator (first gate-def)))
            (sequence-gate-def-to-graph-node (rest gate-def))))
  (:method ((gate-def null))
    nil))

(defun verify-no-loops (graph &optional (path NIL))
  (if graph
      (if path
          (map nil (lambda (x) (if (member x path)
                                   (error "graph contains loops")
                                   (verify-no-loops graph (cons x path))))
               (neighboors-of graph (car (last path))))
          (map nil (lambda (x) (verify-no-loops graph (list (first x)))) graph)) NIL))

(defun neighboors-of (graph key)
  (if graph
      (if (= (first (first graph)) key)
          (rest (first graph))
          (neighboors-of (rest graph) key)) NIL))
