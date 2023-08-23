;;;; print.lisp
;;;;
;;;; Author: Brennen Hill

(in-package #:cl-quil.match)

;;;; print holds methods for printing data on canonical forms for
;;;; visualization and testing purposes

;; Prints out a visualization of a circuit, displaying each gate in
;; order
(defun print-circ (circ) 
  (loop :for gate :in circ
        :for index :from 0
        :do (format T "~&~A: ~A, ~A" index (matrix gate) (qubits gate))))

;; Helper method for print-canon, gathers text-based data according to
;; the passed function on the children of the passed node
(defun print-children (parent func &optional (spaces "    "))
  (loop :for child :in (direct-succs parent)
        :collect (format NIL "~A~@[~&~*~A~2:*~A~]" (funcall func child) (print-children child func (concatenate 'string "  " spaces))  spaces)))

;; Prints out a visualization of a canonical form with a passed
;; function being called on each node to display arbitrary data
(defun print-canon (canon func)
  (loop :for node :across (nodes canon) :unless (direct-preds node)
        :do (format T "~&(~A~@[~&  ~A~])" (funcall func node) (print-children node func))))

;; Prints out a visualization of a canonical form, with each node
;; displaying the matrix and qubits of its gate
;;
;; If a scenario is passed, also prints whether each node is blocked
;; or matched
(defun print-canon-data (canon &optional scenario)
  (if scenario
      (print-canon canon
                   #'(lambda (p-node)
                       (format nil "~A.~A:~{~A~^,~}~@[M~*~]~@[B~*~]"
                               (label p-node)
                               (matrix (gate p-node))
                               (qubits (gate p-node))
                               (matched-p p-node scenario)
                               (blocked-p p-node scenario))))
      (print-canon canon
                   #'(lambda (p-node)
                       (format nil "~A.~A:~{~A~^,~}" (label p-node) (matrix (gate p-node))(qubits (gate p-node)))))))

;; Prints out a visualization of a circuit as if it was a canonical
;; form, showing which gates commute
(defun print-circ-as-canon (circ)
  (print-canon-data (make-instance 'canonical-form :circuit circ)))


;; Prints out a list of nodes in the canonical-form
;;
;; For each node, it displays the direct-predecessors, the direct
;; successors, the predecessors, and the successors
(defun print-canon-relations (canon &optional (output T))
  (loop :for node :across (nodes canon) :do
    (format output "~&N:~A  DP:(~{~A~^,~}) DS:(~{~A~^,~}) P:(~{~A~^,~}) S:(~{~A~^,~})"
            (label node)
            (loop :for p :in (direct-preds node) :collect (label p))
            (loop :for s :in (direct-succs node) :collect (label s))
            (loop :for p :in (preds node) :collect (label p))
            (loop :for s :in (succs node) :collect (label s)))))
