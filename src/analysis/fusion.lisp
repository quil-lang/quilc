;;;; fusion.lisp

(in-package #:cl-quil)

(defclass grid-node ()
  ((tag :initarg :tag
        :reader grid-node-tag
        :documentation "Some piece of arbitrary data.")
   (qubits :initarg :qubits
           :reader grid-node-qubits
           :documentation "A sorted list of qubits that this node acts on.")
   (back :initarg :back
         :reader grid-node-back
         :documentation "A vector of pointers to nodes that precede this node. The pointer will be NIL in the event there's no preceding node. The pointers will be ordered according to the QUBITS slot."
         )
   (forward :initarg :forward
            :reader grid-node-forward
            :documentation "A vector of pointers to nodes that succeed this node. The pointer will be NIL in the event there's no succeeding node. The pointers will be ordered according to the QUBITS slot."))
  (:documentation "A node in a program grid, representing a single gate."))

(defun preceding-node-on-qubit (gn qubit)
  "What is the incoming node to the GRID-NODE GN on qubit QUBIT?"
  (let ((wire (position qubit (grid-node-qubits gn))))
    (if (null wire)
        nil
        (aref (grid-node-back gn) wire))))

(defun (setf preceding-node-on-qubit) (new-node gn qubit)
  (let ((wire (position qubit (grid-node-qubits gn))))
    (if (null wire)
        (error "Can't set preceding node on a wire that doesn't exist.")
        (setf (aref (grid-node-back gn) wire) new-node))))

(defun succeeding-node-on-qubit (gn qubit)
  "What is the outgoing node to the GRID-NODE GN on qubit QUBIT?"
  (let ((wire (position qubit (grid-node-qubits gn))))
    (if (null wire)
        nil
        (aref (grid-node-forward gn) wire))))

(defun (setf succeeding-node-on-qubit) (new-node gn qubit)
  (let ((wire (position qubit (grid-node-qubits gn))))
    (if (null wire)
        (error "Can't set succeeding node on a wire that doesn't exist.")
        (setf (aref (grid-node-forward gn) wire) new-node))))

(defun make-grid-node (tag &rest qubits)
  (let ((n (length qubits)))
    (make-instance 'grid-node :tag tag
                              :qubits (sort (copy-list qubits) #'<)
                              :back (make-array n :initial-element nil)
                              :forward (make-array n :initial-element nil))))

(defun merge-grid-nodes (a b)
  "Merge two grid nodes A and B to produce a new node. All incoming wires to A and outgoing wires to B will be the wires of the merged node.

This function is non-destructive."
  (let* ((all-qubits (sort (union (grid-node-qubits a)
                                  (grid-node-qubits b))
                           #'<))
         (merged-node (apply #'make-grid-node (list (grid-node-tag a)
                                                    (grid-node-tag b))
                             all-qubits)))
    (loop :for wire :from 0
          :for qubit :in all-qubits
          ;; XXX: The order of these OR-clauses matters!
          :for incoming := (or (preceding-node-on-qubit a qubit)
                               (preceding-node-on-qubit b qubit))
          :for outgoing := (or (succeeding-node-on-qubit b qubit)
                               (succeeding-node-on-qubit a qubit))
          :do (assert (and incoming outgoing))
              (setf (aref (grid-node-back    merged-node) wire) incoming
                    (aref (grid-node-forward merged-node) wire) outgoing)
          :finally (return merged-node))))

(defclass program-grid ()
  ((leaders :initarg :leaders
            :accessor leaders
            :documentation "A list of leading nodes to the grid.")
   (trailers :initarg :trailers
             :accessor trailers
             :documentation "A list of trailing nodes to the grid."))
  (:default-initargs :leaders (make-array 0 :adjustable t :initial-element nil)
                     :trailers (make-array 0 :adjustable t :initial-element nil))
  (:documentation "A representation of a program arranged as a collection of nodes on horizontal wires."))

(defun program-grid-num-qubits (pg)
  "How many qubits does PG hold?"
  (length (leaders pg)))

(defun ensure-program-grid-has-qubits (pg n)
  "Grow the program grid PG to have N qubits."
  (when (> n (program-grid-num-qubits pg))
    (adjust-array (leaders pg) n :initial-element nil)
    (adjust-array (trailers pg) n :initial-element nil))
  pg)

(defun append-node (pg gn)
  "Add a node GN to the program PG."
  (ensure-program-grid-has-qubits pg (1+ (loop :for q :in (grid-node-qubits gn) :maximize q)))

  (loop :for qubit :in (grid-node-qubits gn)
        :for leader := (aref (leaders pg) qubit)
        :for trailer := (aref (trailers pg) qubit)
        :do
           ;; Update the PROGRAM-GRID.
           (cond
              ((null leader)
               (assert (null trailer))
               (setf (aref (leaders pg) qubit)  gn
                     (aref (trailers pg) qubit) gn))
              ((null trailer)
               (error "impossible"))
              (t
               ;; Update the GRID-NODE.
               (setf (preceding-node-on-qubit gn qubit) trailer)
               ;; Update the trailer.
               (assert (null (succeeding-node-on-qubit trailer qubit)))
               (setf (succeeding-node-on-qubit trailer qubit) gn))))
  pg)


;;; Input data structure is as follows:
;;;
;;;     <proto program> ::= ( <proto node>* )
;;;
;;;     <proto node> ::= ( <tag> <qubit>+ )
;;;
;;;     <tag> ::= any symbol
;;;
;;;     <qubit> ::= non-negative integer
;;;

(defun build-grid (proto-program)
  "Build a grid from a proto-program.

Example:

    (build-grid
     '((a 1)
       (b 1 2)
       (c 2 3)))
"
  (loop :with pg := (make-instance 'program-grid)
        :for pn :in proto-program
        :for gn := (apply #'make-grid-node (first pn) (rest pn))
        :do (append-node pg gn)
        :finally (return pg)))
