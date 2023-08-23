;;;; data-structures.lisp
;;;;
;;;; Author: Brennen Hill

(in-package #:cl-quil.match)

;;;; data-structures are made up of node, canonical-form, and
;;;; matching-scenario, which hold information used by the pattern
;;;; matching algorithm

;;; Class Definitions

;; A node within a Directed Acyclic Graph, storing the data on a gate
;; as needed for pattern matching
;;
;; Note that two properties of a node, whether it is blocked, and what
;; other node it is matched with, are stored in a matching-scenario
;; for duplication
(defclass node ()
  ((label
    :initarg :label
    :reader label
    :documentation "A unique integer assigned to this node, according to the position of its corresponding gate in the circuit")
   (gate
    :initarg :gate
    :reader gate
    :documentation "The node's primary data, the gate from a circuit")
   (reachable
    :initform T
    :accessor reachable
    :documentation "Used in the creation of a DAG to mark whether this node is a possible successor of another node")
   (succs
    :initform '()
    :accessor succs
    :documentation "A set of the successors to this node in its corresponding DAG")
   (direct-succs
    :initform '()
    :accessor direct-succs
    :documentation "A set of the direct successors to this node in its corresponding DAG")
   (preds
    :initform '()
    :accessor preds
    :documentation "A set of the predecessors to this node in its corresponding DAG")
   (direct-preds
    :initform '()
    :accessor direct-preds
    :documentation "A set of the direct predecessors to this node in its corresponding DAG")
   (succ-to-visit
    :initform '()
    :accessor %succ-to-visit
    :documentation "A sorted list of the unvisited successors to the node as used in forward-match"))
  (:default-initargs
   :label (error "Must supply a label.")
   :gate (error "Must supply a gate."))
  (:documentation "A node within a Directed Acyclic Graph, storing the data on a gate as needed for pattern matching"))

(defclass pat-node (node) ()
  (:documentation "A node from a pattern that realises the identity function"))

;; A type of directed acyclic graph that stores nodes as successors of each other
(defclass canonical-form ()
  ((circuit
    :initarg :circuit
    :accessor circuit
    :documentation "The original circuit that is passed to the canonical form and is only used to build the canonical form. It should be a list of gates")
   (qubits
    :reader qubits
    :documentation "The list of different qubits affected by the passed circuit, and thus by the gates in the nodes in this canonical form")
   (nodes
    :accessor nodes
    :documentation "The array of nodes that make up the canonical form, which the canonical form builds from the passed circuit")
   (pattern
    :initarg :pattern
    :accessor pattern
    :documentation "Currently just used for testing, to determine if it is a member of a pattern"))
  (:default-initargs
   :circuit (error "Must supply a circuit.")
   :pattern NIL)
  (:documentation "A Canonical Form, also known as a directed acyclic graph that stores nodes as successors of each other"))

;; Storage of matching and blocking information on the current
;; scenario designed for duplication of scenarios
(defclass matching-scenario ()
  ((matches
    :initarg :matches
    :accessor matches
    :documentation "The set of pairs of integers that represent the labels of matched nodes")
   (blocked-track-circ
    :initarg :blocked-track-circ
    :accessor blocked-track-circ
    :documentation "An array where each position correlates to the label of a node in the circuit and holds a Boolean of whether that node is blocked")
   (blocked-track-pat
    :initarg :blocked-track-pat
    :accessor blocked-track-pat
    :documentation "An array where each position correlates to the label of a node in the patter and holds a Boolean of whether that node is blocked")
   (matched-track-circ
    :initarg :matched-track-circ
    :accessor matched-track-circ
    :documentation "An array where each position correlates to the label of a node in the circuit and holds an integer representing the label of the matched node")
   (matched-track-pat
    :initarg :matched-track-pat
    :accessor matched-track-pat
    :documentation "An array where each position correlates to the label of a node in the pattern and holds an integer representing the label of the matched node")
   (counter
    :initarg :counter
    :reader counter
    :documentation "The refrence to which node is being checked for a match, to be used in backward-match"))
  (:default-initargs
   :matches (error "Must supply initial matches")
   :blocked-track-circ (error "Must supply initial blocked-track-circ")
   :matched-track-circ (error "Must supply initial matched-track-circ")
   :blocked-track-pat (error "Must supply initial blocked-track-pat")
   :matched-track-pat (error "Must supply initial matched-track-pat")
   :counter 0))

;; Storage of of a circuit holding information on the matched section
(defclass matched-circ ()
  ((circ
    :initarg :circ
    :accessor circ
    :documentation "The circuit that the matches are made on")
   (start-index
    :initarg :start-index
    :accessor start-index
    :documentation "The index of the first matched gate in the circuit")
   (initial-start-index
    :initarg :initial-start-index
    :accessor initial-start-index
    :documentation "The index held by the first matched get in the circuit before being commuted")
   (end-index
    :initarg :end-index
    :accessor end-index
    :documentation "The index of the last matched gate in the curcuit")
   (initial-end-index
    :initarg :initial-end-index
    :accessor initial-end-index
    :documentation "The index held by the last matched get in the circuit before being commuted")
   (cost
    :initarg :cost
    :reader cost
    :documentation "The cost of running the gates that make up the match in this curcuit")
   (matches
    :initarg :matches
    :reader matches
    :documentation "The list of indices of matched gates in the circuit"))
  (:default-initargs
   :circ (error "Must supply circ.")
   :start-index (error "Must supply start-index.")
   :end-index (error "Must supply end-index.")
   :cost (error "Must supply cost.")
   :matches (error "Must supply matches."))
  (:documentation "Holds information on the matched section of the circuit"))

;; The pair of a circuit and pattern that share a match between gates
(defclass match-data ()
  ((matched-circ
    :reader matched-circ
    :documentation "Holds information on the matched section of the circuit")
   (matched-pat
    :reader matched-pat
    :documentation "Holds information on the matched section of the pattern")
   (matches
    :initarg :matches
    :reader matches
    :documentation "The list of pairs of indices of matched gates")
   (replace-cost
    :reader replace-cost
    :documentation "The cost of running the gates that make up the replacement for this match"))
  (:default-initargs
   :matches (error "Must supply matches.")
   :circ (error "Must supply a circ.")
   :pat (error "Must supply a pat."))
  (:documentation "The data on a match required to replace it in a circuit with a more efficient set of gates from a matched pattern"))

;;; The methods that operate on nodes

(defgeneric matched (node match scenario)
  (:documentation "Wrapper for matched-expansion")
  (:method ((node node) (match pat-node) (scenario matching-scenario))
    (matched-expansion node match scenario))
  (:method ((match pat-node) (node node) (scenario matching-scenario))
    (matched-expansion node match scenario)))

(defgeneric unmatched (pat-node scenario)
  (:documentation "Wrapper for unmatched-expansion")
  (:method ((circ-node node) (scenario matching-scenario))
    (unmatched-expansion circ-node (matched-track-circ scenario) scenario #'first))
  (:method ((pat-node pat-node) (scenario matching-scenario))
    (unmatched-expansion pat-node (matched-track-pat scenario) scenario #'second)))

(defgeneric gate-matched-p (node other)
  (:documentation "Returns whether two gates corresponding to the passed nodes perform the same operation on the same qubits.")
  (:method ((node node) (other  node))
    (gate-matched-p (gate node) (gate other))))

(defgeneric gates-commute-p (node other)
  (:documentation "Returns whether the gates corresponding to the passed nodes can commute with one another, based off their operation and location in the circuit")
  (:method ((node node) (other node))
    (gates-commute-p (gate node) (gate other))))
      
(defgeneric equal-gate-operation-p (node other)
  (:documentation "Returns whether the gates corresponding to the passed nodes perform the same operation, albeit possibly on different qubits")
  (:method ((node node) (other node))
    (equal-gate-operation-p (gate node) (gate other))))

(defgeneric gate-act-consequentialy-on-qubit-p (node qubit)
  (:documentation "Returns whether the gate corresponding to the passed node acts non-consequentialy on the passed qubit")
  (:method ((node node) (qubit integer))
    (gate-act-consequentialy-on-qubit-p (gate node) qubit)))
    
(defgeneric blocked (node matching-scenario)
  (:documentation "Sets the position in the vector in respect to the label of the node to true to represent that this node has been blocked")
  (:method ((node node) (scenario matching-scenario))
    (setf (svref (blocked-track-circ scenario) (label node)) T))
  (:method ((node pat-node) (scenario matching-scenario))
    (setf (svref (blocked-track-pat scenario) (label node)) T)))

(defgeneric blocked-p (node matching-scenario)
  (:documentation "Returns whether the passed node is blocked as recorded by blocked-track")
  (:method ((node node) (scenario matching-scenario))
    (svref (blocked-track-circ scenario) (label node)))
  (:method ((node pat-node) (scenario matching-scenario))
    (svref (blocked-track-pat scenario) (label node))))

(defgeneric get-matched (node matching-scenario matched-canon)
  (:documentation "Returns the node in matched-canon that the passed node is matched with within matched-track")
  (:method ((node node) (scenario matching-scenario) (matched-canon canonical-form))
    (get-node matched-canon (svref (matched-track-circ scenario) (label node))))
  (:method ((node pat-node) (scenario matching-scenario) (matched-canon canonical-form))
    (get-node matched-canon (svref (matched-track-pat scenario) (label node)))))

(defgeneric matched-p (node matching-scenario)
  (:documentation "Returns whether the passed node has a match within matched-track")
  (:method ((node node) (scenario matching-scenario))
    (svref (matched-track-circ scenario) (label node)))
  (:method ((node pat-node) (scenario matching-scenario))
    (svref (matched-track-pat scenario) (label node))))

(defgeneric push-succ-to-visit (node succ)
  (:documentation "AddsPushes succ to the successors to visit of the node, into a position such that succ-to-visit is in ascending order by label")
  (:method ((node node) (succ node))
    (setf (%succ-to-visit node)
          (sort
           (cons succ (%succ-to-visit node))
           #'< :key #'label))))

(defgeneric pop-succ-to-visit (node)
  (:documentation "Returns and removes the first node from succ-to-visit of the passed node")
  (:method ((node node))
    (pop (%succ-to-visit node))))

(defgeneric peek-succ-to-visit (node)
  (:documentation "Returns the first node from succ-to-visit of the passed node without editing the list")
  (:method ((node node))
    (first (%succ-to-visit node))))

;;; Methods that operate on canonical form

;; Builds the list of nodes out of the circuit
(defmethod initialize-instance :after ((canon canonical-form) &key)
  (loop :with node
        :and qubits = '()
               :initially (setf (slot-value canon 'nodes) (make-array (length (circuit canon)) :initial-element NIL))
        :for label :from 0
        :for circ-gate :in (circuit canon)
        :do
           ;;Add the node corresponding to the next gate into the canonical-form
           (setf node (setf (svref (slot-value canon 'nodes) label)
                            (make-instance
                             (if (pattern canon) 'pat-node 'node)
                             :gate (coerce circ-gate 'gate)
                             :label label)))
           
           ;;Add this gate's qubits to the list of qubits
           (setf qubits (union qubits (coerce (qubits (gate node)) 'list)))
           ;;Flag each node in the canonical form as a possible predecessor
           (loop :for prior-node :across (nodes canon)
                 :always prior-node
                 :do (setf (slot-value prior-node 'reachable) T))
           
           ;;Find the node's predecessors
           (loop :for i :from (1- label) :downto 0
                 :for prior-node = (get-node canon i) then (get-node canon i)
                 :do
                    ;;A predecessor node must be a node who cannot commute with this node and who's successor
                    ;; hasn't been been flagged as selected predecessor
                    (when (and (reachable prior-node)
                               (not (gates-commute-p node prior-node)))
                      ;;Add the incommutable node and all of its predecessors to the predecessors of the current node
                      (push prior-node (direct-preds node))
                      ;; (push prior-node (preds node))                        
                      (setf (preds node) (union
                                          (cons prior-node (preds node))
                                          (preds prior-node)))
                      ;;Add the current node to the successors of the incomutable node and its predecessors
                      (push node (direct-succs prior-node))
                      (push node (succs prior-node))
                      (loop :for pred :in (preds prior-node)
                            :do (push node (succs pred)))
                      ;;Flag the predecessors of the incomutable nodes as not reachable as  node's direct successors cannot be successors of one another
                      (loop :for pred :in (preds prior-node)
                            :do (setf (slot-value pred 'reachable) NIL))))
        :finally
           ;;Sort the refrences to other nodes of each node
           (loop :for node :across (nodes canon)
                 :for label :from 0
                 :do (setf (direct-preds node) (sort (copy-list (direct-preds node))  #'< :key #'label))
                     (setf (direct-succs node) (sort (copy-list (direct-succs node))  #'< :key #'label))
                     (setf (preds node) (sort (copy-list (preds node))  #'< :key #'label))
                     (setf (succs node) (sort (copy-list (succs node))  #'< :key #'label)))
           ;;Store the gathered qubits as the qubits operated on by the canonical form
           (setf (slot-value canon 'qubits) qubits)))

(defgeneric get-node (canon label)
  (:documentation "Gets the node with the label of label from the canonical form")
  (:method ((canon canonical-form) (label integer))
    (svref (nodes canon) label)))

(defgeneric map-pat-canon (canon qubit-permutation)
  (:documentation "Maps the passed canonical form of a  pattern to a certainn set of qubits held in qubit-permutation")
  (:method ((canon canonical-form) qubit-permutation)
    (make-instance 'canonical-form
                   :circuit (map-pat (circuit canon) (qubits canon) qubit-permutation)
                   :pattern (pattern canon))))

;; Maps the passed pattern to a certain set of qubits held in qubit-permutation
(defun map-pat (pattern pattern-qubits qubit-permutation)
  (loop :for gate :in pattern
        :for new-qubits = (loop :for qubit :in (qubits gate)
                                :collect (nth
                                          ;;Which qubit to use from the permutation is relative to the position of
                                          ;;the original qubit in the original pattern
                                          (position qubit pattern-qubits :test #'same-qubit-p)
                                          qubit-permutation))
        ;;Build a new gate with the permuted qubit, but the same matrix
        :collect (permute-qubits gate new-qubits)))
        
;;; Methods that operate on the matching scenarios

;; Returns a new array containing the same elements as the passed array
(defun duplicate-array (source)
  (make-array (length source) :initial-contents
              (loop :for element :across source
                    :collecting element)))

(defgeneric next-scenario (scenario &optional increment)
  (:documentation "Returns a deep copy of the current scenario with the counter increased by 1")
  (:method ((scenario matching-scenario) &optional (increment T))
  (make-instance 'matching-scenario
                 :matches (loop :for match :in (matches scenario) :collecting (list (first match) (second match))) 
                 :blocked-track-circ (duplicate-array (blocked-track-circ scenario))
                 :blocked-track-pat (duplicate-array (blocked-track-pat scenario))
                 :matched-track-circ (duplicate-array (matched-track-circ scenario))
                 :matched-track-pat (duplicate-array (matched-track-pat scenario))
                 :counter (+ (counter scenario) (if increment 1 0)))))

;;; The methods that operate on match-data

;; Initializes a match data and constructs a matched-circ for the circuit and the pattern
(defmethod initialize-instance :after ((match-data match-data) &key circ pat)
  (loop :for match :in (matches match-data)
        ;; Record data on the matched sections of the circuit and
        ;; pattern to be stored in a matched-circ
        :minimize (first match) :into min-circ-index
        :maximize (first match) :into max-circ-index
        :minimize (second match) :into min-pat-index
        :maximize (second match) :into max-pat-index
        :collect (first match) :into matched-circ
        :collect (nth (first match) circ) :into matched-circ-gates
        :collect (second match) :into matched-pat
        :collect (nth (second match) pat) :into matched-pat-gates
        ;; Construct a matched-circ for the circuit and pattern to
        ;; store information on the matched sections of the circuit
        ;; and the pattern
        :finally (setf (slot-value match-data 'matched-circ) (make-instance 'matched-circ
                                                                            :circ circ
                                                                            :start-index min-circ-index
                                                                            :initial-start-index min-circ-index
                                                                            :end-index max-circ-index
                                                                            :initial-end-index max-circ-index
                                                                            :cost (gate-cost matched-circ-gates)
                                                                            :matches matched-circ))
                 (setf (slot-value match-data 'matched-pat) (make-instance 'matched-circ
                                                                           :circ pat
                                                                           :start-index min-pat-index
                                                                           :initial-start-index min-pat-index
                                                                           :end-index max-pat-index
                                                                           :initial-end-index max-pat-index
                                                                           :cost (gate-cost matched-pat-gates)
                                                                           :matches matched-pat)))
  ;; Calculate the cost of replacing the matched section of the
  ;; circuit with the inverse of the unmatched section of the pattern
  (loop :for pat-gate :in pat
        :for index :from 0
        :unless (find-if (lambda (match-index)
                           (= match-index index))
                         (matches (matched-pat match-data)))
          :collect (gate-inverse pat-gate) :into inverted-gates        
        :finally (setf (slot-value match-data 'replace-cost)
                       (- (gate-cost inverted-gates)
                          (cost (matched-circ match-data))))))
