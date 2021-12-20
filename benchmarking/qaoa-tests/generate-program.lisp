;;;; generate-program.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-benchmarking)

(defmacro remfs (place &rest props)
  "Destructively remove the properties specified in PROPS from the plist stored at PLACE.

NOTE: PROPS must be fully specified at compile time.
NOTE: This copies the list first, and so is safe to apply to &REST lists."
  `(progn
     (setf ,place (copy-list ,place))
     ,@(loop :for prop :in props
             :collect `(remf ,place ,prop))))

(defun qaoa-program-from-graph (program-qubits program-links
                                &key
                                  (depth-count 1)
                                  (final-measure nil)
                                  (commuting-blocks nil))
  "Constructs an Ising-type QAOA program.

   PROGRAM-QUBITS:   a list of integer qubit indices.
   PROGRAM-LINKS:    a list of integer qubit index pairs, each specified as a list.
   DEPTH-COUNT:      number of layers to put in the program.
   FINAL-MEASURE:    append a fleet of MEASUREs to the end of the program.
   COMMUTING-BLOCKS: wrap each product factor of the driver in COMMUTING_BLOCKS. better compiled circuit depth, much harder scheduling problem."
  
  (assert (= 1 depth-count)
          ()
          "I don't know what a QAOA program of depth > 1 looks like.")
  
  (let ((instructions nil))
    (when final-measure
      (error "I don't know how to append MEASUREs to a QAOA program."))
    (dolist (qubit program-qubits)
      (push (build-gate "H" () qubit)
            instructions))
    (when commuting-blocks
      (push (make-instance 'pragma-end-commuting-blocks) instructions))
    (dolist (link program-links)
      (destructuring-bind (qubit0 qubit1) link
        (when commuting-blocks
          (push (make-instance 'pragma-end-block) instructions))
        (push (build-gate "CPHASE" (list (mref "gamma" 0)) qubit0 qubit1)
              instructions)
        (when commuting-blocks
          (push (make-instance 'pragma-block) instructions))))
    (when commuting-blocks
      (push (make-instance 'pragma-commuting-blocks) instructions))
    (dolist (qubit program-qubits)
      (push (build-gate "RX" (list (mref "beta" 0)) qubit)
            instructions))
    (dolist (qubit program-qubits)
      (push (build-gate "H" () qubit)
            instructions))
    (push (make-instance 'pragma-initial-rewiring
                         :rewiring-type ':partial)
          instructions)
    (make-instance 'parsed-program
                   :executable-code (coerce instructions 'vector)
                   :memory-definitions (list (make-memory-descriptor :name "beta"
                                                                     :type quil-real)
                                             (make-memory-descriptor :name "gamma"
                                                                     :type quil-real)))))

(defun generate-natural-qaoa-program (chip-specification qubit-count
                                      &rest rest)
  "Generates a QAOA program which maps perfectly onto the QPU described by CHIP-SPECIFICATION and which occupies QUBIT-COUNT many qubits.  (Should result in a trivial scheduling problem for the NAIVE rewiring method.)  Pass along any keyword arguments to QAOA-PROGRAM-FROM-GRAPH."
  (let* ((live-ccs (chip-spec-live-qubit-cc chip-specification))
         (biggest-cc (a:extremum live-ccs #'> :key #'length))
         (program-qubits nil)
         (program-links nil))
    
    (assert (>= (length biggest-cc) qubit-count)
            ()
            "Too many qubits requested: chip has ~A in its largest connected component, but asked for ~A."
            (length biggest-cc) qubit-count)
    (assert (plusp qubit-count)
            ()
            "Must request at least one qubit.")
    
    (push (a:random-elt biggest-cc)
          program-qubits)
    
    ;; collect qubits to find a connected subgraph of chip-specification of size qubit-count
    (loop :with nearby-qubits := (chip-spec-adj-qubits chip-specification (first program-qubits))
          :for next-qubit := (a:random-elt nearby-qubits)
          :do (push next-qubit program-qubits)
              (setf nearby-qubits
                    (set-difference (union nearby-qubits
                                           (chip-spec-adj-qubits chip-specification next-qubit))
                                    program-qubits))
          :when (= qubit-count (length program-qubits))
            :do (return))
    ;; collect all links between these qubits
    (loop :for link-obj :across (vnth 1 (chip-specification-objects chip-specification))
          :for qubit-list := (first (quil::hardware-object-cxns link-obj))
          :when (subsetp qubit-list program-qubits)
            :do (push qubit-list program-links))
    (apply 'qaoa-program-from-graph
           program-qubits program-links
           rest)))

(defun generate-random-qaoa-program (qubit-count &rest rest)
  "Constructs a QAOA program to be run on a random graph with QUBIT-COUNT many vertices, vertex valency bounded by GRAPH-VALENCY, and with SELF-CONNECTIVITY controlling a propensity to randomly add edges.

NOTE: SELF-CONNECTIVITY must lie in the region [0, 1). For values very close to 1, graph generation will take very long."
  (let ((graph-valency (getf rest ':graph-valency 3))
        (self-connectivity (getf rest ':self-connectivity 0.2))
        (qubit-bound 1)
        (qubit-connections (list)))
    (remfs rest ':graph-valency ':self-connectivity)
    (loop
      (let ((used-valences (make-array qubit-bound :initial-element graph-valency)))
        (dolist (link qubit-connections)
          (destructuring-bind (qubit0 qubit1) link
            (decf (aref used-valences qubit0))
            (decf (aref used-valences qubit1))))
        (cond
          ((< (random 1d0) self-connectivity)
           ;; add an edge to the graph
           (when (< 2 (count-if-not #'zerop used-valences))
             (let* ((qubit0 (loop :for j := (random qubit-bound)
                                  :when (plusp (aref used-valences j))
                                    :return j))
                    (qubit1 (loop :for j := (random qubit-bound)
                                  :when (and (not (eql j qubit0))
                                             (plusp (aref used-valences j)))
                                    :return j)))
               (unless (or (member (list qubit0 qubit1) qubit-connections :test #'equalp)
                           (member (list qubit1 qubit0) qubit-connections :test #'equalp))
                 (push (list qubit0 qubit1) qubit-connections)))))
          ((>= qubit-bound qubit-count)
           (return))                    ; exit the LOOP
          (t
           (push (list qubit-bound
                       (loop :for j := (random qubit-bound)
                             :when (plusp (aref used-valences j))
                               :return j))
                 qubit-connections)
           (incf qubit-bound)))))
    (apply 'qaoa-program-from-graph
           (a:iota qubit-count)
           qubit-connections
           rest)))
