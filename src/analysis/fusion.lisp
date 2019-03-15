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
         :documentation "A vector of pointers to nodes that precede this node. The pointer will be NIL in the event there's no preceding node. The pointers will be ordered according to the QUBITS slot.")
   (forward :initarg :forward
            :reader grid-node-forward
            :documentation "A vector of pointers to nodes that succeed this node. The pointer will be NIL in the event there's no succeeding node. The pointers will be ordered according to the QUBITS slot."))
  (:documentation "A node in a program grid, representing a single gate."))

(defmethod print-object ((o grid-node) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (let ((*print-pretty* nil))
      (format stream "~S" (grid-node-qubits o)))))

(defun root-node-p (gn)
  (every #'null (grid-node-back gn)))

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


(defun trailer-node-on-qubit-p (gn qubit)
  "Does GN look like a trailer node on qubit QUBIT?"
  (null (succeeding-node-on-qubit gn qubit)))

(defun make-grid-node (tag &rest qubits)
  "Make a GRID-NODE with the tag TAG and qubits QUBITS. There should be at least one qubit."
  (assert (not (null qubits)))
  (let ((n (length qubits)))
    (make-instance 'grid-node :tag tag
                              :qubits (sort (copy-list qubits) #'<)
                              :back (make-array n :initial-element nil)
                              :forward (make-array n :initial-element nil))))

(defgeneric fuse-objects (a b)
  (:documentation "Compute the fusion (in the \"gate fusion\" sense) of objects A and B.")
  (:method (a b)
    (append (alexandria:ensure-list a) (alexandria:ensure-list b))))

(defun merge-grid-nodes (a b)
  "Merge two grid nodes A and B to produce a new node. All incoming wires to A and outgoing wires to B will be the wires of the merged node.

This function is non-destructive."
  (let* ((all-qubits (sort (copy-list (union (grid-node-qubits a)
                                             (grid-node-qubits b)))
                           #'<))
         (merged-node (apply #'make-grid-node
                             (fuse-objects (grid-node-tag a) (grid-node-tag b))
                             all-qubits)))
    (loop :for qubit :in all-qubits
          ;; ATTENTION! The order of these OR-clauses matters!
          :for incoming := (or (preceding-node-on-qubit a qubit)
                               (preceding-node-on-qubit b qubit))
          :for outgoing := (or (succeeding-node-on-qubit b qubit)
                               (succeeding-node-on-qubit a qubit))
          :unless (eq incoming a)       ; Account for A -> B
            :do (setf (preceding-node-on-qubit merged-node qubit) incoming)
          :unless (eq outgoing b)       ; Account for A <- B
            :do (setf (succeeding-node-on-qubit merged-node qubit) outgoing)
          :finally (return merged-node))))

(defclass program-grid ()
  ((roots :initarg :roots
            :accessor roots
            :documentation "A list of root nodes (nodes that have no predecessors).")
   (trailers :initarg :trailers
             :accessor trailers
             :documentation "A vector of trailing nodes to the grid, where the index into the vector is the qubit/wire number."))
  (:default-initargs :roots nil
                     :trailers (make-array 0 :adjustable t :initial-element nil))
  (:documentation "A representation of a program arranged as a collection of nodes on horizontal wires."))

(defun program-grid-num-qubits (pg)
  "How many qubits does PG hold?"
  (length (trailers pg)))

(defun ensure-program-grid-has-qubits (pg n)
  "Grow the program grid PG to have N qubits."
  (when (> n (program-grid-num-qubits pg))
    (adjust-array (trailers pg) n :initial-element nil))
  pg)

(defun append-node (pg gn)
  "Add a node GN to the program PG."
  (ensure-program-grid-has-qubits pg (1+ (loop :for q :in (grid-node-qubits gn) :maximize q)))

  (loop :for qubit :in (grid-node-qubits gn)
        :for trailer := (aref (trailers pg) qubit)
        :do
           ;; Update the PROGRAM-GRID.
           (cond
              ((null trailer)
               (setf (aref (trailers pg) qubit) gn))
              (t
               ;; Make sure we aren't updating something that isn't
               ;; actually a trailer. This would be a bug somewhere
               ;; else!
               (assert (trailer-node-on-qubit-p trailer qubit)
                       ()
                       "Inconsistency in program grid. Found a non-trailer ~
                        node in the trailer vector.")
               ;; Update the GRID-NODE.
               (setf (preceding-node-on-qubit gn qubit) trailer)
               (setf (succeeding-node-on-qubit trailer qubit) gn)
               (setf (aref (trailers pg) qubit) gn))))
  (when (root-node-p gn)
    (push gn (roots pg)))
  pg)

(defun program-grid-nodes (pg)
  "Return a list of nodes in the program grid PG."
  (let ((nodes (make-hash-table :test 'eq)))
    (labels ((chase-node (n)
               (when (and (not (null n))
                          (null (gethash n nodes)))
                 (setf (gethash n nodes) t)
                 (map nil #'chase-node (grid-node-forward n))
                 ;; We go backward too just in case there are some
                 ;; dangling nodes.
                 (map nil #'chase-node (grid-node-back n))
                 nil)))
      (map nil #'chase-node (roots pg))
      ;; Same here... we go backward in case there are some dangling
      ;; nodes.
      (map nil #'chase-node (trailers pg))
      ;; Return the nodes.
      (alexandria:hash-table-keys nodes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fusion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subsumed-ahead-p (node)
  "Do all of NODE's outgoing wires lead into the same node?"
  (let* ((forward (grid-node-forward node))
         (x (aref forward 0)))
    (if (null x)
        (values nil nil)
        (loop :for i :from 1 :below (length forward)
              :unless (eq x (aref forward i))
                :do (return (values nil nil))
              :finally (return (values t x))))))

(defun subsumed-behind-p (node)
  "Do all of NODE's incoming wires lead into the same node?"
  (let* ((back (grid-node-back node))
         (x (aref back 0)))
    (if (null x)
        (values nil nil)
        (loop :for i :from 1 :below (length back)
              :unless (eq x (aref back i))
                :do (return (values nil nil))
              :finally (return (values t x))))))

(defun jam-node-in (node)
  "Whatever NODE is connected to, rewire the connected nodes to connect back to NODE. For instance, suppose we have


    A <--> X <--> B

and

    A <-- Y --> B,

then (JAM-NODE-IN Y) will result in

    A <-- X --> B

and

    A <--> Y <--> B,

forcing \"Y\" to be the middle link."
  (loop :for qubit :in (grid-node-qubits node)
        :for ahead := (succeeding-node-on-qubit node qubit)
        :for behind := (preceding-node-on-qubit node qubit)
        :unless (null ahead)
          :do (setf (preceding-node-on-qubit ahead qubit) node)
        :unless (null behind)
          :do (setf (succeeding-node-on-qubit behind qubit) node))
  nil)

(defun fuse-node-forward (node)
  (multiple-value-bind (subsumed? node-ahead)
      (subsumed-ahead-p node)
    (when subsumed?
      (let ((new-node (merge-grid-nodes node node-ahead)))
        (jam-node-in new-node)
        (values new-node node node-ahead)))))

(defun fuse-node-backward (node)
  (multiple-value-bind (subsumed? node-behind)
      (subsumed-behind-p node)
    (when subsumed?
      (let ((new-node (merge-grid-nodes node-behind node)))
        (jam-node-in new-node)
        (values new-node node-behind node)))))

(defun attempt-trivial-fusion (fuse-function node seen-table final-table)
  ;; Helper function to FUSE-TRIVIALLY.
  (multiple-value-bind (new-node old1 old2)
      (funcall fuse-function node)
    (when new-node
      ;; Back out our added node.
      (remhash node final-table)
      ;; We don't want to see these nodes and they won't be a
      ;; part of the final program, since they're now fused.
      (setf (gethash old1 seen-table) t
            (gethash old2 seen-table) t)
      (remhash old1 final-table)
      (remhash old2 final-table)
      new-node)))

(defun fuse-trivially (pg)
  "Perform only trivial fusions (i.e., fuse nodes that are trivially subsumed in some direction) on the program grod PG."
  (let* ((nodes (program-grid-nodes pg))
         (seen (make-hash-table :test 'eq
                                :size (length nodes)))
         (final (make-hash-table :test 'eq
                                 :size (length nodes))))
    (dolist (old-node nodes)
      (prog ((node old-node))
       :RESTART
         (unless (gethash node seen)
           ;; We by default assume the node will be in the final
           ;; list. It will get deleted if it's not.
           (setf (gethash node final) t)
           ;;Mark that we've seen it and don't want to see it again.
           (setf (gethash node seen) t)
           ;; Fuse forward.
           (alexandria:when-let
               ((new-node (attempt-trivial-fusion #'fuse-node-forward node seen final )))
             (setf node new-node))

           ;; Fuse backward.
           (alexandria:when-let
               ((new-node (attempt-trivial-fusion #'fuse-node-backward node seen final )))
             (setf node new-node))

           ;; Check if this node is the same as the original. If it
           ;; is, fusion is done. If it's not, we need to go back and
           ;; re-process it. (In *that* process, the node will be
           ;; marked as being in the final program.)
           (unless (eq node old-node)
             (go :RESTART)))))
    ;; Now we have all of our final nodes. We need to find the roots
    ;; and trailers.
    (loop :with roots := nil
          :with trailers := (make-array (program-grid-num-qubits pg)
                                        :adjustable t
                                        :initial-element nil)
          :for node :being :the :hash-keys :of final
          :do
             (when (root-node-p node)
               (push node roots))
             (dolist (qubit (grid-node-qubits node))
               (when (trailer-node-on-qubit-p node qubit)
                 (assert (null (aref trailers qubit))
                         ()
                         "Inconsistency in program grid. Found a duplicate ~
                          trailer node on qubit ~D"
                         qubit)
                 (setf (aref trailers qubit) node)))
          :finally (return (make-instance 'program-grid :roots roots
                                                        :trailers trailers)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test Harness ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
