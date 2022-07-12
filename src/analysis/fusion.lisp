;;;; fusion.lisp
;;;;
;;;; Author: Robert Smith
;;;;
;;;; (Many ideas inspired by Aaron Vontell, who write the first but
;;;; altogether different implementation of gate fusion.)

(in-package #:cl-quil/frontend)

;;; This file has data structures and algorithms for performing "gate
;;; fusion", a program transformation to reduce the number of gates by
;;; multiplying them together. The main purpose of this transformation
;;; is faster simulation.
;;;
;;; The main object is a GRID-NODE. This is a doubly-linked-list-like
;;; node that has multiple incoming and outgoing pointers. It is
;;; modeled after a gate happening on qubits, though it doesn't
;;; contain any actual information about the linear algebra of the
;;; gate. (That information will most likely be found in its TAG
;;; slot.) Instead, GRID-NODEs just contain the information necessary
;;; to determine both if and how it can get fused with other, usually
;;; neighboring nodes.
;;;
;;; The other object of interest is the PROGRAM-GRID. This is nothing
;;; more than a record of which nodes are "roots" (i.e., the start of
;;; a program), and which nodes are "trailers" (i.e., the last nodes
;;; to exist on a qubit).
;;;
;;; The main way to extend the fusion routines is to teach it *when*
;;; and *how* to fuse.
;;;
;;;     WHEN: Not currently extensible, but controlled by
;;;           FUSEABLE-GATE-P.
;;;
;;;     HOW: Add a method to the FUSE-OBJECTS generic function.
;;;
;;; By default, fusion just builds lists out of the tags of the
;;; GRID-NODE objects.

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
  "Is the GRID-NODE GN a \"root node\", i.e., does it have no predecessors? "
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
  (:documentation "Compute the fusion (in the \"gate fusion\" sense) of objects A and B."))

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
  (:documentation "A representation of a program arranged as a collection of nodes on horizontal qubit wires."))

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
      (a:hash-table-keys nodes))))

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
  (dolist (qubit (grid-node-qubits node))
    (let ((ahead  (succeeding-node-on-qubit node qubit))
          (behind (preceding-node-on-qubit node qubit)))
      (unless (null ahead)
        (setf (preceding-node-on-qubit ahead qubit) node))
      (unless (null behind)
        (setf (succeeding-node-on-qubit behind qubit) node)))))

(defun fuse-node-forward (node)
  "Given a node, and provided it's subsumed, fuse it with the node ahead of it. NODE will remain unchanged, but the nodes it is connected to will change."
  (multiple-value-bind (subsumed? node-ahead)
      (subsumed-ahead-p node)
    (when subsumed?
      (let ((new-node (merge-grid-nodes node node-ahead)))
        (jam-node-in new-node)
        (values new-node node node-ahead)))))

(defun fuse-node-backward (node)
  "Given a node, and provided it's subsumed, fuse it with the node behind it. NODE will remain unchanged, but the nodes it is connected to will change."
  (multiple-value-bind (subsumed? node-behind)
      (subsumed-behind-p node)
    (when subsumed?
      (let ((new-node (merge-grid-nodes node-behind node)))
        (jam-node-in new-node)
        (values new-node node-behind node)))))

(defun attempt-trivial-fusion (fuse-function node seen-table final-table)
  ;; Helper function to FUSE-TRIVIALLY that does the bookkeeping of
  ;; added and removed nodes during a fusion.
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
  "Perform only trivial fusions (i.e., fuse nodes that are trivially subsumed in some direction) on the PROGRAM-GRID PG."
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
           (a:when-let
               ((new-node (attempt-trivial-fusion #'fuse-node-forward node seen final )))
             (setf node new-node))

           ;; Fuse backward.
           (a:when-let
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


;;;;;;;;;;;;;;;;;;;;;;;;; Sorting a Program ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The topological sort procedure in this section below is adapted
;;; from
;;;
;;;     https://github.com/stylewarning/lisp-random/blob/master/tsort.lisp
;;;
;;; which is licensed under the BSD 3-clause
;;; (https://github.com/stylewarning/lisp-random/blob/master/LICENSE).

;;; Copyright (c) 2010-2015, Robert Smith
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;;
;;; 2. Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;;
;;; 3. Neither the name of the copyright holder nor the names of its
;;; contributors may be used to endorse or promote products derived from
;;; this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defun sort-program-grid (pg)
  "Produce a list of nodes of the PROGRAM-GRID PG a topologically sorted order, according to a temporal-<= relation.

The list will actually be a list of lists, where each sublist commutes and can be freely ordered. (Note that even up to permutation of the elements of the sublists, the answer is still not unique.)"
  (let* ((sorted        nil)            ; Final sorted list.
         ;; Note! We could merge NEEDS-SORTING with SORTED-SET if
         ;; NEEDS-SORTING itself was a hash table, which
         ;; PROGRAM-GRID-NODES could naturally return. This would be
         ;; both time and memory efficient.
         (needs-sorting (program-grid-nodes pg))
         (sorted-set    (make-hash-table :test 'eq :size (length needs-sorting)))
         (sinks         (remove-if-not #'root-node-p needs-sorting)))
    (flet ((dependencies-sorted-p (node)
             (every (lambda (pred)
                      (or (null pred)
                          (gethash pred sorted-set)))
                    (grid-node-back node))))
      (loop :until (null sinks)
            :do (progn
                  ;; Remove the sinks.
                  (setf needs-sorting (delete-if #'dependencies-sorted-p needs-sorting))
                  ;; Record the sinks, which commute which one
                  ;; another.
                  (push sinks sorted)
                  (dolist (sink sinks)
                    (setf (gethash sink sorted-set) t))

                  ;; Find the new sinks.
                  (setf sinks (remove-if-not #'dependencies-sorted-p needs-sorting)))
            :finally (return (if (null needs-sorting)
                                 ;; Our DAG is empty. We're good!
                                 (nreverse sorted)
                                 ;; Our DAG isn't empty but has no
                                 ;; sinks. It must be cyclic!
                                 (error "Cannot sort a cyclic graph. ~
                                         The cycles are ~S." needs-sorting)))))))

(define-global-counter **premultiplied-gate-count** incf-premultiplied-gate-count)

;;; NOTE: The QVM needs gate fusion, so we can't use the QVM here. Hence we use MAGICL:@.
(defun premultiply-gates (instructions)
  "Given a list of (gate) applications INSTRUCTIONS, construct a new gate application which is their product.

Instructions are multiplied out in \"Quil\" order, that is, the instruction list (A B C) will be multiplied as if by the Quil program

    A
    B
    C

or equivalently as

    C * B * A

as matrices."
  (let ((u (const #C(1d0 0d0) '(1 1)))
        (qubits (list)))
    (dolist (instr instructions)
      (let ((new-qubits (set-difference (mapcar #'qubit-index (application-arguments instr))
                                        qubits)))
        (unless (endp new-qubits)
          (setf u (kq-gate-on-lines u
                                    (+ (length qubits) (length new-qubits))
                                    (a:iota (length qubits)
                                            :start (1- (length qubits))
                                            :step -1)))
          (setf qubits (append new-qubits qubits)))
        (setf u (magicl:@
                 (kq-gate-on-lines (gate-matrix instr)
                                   (length qubits)
                                   (mapcar (lambda (q)
                                             (- (length qubits) 1 (position (qubit-index q) qubits)))
                                           (application-arguments instr)))
                 u))))
    (make-instance 'gate-application
                   :gate (make-instance 'simple-gate :matrix u)
                   :operator (named-operator (format nil "FUSED-GATE-~D"
                                                     (incf-premultiplied-gate-count)))
                   :arguments (mapcar #'qubit qubits))))



;;;;;;;;;;;;;;;;;;;; Fusion on Real Programs (TM) ;;;;;;;;;;;;;;;;;;;;

(defmethod fuse-objects ((a gate-application) (b gate-application))
  ;; A and B are temporally ordered.
  (premultiply-gates (list a b)))

(defun fuseable-gate-p (x)
  "Is X a gate that's a candidate for fusion?"
  ;; Currently we can only fuse gates that don't have anything
  ;; undetermined about them.
  (static-gate-application-p x))

(defun gate-sequence-to-program-grid (gate-sequence)
  "Convert a list of gates GATE-SEQUENCE into a PROGRAM-GRID object."
  (loop :with pg := (make-instance 'program-grid)
        :for gate-app :in gate-sequence
        :for gn := (apply #'make-grid-node gate-app (mapcar #'qubit-index (arguments gate-app)))
        :do (append-node pg gn)
        :finally (return pg)))

(defun program-grid-to-gate-sequence (pg)
  "Convert a PROGRAM-GRID PG back into a list of gates."
  (mapcan (a:curry #'mapcar #'grid-node-tag) (sort-program-grid pg)))

(defun fuse-gate-sequence (gate-sequence)
  "Given a list of gates GATE-SEQUENCE containing *only* fuseable gates, perform gate fusion, returning a new list of gates that is purportedly mathematically equivalent."
  (assert (every #'fuseable-gate-p gate-sequence))
  (program-grid-to-gate-sequence
   (fuse-trivially
    (gate-sequence-to-program-grid gate-sequence))))

(defun nop-sequence (n)
  "Make a list of NOP's of length N."
  (make-list n :initial-element (make-instance 'no-operation)))

(defun fuse-gates-in-executable-code (code)
  "Given a code vector (i.e., that of PARSED-PROGRAM-EXECUTABLE-CODE), produce a new code vector with gates fused."
  (multiple-value-bind (gate-segments first?)
      ;; XXX: Note that with this approach to identifying fuseable
      ;; gates, certain gate sequences will *NOT* be fused. For
      ;; instance, consider the following program:
      ;;
      ;;     X 0
      ;;     RZ(theta) 1
      ;;     X 0
      ;;
      ;; The RZ will stop the sequence of X gates from getting fused.
      (partition-sequence-into-segments #'fuseable-gate-p code)
    (coerce
     (loop :for segment :in gate-segments
           :for gate-seq-p := first? :then (not gate-seq-p)
           :if gate-seq-p
             :append (let ((fused-sequence (fuse-gate-sequence segment)))
                       (append fused-sequence
                               ;; Pad with NOPs so we are OK after a
                               ;; PATCH-LABELS transform.
                               (nop-sequence (- (length segment)
                                                (length fused-sequence)))))
           :else
             :append segment)
     'vector)))

(defun fuse-gates-in-program (parsed-program)
  (when (transformedp parsed-program 'patch-labels)
    ;; We can't fuse gates when labels have been patched, because
    ;; absolute addresses will change in the process.
    (error "Can't fuse gates when program has had labels patched."))
  (setf (parsed-program-executable-code parsed-program)
        (fuse-gates-in-executable-code
         (parsed-program-executable-code parsed-program)))
  parsed-program)

(define-transform gate-fusion (fuse-gates-in-program)
  "Fuse gates together producing a new program that we stipulate will be more efficient to simulate.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test Harness ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Input data structure is as follows:
;;;
;;;     <proto program> ::= ( <proto node>* )
;;;
;;;     <proto node> ::= ( <tag> <qubit>+ )
;;;
;;;     <tag> ::= any object
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

(defun proto-program (pg)
  "Reconstitute a proto-program from a PROGRAM-GRID.

Note: This is not guaranteed to be a perfect inverse to BUILD-GRID, but it is guaranteed to be equivalent up-to qubit subsystem commutativity."
  (loop :for commuting-nodes :in (sort-program-grid pg)
        :nconc (loop :for node :in commuting-nodes
                     :collect (list* (grid-node-tag node) (grid-node-qubits node)))))
