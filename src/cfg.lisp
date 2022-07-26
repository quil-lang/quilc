;;;; cfg.lisp
;;;;
;;;; Author: Robert Smith
;;;;         Aaron Vontell

(in-package #:cl-quil)

(deftype block-name ()
  "The type of object that names blocks."
  'symbol)

(defclass cfg (transformable)
  ((entry-point :initarg :entry-point
                :accessor entry-point
                :documentation "The program entry point (a BASIC-BLOCK).")
   (blocks :initarg :blocks
           :accessor cfg-blocks
           :documentation "A list of BASIC-BLOCK instances contained in this graph.")
   (label-table :initarg :label-table
                :accessor label-table
                :documentation "A map between label names (strings) and blocks."))
  (:documentation "A representation of a control flow graph for Quil code.")
  (:default-initargs :blocks nil
                     :label-table (make-hash-table :test 'equal)))

(defmethod print-object ((obj cfg) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "of ~D block~:P" (length (cfg-blocks obj)))))

(defun associate-label-with-block (cfg label blk)
  "Associate the label LABEL with the block BLK in the CFG."
  (let ((label-name (label-name label))
        (blk-name (basic-block-name blk))
        (label-table (label-table cfg)))
    ;; Sanity check.
    (multiple-value-bind (existing-blk-name exists?)
        (gethash label-name label-table)
      (when (and exists? (not (eq existing-blk-name blk-name)))
        (warn "The label ~S is already associated with the block ~S. I am going ~
               to re-associate it with ~S."
              label-name
              existing-blk-name
              blk-name)))
    ;; Associate LABEL-NAME -> BLK-NAME.
    (setf (gethash label-name label-table) blk)
    nil))

(defun find-block (cfg blk-name)
  "Find the block named BLK-NAME in the CFG. Return NIL if not found."
  (find blk-name (cfg-blocks cfg) :test #'eq :key #'basic-block-name))

(adt:defdata outgoing-edge
  "The various kinds of outgoing edges from a block."
  ;; An "outgoing" edge that points nowhere. This is equivalent to a HALT and
  ;; signals the end of an execution path.
  terminating-edge
  ;; An unconditional edge to target block (a BASIC-BLOCK).
  (unconditional-edge basic-block)
  ;; A conditional edge with a bit condition represented by a
  ;; classical address, a true target (bit = 1) block, and
  ;; a false target (bit = 0) block (both BASIC-BLOCKs).
  (conditional-edge memory-ref basic-block basic-block))

(defclass basic-block ()
  ((name :initarg :name
         :reader basic-block-name
         :documentation "The name (a symbol, generally uninterned) of the basic block.")
   (incoming :initarg :incoming
             :accessor incoming
             :documentation "A list of BASIC-BLOCKs that have an edge pointing to this block.")
   (outgoing :initarg :outgoing
             :accessor outgoing
             :documentation "The OUTGOING-EDGE this block points to.")
   (labeled :initarg :labeled
            :accessor labeled
            :initform nil
            :documentation "A LABEL that was originally used to reach this block of code, or NIL if not applicable.")
   (code :initarg :code
         :accessor basic-block-code
         :documentation "A vector of instructions that this block contains.")
   (in-rewiring :initarg :in-rewiring
                :initform nil
                :accessor basic-block-in-rewiring
                :documentation "A REWIRING describing the assignment of qubits at block entry.")
   (out-rewiring :initarg :out-rewiring
                 :initform nil
                 :accessor basic-block-out-rewiring
                 :documentation "A REWIRING describing the assignment of qubits at block exit."))
  (:documentation "A basic block in a control flow graph.")
  (:default-initargs :name (gensym "BLK-")
                     :incoming nil
                     :code (make-array 4 :initial-element nil
                                         :adjustable t
                                         :fill-pointer 0)))

(defclass preserved-block (basic-block) ()
  (:documentation "A basic-block that is marked as untouchable by the optimizing compiler."))

(defclass reset-block (basic-block) ()
  (:documentation "A basic-block that begins with a RESET instruction, so that qubit rewiring becomes free again."))

(defmethod print-object ((obj basic-block) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    ;; name info
    (format stream "~A" (basic-block-name obj))
    (when (labeled obj)
      (format stream "(~S)" (label-name (labeled obj))))
    (write-char #\Space stream)
    ;; instructions
    (format stream "len:~D " (length (basic-block-code obj)))
    ;; incoming and outgoing
    (format stream "in:~D out:" (length (incoming obj)))
    (if (not (slot-boundp obj 'outgoing))
        (write-string "unbound" stream)
        (adt:match outgoing-edge (outgoing obj)
          (terminating-edge (write-string "term" stream))
          ((unconditional-edge _) (write-string "uncond" stream))
          ((conditional-edge _ _ _) (write-string "cond" stream))))))

(defun make-block-from-label (label)
  "Make a new block from the label LABEL."
  (check-type label label)
  (make-instance 'basic-block :labeled label
                              :name (gensym (concatenate 'string (label-name label) "-"))))

(defun get-label-from-block (blk)
  "Returns the label of block BLK if it exists, or a label with its name."
  (or (labeled blk)
      (label (princ-to-string (basic-block-name blk)))))

(defun find-or-make-block-from-label (cfg label)
  "Find a block associated with the label LABEL in the CFG, or make one."
  (multiple-value-bind (blk found?)
      (gethash (label-name label) (label-table cfg))
    (if found?
        blk
        (let ((new-blk (make-block-from-label label)))
          (associate-label-with-block cfg label new-blk)
          new-blk))))

(defun link-blocks (from edge)
  "Link the CFG block FROM to the blocks in EDGE."
  ;; Careful, this doesn't check with overwriting blocks and could
  ;; lead to an inconsistent state.
  (check-type from basic-block)
  (check-type edge outgoing-edge)
  (adt:match outgoing-edge edge
    ((conditional-edge _ true-target false-target)
     (pushnew from (incoming true-target))
     (pushnew from (incoming false-target)))
    ((unconditional-edge target)
     (pushnew from (incoming target)))
    (terminating-edge nil))
  (setf (outgoing from) edge)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;; CFG Construction ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; PROCESS-INSTRUCTION is the principal (generic) function for
;;; constructing the CFG.

(defgeneric process-instruction (cfg blk instr)
  (:documentation "Process the Quil instruction INSTR for the control flow graph CFG. BLK is the current block being processed.

Return the following values:

    1. The block to continue operating on.
    2. A block that has finished and should be added to the CFG, or NIL.
    3. Whether the finished block should be succeeded by the next block in the sequence. If it shouldn't, this value is NIL. If it should, this value should be a function which takes the successor block as input and produces an OUTGOING-EDGE as output.
"))

(defmethod process-instruction (cfg blk (instr instruction))
  (declare (ignore cfg))
  (assert (not (null blk)) (blk))
  (vector-push-extend instr (basic-block-code blk))
  (values blk nil nil))

(defmethod process-instruction (cfg blk (instr pragma-preserve-block))
  (if (and
       ;; RESET-BLOCK is treated somewhat specially. If a RESET-BLOCK
       ;; immediately precedes a PRESERVED-BLOCK then it will contain
       ;; no code. So we need to prevent it being dropped from the
       ;; graph in that case.
       (not (typep blk 'reset-block))
       ;; we don't need to make a new empty block. just change the current one
       ;; to be preserved
       (= 0 (length (basic-block-code blk))))
      (progn
        (change-class blk 'preserved-block)
        (values blk nil nil))
      ;; the old block is non-empty, so make a new preserved block
      (let ((label-blk (find-or-make-block-from-label cfg (label (princ-to-string (gensym "PRAGMA-"))))))
        (change-class label-blk 'preserved-block)
        (link-blocks blk (unconditional-edge label-blk))
        (values label-blk blk nil))))

(defmethod process-instruction (cfg blk (instr pragma-end-preserve-block))
  (let ((label-blk (find-or-make-block-from-label cfg (label (princ-to-string (gensym "PRAGMA-"))))))
    (link-blocks blk (unconditional-edge label-blk))
    (values label-blk blk nil)))

(defmethod process-instruction (cfg blk (instr halt))
  (assert (not (null blk)) (blk))
  (link-blocks blk terminating-edge)
  (values nil blk nil))

(defmethod process-instruction (cfg blk (instr reset))
  (assert (not (null blk)) (blk))
  ;; is the current block non-empty?
  (unless (zerop (length (basic-block-code blk)))
    ;; build a new block with an unconditional edge coming from the old one
    (let ((new-blk (find-or-make-block-from-label cfg (label (princ-to-string (gensym "RESET-"))))))
      (link-blocks blk
                   (unconditional-edge new-blk))
      (setf blk new-blk)))
  ;; set the block type to a reset-block
  (change-class blk 'reset-block)
  (values blk nil nil))

(defmethod process-instruction (cfg blk (instr include))
  (declare (ignore cfg blk))
  (error "Can't make a CFG from a program without INCLUDEs having been processed first."))

(defmethod process-instruction (cfg blk (instr unconditional-jump))
  ;; Add a link from BLK to the label of the jump.
  (let ((label-block (find-or-make-block-from-label cfg (jump-label instr))))
    (link-blocks blk
                 (unconditional-edge label-block)))
  (values nil blk nil))

(defmethod process-instruction (cfg blk (instr jump-when))
  (let* ((label-block (find-or-make-block-from-label cfg (jump-label instr)))
         (link-successor (lambda (successor)
                           (conditional-edge (conditional-jump-address instr)
                                             label-block
                                             successor))))
    ;; Remember to link this BLK to its successor, in the event that the
    ;; condition falls through.
    (values nil blk link-successor)))

(defmethod process-instruction (cfg blk (instr jump-unless))
  (let* ((label-block (find-or-make-block-from-label cfg (jump-label instr)))
         (link-successor (lambda (successor)
                           (conditional-edge (conditional-jump-address instr)
                                             successor
                                             label-block))))
    (values nil blk link-successor)))

(defmethod process-instruction (cfg blk (instr jump-target))
  ;; Link the outgoing of the current one to the labeled block.
  (let ((label-blk (find-or-make-block-from-label cfg (jump-target-label instr))))
    (link-blocks blk (unconditional-edge label-blk))
    (values label-blk blk nil)))

(defun add-block-to-cfg (blk cfg)
  "Add the block BLK to the control flow graph CFG."
  (pushnew blk (cfg-blocks cfg) :test #'eq))

(defun eliminate-dead-code (cfg)
  "Removes dead code from a CFG. Dead code is considered to be any block of code that cannot be reached from the entry-point of the CFG. The CFG is modified by removing blocks from the list of cfg-blocks and removing any incoming edges within blocks and labels from the label-table which corresponding to blocks that were removed."
  (labels ((remove-visited (blk-list visited)
             ;; Returns a modified BLK-LIST, by removing any blocks in BLK-LIST that are also in VISITED
             (remove-if (lambda (blk)
                          (find blk visited))
                        blk-list)))

    ;; Starting with the entry point, maintain a list of visited blocks and blocks to expand
    (let ((visited '())
          (blocks (cfg-blocks cfg))
          (to-expand (list (entry-point cfg))))
      ;; Pop off a block, add to visited, and add new children to the to-expand list (unless there are no children))
      (loop :until (null to-expand) :do
        (let ((blk (pop to-expand)))
          (pushnew blk visited)
          (setq to-expand (append (remove-visited (children blk) visited) to-expand))))

      ;; Remove blocks from the cfg, including edges and labels
      (loop :for blk :in blocks :do
        (if (not (find blk visited))
            (remove-block blk cfg :update-edges nil)
            ;; Remove incoming blocks from the incoming list
            (setf (incoming blk)
                  (remove-if (lambda (incoming-block)
                               (not (find incoming-block visited)))
                             (incoming blk))))))))


(defun remove-block (blk cfg &key (update-edges t))
  "Removes the block BLK from CFG, and updates the incoming edges of children blocks if UPDATE-EDGES (default T)."
  ;; Remove labels from the label table, if this block has a label
  (when (labeled blk)
    (remhash (label-name (labeled blk)) (label-table cfg)))

  ;; Remove the block from the CFG
  (setf (cfg-blocks cfg)
        (remove blk (cfg-blocks cfg)))

  ;; Update the incoming edges of child nodes
  (when update-edges
    (dolist (child (children blk))
      (setf (incoming child) (remove blk (incoming child))))))


(defun simplify-cfg (cfg)
  "Modifies the CFG into a functionally equivalent, simplified version through a series of modifications. Current steps include self loop removal and path contraction."

  ;; Path contraction and loop removal.

  ;; For each edge, if the source vertex has only one outgoing edge, and the sink vertex has only
  ;; one incoming edge, combine the vertices into one. Additionally, if there is an empty block that
  ;; joins a block to itself (i.e. a self loop), remove it.
  (contract-paths cfg))

(defun redirect-edge (old-block new-block edge)
  "Returns a new edge which is identical to EDGE, but any pointers to OLD-BLOCK are replaced by pointers to NEW-BLOCK (both of which are BASIC-BLOCKs)."
  (adt:match outgoing-edge edge
    ((conditional-edge address true-target false-target)
     (conditional-edge address
                       (if (eq true-target old-block)
                           new-block
                           true-target)
                       (if (eq false-target old-block)
                           new-block
                           false-target)))
    ((unconditional-edge target)
     (unconditional-edge (if (eq target old-block)
                             new-block
                             target)))
    (terminating-edge
     terminating-edge)))

(defun children (blk)
  "Returns a list of unique blocks that this block BLK points to."
  (let ((children '()))
    (adt:match outgoing-edge (outgoing blk)
      ((conditional-edge _ true-target false-target)
       (push true-target children)
       (push false-target children))

      ((unconditional-edge target)
       (push target children))

      (terminating-edge nil))
    (assert (not (some #'null children)))
    (remove-duplicates children)))

(defun children-without-self (blk)
  "Returns a unique list of blocks that are referenced by the outgoing edge of BLK in CFG, without BLK (in the case where BLK has a self loop)."
  (remove blk (children blk)))

(defun parents-without-self (blk)
  "Returns a list of unique blocks that having edges which point to BLK."
  (remove blk (incoming blk)))

(defun num-edges-in (blk)
  "Returns the number of blocks that point to BLK through some edge, excluding itself (in the case of a self loop)."
  (length (parents-without-self blk)))

(defun merge-sequentially (blk1 blk2)
  "Returns a new BASIC-BLOCK representing a merged BLK1 and BLK2, such that its incoming list is a copy of BLK1's incoming list, its outgoing edge is a copy of BLK2's outgoing edge, and its code is the concatentation of BLK1's code and BLK2's code. Any reference to BLK1 or BLK2 within this new block's edge or incoming list is replaced with a reference to itself."
  (assert (and (or (and (typep blk1 'preserved-block)
                        (typep blk2 'preserved-block))
                   (and (not (typep blk1 'preserved-block))
                        (not (typep blk2 'preserved-block))))
               (or (typep blk2 'reset-block)
                   (not (basic-block-out-rewiring blk1))
                   (not (basic-block-in-rewiring blk2))
                   (equalp (basic-block-out-rewiring blk1)
                           (basic-block-in-rewiring blk2)))))
  (let ((new-blk (make-instance (type-of blk1)
                                :code (concatenate 'vector
                                                   (basic-block-code blk1)
                                                   (when (typep blk2 'reset-block)
                                                     (list (make-instance 'reset)))
                                                   (basic-block-code blk2))
                                :labeled (labeled blk1))))
    ;; Update incoming of the new block
    (setf (incoming new-blk) (substitute new-blk blk2 (substitute new-blk blk1 (incoming blk1))))
    ;; If an outgoing edge includes a reference to itself, however, we need to update its name
    (setf (outgoing new-blk) (redirect-edge blk1 new-blk (redirect-edge blk2 new-blk (outgoing blk2))))
    ;; set up the rewirings of the new block
    (setf (basic-block-out-rewiring new-blk) (basic-block-out-rewiring blk2)
          (basic-block-in-rewiring new-blk) (basic-block-in-rewiring blk1))
    new-blk))

(defun empty-block-p (blk)
  "Return T if BLK is an empty code block (has an empty code list)."
  (zerop (length (basic-block-code blk))))


(defun contract-paths (cfg &optional (blk-set (cfg-blocks cfg)))
  "Within the subgraph spanned by blocks in BLK-SET in CFG, contract paths by removing blocks that simply act as a connection from one block to another (i.e. in A -> B -> C, if removing B and placing the code of B after the code of A to form (AB)-> C would result in a CFG with the same functionality, then that conversion will occur. If there is a self loop (i.e. A -> B -> A where B has no code or other edges, B and its edges will be eliminated, with A now pointing to itself."
  (let ((dirty (remove-duplicates blk-set)))
    (flet ((add-dirty-block (blk)
             (unless (null blk)
               (pushnew blk dirty))))
      (loop :until (null dirty) :do
        (let ((blk (pop dirty)))
          (when (= 1 (num-edges-in blk))
            (let ((parent (first (parents-without-self blk))))
              ;; Check each merging condition until one has been reached, which then applies a specific operation to merge the blocks.
              (cond
                ;; Condition 1) Extended self-loops can be merged into smaller self loops if there is only 1 edge incoming (excluding self loops, 1 child, if the child is the same block as the parent, and if there is no code within the block
                ((and
                  (= 1 (length (children blk)))
                  (eq parent (first (children blk)))
                  (eq (type-of parent) (type-of blk))
                  (empty-block-p blk)
                  (or (and (typep parent 'preserved-block)
                           (typep blk 'preserved-block))
                      (and (not (typep parent 'preserved-block))
                           (not (typep blk 'preserved-block))))
                  (or (typep blk 'reset-block)
                      (not (basic-block-out-rewiring parent))
                      (not (basic-block-in-rewiring blk))
                      (equalp (basic-block-out-rewiring parent)
                              (basic-block-in-rewiring blk)))
                  (or (typep parent 'reset-block)
                      (not (basic-block-in-rewiring parent))
                      (not (basic-block-out-rewiring blk))
                      (equalp (basic-block-in-rewiring parent)
                              (basic-block-out-rewiring blk))))

                 ;; update the rewiring data
                 (setf (basic-block-in-rewiring parent) (or (basic-block-in-rewiring parent)
                                                            (basic-block-out-rewiring parent))
                       (basic-block-out-rewiring parent) (basic-block-in-rewiring parent))
                 ;; Update the outgoing edge of the parent to point to itself, rather than this block
                 (setf (outgoing parent) (redirect-edge blk
                                                        parent
                                                        (outgoing parent)))
                 (pushnew parent (incoming parent))
                 (setf (incoming parent) (remove blk (incoming parent)))
                 ;; Remove the old block from the cfg
                 (remove-block blk cfg :update-edges nil)
                 ;; Add the parent to the node list
                 (add-dirty-block parent))

                ;; Condition 2) Paths can be contracted when a block has one parent, that parent has one outgoing edge, and neither are the exit or entry block. There is
                ;; also the extra condition that and edge cannot be contracted if doing so would cause previously isolated code to be possibly run within a loop.
                ((and (or (empty-block-p blk)
                          (not (> (length (children parent)) 1)))
                      (or (empty-block-p parent)
                          (not (find blk (incoming blk))))
                      (= 1 (length (children parent)))
                      (or (and (typep parent 'preserved-block)
                               (typep blk 'preserved-block))
                          (and (not (typep parent 'preserved-block))
                               (not (typep blk 'preserved-block))))
                      (or (typep blk 'reset-block)
                          (not (basic-block-out-rewiring parent))
                          (not (basic-block-in-rewiring blk))
                          (equalp (basic-block-out-rewiring parent)
                                  (basic-block-in-rewiring blk))))

                 ;; The conditions are met to sequentially merge these blocks
                 (let ((new-blk (merge-sequentially parent blk)))
                   ;; After getting a merged block, update the CFG
                   ;; Update the references of affected blocks within the cfg
                   (dolist (child (children new-blk))
                     (setf (incoming child)
                           (substitute new-blk blk (incoming child))))
                   ;; Update the outgoing edges of the parents
                   (dolist (par (parents-without-self new-blk))
                     (setf (outgoing par) (redirect-edge parent new-blk (outgoing par))))
                   ;; if the parent used to be the entry-point, then we are the new entry-point
                   (when (eq parent (entry-point cfg))
                     (setf (entry-point cfg) new-blk))
                   ;; Remove the BLK and PARENT from the CFG and dirty list
                   (remove-block blk cfg :update-edges NIL)
                   (remove-block parent cfg :update-edges NIL)
                   (setq dirty (remove parent dirty))
                   ;; Add the new block and reassign any old labels
                   (add-block-to-cfg new-blk cfg)
                   (when (labeled parent)
                     (associate-label-with-block cfg (labeled parent) new-blk))
                   ;; Add anything that was touching the node, including the node, to the dirty list
                   (add-dirty-block new-blk)
                   (mapc #'add-dirty-block (append (incoming new-blk) (children new-blk)))))))))))))


(defun reconstitute-basic-block (blk cfg)
  "Constructs a vector of instructions that represents BLK. Takes in the block, the CFG it belongs to, and an END-LABEL to use to reach the end of the program."
  (let* ((code-section '())
         (end-jump (make-instance 'halt))
         (jump-used nil))
    (labels ((add-to-section (instr)
               (setq code-section (concatenate 'vector
                                               code-section
                                               (if (typep instr 'sequence) instr (vector instr))))))

      ;; If this block has a label, use it. Otherwise, use its name as a label. Don't worry, extraneous
      ;; blocks and labels cause in this way will be excised by later optimizations (?) Also note that
      ;; there should not, however, be a label if this is the entry block
      (unless (eq blk (entry-point cfg))
        (add-to-section (make-instance 'jump-target :label (get-label-from-block blk))))

      ;; if this block is of type RESET-BLOCK, add back the implicit RESET instruction
      (when (typep blk 'reset-block)
        (add-to-section (make-instance 'reset)))

      ;; Attach the basic code of this block
      (add-to-section (basic-block-code blk))

      ;; Attach JUMPs which indicate edges to children blocks from blk
      (adt:match outgoing-edge (outgoing blk)
        ;; If there is a conditional edge, add the true block as a JUMP-WHEN, and the false block as a JUMP.
        ;; Also note that this canonicalizes conditional JUMPs in that it represents all conditional JUMPs
        ;; as a JUMP-WHEN followed by a JUMP, even if the original instruction was a JUMP-UNLESS
        ((conditional-edge address true-target false-target)
         (add-to-section (make-instance 'jump-when
                                        :address address
                                        :label (get-label-from-block true-target)))
         (add-to-section (make-instance 'unconditional-jump
                                        :label (get-label-from-block false-target)))
         (setq jump-used t))

        ((unconditional-edge target)
         (add-to-section (make-instance 'unconditional-jump
                                        :label (get-label-from-block target)))
         (setq jump-used t))

        (terminating-edge nil))

      ;; If a jump was never used, we better add one to the end
      (unless jump-used
        (add-to-section end-jump)))
    (cond
      ((and (basic-block-in-rewiring blk)
            (basic-block-out-rewiring blk)
            (= 1 (length code-section)))
       (setf (comment (aref code-section 0))
             (make-rewiring-comment :entering (basic-block-in-rewiring blk)
                                    :exiting (basic-block-out-rewiring blk))))
      (t
       (when (basic-block-in-rewiring blk)
         (setf (comment (aref code-section 0))
               (make-rewiring-comment :entering (basic-block-in-rewiring blk))))
       (when (basic-block-out-rewiring blk)
         (setf (comment (aref code-section (1- (length code-section))))
               (make-rewiring-comment :exiting (basic-block-out-rewiring blk))))))
    code-section))

(defun reconstitute-program (cfg)
  "Reconstructs a Quil program given its control flow graph CFG."
  (let* ((blocks (cfg-blocks cfg))
         (code-list '()))

    ;; Loop though each basic-block
    (dolist (block blocks)
      (let ((reconstituted-code (reconstitute-basic-block block cfg)))
        (setq code-list
              (if (eq block (entry-point cfg))
                  (concatenate 'vector reconstituted-code code-list)
                  (concatenate 'vector code-list reconstituted-code)))))

    (make-instance 'parsed-program
                   :gate-definitions '()
                   :circuit-definitions '()
                   :executable-code (coerce code-list 'simple-vector))))

(defun program-cfg (pp &key (dce nil) (simplify nil))
  "Compute the control flow graph for the parsed program PP. Dead code in the CFG is eliminated if DCE is true (default: NIL). Measures are appended before HALT instructions if ADD-MEASURES is set to a CHIP-SPECIFICATION."
  (let* ((code (parsed-program-executable-code pp))
         (entry (make-instance 'basic-block :name (gensym "ENTRY-BLK-") :outgoing terminating-edge))
         (cfg (make-instance 'cfg :entry-point entry
                                  :blocks (list entry))))
    (flet ((next (current-block)
             (or current-block (make-instance 'basic-block))))
      (loop :with finished-block := nil
            :with link-successor? := nil
            :for instr :across code
            :for current-block := (entry-point cfg) :then (next current-block)
            :do (progn
                  ;; Process the instruction.
                  (multiple-value-setq (current-block finished-block link-successor?)
                    (process-instruction cfg current-block instr))

                  (when link-successor?
                    ;; Start a new block, and link it to its predecessor.
                    (assert (null current-block))
                    (assert (not (null finished-block)))
                    (setf current-block (next current-block))
                    (let ((edge (funcall link-successor? current-block)))
                      (link-blocks finished-block edge)))

                  ;; Add the finished block if necessary.
                  (unless (null finished-block)
                    (add-block-to-cfg finished-block cfg)))
            :finally (progn
                       ;; If we have a CURRENT-BLOCK that evidently
                       ;; wasn't finished, we ought to add it to the
                       ;; CFG, remembering to link it to the exit point.
                       (unless (null current-block)
                         (link-blocks current-block terminating-edge)
                         (add-block-to-cfg current-block cfg)))))

    ;; Remove dead code when desired.
    (when dce
      (eliminate-dead-code cfg))

    ;; Simplify the graph when enabled.
    (when simplify
      (simplify-cfg cfg))

    ;; Return the CFG.
    cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Visualization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-code-list (code &optional (out *standard-output*))
  "Prints a representation of CODE, which is valid sequence of Quil instructions, to an optional stream OUT, with the *standard-output* stream as the default."
  (map nil (lambda (isn) (print-instruction isn out) (terpri out)) code))

(defun underscorize (name)
  (substitute #\_ #\- name))

(defun block-text-label (blk)
  (with-output-to-string (s)
    (format s "Block: ~A" (basic-block-name blk))
    (format s "\\n\\n")
    (loop :for x :across (basic-block-code blk) :do
      (print-instruction x s)
      (format s "\\l"))
    (when (slot-boundp blk 'outgoing)
      (adt:match outgoing-edge (outgoing blk)
        ((conditional-edge instr _ _)
         (format s "\\nConditioned on ~/cl-quil:instruction-fmt/" instr)
         (format s "\\l"))
        (_ nil)))))


(defun generate-graphviz (cfg &optional (stream *standard-output*))
  (check-type cfg cfg)
  ;; Header
  (format stream "digraph {~%")

  ;; Declaration of all nodes
  (dolist (blk (cfg-blocks cfg))
    (let ((name (symbol-name (basic-block-name blk))))
      (format stream "~A[shape=\"box\",fontname=\"courier\",label=\"~A\"];~%"
              (underscorize name)
              (block-text-label blk))))

  ;; All links
  (dolist (from-blk (cfg-blocks cfg))
    (when (slot-boundp from-blk 'outgoing)
      (adt:match outgoing-edge (outgoing from-blk)
        (terminating-edge nil)
        ((unconditional-edge target)
         (format stream "~A -> ~A;~%"
                 (underscorize (symbol-name (basic-block-name from-blk)))
                 (underscorize (symbol-name (basic-block-name target)))))
        ((conditional-edge _ true-target false-target)
         (format stream "~A -> ~A [label = \"1\", fontname = \"times-italic\"];~%"
                 (underscorize (symbol-name (basic-block-name from-blk)))
                 (underscorize (symbol-name (basic-block-name true-target))))
         (format stream "~A -> ~A [label = \"0\", fontname = \"times-italic\"];~%"
                 (underscorize (symbol-name (basic-block-name from-blk)))
                 (underscorize (symbol-name (basic-block-name false-target))))))))

  ;; Footer
  (format stream "}~%")
  nil)

(defun output-cfg (quil out-file &key parallel dce simplify)
  (let ((pp (parse-quil (if (pathnamep quil)
                            (a:read-file-into-string quil)
                            quil)
                        :originating-file (and (pathnamep quil) quil))))
    (output-cfg-from-program pp out-file :parallel parallel :dce dce :simplify simplify)))

(defun output-cfg-from-program (pp out-file &key parallel dce simplify)
  (setf pp (transform 'expand-circuits pp))
  (let ((cfg (program-cfg pp :dce dce :simplify simplify)))
    ;; Parallelize the CFG if asked for.
    (when parallel (transform 'parallelize cfg))
    ;; Print to a file
    (with-open-file (s out-file :direction ':output
                                :if-exists ':supersede
                                :if-does-not-exist ':create)
      (generate-graphviz cfg s))
    cfg))
