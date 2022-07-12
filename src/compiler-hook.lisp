;;;; compiler-hook.lisp
;;;;
;;;; Authors: Eric Peterson
;;;;          Robert Smith

(in-package #:cl-quil)

;;;; This file contains the main entry point to compiling a
;;;; PARSED-PROGRAM into one native to a particular chip architecture.
;;;
;;;; For historical reasons, this entry point has been called
;;;; "COMPILER-HOOK".


(defvar *standard-pre-compilation-transforms*
  '(simplify-individual-instructions)
  "The standard transforms that are applied by COMPILER-HOOK before compilation. (See also: *STANDARD-POST-PARSING-TRANSFORMS*)")

;; Forward declaration from compressor.lisp
(declaim (special *compressor-passes*))

(defvar *default-addresser-state-class* 'fidelity-addresser-state)

;; TODO: deal with classical control and basic-blocks
(defun compiler-hook (parsed-program
                      chip-specification
                      &key
                        (protoquil nil)
                        (rewiring-type (prog-initial-rewiring-heuristic parsed-program chip-specification))
                        (transforms *standard-pre-compilation-transforms*)
                        (destructive nil))
  "Runs a full compiler pass on a parsed-program object.

Arguments:

  - PARSED-PROGRAM: The parsed program to compile

  - CHIP-SPECIFICATION: The chip specification describing the target chip

Keyword arguments:

  - PROTOQUIL: Whether the input and output programs should conform to protoquil restrictions

  - REWIRING-TYPE: The scheme by which logical qubits are mapped (rewired) to physical qubits

  - DESTRUCTIVE: Default NIL. If T, the input program is mutated and after compilation contains the compiled program. Otherwise, a copy is made of the input program and the compiled copy is returned. Note that for large programs you may prefer to mutate the original program rather than waste time copying it. For example, the server interface will mutate the program since it has no use for the unmolested original program after compilation.

Returns a value list: (processed-program, of type parsed-program
                       topological-swaps, of type integer
                       unpreserved-block-duration, of type real)"
  (format-noise "COMPILER-HOOK: entrance.")

  (unless destructive
    (setf parsed-program (copy-instance parsed-program)))

  ;; Technically this could be fused with the above, but remains
  ;; separate for clarity. We should not compress if the rewiring type
  ;; is naive, or the program uses more qubits than are available on
  ;; the chip (duh), or if the program has blocks of preserved qubits.
  (when (and (not (eql ':naive rewiring-type))
             (> (qubits-needed parsed-program)
                (length (chip-spec-live-qubits chip-specification)))
             (not (parsed-program-has-preserve-blocks-p parsed-program)))
    (format-noise "COMPILER-HOOK: Compressing qubits.")
    (setf parsed-program (compress-qubits parsed-program)))

  ;; Apply transforms
  (dolist (xform transforms)
    (format-noise "COMPILER-HOOK: Applying transform ~A." xform)
    (setf parsed-program (transform xform parsed-program)))
  
  ;; Warm the lookup cache
  (warm-chip-spec-lookup-cache chip-specification)

  ;; we disallow compilation of programs that use memory aliasing
  (loop :for mdesc :in (parsed-program-memory-definitions parsed-program)
        :when (memory-descriptor-sharing-parent mdesc)
          :do (error "Programs with aliased memory are currently unsupported."))

  ;; check that the program obeys the dead qubit rule
  (when (eql ':naive rewiring-type)
    (check-program-skips-dead-qubits parsed-program chip-specification))

  ;; check that a protoquil program is in fact protoquil
  (when protoquil
    (check-protoquil-program parsed-program))

  ;; now we walk the CFG associated to the program
  (multiple-value-bind (initial-rewiring l2p-components)
      (prog-initial-rewiring parsed-program chip-specification
                             :type rewiring-type)
    (let* ((cfg (program-cfg parsed-program :dce t))
           ;; this is a list of pairs (block-to-be-traversed registrant)
           (block-stack (list (list (entry-point cfg) nil)))
           (topological-swaps 0)
           (unpreserved-duration 0))

      ;; In any rewiring scheme a preserved block must not touch dead
      ;; qubits.
      (check-preserved-blocks-skip-dead-qubits cfg chip-specification)

      (let ((*print-pretty* nil))
        (format-noise "COMPILER-HOOK: initial rewiring ~A" initial-rewiring))

      ;; if we are expecting to manipulate protoquil, we segment the program-final
      ;; sequence of MEASURE instructions out into a separate CFG block, so that
      ;; the greedy addresser doesn't try to move them forward.
      (when protoquil
        (let (final-blk
              (instrs-measures nil)
              (instrs-rest nil)
              (new-final-blk (make-instance 'basic-block)))
          ;; find the exit block
          (dolist (blk (cfg-blocks cfg))
            (when (typep (outgoing blk) 'terminating-edge)
              (setf final-blk blk)))
          ;; segments its instructions into the MEASURES and the non-MEASURES
          (loop :for instr :across (basic-block-code final-blk)
                :if (typep instr 'measure)
                  :do (push instr instrs-measures)
                :else
                  :do (push instr instrs-rest))
          ;; store its non-MEASURE instructions back into the block
          (setf (basic-block-code final-blk) (coerce (nreverse instrs-rest) 'vector))
          ;; store its MEASURE instructions into the cordoned-off block
          (setf (basic-block-code new-final-blk) (coerce (nreverse instrs-measures) 'vector))
          ;; place the new block in the CFG and re-link them
          (push new-final-blk (cfg-blocks cfg))
          (link-blocks new-final-blk terminating-edge)
          (link-blocks final-blk (unconditional-edge new-final-blk))))

      ;; these local functions describe how we traverse / modify the CFG.
      (labels
          ;; this function introduces a new block that cajoles the compiler into
          ;; introducing rewiring SWAPs to match the exit/enter rewires across
          ;; a jump REGISTRANT --> BLK
          ((edge-to-rewiring-block (blk registrant target-rewiring)
             (let* ((pseudoinstruction (make-instance 'application-force-rewiring
                                                      :target target-rewiring))
                    (fresh-block (make-instance 'basic-block
                                                :code (make-array 1 :initial-element pseudoinstruction)
                                                :incoming (list registrant)
                                                :outgoing (unconditional-edge blk))))
               ;; push it into the CFG
               (push fresh-block (cfg-blocks cfg))
               ;; update the target's parents
               (setf (incoming blk)
                     (list* fresh-block (remove registrant (incoming blk))))
               ;; and update the source's outgoing edge
               (setf (outgoing registrant)
                     (redirect-edge blk fresh-block (outgoing registrant)))
               ;; try again, but with this new jump
               (push (list fresh-block registrant) block-stack)
               (format-noise "COMPILER-HOOK: Introduced ~A to deal with the rewiring."
                             (basic-block-name fresh-block))))

           (touch-preserved-block (blk)
             ;; if so, then we don't have any business compiling it. treat
             ;; it as marked, with the identity rewiring on both ends,
             ;; and proceed
             (setf (basic-block-in-rewiring blk) (make-rewiring (chip-spec-n-qubits chip-specification)))
             (setf (basic-block-out-rewiring blk) (make-rewiring (chip-spec-n-qubits chip-specification)))
             (change-class blk 'basic-block))

           (touch-unpreserved-block (blk registrant)
             ;; actually process this block
             (multiple-value-bind (chip-schedule initial-l2p final-l2p)
                 (do-greedy-addressing
                     (make-instance *default-addresser-state-class*
                                    :chip-spec chip-specification
                                    :l2p-components l2p-components
                                    :initial-l2p (if registrant
                                                     (basic-block-in-rewiring blk)
                                                     initial-rewiring))
                   (coerce (basic-block-code blk) 'list)
                   :use-free-swaps (null registrant))
               (let* ((duration (chip-schedule-duration chip-schedule))
                      (straight-line-quil (chip-schedule-to-straight-quil chip-schedule))
                      (local-topological-swaps (count-if #'swap-application-p straight-line-quil))
                      (fully-native-quil (expand-to-native-instructions straight-line-quil chip-specification))
                      (processed-quil fully-native-quil))
                 ;; This is useful for debugging, but can be
                 ;; extremely, extremely noisy.
                 #+#:ignore
                 (progn
                   (format-noise "COMPILER-HOOK: Finished addressing, got:")
                   (cond
                     ((null processed-quil)
                      (format-noise "    *empty program*"))
                     (t
                      (format-noise "~{    ~/cl-quil:instruction-fmt/~%~}" processed-quil))))
                 (dotimes (n *compressor-passes*)
                   (format-noise "COMPILER-HOOK: Compressing, pass ~D/~D." (1+ n) *compressor-passes*)
                   (setf processed-quil
                         (compress-instructions processed-quil chip-specification
                                                :protoquil (null registrant))))
                 ;; This is useful for debugging, but can be
                 ;; extremely, extremely noisy.
                 #+#:ignore
                 (progn
                   (format-noise "COMPILER-HOOK: Finished compressing, got:")
                   (cond
                     ((null processed-quil)
                      (format-noise "    *empty program*"))
                     (t
                      (format-noise "~{    ~/cl-quil:instruction-fmt/~%~}" processed-quil))))
                 ;; we're done processing. store the results back into the CFG block.
                 (setf (basic-block-code blk) processed-quil)
                 (setf (basic-block-in-rewiring blk) initial-l2p)
                 (setf (basic-block-out-rewiring blk) final-l2p)
                 (incf topological-swaps local-topological-swaps)
                 (incf unpreserved-duration duration)
                 (format-noise "COMPILER-HOOK: Done processing block ~A." (basic-block-name blk)))))

           (touch-reset-block (blk)
             ;; actually process this block
             (multiple-value-bind (chip-schedule initial-l2p final-l2p)
                 (do-greedy-addressing
                     (make-instance *default-addresser-state-class*
                                    :chip-spec chip-specification
                                    :l2p-components l2p-components
                                    :initial-l2p (prog-initial-rewiring parsed-program chip-specification
                                                                        :type rewiring-type))
                   (coerce (basic-block-code blk) 'list))
               (let* ((duration (chip-schedule-duration chip-schedule))
                      (straight-line-quil (chip-schedule-to-straight-quil chip-schedule))
                      (local-topological-swaps (count-if #'swap-application-p straight-line-quil))
                      (fully-native-quil (expand-to-native-instructions straight-line-quil chip-specification))
                      (processed-quil fully-native-quil))
                 (dotimes (n *compressor-passes*)
                   (format-noise "COMPILER-HOOK: Compressing, pass ~D/~D." (1+ n) *compressor-passes*)
                   (setf processed-quil
                         (compress-instructions processed-quil chip-specification
                                                :protoquil t)))
                 ;; we're done processing. store the results back into the CFG block.
                 (setf (basic-block-code blk) processed-quil)
                 (setf (basic-block-in-rewiring blk) initial-l2p)
                 (setf (basic-block-out-rewiring blk) final-l2p)
                 (incf topological-swaps local-topological-swaps)
                 (incf unpreserved-duration duration)
                 (format-noise "COMPILER-HOOK: Done processing block ~A." (basic-block-name blk)))))

           (process-block (blk registrant)
             ;; if this block is expecting a rewiring, we should make sure the
             ;; exit/enter rewirings match.
             (when (and registrant
                        (not (typep blk 'reset-block))
                        (or (basic-block-in-rewiring blk)
                            (typep blk 'preserved-block)))
               ;; compare incoming and outgoing l2ps.
               (let ((final-l2p (basic-block-out-rewiring registrant))
                     (initial-l2p (if (typep blk 'preserved-block)
                                      (make-rewiring (chip-spec-n-qubits chip-specification))
                                      (basic-block-in-rewiring blk))))
                 (format-noise "COMPILER-HOOK: Matching rewiring from ~A (~A) to ~A (~A)."
                               (basic-block-name registrant)
                               final-l2p
                               (basic-block-name blk)
                               initial-l2p)
                 ;; if they're the same, proceed with analyzing the jump
                 (unless (equalp final-l2p initial-l2p)
                   (return-from process-block
                     (edge-to-rewiring-block blk registrant initial-l2p)))))

             ;; the source's exit rewiring now matches the target's entry rewiring.
             ;; if this block has already been visited, skip it.
             (when (basic-block-in-rewiring blk)
               (return-from process-block))

             (let ((*print-pretty* nil))
               (format-noise "COMPILER-HOOK: Visiting ~A for the first time, coming from ~A (~A)."
                             (basic-block-name blk)
                             (and registrant (basic-block-name registrant))
                             (and registrant (basic-block-out-rewiring registrant))))
             ;; set the block-initial-l2p, forced by the previous block
             (when registrant
               (setf (basic-block-in-rewiring blk)
                     (basic-block-out-rewiring registrant)))
             ;; add the block's children to the traversal stack
             (adt:match outgoing-edge (outgoing blk)
                        ((conditional-edge _ true-target false-target)
                         (push (list true-target blk) block-stack)
                         (push (list false-target blk) block-stack))
                        ((unconditional-edge target)
                         (push (list target blk) block-stack))
                        (terminating-edge nil))

             ;; now fork based on whether the block is PRESERVEd.
             ;; note that touch-* will set block-initial-l2p, which indicates the block has been visited
             (typecase blk
               (preserved-block
                (touch-preserved-block blk))
               (reset-block
                (touch-reset-block blk))
               (otherwise
                (touch-unpreserved-block blk registrant))))

           ;; this is the main loop that pushes through the CFG
           (exhaust-stack ()
             (loop :until (endp block-stack) :do
               (apply #'process-block (pop block-stack)))))

        ;; kick off the traversal
        (exhaust-stack)

        ;; one more pass of CFG collapse
        (simplify-cfg cfg)

        (let ((processed-program (reconstitute-program cfg)))
          ;; Keep global PRAGMAS in the code, at the top of the file.
          (setf (parsed-program-executable-code processed-program)
                (concatenate
                 'vector
                 (remove-if-not #'global-pragma-instruction-p
                                (parsed-program-executable-code parsed-program))
                 (parsed-program-executable-code processed-program)))
          ;; retain the old circuit and gate definitions
          (setf (parsed-program-gate-definitions processed-program)
                (parsed-program-gate-definitions parsed-program))
          (setf (parsed-program-circuit-definitions processed-program)
                (parsed-program-circuit-definitions parsed-program))
          (setf (parsed-program-memory-definitions processed-program)
                (parsed-program-memory-definitions parsed-program))
          ;; ... and output the results.
          (values
           processed-program
           topological-swaps
           unpreserved-duration))))))
