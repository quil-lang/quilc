;;;; compilation-methods.lisp
;;;;
;;;; Author: Eric Peterson
;;;;

(in-package #:cl-quil)

;;; A compilation method is a function with inputs
;;;   * a resolved gate-application object
;;;
;;;   * keyword arguments that further specialize the behavior of the
;;;     method and outputs a list of Quil instructions which encode
;;;     the matrix up to global phase
;;;
;;;   OR the signal of (a subcondition of) COMPILER-DOES-NOT-APPLY,
;;;      which means that the supplied instruction is cannot or
;;;      "should" not be compiled with this method. The preferred
;;;      method for signalling a condition is calling
;;;      GIVE-UP-COMPILATION, so that other quilc authors have the
;;;      opportunity to install more complicated error handling.
;;;
;;;   OR an error, which means that the compilation routine
;;;      experienced an error.
;;;
;;; Compilation methods should bring you *closer* to some desired
;;; output. If it doesn't, or if it can't, one of these conditions
;;; should be signalled.

(define-condition compiler-does-not-apply (serious-condition)
  ()
  (:documentation "A condition that is signalled anytime a compiler doesn't apply. In general, a sub-condition should be preferred over signalling this one."))

(define-condition compiler-invalid-domain (compiler-does-not-apply)
  ()
  (:documentation "This is signaled by a compilation method when the input matrix is outside the input domain of the method."))

(define-condition compiler-acts-trivially (compiler-does-not-apply)
  ()
  (:documentation "This is signaled when a compiler is technically applicable, but would act as an identity."))

(define-condition compiler-rewrite-does-not-apply (compiler-does-not-apply)
  ()
  (:documentation "This is signaled when a rewriting rule cannot be applied to an instruction sequence."))


(defun give-up-compilation (&key (because ':unknown))
  (ecase because
    (:invalid-domain         (error 'compiler-invalid-domain))
    (:acts-trivially         (error 'compiler-acts-trivially))
    (:unknown                (error 'compiler-does-not-apply))))


;;; Core routines governing how a chip-specification's compiler list is walked

(defun apply-translation-compilers (instruction chip-spec hardware-object)
  "Wrapper function that calls the compilers associated to HARDWARE-OBJECT and the generic compilers associated to CHIP-SPEC in precedence order, returning the first found expansion of INSTRUCTION as a sequence."
  (let ((context (make-compilation-context :chip-specification chip-spec)))
    (labels ((try-compiler (compilation-method)
               "Applies COMPILATION-METHOD to INSTRUCTION. If it succeeds, end
              the whole procedure and return the resulting instruction sequence.
              If it fails, cede control by returning NIL."
               (restart-case
                   (handler-case
                       (let ((result (funcall compilation-method instruction :context context)))
                         (let ((*print-pretty* nil))
                           (format-noise
                            "APPLY-TRANSLATION-COMPILERS: Applying ~A to ~/quil:instruction-fmt/."
                            compilation-method instruction))
                         (format-noise "~{    ~/quil:instruction-fmt/~%~}" result)
                         (return-from apply-translation-compilers result))
                     (compiler-does-not-apply () nil))
                 (try-next-compiler ()
                   :report "Ignore this error and try the next compiler in the list."))))
      ;; if this is a thread invocation, call its expander
      (when (typep instruction 'application-thread-invocation)
        (return-from apply-translation-compilers
          (application-thread-invocation-thread instruction)))
      ;; then try the compilers attached to the hardware object
      (when hardware-object
        (map nil #'try-compiler (hardware-object-compilation-methods hardware-object)))
      ;; if those fail, try the global compilers
      (map nil #'try-compiler (chip-specification-generic-compilers chip-spec))
      ;; if those failed too, there's really nothing more to do.
      (format-noise
       "APPLY-TRANSLATION-COMPILERS: Could not find a compiler for ~/quil:instruction-fmt/."
       instruction)
      (give-up-compilation))))


;;; Core public-facing routine for a full compilation pass.

(define-condition not-protoquil (simple-error)
  ((program :reader not-protoquil-program :initarg :program)
   (index :reader not-protoquil-index :initarg :index))
  (:report (lambda (c s)
             (let ((j (not-protoquil-index c))
                   (p (parsed-program-executable-code (not-protoquil-program c))))
               (format s "Misplaced or illegal instruction in ProtoQuil program:~%  ")
               (cond ((zerop j)
                      (write-string "(BEGINNING OF PROGRAM)" s))
                     (t
                      (print-instruction (aref p (1- j)) s)))
               (format s "~%>>>")
               (print-instruction (aref p j) s)
               (format s "~%  ")
               (cond ((= (length p) (1+ j))
                      (write-string "(END OF PROGRAM)" s))
                     (t
                      (print-instruction (aref p (1+ j)) s))))))
  (:documentation "Error raised when a program does not validate as protoquil."))

(defun check-protoquil-program (program)
  "Checks that PROGRAM, an application-resolved parsed-program instance, conforms to the present definition of \"protoQuil\". Signals an error on failure."
  ;; a protoquil program carves up into 3 regions, each optional, of the form:
  ;; (1) a RESET instruction
  ;; (2) quantum gates, perhaps with classical memory references or classical arithmetic
  ;; (3) a collection of MEASURE instructions
  (let ((region-counter 1))
    (dotimes (j (length (parsed-program-executable-code program)))
      (let ((instr (aref (parsed-program-executable-code program) j)))
        (cond
          ;; PRAGMAs can appear anywhere.
          ((typep instr 'pragma)
           t)
          ((and (= 1 region-counter)
                (typep instr 'reset))
           (setf region-counter 2))
          ((and (>= 2 region-counter)
                (typep instr 'application))
           (setf region-counter 2))
          ((and (>= 3 region-counter)
                (typep instr 'measurement))
           (setf region-counter 3))
          (t
           (error 'not-protoquil :program program :index j)))))))

(defun protoquil-program-p (program)
  "Returns T if program satisfies the requirements of a protoquil program, NIL otherwise."
  (check-type program parsed-program)
  (handler-case (check-protoquil-program program)
    (not-protoquil () (return-from protoquil-program-p nil)))
  t)

;; Forward declaration from compressor.lisp
(declaim (special *compressor-passes*))

(defvar *default-addresser-state-class* 'fidelity-addresser-state)

;; TODO: deal with classical control and basic-blocks
(defun compiler-hook (parsed-program
                      chip-specification
                      &key
                        (protoquil nil)
                        (rewiring-type (prog-initial-rewiring-heuristic parsed-program chip-specification))
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
    (setf parsed-program (compress-qubits parsed-program)))

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
                 ;; order >1 hardware objects not yet supported by
                 ;; compressor.
                 (when (<= (length (chip-specification-objects chip-specification)) 2)
                   (dotimes (n *compressor-passes*)
                     (format-noise "COMPILER-HOOK: Compressing, pass ~D/~D." (1+ n) *compressor-passes*)
                     (setf processed-quil
                           (compress-instructions processed-quil chip-specification
                                                  :protoquil (null registrant)))))
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
