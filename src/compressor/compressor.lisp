;;;; compressor.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

;;
;; the rough data flow of the routines in this file is
;;
;; [ completely       ]
;; [ undifferentiated ]
;; [ instructions     ]
;;        |
;;     compress-
;;   instructions
;;        |
;;        v
;; [ baskets of instrs     ]
;; [ which act on over-    ]
;; [ lapping sets of qubits]
;;        |
;;     actually-
;;     compress-
;;   instructions
;;        |
;;        v
;; [ subsegment of ]
;; [ instructions  ]    decompile-    [ single massive matrix,]
;; [ likely to have] --instructions-> [ blindly recompiled    ]
;; [ a matrix repn ]   -in-context    [ into instructions     ]
;;        |                                      |
;;  algebraically-                        algebraically-
;;     reduce-                               reduce-
;;   instructions                          instructions
;;        |                                      |
;;        v                                      v
;; [ reduced       ]                  [ reduced blindly        ]
;; [ instructions  ]        >?<       [ recompiled instructions]
;;


;;; generic helper functions

(defun application-qubit-indices (appl)
  "Abbreviation: extracts the QUBIT-INDEX values of an application APPL."
  (mapcar #'qubit-index (application-arguments appl)))

(defun format-quil-sequence (s instructions &optional prefix)
  "Nicely prints a sequence of INSTRUCTIONS to a stream S. If PREFIX is present, prepend it to the overall output."
  (when prefix
    (format s prefix)
    (terpri s))
  (dolist (instr instructions)
    (format s "    ~/cl-quil:instruction-fmt/~%" instr)))

(defun qubits-in-instr-list (instructions)
  "Produces a list of all of the (unboxed) qubit indices appearing in INSTRUCTIONS, a list of applications."
  (delete-duplicates
   (mapcan (lambda (isn)
             (cond
               ((typep isn 'application)
                (application-qubit-indices isn))))
           instructions)))


;;; helper functions for algebraically-reduce-instructions, including the doubly-linked
;;; list structure peephole-rewriter-node which is used for rewinding the peephole rewriter

(defun calculate-instructions-duration (instructions chip-specification)
  "Calculates the runtime of a sequence of native INSTRUCTIONS on a chip with architecture governed by CHIP-SPECIFICATION (and with assumed perfect parallelization across resources)."
  (let ((lschedule (make-lscheduler)))
    ; load up the logical schedule
    (append-instructions-to-lschedule lschedule instructions)
    ; sift through it for durations
    (lscheduler-calculate-duration lschedule chip-specification)))


(defun find-noncommuting-instructions (node)
  "Return at most *REWRITING-PEEPHOLE-SIZE* of the earliest instructions below NODE,
of type PEEPHOLE-REWRITER-NODE, whose qubit-complexes intersect with INSTR's and each
other's."
  (let ((output-count 0)
        (current-qubit-complex (application-qubit-indices (peephole-rewriter-node-instr node))))
    (loop :for n := node :then (peephole-rewriter-node-next n)
          :when (or (null n) (<= *rewriting-peephole-size* output-count))
            :do (return output)
          :when (intersection current-qubit-complex
                              (application-qubit-indices (peephole-rewriter-node-instr n)))
            :do (setf current-qubit-complex
                      (nunion current-qubit-complex
                              (application-qubit-indices (peephole-rewriter-node-instr n))))
                (incf output-count)
            :and
              :collect n :into output)))

(defun splice-instructions (original peephole new position)
  "Return a new list of instructions consisting of NEW inserted into
  ORIGINAL at POSITION, with any members of PEEPHOLE removed from
  ORIGINAL. Members of PEEPHOLE must occur in order (though not
  necessarily consecutively) in ORIGINAL."
  (let ((result '())
        (remove (pop peephole))
        (pos 0))
    (declare (type fixnum pos))
    (loop
      (when (= pos position)
        (dolist (n new)
          (push n result)))
      (when (endp original)
        (when (< pos position)
          (error "Invalid insertion index ~A in list of length ~A"
                 position
                 pos))
        (when (or peephole remove)
          ;; This can happen if peephole is not a subset of original,
          ;; or if peephole elements are not present in the same order
          ;; in original
          (error "Peephole elements not fully removed from original"))
        (return (nreverse result)))
      (let ((orig (pop original)))
        (if (eq orig remove)
            (setf remove (pop peephole))
            (push orig result))
        (incf pos)))))

(defstruct peephole-rewriter-node
  "A node housing an instruction and attendant metadata used during peephole rewriting.  Essentially a doubly-linked list."
  (instr nil :type application)
  (prev nil :type (or null peephole-rewriter-node))
  (next nil :type (or null peephole-rewriter-node))
  (context nil)) ; compressor context *after* this instruction is applied

(defun instrs->peephole-rewriter-nodes (instrs)
  "Converts a list of instructions to a doubly-linked list of peephole rewriting nodes."
  (let (prev
        this
        first)
    (dolist (instr instrs)
      (setf this (make-peephole-rewriter-node :instr instr
                                              :prev prev))
      (when prev
        (setf (peephole-rewriter-node-next prev) this))
      (unless first
        (setf first this))
      (setf prev this))
    (values first this)))

(defun peephole-rewriter-nodes->instrs (node)
  "Linearizes the peephole rewriter nodes at-and-below NODE into a list of instructions."
  (loop :for n := node :then (peephole-rewriter-node-next n)
        :while n
          :collect (peephole-rewriter-node-instr n)))

(defun rewind-node (node n)
  "Makes N calls to PEEPHOLE-REWRITER-NODE-PREV starting on NODE."
  (cond
    ((zerop n) node)
    ((peephole-rewriter-node-prev node)
     (rewind-node (peephole-rewriter-node-prev node) (1- n)))
    (t node)))

(defun find-safe-insertion-node (relevant-nodes-for-inspection)
  "Given a list RELEVANT-NODES-FOR-INSPECTION of peephole rewriter nodes, calculates the last such node that accesses new qubits not accessed by previous instructions in the list."
  (loop
    :for n :in (rest relevant-nodes-for-inspection)
    :for i := (peephole-rewriter-node-instr n)
    :with target := (first relevant-nodes-for-inspection)
    :with qubit-complex := (application-qubit-indices
                            (peephole-rewriter-node-instr
                             (first relevant-nodes-for-inspection)))
    :do (let ((incoming-qubits (application-qubit-indices i)))
          (unless (subsetp incoming-qubits qubit-complex)
            (setf qubit-complex (union qubit-complex incoming-qubits))
            (setf target n)))
    :finally (return target)))

(defun splice-instrs-in-at-node (instrs node)
  "Given a list of instructions INSTRS, replaces a peephole rewriter NODE (which possibly belongs to a larger family of nodes) with a family of peephole rewriter nodes equivalent to INSTRS."
  (cond
    ((null instrs)
     t)
    (t
     (multiple-value-bind (new-head new-tail) (instrs->peephole-rewriter-nodes instrs)
       (let ((node-next (peephole-rewriter-node-next node)))
         (setf (peephole-rewriter-node-next node) new-head)
         (setf (peephole-rewriter-node-prev new-head) node)
         (setf (peephole-rewriter-node-next new-tail) node-next)
         (when node-next
           (setf (peephole-rewriter-node-prev node-next) new-tail)))
       t))))

(defun delete-node (node)
  "Removes a peephole rewriter node NODE from a larger network of peephole rewriter nodes, patching over any links that touched this node."
  (let ((prev (peephole-rewriter-node-prev node))
        (next (peephole-rewriter-node-next node)))
    (when prev
      (setf (peephole-rewriter-node-next prev) next))
    (when next
      (setf (peephole-rewriter-node-prev next) prev))
    t))

(defun print-node-list (node)
  "Pretty-prints a list of peephole rewriter nodes, beginning with NODE, together with the structure as a doubly-linked list."
  (unless node
    (return-from print-node-list nil))
  (format t "~30a <-- ~30a --> ~30a~%"
          (alexandria:when-let ((prev (peephole-rewriter-node-prev node)))
            (peephole-rewriter-node-instr prev))
          (peephole-rewriter-node-instr node)
          (alexandria:when-let ((next (peephole-rewriter-node-next node)))
            (peephole-rewriter-node-instr next)))
  (print-node-list (peephole-rewriter-node-next node)))

(defun algebraically-reduce-instructions (instructions
                                          chip-specification
                                          context)
  "Applies peephole rewriter rules from a CHIP-SPECIFICATION to a sequence of INSTRUCTIONS, using CONTEXT to activate context-sensitive rules."
  (labels
      (;; let the context know that we've passed inspection of NODE, so that the
       ;; effect of that instruction is visible during inspection of the next node
       (update-context (node)
         (setf (peephole-rewriter-node-context node)
               (update-compressor-context (peephole-rewriter-node-context
                                           (peephole-rewriter-node-prev node))
                                          (peephole-rewriter-node-instr node)
                                          :destructive? nil)))

       ;; having selected an appropriate sequence of instructions, actually
       ;; apply the available rewriting rules. if we find one that applies,
       ;; splices the results in to replace the nodes and return the location
       ;; of the new node (so that the outer loop can rewind). if none applies,
       ;; announce failure (so that the outer loop can step ahead by one).
       (apply-rules (rewrite-rules nodes-for-inspection)
         (loop :for rule :across rewrite-rules :do
           ;; make sure we have enough terms, then apply the rule's consumer
           (with-simple-restart (try-next-compiler "Ignore this error and try the next rewrite rule in the list.")
             (handler-case
                 (alexandria:when-let*
                     ((relevant-nodes-for-inspection
                       (and (>= (length nodes-for-inspection) (rewriting-rule-count rule))
                            ;; TODO: consider calculating this subseq only once.
                            (subseq nodes-for-inspection 0 (rewriting-rule-count rule)))))
                   (let ((output
                           (apply (rewriting-rule-consumer rule)
                                  (peephole-rewriter-node-context
                                   (peephole-rewriter-node-prev
                                    (first relevant-nodes-for-inspection)))
                                  (mapcar #'peephole-rewriter-node-instr relevant-nodes-for-inspection))))
                     (format *compiler-noise-stream*
                             "ALGEBRAICALLY-REDUCE-INSTRUCTIONS: Applying the rewriting rule called ~a.~%"
                             (rewriting-rule-readable-name rule))
                     ;; if the rule was triggered, splice it in and remove
                     ;; all of the instructions that the rule touched.
                     ;;
                     ;; NOTE: a delicate point here is *where* the new
                     ;; instructions ought to be inserted. to avoid sliding
                     ;; any noncommuting instructions past each other, we
                     ;; pick a 'bottleneck' instruction.
                     (let ((insertion-point (find-safe-insertion-node relevant-nodes-for-inspection))
                           (new-node (rewind-node (first nodes-for-inspection) *rewriting-peephole-size*)))
                       (splice-instrs-in-at-node output insertion-point)
                       (mapc #'delete-node relevant-nodes-for-inspection)
                       (return-from apply-rules new-node))))
               (compiler-does-not-apply () nil)))))

       ;; main loop for the peephole rewriter. for any particular node, it
       ;; assembles a list of instructions which might be subject to rewriting
       ;; rules, then passes those to APPLY-RULES. if APPLY-RULES returns
       ;; successfully, we rewind by the peephole window and try again. if it
       ;; fails, we fall through, step through to the next node, and try again.
       (outer-instruction-loop (node)
         ;; for each instruction...
         (setf node (peephole-rewriter-node-next node))
         (unless node (return-from outer-instruction-loop))

         (update-context node)

         ;; build the noncommuting instruction list
         (let* ((nodes-for-inspection (find-noncommuting-instructions node))
                (qubit-complex nil)
                (exhausted-specializations nil))
           ;; for each instruction in the noncommuting tail...
           (dolist (inspection-node nodes-for-inspection)
             (let ((new-complex (application-qubit-indices (peephole-rewriter-node-instr inspection-node))))
               ;; if we've already hit the generic object, continue to the next list head element.
               (when exhausted-specializations
                 (return-from outer-instruction-loop
                   (outer-instruction-loop node)))
               ;; does the next noncommuting instruction yield some new set of rules?
               ;; if not, skip it.
               (when (subsetp new-complex qubit-complex)
                 (return))
               ;; enlarge the complex
               (setf qubit-complex (union qubit-complex new-complex))
               ;; try to find an associated hardware object for this complex
               (let ((obj (nth-value 2 (lookup-hardware-address-by-qubits chip-specification qubit-complex))))
                 ;; if we can, then we want to loop over the object's rewrite rules.
                 ;; if we can't, we fall through and do nothing.
                 (alexandria:when-let
                     ((rewrite-rules
                       (cond
                         ;; if we found a new hardware object, then use its rules
                         (obj (hardware-object-rewriting-rules obj))
                         ;; if we didn't find a new object but we haven't tried the
                         ;; generic rules, try them now
                         ((not exhausted-specializations)
                          (setf exhausted-specializations t)
                          (chip-specification-generic-rewriting-rules chip-specification))
                         ;; otherwise, give up.
                         (t nil))))
                   (let ((node-to-jump-to (apply-rules rewrite-rules nodes-for-inspection)))
                     (when node-to-jump-to
                       (return-from outer-instruction-loop
                         (outer-instruction-loop node-to-jump-to)))))))))
         (outer-instruction-loop node)))

    ;; strip out all the NOPs.
    (let* ((instructions (remove-if (lambda (x) (typep x 'no-operation)) instructions))
           (head (instrs->peephole-rewriter-nodes instructions))
           (node head))

      ;; actually, we need to prepend a dummy head
      (setf head (make-peephole-rewriter-node :instr (build-gate "Z" () 0)
                                              :next head
                                              :context context))
      (setf (peephole-rewriter-node-prev (peephole-rewriter-node-next head)) head)
      (setf node head)

      (outer-instruction-loop head)
      ;; when we make it to this point, no rewrite rules apply, so quit.
      (peephole-rewriter-nodes->instrs (peephole-rewriter-node-next head)))))

(defun expand-to-native-instructions (instrs chip-specification &optional output-string)
  "Repeatedly applies nativization routines to expand a list of addressed instructions into a list of addressed, native instructions. Makes no attempt to perform any kind of rewiring or any kind of simplification."
  ;; dispatch on the top instruction type
  (cond
    ;; if we've exhausted the input, then return
    ((endp instrs)
     (nreverse output-string))
    ;; discard any NOPs
    ((typep (first instrs) 'no-operation)
     (expand-to-native-instructions (rest instrs)
                                    chip-specification
                                    output-string))
    ;; pass any classical instructions through
    ((not (typep (first instrs) 'application))
     (expand-to-native-instructions (rest instrs)
                                    chip-specification
                                    (cons (first instrs) output-string)))
    ;; otherwise, we have a quantum instruction
    (t
     ;; try to locate it on hardware.
     (multiple-value-bind (order address obj)
         (lookup-hardware-address chip-specification (first instrs))
       (declare (ignore order) (ignore address))
       ;; are we native? then stick this instruction onto the output.
       (when (and obj
                  (funcall (hardware-object-native-instructions obj) (first instrs)))
         (return-from expand-to-native-instructions
           (expand-to-native-instructions (rest instrs)
                                          chip-specification
                                          (cons (first instrs) output-string))))
       ;; otherwise, we are nonnative. translate us.
       (let ((translation-results (apply-translation-compilers (first instrs)
                                                               chip-specification
                                                               obj)))
         ;; if we managed a translation, use these instructions instead.
         ;; otherwise, throw an error: we failed to perform translation.
         (cond
           (translation-results
            (when (and *compress-carefully*
                       (not *enable-approximate-compilation*)
                       (notany (lambda (instr) (typep instr 'state-prep-application))
                               instrs))
              (let* ((reassignment
                       ;; the actual reassignment we use is unimportant. this is
                       ;; more along the lines of COMPRESS-QUBITs, so that our
                       ;; matrices don't take up quite so much space.
                       (standard-qubit-relabeler (union (qubits-in-instr-list instrs)
                                                        (qubits-in-instr-list translation-results))))
                     (ref-mat (make-matrix-from-quil (list (first instrs))
                                                     :relabeling reassignment))
                     (mat (make-matrix-from-quil translation-results
                                                 :relabeling reassignment))
                     (kron-size (max (ilog2 (magicl:matrix-rows ref-mat))
                                     (ilog2 (magicl:matrix-rows mat))))
                     (kroned-mat (kron-matrix-up mat kron-size))
                     (kroned-ref-mat (kron-matrix-up ref-mat kron-size)))
                (assert
                 (matrix-equality
                  kroned-ref-mat
                  (scale-out-matrix-phases kroned-mat kroned-ref-mat)))))
            (expand-to-native-instructions (append translation-results
                                                   (rest instrs))
                                           chip-specification
                                           output-string))
           (t
            (error "Failed to expand ~a into native instructions."
                   (print-instruction (first instrs) nil)))))))))


(defun decompile-instructions-in-context (instructions chip-specification context)
  "This routine is called by COMPRESS-INSTRUCTIONS-IN-CONTEXT to make a decision about how to prefer 'linear algebraic compression': the list of INSTRUCTIONS can always be rewritten as its associated action matrix, but under certain conditions (governed by CONTEXT) we can sometimes get away with something less."
  (let ((qubits-on-obj (qubits-in-instr-list instructions)))
    (labels
        (;; produce a sequence of native instructions that have the effect of
         ;; carrying START-WF to FINAL-WF (= INSTRUCTIONS |START-WF>)
         (decompile-instructions-into-state-prep (start-wf final-wf qc)
           (expand-to-native-instructions
            (list (make-instance 'state-prep-application
                                 :source-wf (copy-seq start-wf)
                                 :target-wf final-wf
                                 :arguments (nreverse (mapcar #'qubit qc))))
            chip-specification))

         ;; produce a sequence of native instructions that have the same effect
         ;; as the matrix representation of INSTRUCTIONS
         (decompile-instructions-into-full-unitary ()
           (alexandria:when-let ((matrix (make-matrix-from-quil
                                          instructions
                                          :relabeling (standard-qubit-relabeler qubits-on-obj))))
             (expand-to-native-instructions
              (list (make-instance 'gate-application
                                   :operator #.(named-operator "WHOLEPROGRAM")
                                   :arguments (mapcar #'qubit qubits-on-obj)
                                   :gate matrix))
              chip-specification))))

      (destructuring-bind (start-wf wf-qc)
          (if (compressor-context-aqvm context)
              (aqvm-extract-state (compressor-context-aqvm context) qubits-on-obj)
              '(:not-simulated :not-simulated))
        ;; there's one case where we know state prep applies: when qubits-on-obj
        ;; exhausts wf-qc. in this case, wf corresponds to a single column of the
        ;; unitary encoded by instructions.
        ;;
        ;; it's conceivable that something more fine-grained could go here, if
        ;; there were a theory of multidimensional state prep.
        (when (and *enable-state-prep-compression*
                   (not (eql ':not-simulated start-wf))
                   (subsetp wf-qc qubits-on-obj))
          (let ((final-wf (nondestructively-apply-instrs-to-wf instructions
                                                               start-wf
                                                               wf-qc)))
            (when (and final-wf
                       (not (eql ':not-simulated final-wf)))
              (return-from decompile-instructions-in-context
                (decompile-instructions-into-state-prep start-wf final-wf wf-qc)))))
        ;; otherwise, we're obligated to do full unitary compilation.
        (decompile-instructions-into-full-unitary)))))

(defmacro assert-compiler-correctness (condition-name check-form &rest init-forms &key &allow-other-keys)
  (check-type condition-name symbol)
  `(unless ,check-form
     (error ',condition-name ,@init-forms)))

(define-condition quil-compression-error (simple-error) ())
(define-condition quil-compression-matrices-disagree-error (quil-compression-error) ())
(define-condition quil-compression-states-disagree-error (quil-compression-error)
  ((final-wf :initarg :final-wf :accessor quil-compression-error-final-wf)
   (final-wf-reduced :initarg :final-wf-reduced :accessor quil-compression-error-final-wf-reduced)
   (type :initarg :type :accessor quil-compression-error-type))
  (:report (lambda (c s) (format s "During careful checking of instruction compression, the wavefunction produced by by ~A was detected to not be collinear with the target wavefunction."
                            (quil-compression-error-type c)))))
(define-condition quil-compression-matrices-not-close-error (quil-compression-error)
  ()
  (:report (lambda (c s) (format s "During careful checking of instruction compression, the recomputed instruction sequence has an unreasonably large fidelity drop from the original sequence."))))

(defun check-contextual-compression-was-well-behaved (instructions
                                                      decompiled-instructions
                                                      reduced-instructions
                                                      reduced-decompiled-instructions
                                                      context)
  "Checks that INSTRUCTIONS, DECOMPILED-INSTRUCTIONS, REDUCED-INSTRUCTIONS, and REDUCED-DECOMPILED-INSTRUCTIONS are sufficiently faithful models of one another (whose precise meaning depends upon CONTEXT)."
  (unless *compress-carefully*
    (return-from check-contextual-compression-was-well-behaved t))

  ;; REM: we use DECOMPILED-INSTRUCTIONS here rather than INSTRUCTIONS to read
  ;;      off the qubit complex because a state-preparation circuit might
  ;;      involve a strictly larger qubit complex than the one associated to
  ;;      the original instruction sequence.
  (let ((qubits-on-obj (or (qubits-in-instr-list decompiled-instructions)
                           (qubits-in-instr-list instructions))))
    (labels
        ((check-quil-agrees-as-matrices ()
           (let* ((relabeling
                   ;; the actual reassignment we use is unimportant. this is
                   ;; more along the lines of COMPRESS-QUBITs, so that our
                   ;; matrices don't take up quite so much space.
                    (standard-qubit-relabeler
                     (reduce #'union
                             (list instructions
                                   reduced-instructions
                                   decompiled-instructions
                                   reduced-decompiled-instructions)
                             :key #'qubits-in-instr-list)))
                  (stretched-matrix (or (make-matrix-from-quil instructions :relabeling relabeling)
                                        (return-from check-quil-agrees-as-matrices nil)))
                  (decompiled-matrix
                    (make-matrix-from-quil decompiled-instructions
                                           :relabeling relabeling))
                  (reduced-matrix
                    (kron-matrix-up (make-matrix-from-quil reduced-instructions
                                                           :relabeling relabeling)
                                    (ilog2 (magicl:matrix-rows stretched-matrix))))
                  (reduced-decompiled-matrix
                    (kron-matrix-up (make-matrix-from-quil reduced-decompiled-instructions
                                                           :relabeling relabeling)
                                    (ilog2 (magicl:matrix-rows stretched-matrix)))))
             (assert-compiler-correctness
              quil-compression-matrices-disagree-error
              (and (matrix-equality stretched-matrix
                                    (scale-out-matrix-phases reduced-matrix
                                                             stretched-matrix))))
             (when decompiled-instructions
               (assert-compiler-correctness
                quil-compression-matrices-disagree-error
                (and (matrix-equality stretched-matrix
                                      (scale-out-matrix-phases decompiled-matrix
                                                               stretched-matrix))
                     (matrix-equality stretched-matrix
                                      (scale-out-matrix-phases reduced-decompiled-matrix
                                                               stretched-matrix)))))))

         (check-quil-agrees-as-states (start-wf final-wf wf-qc)
           (let* ((final-wf-reduced-instrs (nondestructively-apply-instrs-to-wf
                                            reduced-instructions
                                            start-wf
                                            wf-qc))
                  (final-wf-reduced-instrs-collinearp
                    (or (and (eql final-wf ':not-simulated)
                             (eql final-wf-reduced-instrs ':not-simulated))
                        (collinearp final-wf final-wf-reduced-instrs)))
                  (final-wf-reduced-prep (nondestructively-apply-instrs-to-wf
                                          reduced-decompiled-instructions
                                          start-wf
                                          wf-qc))
                  (final-wf-reduced-prep-collinearp
                    (or (null decompiled-instructions)
                        (and (eql final-wf ':not-simulated)
                             (eql final-wf-reduced-prep ':not-simulated))
                        (collinearp final-wf final-wf-reduced-prep))))
             (assert-compiler-correctness
              quil-compression-states-disagree-error
              final-wf-reduced-instrs-collinearp)
             (assert-compiler-correctness
              quil-compression-states-disagree-error
              final-wf-reduced-prep-collinearp)))

         (check-quil-is-near-as-matrices ()
           (alexandria:when-let ((stretched-matrix (make-matrix-from-quil instructions)))
             (let* ((n (ilog2 (magicl:matrix-rows stretched-matrix)))
                    (reduced-matrix
                      (kron-matrix-up (make-matrix-from-quil reduced-instructions)
                                      (ilog2 (magicl:matrix-rows stretched-matrix))))
                    (reduced-decompiled-matrix
                      (kron-matrix-up (make-matrix-from-quil reduced-decompiled-instructions)
                                      (ilog2 (magicl:matrix-rows stretched-matrix)))))
               (assert-compiler-correctness
                quil-compression-matrices-not-close-error
                (matrix-equality stretched-matrix
                                 (scale-out-matrix-phases reduced-matrix stretched-matrix)))
               (when decompiled-instructions
                 (let* ((prod (magicl:multiply-complex-matrices
                               reduced-matrix (magicl:dagger reduced-decompiled-matrix)))
                        (tr (matrix-trace prod))
                        (trace-fidelity (/ (+ n (abs (* tr tr)))
                                           (+ n (* n n))))
                        (ls-reduced (make-lscheduler))
                        (ls-reduced-decompiled (make-lscheduler))
                        (chip-spec (compressor-context-chip-specification context)))
                   (append-instructions-to-lschedule ls-reduced reduced-instructions)
                   (append-instructions-to-lschedule ls-reduced-decompiled reduced-decompiled-instructions)
                   (assert-compiler-correctness
                    quil-compression-matrices-not-close-error
                    (>= (* trace-fidelity
                           (lscheduler-calculate-fidelity ls-reduced-decompiled chip-spec))
                        (lscheduler-calculate-fidelity ls-reduced chip-spec)))))))))

      (destructuring-bind (start-wf wf-qc)
          (aqvm-extract-state (compressor-context-aqvm context) qubits-on-obj)
        (when (and (not (eql ':not-simulated start-wf))
                   *enable-state-prep-compression*)
          (let ((final-wf (nondestructively-apply-instrs-to-wf instructions
                                                               start-wf
                                                               wf-qc)))
            (return-from check-contextual-compression-was-well-behaved
              (check-quil-agrees-as-states start-wf final-wf wf-qc))))
        ;; otherwise, we're obligated to check the full unitary.
        ;; we need only make a decision about whether we're allowing approximate methods.
        (cond
          (*enable-approximate-compilation*
           (check-quil-is-near-as-matrices))
          (t
           (check-quil-agrees-as-matrices)))))))


(defun compress-instructions-in-context (instructions chip-specification context)
  "Dispatch routine for doing rewriting, algebraic and linear-algebraic, on a sequence of INSTRUCTIONS."
  (handler-case
      ;; start by making a decision about how we're going to do linear algebraic compression
      (let ((decompiled-instructions (decompile-instructions-in-context instructions
                                                                        chip-specification
                                                                        context))
            reduced-instructions
            reduced-decompiled-instructions)

        ;; now proceed to do the reductions
        (format *compiler-noise-stream*
                "COMPRESS-INSTRUCTIONS: Applying algebraic rewrites to original string.~%")
        (setf reduced-instructions
              (algebraically-reduce-instructions instructions chip-specification context))
        (format *compiler-noise-stream*
                "COMPRESS-INSTRUCTIONS: Applying algebraic rewrites to recompiled string.~%")
        (when decompiled-instructions
          (setf reduced-decompiled-instructions
                (algebraically-reduce-instructions decompiled-instructions
                                                   chip-specification
                                                   context)))

        ;; check that the quil that came out was the same as the quil that went in
        (check-contextual-compression-was-well-behaved instructions
                                                       decompiled-instructions
                                                       reduced-instructions
                                                       reduced-decompiled-instructions
                                                       context)

        ;; compare their respective runtimes and return the shorter one
        (let ((result-instructions
                (cond
                  ((and decompiled-instructions
                        (< (calculate-instructions-duration reduced-decompiled-instructions chip-specification)
                           (calculate-instructions-duration reduced-instructions chip-specification)))
                   reduced-decompiled-instructions)
                  (t
                   reduced-instructions))))
          (format-quil-sequence *compiler-noise-stream*
                                result-instructions
                                "COMPRESS-INSTRUCTIONS: Replacing the above sequence with the following:~%")
          result-instructions))
    (quil-compression-error (c)
      (let ((*print-circle* nil)
            (*print-pretty* nil)
            (*print-fractional-radians* nil))
        (write-line "A violent error occurred when compressing a subsequence." *error-output*)
        (write-line "The offending subsequence is:" *error-output*)
        (print-code-list instructions *error-output*)
        (write-line "The current compression context is:" *error-output*)
        (princ context *error-output*)
        (finish-output *error-output*))
      (error c))))


(defun compress-instructions-with-possibly-unknown-params (instructions chip-specification context &optional processed-instructions)
  "Dispatch routine for compressing a sequence of INSTRUCTIONS, perhaps with unknown parameter values sprinkled through, based on the routines specified by a CHIP-SPECIFICATION and the current CONTEXT."
  (format-quil-sequence *compiler-noise-stream*
                        instructions
                        "COMPRESS-INSTRUCTIONS: Selected the following sequence for compression:~%")
  ;;
  ;; we can't apply linear algebraic rewriting to instructions with unknown
  ;; parameters, so the plan is to carve up the incoming sequence of INSTRUCTIONS
  ;; into alternating blocks of instructions with known and unknown parameters,
  ;; so that we can interleave linear algebraic rewriting where appropriate and
  ;; also use peephole writing all around.
  ;;
  (unless instructions
    (return-from compress-instructions-with-possibly-unknown-params processed-instructions))
  (labels ((instruction-type (instr)
             (if (every (lambda (p) (typep p 'constant)) (application-parameters instr))
                 ':known-parameters
                 ':unknown-parameters))
           ;; grouping together blocks by their determination type, this extracts
           ;; the top two blocks of instructions
           (grab-first-two-blocks (instructions)
             (let ((first-type (instruction-type (first instructions)))
                   second-type
                   first-block
                   second-block
                   (rest instructions))
               (dolist (instr instructions)
                 (let ((instr-type (instruction-type instr)))
                   (cond
                     ;; if we haven't moved to the second block and we still match the first...
                     ((and (null second-type)
                           (eq first-type instr-type))
                      (push instr first-block))
                     ;; if we haven't moved to the second block and we don't match the first...
                     ((and (null second-type)
                           (not (eq first-type instr-type)))
                      (setf second-type instr-type)
                      (push instr second-block))
                     ;; if we match the second type (and have implicitly moved past the first)...
                     ((eq second-type instr-type)
                      (push instr second-block))
                     ;; if we don't match the second type and have moved past the first...
                     ((and second-type
                           (not (eq second-type instr-type)))
                      (return)))
                   (setf rest (cdr rest))))
               (values (nreverse first-block)
                       (nreverse second-block)
                       rest)))
           (process-mixed-block (instructions)
             (compress-instructions-in-context instructions chip-specification context))
           (process-block (instructions)
             (compress-instructions-in-context instructions chip-specification context)))
    ;; extract the top two blocks of instructions.
    (multiple-value-bind (first-block second-block instr-rest)
        (grab-first-two-blocks instructions)
      ;; if the first block consists of known parameters, run compression on it alone, and substitute in the results.
      (when (eql ':known-parameters (instruction-type (first instructions)))
        (setf first-block (process-block first-block)))
      (unless (and (endp first-block) (endp second-block))
        ;; try to compress across block boundaries
        (multiple-value-bind (processed-first-block processed-second-block processed-remainder)
            (grab-first-two-blocks
             (process-mixed-block (append first-block second-block)))
          (setf first-block processed-first-block)
          (setf second-block processed-second-block)
          (when processed-remainder
            (setf instr-rest (nconc processed-remainder instr-rest)))))

      ;; write out the first resulting block of instructions, and loop.
      (let ((new-context context))
        (dolist (instr first-block)
          (setf new-context
                (update-compressor-context new-context instr
                                           :destructive? nil)))
        (return-from compress-instructions-with-possibly-unknown-params
          (compress-instructions-with-possibly-unknown-params
           (nconc second-block instr-rest)
           chip-specification
           new-context
           (nconc processed-instructions first-block)))))))

(deftype governor-state ()
  "Encodes the state of a governed queue."
  '(member    ; associated contents type:
    :empty    ; nil
    :queueing ; list of instructions
    :passing  ; pair: '(order address), which can be '(:global :global) for global queue
    :flushing ; nil
    :fragile  ; nil
    ))

(defstruct governed-queue
  (state :empty)
  (contents nil)
  (resources (make-null-resource)))

(defun set-gq-fields (queue state contents resources)
  (setf (governed-queue-state queue) state)
  (setf (governed-queue-contents queue) contents)
  (setf (governed-queue-resources queue) resources))

;; the broad strokes of this routine is that there is instructions are loaded
;; into a queueing system based on what chip resources they use, and these queues
;; are coalesced based on the noncommutativity of instructions.  when any queue
;; uses "too many" resources, its content is selected for compression and the
;; results are piped out.
(defun compress-instructions (instructions chip-specification &key (protoquil nil))
  "Compresses a sequence of INSTRUCTIONS based on the routines specified by a CHIP-SPECIFICATION.

This specific routine is the start of a giant dispatch mechanism. Its role is to find SHORT SEQUENCES (so that producing their matrix form is not too expensive) of instructions WHOSE RESOURCES OVERLAP (so that the peephole rewriter stands a chance of finding instructions that cancel)."
  (format *compiler-noise-stream* "COMPRESS-INSTRUCTIONS: entrance.~%")
  ;; set up the places where our state will live
  (let* ((output nil)
         (n-qubits (chip-spec-n-qubits chip-specification))
         (governors (make-list (length (chip-specification-objects chip-specification))))
         (global-governor (make-instance 'governed-queue))
         (context (set-up-compressor-context :qubit-count n-qubits
                                             :simulate (and *enable-state-prep-compression* protoquil)
                                             :chip-specification chip-specification)))
    (labels (;; these are some routines that govern the behavior of the massive FSM
             ;; we're constructing.
             ;;
             ;;
             ;; this is a utility routine that tracks the size of the global queue
             (global-queue-qubit-complex ()
               (let ((ret nil))
                 (dotimes (order (length governors) ret)
                   (dotimes (address (length (nth order governors)))
                     (let ((governor (nth address (nth order governors)))
                           (obj (chip-spec-hw-object chip-specification order address)))
                       (when (and (eq ':passing (governed-queue-state governor))
                                  (equal (list ':global ':global) (governed-queue-contents governor)))
                         (setf ret
                               (union ret
                                      (if (zerop order)
                                          (list address)
                                          (coerce (vnth 0 (hardware-object-cxns obj)) 'list))))))))))

             ;;
             ;; this is a switch block containing all the different governor
             ;; transition logic, depending on the old-state -> new-state pair.
             ;;
             ;; MAXIMS FOR THESE TRANSITION RULES:
             ;; (1) state transition comes FIRST, re/introduction of instructions comes SECOND
             ;; (2) the queue structure should be made sane before and after any recursive call with SETFs.
             ;;     this might mean making a before/after comparison of queueing system state.
             ;; If you don't obey these, you're very likely to introduce subtle bugs.
             (transition-governor-state (order address new-state &optional arg)
               (when (and (typep order 'number) (> order 1))
                 (format *error-output* "WARNING: No support for higher order hardware objects. Compressor queue may behave badly...~%"))
               (let* ((governed-queue
                        (if (eq order ':global)
                            global-governor
                            (nth address (nth order governors))))
                      (old-state (governed-queue-state governed-queue)))
                 (alexandria:eswitch ((list old-state new-state) :test #'equal)
                   ;;
                   ;; QUEUEING --> PASSING(B)
                   ;;
                   ('(:queueing :passing)
                     (when (and (eq order ':global)
                                (eq address ':global))
                       (return-from transition-governor-state))
                     ;; if we're about to target the global queue, we need to
                     ;; make sure that there's room for us there.
                     (when (and (eq (first arg) ':global)
                                (eq (second arg) ':global))
                       (let ((qubit-complex (union (global-queue-qubit-complex)
                                                   (coerce (vnth 0 (hardware-object-cxns
                                                                    (chip-spec-hw-object chip-specification order address)))
                                                           'list))))
                         (when (> (length qubit-complex) *global-queue-tolerance-threshold*)
                           (transition-governor-state ':global ':global ':flushing))))
                     ;; now that we're sure we're OK to forward, do so.
                     (let ((old-contents (governed-queue-contents governed-queue)))
                       (set-gq-fields governed-queue ':passing arg (make-null-resource))
                       ;; if we're a link, make sure our subgovernors are passing too
                       (when (= 1 order)
                         (let* ((subaddresses
                                  (vnth 0 (hardware-object-cxns
                                           (chip-spec-hw-object chip-specification order address))))
                                (left-governor (nth (vnth 0 subaddresses) (first governors)))
                                (right-governor (nth (vnth 1 subaddresses) (first governors)))
                                (left-queue-contents
                                  (when (eq ':queueing (governed-queue-state left-governor))
                                    (governed-queue-contents left-governor)))
                                (right-queue-contents
                                  (when (eq ':queueing (governed-queue-state right-governor))
                                    (governed-queue-contents right-governor))))
                           (set-gq-fields left-governor ':passing '(:global :global) (make-null-resource))
                           (set-gq-fields right-governor ':passing '(:global :global) (make-null-resource))
                           (dolist (instr left-queue-contents)
                             (process-instruction instr '(:global :global)))
                           (dolist (instr right-queue-contents)
                             (process-instruction instr '(:global :global)))))
                       ;; finally, reprocess the contents of this queue
                       (dolist (instr old-contents)
                         (process-instruction instr (first arg) (second arg)))))
                   ;;
                   ;; EMPTY --> PASSING(B)
                   ;;
                   ('(:empty :passing)
                     (setf (governed-queue-state governed-queue) ':passing)
                     (setf (governed-queue-contents governed-queue) arg))
                   ;;
                   ;; EMPTY --> QUEUEING
                   ;;
                   ('(:empty :queueing)
                     (setf (governed-queue-state governed-queue) ':queueing)
                     (setf (governed-queue-contents governed-queue) nil)
                     ;; if we're not the global queue or a bottom queue, then we
                     ;; have children who we need to transition to passing.
                     (when (or (eq order ':global)
                               (= order 0))
                       (return-from transition-governor-state))
                     (let* ((qubit-indices (chip-spec-qubits-on-link chip-specification address))
                            (left-subgovernor (nth (vnth 0 qubit-indices) (nth 0 governors)))
                            (right-subgovernor (nth (vnth 1 qubit-indices) (nth 0 governors)))
                            (forwarding-address (list order address))
                            (qubits-altogether
                              (remove-duplicates
                               (append (coerce qubit-indices 'list)
                                       (when (and (eq ':passing (governed-queue-state left-subgovernor))
                                                  (not (equal (list ':global ':global)
                                                              (governed-queue-contents left-subgovernor))))
                                         (coerce (chip-spec-qubits-on-link chip-specification (second (governed-queue-contents left-subgovernor)))
                                                 'list))
                                       (when (and (eq ':passing (governed-queue-state right-subgovernor))
                                                  (not (equal (list ':global ':global)
                                                              (governed-queue-contents right-subgovernor))))
                                         (coerce (chip-spec-qubits-on-link chip-specification (second (governed-queue-contents right-subgovernor)))
                                                 'list))
                                       (global-queue-qubit-complex)))))
                       ;; if this coalescence would burst the global queue, send
                       ;; the burst signal.
                       (when (< *global-queue-tolerance-threshold* (length qubits-altogether))
                         (transition-governor-state 0 (vnth 0 qubit-indices) ':flushing)
                         (transition-governor-state 0 (vnth 1 qubit-indices) ':flushing))
                       ;; there's an obnoxious edge case here: if the global queue
                       ;; is FRAGILE and we are about to cause a coalescing, then
                       ;; we preempt that with a FLUSH instead.
                       (when (and (or (eq ':fragile (governed-queue-state global-governor))
                                      (eq ':flushing (governed-queue-state global-governor)))
                                  (eq ':passing (governed-queue-state left-subgovernor)))
                         (transition-governor-state 0 (vnth 0 qubit-indices) ':flushing))
                       (transition-governor-state 0 (vnth 0 qubit-indices) ':passing forwarding-address)
                       ;; if just this change caused a coalescence, we need to
                       ;; update our coalescing target to match.
                       (when (not (equal forwarding-address (governed-queue-contents left-subgovernor)))
                         (setf forwarding-address (governed-queue-contents left-subgovernor)))
                       (when (and (or (eq ':fragile (governed-queue-state global-governor))
                                      (eq ':flushing (governed-queue-state global-governor)))
                                  (eq ':passing (governed-queue-state right-subgovernor)))
                         (transition-governor-state 0 (vnth 1 qubit-indices) ':flushing))
                       (transition-governor-state 0 (vnth 1 qubit-indices) ':passing forwarding-address)
                       ;; if this second operation just put us into a weird state,
                       ;; try the whole operation again.
                       (when (or (not (equal forwarding-address (governed-queue-contents right-subgovernor)))
                                 (not (equal forwarding-address (list order address))))
                         (transition-governor-state order address ':passing (governed-queue-contents right-subgovernor)))))
                   ;;
                   ;; PASSING(A) --> PASSING(B)
                   ;;
                   ('(:passing :passing)
                     ;; if this is secretly the trivial transition, do nothing
                     (when (equal (governed-queue-contents governed-queue) arg)
                       (return-from transition-governor-state))
                     ;; tell our current pointer that we're coalescing into the global queue
                     (let ((address-pair (governed-queue-contents governed-queue)))
                       ;; set ourselves up
                       (set-gq-fields governed-queue ':passing arg (make-null-resource))
                       (transition-governor-state (first address-pair) (second address-pair) ':passing '(:global :global)))
                     ;; tell our new target that it too is coalescing into the global queue
                     (transition-governor-state (first arg) (second arg) ':passing '(:global :global))
                     ;; if we have children...
                     (when (or (eq order ':global) (= 0 order))
                       (return-from transition-governor-state))
                     ;; .. then tell them that they are coalescing into the global queue
                     (let* ((subaddresses (chip-spec-qubits-on-link chip-specification address))
                            (left-governor (nth (first subaddresses) (second governors)))
                            (right-governor (nth (second subaddresses) (second governors)))
                            (left-queue-contents
                              (when (eq ':queueing (governed-queue-state left-governor))
                                (governed-queue-contents left-governor)))
                            (right-queue-contents
                              (when (eq ':queueing (governed-queue-state right-governor))
                                (governed-queue-contents right-governor))))
                       (set-gq-fields left-governor ':passing '(:global :global) (make-null-resource))
                       (set-gq-fields right-governor ':passing '(:global :global) (make-null-resource))
                       (dolist (instr left-queue-contents)
                         (process-instruction instr ':global ':global))
                       (dolist (instr right-queue-contents)
                         (process-instruction instr ':global ':global))))
                   ;;
                   ;; PASSING(A) --> FLUSHING
                   ;;
                   ('(:passing :flushing)
                     (let ((address-pair (governed-queue-contents governed-queue)))
                       (set-gq-fields governed-queue ':empty nil (make-null-resource))
                       (transition-governor-state (first address-pair) (second address-pair) ':flushing)))
                   ;;
                   ;; EMPTY --> FLUSHING
                   ;;
                   ('(:empty :flushing)
                     t)
                   ;;
                   ;; FRAGILE --> EMPTY
                   ;;
                   ('(:fragile :empty)
                     (set-gq-fields governed-queue ':empty nil (make-null-resource)))
                   ;;
                   ;; EMPTY --> FRAGILE
                   ;;
                   ('(:empty :fragile)
                     (set-gq-fields governed-queue ':fragile nil (make-null-resource)))
                   ;;
                   ;; FLUSHING --> FRAGILE
                   ;;
                   ('(:flushing :fragile)
                     (set-gq-fields governed-queue ':fragile nil (make-null-resource)))
                   ;;
                   ;; FRAGILE --> FLUSHING
                   ;;
                   ('(:fragile :flushing)
                     (set-gq-fields governed-queue ':flushing nil (make-null-resource))
                     ;; now we need to flush all our children
                     (cond
                       ;; if we are the global pseudodevice...
                       ((eq order ':global)
                        ;; flush all of the hardware objects whose qubit complexes are a subset of ours
                        (let ((qubit-complex (global-queue-qubit-complex)))
                          (dotimes (suborder (length (chip-specification-objects chip-specification)))
                            (dotimes (subaddress (length (vnth suborder (chip-specification-objects chip-specification))))
                              (when (subsetp (cond
                                               ((= suborder 0)
                                                (list subaddress))
                                               ((= suborder 1)
                                                (chip-spec-qubits-on-link chip-specification subaddress)))
                                             qubit-complex)
                                (transition-governor-state suborder subaddress ':flushing))))))
                       ;; if we are a physical hardware device...
                       (t
                        (let ((obj (chip-spec-hw-object chip-specification order address)))
                          (dotimes (suborder order)
                            (dolist (subobj-index (coerce (vnth suborder (hardware-object-cxns obj)) 'list))
                              (transition-governor-state suborder subobj-index ':flushing)))))))
                   ;;
                   ;; QUEUEING --> FLUSHING
                   ;;
                   ('(:queueing :flushing)
                     (let ((compressed-instructions
                             (compress-instructions-with-possibly-unknown-params
                              (governed-queue-contents governed-queue)
                              chip-specification
                              context)))
                       ;; set all of the hardware devices that we subsume to empty
                       (cond
                         ;; if we are the global pseudodevice...
                         ((eq order ':global)
                          (let ((qubit-complex (global-queue-qubit-complex)))
                            (dotimes (suborder (length (chip-specification-objects chip-specification)))
                              (dotimes (subaddress (length (vnth suborder (chip-specification-objects chip-specification))))
                                (let ((subobj (chip-spec-hw-object chip-specification suborder subaddress))
                                      (subgovernor (nth subaddress (nth suborder governors))))
                                  (when (subsetp (if (= suborder 0)
                                                     (list subaddress)
                                                     (coerce (vnth 0 (hardware-object-cxns subobj)) 'list))
                                                 qubit-complex)
                                    (assert (not (typep (first (governed-queue-contents subgovernor)) 'application)))
                                    (set-gq-fields subgovernor ':empty nil (make-null-resource))))))))
                         ;; otherwise, we are a physical hardware device...
                         (t
                          (let ((obj (vnth address (vnth order (chip-specification-objects chip-specification)))))
                            (dotimes (suborder order)
                              (dolist (subaddress (coerce (vnth suborder (hardware-object-cxns obj)) 'list))
                                (let ((subgovernor (nth subaddress (nth suborder governors))))
                                  (assert (not (typep (first (governed-queue-contents subgovernor)) 'application)))
                                  (set-gq-fields subgovernor ':empty nil (make-null-resource))))))))
                       ;; now set us up as fragile and start dumping instructions in
                       (set-gq-fields governed-queue ':fragile nil (make-null-resource))
                       (dolist (instr compressed-instructions)
                         (process-instruction instr))
                       (set-gq-fields governed-queue ':empty nil (make-null-resource)))))))

             ;;
             ;; this routine handles the task of adding an instruction to the
             ;; relevant queue.
             (process-instruction (instr &optional order address)
               (cond
                 ;;
                 ;; the case of a blocking global instruction
                 ;;
                 ((global-instruction-p instr)
                  ;; flush everybody
                  (transition-governor-state ':global ':global ':flushing)
                  (loop
                    :for order
                    :from (1- (length (chip-specification-objects chip-specification)))
                    :downto 0
                    :do (dotimes (address (length (nth order governors)))
                          (transition-governor-state order address ':flushing)))
                  (update-compressor-context context instr :destructive? t)
                  ;; and output this instruction
                  (push instr output))

                 ;;
                 ;; the case of a local instruction with hybrid effects
                 ;;
                 ((or (local-classical-quantum-instruction-p instr)
                      (local-classical-instruction-p instr)
                      (typep instr 'measure-discard)
                      (typep instr 'reset-qubit))
                  ;; flush the relevant hardware objects
                  (let ((resources (instruction-resources instr)))
                    (when (resources-intersect-p resources (governed-queue-resources global-governor))
                      (transition-governor-state ':global ':global ':flushing)
                      (unless (eq (governed-queue-state global-governor) ':empty)
                        (transition-governor-state ':global ':global ':flushing)))
                    (loop :for order
                          :from (1- (length (chip-specification-objects chip-specification)))
                          :downto 0
                          :do (dotimes (address (length (nth order governors)))
                                (let ((governed-queue (nth address (nth order governors))))
                                  (when (resources-intersect-p resources
                                                               (governed-queue-resources
                                                                governed-queue))
                                    (transition-governor-state order address ':flushing)
                                    ;; if we were passing, this might be a two-flusher.
                                    (unless (eq (governed-queue-state governed-queue) ':empty)
                                      (transition-governor-state order address ':flushing))))))
                    (update-compressor-context context instr :destructive? t))
                  ;; and write out the instruction
                  (push instr output))

                 ;;
                 ;; the case of a local purely quantum instruction
                 ;;
                 (t
                  (unless (and order address)
                    (multiple-value-bind (lookup-order lookup-address)
                        (lookup-hardware-address chip-specification instr)
                      (setf order lookup-order)
                      (setf address lookup-address)))
                  (let* ((governed-queue
                           (if (eq order ':global)
                               global-governor
                               (nth address (nth order governors))))
                         (old-state (governed-queue-state governed-queue)))
                    (ecase old-state
                      (:empty
                       (transition-governor-state order address ':queueing)
                       (process-instruction instr order address))
                      (:passing
                       (apply #'process-instruction instr
                              (governed-queue-contents governed-queue)))
                      (:flushing
                       (update-compressor-context context instr :destructive? t)
                       (push instr output))
                      (:fragile
                       (transition-governor-state order address ':flushing)
                       (process-instruction instr order address)
                       (transition-governor-state order address ':fragile))
                      (:queueing
                       (setf (governed-queue-resources governed-queue)
                             (resource-union (governed-queue-resources governed-queue)
                                             (instruction-resources instr)))
                       (postpend instr (governed-queue-contents governed-queue)))))))))

      ;; set up the queue governors
      (setf governors
            (loop
              :for order :below (length (chip-specification-objects chip-specification))
              :collect (loop
                         :repeat (length (vnth order (chip-specification-objects chip-specification)))
                         :collect (make-governed-queue))))
      ;; iterate over the incoming instructions
      (dolist (instr instructions)
        (process-instruction instr)
        (clean-up-compressor-context context :destructive? t))
      ;; we're done processing the instructions, but the queueing system might
      ;; still have gunk left in it.  flush all of the governors, biggest first
      (transition-governor-state ':global ':global ':flushing)
      (loop
        :for order
        :from (1- (length (chip-specification-objects chip-specification)))
        :downto 0
        :do (dotimes (address (length (nth order governors)))
              (transition-governor-state order address ':flushing))))
    (format *compiler-noise-stream*
            "COMPRESS-INSTRUCTIONS: departure.~%")
    (nreverse output)))



;; this is a debug routine used to see the current state of the
;; queueing system.
(defun print-queue-state (governors global-governor)
  (dotimes (order (length governors))
    (dotimes (address (length (nth order governors)))
      (let ((governor (nth address (nth order governors))))
        (format *standard-output* "(~a, ~a) : ~a / ~a / ~a~%" order address
                (governed-queue-resources governor)
                (governed-queue-state governor)
                (if (typep (first (governed-queue-contents governor)) 'application)
                    (with-output-to-string (s)
                      (dolist (instr (governed-queue-contents governor))
                        (terpri s)
                        (print-instruction instr s)))
                    (governed-queue-contents governor))))))
  (format *standard-output* "(~a, ~a) : ~a / ~a / ~a ~%" ':global ':global
          (governed-queue-resources global-governor)
          (governed-queue-state global-governor)
          (if (typep (first (governed-queue-contents global-governor)) 'application)
              (with-output-to-string (s)
                (dolist (instr (governed-queue-contents global-governor))
                  (terpri s)
                  (print-instruction instr s)))
              (governed-queue-contents global-governor))))
