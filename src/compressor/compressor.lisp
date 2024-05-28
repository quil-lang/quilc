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

(defun calculate-instructions-2q-depth (instructions)
  (let ((lschedule (make-lschedule)))
    (append-instructions-to-lschedule lschedule instructions)
    (or (lschedule-walk-graph lschedule
                               :base-value 0
                               :bump-value (lambda (instr value)
                                             (if (and (typep instr 'gate-application)
                                                      (< 1 (length (application-arguments instr))))
                                                 (1+ value)
                                                 value))
                               :combine-values #'max)
        0)))

(defun calculate-instructions-duration (instructions chip-specification)
  "Calculates the runtime of a sequence of native INSTRUCTIONS on a chip with architecture governed by CHIP-SPECIFICATION (and with assumed perfect parallelization across resources)."
  (let ((lschedule (make-lschedule)))
    ;; load up the logical schedule
    (append-instructions-to-lschedule lschedule instructions)
    ;; sift through it for durations
    (lschedule-calculate-duration lschedule chip-specification)))


(defun calculate-instructions-fidelity (instructions chip-specification)
  "Calculates the fidelity of a sequence of native INSTRUCTIONS on a chip with architecture governed by CHIP-SPECIFICATION (and with assumed perfect parallelization across resources).

The fidelity returned is a real (double-float) number in the interval [0.0, 1.0]"
  (flet ((instr-fidelity (instr)
           (get-instruction-fidelity instr chip-specification))) 
    (reduce #'min instructions :key #'instr-fidelity :initial-value 1.0d0)))

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
  (format t "~30A <-- ~30A --> ~30A~%"
          (a:when-let ((prev (peephole-rewriter-node-prev node)))
            (peephole-rewriter-node-instr prev))
          (peephole-rewriter-node-instr node)
          (a:when-let ((next (peephole-rewriter-node-next node)))
            (peephole-rewriter-node-instr next)))
  (print-node-list (peephole-rewriter-node-next node)))

(defun algebraically-reduce-instructions (instructions
                                          chip-specification
                                          context)
  "Applies peephole rewriter rules from a CHIP-SPECIFICATION to a sequence of INSTRUCTIONS, using CONTEXT to activate context-sensitive rules."
  (labels
      (;; utility for calculating how many instructions a rewriting rule requests
       (rewriting-rule-count (compiler)
         (length (compiler-bindings compiler)))

       ;; let the context know that we've passed inspection of NODE, so that the
       ;; effect of that instruction is visible during inspection of the next node
       (update-context (node)
         (setf (peephole-rewriter-node-context node)
               (update-compilation-context (peephole-rewriter-node-context
                                            (peephole-rewriter-node-prev node))
                                           (peephole-rewriter-node-instr node)
                                           :destructive? nil)))

       ;; having selected an appropriate sequence of instructions, actually
       ;; apply the available rewriting rules. if we find one that applies,
       ;; splices the results in to replace the nodes and return the location
       ;; of the new node (so that the outer loop can rewind). if none applies,
       ;; announce failure (so that the outer loop can step ahead by one).
       (apply-rules (rewrite-rules nodes-for-inspection)
         (let* ((number-of-nodes-for-inspection (length nodes-for-inspection))
                (subsequences-for-inspection (make-array (1+ number-of-nodes-for-inspection)
                                                         :initial-element nil)))
           (loop :for rule :across rewrite-rules
                 :for rule-count := (rewriting-rule-count rule) :do
             ;; make sure we have enough terms, then apply the rule's consumer
             (with-simple-restart (try-next-compiler "Ignore this error and try the next rewrite rule in the list.")
               (handler-case
                   (a:when-let*
                       ((relevant-nodes-for-inspection
                         (and (>= number-of-nodes-for-inspection rule-count)
                              (or (aref subsequences-for-inspection rule-count)
                                  (setf (aref subsequences-for-inspection rule-count)
                                        (subseq nodes-for-inspection 0 rule-count))))))
                     (let ((output
                             (apply rule
                                    (append
                                     (mapcar #'peephole-rewriter-node-instr relevant-nodes-for-inspection)
                                     (list ':context (peephole-rewriter-node-context
                                                      (peephole-rewriter-node-prev
                                                       (first relevant-nodes-for-inspection))))))))
                       (format-noise
                        "ALGEBRAICALLY-REDUCE-INSTRUCTIONS: Applying the rewriting rule called ~A."
                        (compiler-name rule))
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
                 (compiler-does-not-apply () nil))))))

       ;; main loop for the peephole rewriter. for any particular node, it
       ;; assembles a list of instructions which might be subject to rewriting
       ;; rules, then passes those to APPLY-RULES. if APPLY-RULES returns
       ;; successfully, we rewind by the peephole window and try again. if it
       ;; fails, we fall through, step through to the next node, and try again.
       (outer-instruction-loop (node)
         (do (;; for each instruction...
              (node #1=(peephole-rewriter-node-next node) #1#))
             ((null node))
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
                   (go :ADVANCE-NODE))
                 ;; does the next noncommuting instruction yield some new set of rules?
                 ;; if not, skip it.
                 (when (subsetp new-complex qubit-complex)
                   (return))
                 ;; enlarge the complex
                 (setf qubit-complex (union qubit-complex new-complex))
                 ;; try to find an associated hardware object for this complex
                 (let ((obj (lookup-hardware-object-by-qubits chip-specification qubit-complex)))
                   ;; if we can, then we want to loop over the object's rewrite rules.
                   ;; if we can't, we fall through and do nothing.
                   (a:when-let
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
                         (setf node node-to-jump-to)
                         (go :ADVANCE-NODE))))))))
          :ADVANCE-NODE
          nil)))

    ;; strip out all the NOPs.
    (let* ((instructions (remove-if (lambda (x) (typep x 'no-operation)) instructions))
           (head (instrs->peephole-rewriter-nodes instructions)))

      ;; actually, we need to prepend a dummy head
      (setf head (make-peephole-rewriter-node :instr (build-gate "Z" () 0)
                                              :next head
                                              :context context))
      (setf (peephole-rewriter-node-prev (peephole-rewriter-node-next head)) head)

      (outer-instruction-loop head)
      ;; when we make it to this point, no rewrite rules apply, so quit.
      (peephole-rewriter-nodes->instrs (peephole-rewriter-node-next head)))))

(defgeneric expand-instruction-to-native-instructions (instr chip)
  (:documentation "Expand INSTR into a list of instructions native for CHIP by applying nativization routines available on CHIP.")
  (:method ((instr instruction) chip)
    (list instr))

  (:method ((instr no-operation) chip)
    ;; A NO-OPERATION is not native, and should be popped from the
    ;; instruct list (i.e. does not expand into any instructions).
    nil)

  (:method ((instr gate-application) chip)
    (let ((obj (lookup-hardware-object chip instr)))
      (if (and obj (hardware-object-native-instruction-p obj instr))
          (list instr)
          (let ((instructions (apply-translation-compilers instr chip obj)))
            (check-instructions-matrix-consistency (list instr) instructions)
            (loop :for new-instruction :in instructions
                  :nconc (expand-instruction-to-native-instructions new-instruction chip)))))))

(defun expand-to-native-instructions (instructions chip)
  "Expand INSTRUCTIONS into a list of instructions that are native for CHIP. Makes no attempt to perform rewiring or simplication."
  (a:mappend (a:rcurry #'expand-instruction-to-native-instructions chip) instructions))

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
           (a:when-let ((matrix (make-matrix-from-quil
                                 instructions
                                 :relabeling (standard-qubit-relabeler qubits-on-obj))))
             (expand-to-native-instructions
              (list (apply 'anon-gate (append (list "UNITARY" matrix) (mapcar #'qubit qubits-on-obj))))
              chip-specification))))

      (destructuring-bind (start-wf wf-qc)
          (if (compilation-context-aqvm context)
              (aqvm-extract-state (compilation-context-aqvm context) qubits-on-obj)
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

(define-condition state-prep-compression-tolerance-error (error)
  ((compilation-tolerance :type real :initarg :compilation-tolerance :reader state-prep-compression-tolerance-error-tolerance)
   (compilation-precision :type real :initarg :compilation-precision :reader state-prep-compression-tolerance-error-precision))
  (:documentation "This error is raised when the state vector produced by state-preparation compilation is not within some tolerance.")
  (:report (lambda (c s)
             (format s "The state vector produced by state-preparation compilation was found to be not colinear with the state vector produced by standard compilation. Compilation has a tolerance of ~A but compilation had a precision of ~A."
                     (state-prep-compression-tolerance-error-tolerance c)
                     (state-prep-compression-tolerance-error-precision c)))))

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
  (let ((qubits-on-obj (qubits-in-instr-list (append decompiled-instructions instructions))))
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
                                   (ilog2 (magicl:nrows stretched-matrix))))
                  (reduced-decompiled-matrix
                   (kron-matrix-up (make-matrix-from-quil reduced-decompiled-instructions
                                                          :relabeling relabeling)
                                   (ilog2 (magicl:nrows stretched-matrix)))))
             (assert (matrix-equality stretched-matrix
                                      (scale-out-matrix-phases reduced-matrix
                                                               stretched-matrix)))
             (when decompiled-instructions
               (assert (matrix-equality stretched-matrix
                                        (scale-out-matrix-phases decompiled-matrix
                                                                 stretched-matrix)))
               (assert (matrix-equality stretched-matrix
                                        (scale-out-matrix-phases reduced-decompiled-matrix
                                                                 stretched-matrix))))))

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
                                          wf-qc)))
             (assert final-wf-reduced-instrs-collinearp
                     (final-wf final-wf-reduced-instrs)
                     "During careful checking of instruction compression, the produced ~
                   wavefunction by instruction reduction was detected to not be ~
                   collinear with the target wavefunction.")
             (unless (or (null decompiled-instructions)
                         (and (eql final-wf ':not-simulated)
                              (eql final-wf-reduced-prep ':not-simulated)))
               (multiple-value-bind (col precision)
                   (collinearp final-wf final-wf-reduced-prep)
                 (unless col
                   (cerror "continue with possibly incorrect compilation"
                           'state-prep-compression-tolerance-error
                           :compilation-tolerance +double-comparison-threshold-strict+
                           :compilation-precision precision))))))

         (check-quil-is-near-as-matrices ()
           (a:when-let ((stretched-matrix (make-matrix-from-quil instructions)))
             (let* ((n (ilog2 (magicl:nrows stretched-matrix)))
                    (reduced-matrix
                     (kron-matrix-up (make-matrix-from-quil reduced-instructions)
                                     (ilog2 (magicl:nrows stretched-matrix))))
                    (reduced-decompiled-matrix
                     (kron-matrix-up (make-matrix-from-quil reduced-decompiled-instructions)
                                     (ilog2 (magicl:nrows stretched-matrix)))))
               (assert (matrix-equality stretched-matrix
                                        (scale-out-matrix-phases reduced-matrix stretched-matrix)))
               (when decompiled-instructions
                 (let* ((prod (magicl:@
                               reduced-matrix (magicl:dagger reduced-decompiled-matrix)))
                        (tr (magicl:trace prod))
                        (trace-fidelity (/ (+ n (abs (* tr tr)))
                                           (+ n (* n n))))
                        (chip-spec (compilation-context-chip-specification context)))
                   (assert (>= (* trace-fidelity
                                  (calculate-instructions-fidelity reduced-decompiled-instructions chip-spec))
                               (calculate-instructions-fidelity reduced-instructions chip-spec))
                           ()
                           "During careful checking of instruction compression, ~
                            the recomputed instruction sequence has an ~
                            unreasonably large a fidelity drop from the original ~
                            sequence.")))))))

      (destructuring-bind (start-wf wf-qc)
          (aqvm-extract-state (compilation-context-aqvm context) qubits-on-obj)
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
  (handler-bind ((error (lambda (c)
                          (declare (ignore c))
                          (let ((*print-circle* nil)
                                (*print-pretty* nil)
                                (*print-fractional-radians* nil))
                            (write-line "A violent error occurred when compressing a subsequence." *error-output*)
                            (write-line "The offending subsequence is:" *error-output*)
                            (print-code-list instructions *error-output*)
                            (write-line "The current compression context is:" *error-output*)
                            (princ context *error-output*)
                            (finish-output *error-output*)
                            ;; We explicitly do *NOT* handle the error
                            ;; here. We just want to indicate to the
                            ;; user that an error happened down here.
                            ))))
    ;; start by making a decision about how we're going to do linear algebraic compression
    (let ((decompiled-instructions (decompile-instructions-in-context instructions
                                                                      chip-specification
                                                                      context))
          reduced-instructions
          reduced-decompiled-instructions)

      ;; now proceed to do the reductions
      (format-noise "COMPRESS-INSTRUCTIONS: Applying algebraic rewrites to original string.")
      (setf reduced-instructions
            (algebraically-reduce-instructions instructions chip-specification context))
      (format-noise "COMPRESS-INSTRUCTIONS: Applying algebraic rewrites to recompiled string.")
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
                ((null decompiled-instructions)
                 reduced-instructions)
                (t
                 (let ((decompiled-fidelity
                         (calculate-instructions-fidelity reduced-decompiled-instructions chip-specification))
                       (original-fidelity
                         (calculate-instructions-fidelity reduced-instructions chip-specification)))
                   (cond
                     ((> decompiled-fidelity original-fidelity)
                      reduced-decompiled-instructions)
                     ((< decompiled-fidelity original-fidelity)
                      reduced-instructions)
                     ;; Sometimes the fidelities are the same, but one
                     ;; sequence is longer. This can happen if, for
                     ;; instance, a chip has a perfect-fidelity RZ
                     ;; gate which appears when we decompile. See
                     ;; issue #801.
                     (t
                      ;; Style note: We don't extend the COND above
                      ;; with this IF condition for clarity.
                      (if (< (length reduced-instructions)
                             (length reduced-decompiled-instructions))
                          reduced-instructions
                          reduced-decompiled-instructions))))))))
        (when *compiler-noise*
          (format-quil-sequence *compiler-noise*
                                result-instructions
                                "COMPRESS-INSTRUCTIONS: Replacing the above sequence with the following:~%"))
        result-instructions))))


(defun compress-instructions-with-possibly-unknown-params (instructions chip-specification context &optional processed-instructions)
  "Dispatch routine for compressing a sequence of INSTRUCTIONS, perhaps with unknown parameter values sprinkled through, based on the routines specified by a CHIP-SPECIFICATION and the current CONTEXT."
  (when *compiler-noise*
    (format-quil-sequence *compiler-noise*
                          instructions
                          "COMPRESS-INSTRUCTIONS: Selected the following sequence for compression:~%"))
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
             (when (endp instructions)
               (return-from grab-first-two-blocks nil))
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
                (update-compilation-context new-context instr :destructive? nil)))
        (return-from compress-instructions-with-possibly-unknown-params
          (compress-instructions-with-possibly-unknown-params
           (nconc second-block instr-rest)
           chip-specification
           new-context
           (nconc processed-instructions first-block)))))))

(defstruct compression-queue
  (resources (make-null-resource) :type resource-collection)
  (contents  nil))

(defun compress-instructions (instructions chip-specification
                              &key protoquil
                                (queue-tolerance-threshold *global-queue-tolerance-threshold*)
                                ((:context context-arg)))
  "Compresses a sequence of INSTRUCTIONS based on the routines specified by a CHIP-SPECIFICATION.

Its role is to find SHORT SEQUENCES (so that producing their matrix form is not too expensive) of instructions WHOSE RESOURCES OVERLAP (so that the peephole rewriter stands a chance of finding instructions that cancel)."
  (format-noise "COMPRESS-INSTRUCTIONS: entrance.")
  (let* (output             ; instructions to return
         compression-queues ; Each queue is a list of instructions. Resources used by each queue do not overlap.
         (toplevel? (not context-arg))
         (context (if toplevel?
                      (set-up-compilation-context :qubit-count (chip-spec-n-qubits chip-specification)
                                                  :simulate (and *enable-state-prep-compression* protoquil)
                                                  :chip-specification chip-specification)
                      context-arg)))
    (labels ((compression-queue-num-qubits (compression-queue)
               "How many qubits this compression queue's resources involve"
               (length (cl-quil.resource::resource-qubits-list (compression-queue-resources compression-queue))))

             (find-queues-by-resources (resources)
               "Find all existing queues whose resources intersect with the given ones"
               (remove-if-not (a:curry #'resources-intersect-p resources)
                              compression-queues
                              :key #'compression-queue-resources))

             (merge-queue (base-queue other-queue)
               "Merge OTHER-QUEUE into BASE-QUEUE, modifying BASE-QUEUE in place. Places the contents/instructions of OTHER-QUEUE after those of BASE-QUEUE"
               (a:appendf (compression-queue-contents base-queue) (compression-queue-contents other-queue))
               (setf (compression-queue-resources base-queue)
                     (resource-union (compression-queue-resources base-queue)
                                     (compression-queue-resources other-queue)))
               base-queue)

             (merge-queues (queues)
               "Non-destructively merge multiple queues."
               (reduce #'merge-queue queues :initial-value (make-compression-queue)))

             (flush-queue (queue)
               "Compresses the queue's contents, then recursively re-compresses them with a smaller queue-tolerance-threshold. This helps apply rewrite rules that work on small numbers of qubits which might fail to match the larger sequence of instructions. Any instructions which follow the recursion all the way down to the base case are pushed to OUTPUT. FLUSH-QUEUE returns a list of queues of instructions that did not get flushed all the way to OUTPUT as a result of recursion."
               (let* ((once-compressed-instructions
                        (compress-instructions-with-possibly-unknown-params
                         (compression-queue-contents queue)
                         chip-specification
                         context)))
                 (cond
                   ;; Base case
                   ((or (= 1 (compression-queue-num-qubits queue))
                        (null once-compressed-instructions))
                    (dolist (instr once-compressed-instructions)
                      (update-compilation-context context instr :destructive? t)
                      (push instr output)))

                   ;; Queue has width>1, may make some headway by re-compressing subsequences that
                   ;; involve fewer qubits
                   (t
                    (multiple-value-bind (r-output r-queues)
                        ;; After running compression at QUEUE-TOLERANCE-THRESHOLD, it's desirable to
                        ;; shrink the queue width down to 2 immediately because no new
                        ;; compilers/reducers will be introduced until the queue width is shrunk
                        ;; down to a hardware object. While it is theoretically possible for a
                        ;; general compiler to apply at queue width 3 but not at queue width 4,
                        ;; (because a slightly different set of instructions will be selected for
                        ;; compilation), it's highly unlikely and generally not worth the large
                        ;; performance impact
                        (compress-instructions once-compressed-instructions chip-specification
                                               :protoquil protoquil
                                               :queue-tolerance-threshold (min 2 (1- (compression-queue-num-qubits queue)))
                                               :context context)
                      (dolist (instr r-output)
                        (push instr output))
                      r-queues)))))

             (flush-queue-in-place (queue)
               "Like FLUSH-QUEUE, but removes queue from COMPRESSION-QUEUES and re-inserts new ones as needed."
               (setf compression-queues (remove queue compression-queues))
               (a:appendf compression-queues (flush-queue queue)))

             (fully-flush-queue (queue)
               "Like FLUSH-QUEUE, but recursively flushes sub-queues instead of returning them."
               (map nil #'fully-flush-queue (flush-queue queue)))

             (fully-flush-queue-in-place (queue)
               (setf compression-queues (remove queue compression-queues))
               (fully-flush-queue queue))

             (instruction-forces-flush-p (instr)
               "Whether all instructions using overlapping resources to this instruction should be flushed before processing this instruction."
               (or (global-instruction-p instr)
                   (local-classical-quantum-instruction-p instr)
                   (local-classical-instruction-p instr)
                   (typep instr 'measure-discard)
                   (typep instr 'reset-qubit)
                   (> (length (cl-quil.resource::resource-qubits-list (instruction-resources instr)))
                      queue-tolerance-threshold)))

             (process-instruction (instr)
               (let* ((resources (instruction-resources instr))
                      (existing-intersecting-queues (find-queues-by-resources resources)))
                 (cond
                   ;; Global or hybrid instruction: Flush and remove all related queues.
                   ((instruction-forces-flush-p instr)
                    (map nil #'fully-flush-queue-in-place existing-intersecting-queues)
                    (update-compilation-context context instr :destructive? t)
                    (push instr output))

                   ;; Local pure-quantum instruction: Merge related queues together
                   (t
                    (let* ((new-queue (make-compression-queue :resources (instruction-resources instr)
                                                              :contents (list instr)))
                           (combined-queue (merge-queues `(,@existing-intersecting-queues ,new-queue))))
                      (cond
                        ;; If this instruction causes a queue to become too large, then flush the
                        ;; queue (splits it into smaller queues) and retry. If this happens
                        ;; repeatedly, eventually the old queue will disappear completely, and the
                        ;; new instruction will fit because we already checked
                        ;; (instruction-forces-flush-p instr) = nil
                        ((< queue-tolerance-threshold (compression-queue-num-qubits combined-queue))
                         (flush-queue-in-place (a:extremum existing-intersecting-queues #'>
                                                           :key #'compression-queue-num-qubits))
                         (process-instruction instr))
                        (t
                         (setf compression-queues (set-difference compression-queues existing-intersecting-queues))
                         (push combined-queue compression-queues)))))))
               (when toplevel?
                 (clean-up-compilation-context context :destructive? t))))

      (map nil #'process-instruction instructions)
      (cond
        (toplevel?
         (map nil #'fully-flush-queue compression-queues)
         (format-noise "COMPRESS-INSTRUCTIONS: departure")
         (nreverse output))
        (t
         (values (nreverse output) compression-queues))))))
