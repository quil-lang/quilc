(in-package #:cl-quil.frontend)

(define-transform process-protoquil (process-protoquil)
  "Removes HALT, DEFCIRCUIT, and DEFGATE instructions.")

(defun process-protoquil (parsed-program)
  (setf (parsed-program-circuit-definitions parsed-program) nil
        (parsed-program-gate-definitions parsed-program) nil)

  ;; if we're supposed to output protoQuil, we also need to
  ;; strip the final HALT instructions from the output
  (setf (parsed-program-executable-code parsed-program)
        (strip-final-halt-respecting-rewirings parsed-program)))

(defun strip-final-halt-respecting-rewirings (processed-program)
  "Remove the final HALT instruction, if any, from PROCESSED-PROGRAM, retaining any attached rewiring comments."
  (let* ((instructions (parsed-program-executable-code processed-program))
         (last-instruction (and (plusp (length instructions))
                                (cl-quil::nth-instr 0 processed-program :from-end t)))
         (penultimate-instruction (and (< 1 (length instructions))
                                       (cl-quil::nth-instr 1 processed-program :from-end t)))
         (must-transfer-comment-p (and (not (null penultimate-instruction))
                                       (comment last-instruction))))

    (unless (cl-quil::haltp last-instruction)
      (return-from strip-final-halt-respecting-rewirings instructions))

    (when must-transfer-comment-p
      ;; Transfer the rewiring comment from LAST-INSTRUCTION to
      ;; PENULTIMATE-INSTRUCTION.
      (multiple-value-bind (last-entering last-exiting)
          (cl-quil::instruction-rewirings last-instruction)
        (multiple-value-bind (penultimate-entering penultimate-exiting)
            (cl-quil::instruction-rewirings penultimate-instruction)
          (flet ((assert-rewirings-compatible (rewiring-type last-rewiring penultimate-rewiring)
                   ;; This bit of hoop-jumping guards against the
                   ;; unlikely event that both PENULTIMATE-INSTRUCTION
                   ;; and LAST-INSTRUCTION have rewiring comments
                   ;; attached which might be incompatible. We check
                   ;; to ensure that either one of the rewirings is
                   ;; NULL, or else they are EQUALP and can safely be
                   ;; merged.
                   (assert (or (or (null last-rewiring)
                                   (null penultimate-rewiring))
                               (equalp last-rewiring penultimate-rewiring))
                       ()
                       "Failed to strip final HALT. Instructions have incompatible ~A rewirings:~@
                           LAST: ~A ~A~@
                           PREV: ~A ~A"
                       rewiring-type last-instruction last-rewiring
                       penultimate-instruction penultimate-rewiring)))
            (assert-rewirings-compatible ':ENTERING last-entering penultimate-entering)
            (assert-rewirings-compatible ':EXITING last-exiting penultimate-exiting))
          ;; Consider the following cases for the :ENTERING rewirings
          ;; (the same case analysis applies to the :EXITING rewiring
          ;; pair as well).
          ;;
          ;; 1) If both the rewirings are non-NIL, then the
          ;;    ASSERT-REWIRINGS-COMPATIBLE check above guarantees
          ;;    that they are EQUALP, and it doesn't matter which one
          ;;    we select.
          ;;
          ;; 2) If only one is non-NIL, the OR selects it.
          ;;
          ;; 3) If both are NIL, then MAKE-REWIRING-COMMENT just
          ;;    ignores that keyword argument, and returns an :EXITING
          ;;    rewiring.
          ;;
          ;; Finally, (COMMENT LAST-INSTRUCTION) is non-NIL (otherwise
          ;; MUST-TRANSFER-COMMENT-P would be NIL), so at least one of
          ;; LAST-ENTERING and LAST-EXITING is non-NIL, which means
          ;; that at least one of the :ENTERING and :EXITING keyword
          ;; args to MAKE-REWIRING-COMMENT is non-NIL and hence the
          ;; call will produce a rewiring comment.
          (setf (comment penultimate-instruction)
                (cl-quil::make-rewiring-comment :entering (or last-entering penultimate-entering)
                                                :exiting (or last-exiting penultimate-exiting))))))

    ;; Strip the final HALT instruction.
    (subseq instructions 0 (1- (length instructions)))))
