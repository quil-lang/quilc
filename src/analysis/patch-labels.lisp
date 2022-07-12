;;;; patch-labels.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil/frontend)

(define-transform patch-labels (patch-labels)
  "This transform resolves labels. Label positions are calculated, and corresponding JUMP instruction labels are patched with their absolute position in a program.

This changes the AST!

    JUMP-LABEL : JUMP -> LABEL

becomes

    JUMP-LABEL : JUMP -> non-negative integer.")

(defun jump-target-positions (code)
  (loop :with positions := (make-hash-table :test 'equal)
        :with i := 0
        :for x :across code
        :do (cond
              ((instructionp x) (incf i))
              ((jump-target-p x)
               (let ((name (label-name (jump-target-label x))))
                 (when (gethash name positions)
                   (cerror "Consider this to be the representative label from here on."
                           "Duplicate label: ~A" name))
                 (setf (gethash name positions) i))))
        :finally (return positions)))

(defun patch-labels (parsed-program)
  (let* ((code (parsed-program-executable-code parsed-program))
         (positions (jump-target-positions code)))
    ;; Delete jump targets.
    (setf (parsed-program-executable-code parsed-program) (remove-if #'jump-target-p code))

    ;; Patch label references.
    (loop :for thing :across code
          :when (typep thing 'jump) :do
            (let ((name (label-name (jump-label thing))))
              (setf (jump-label thing)
                    (gethash name positions)))))
  parsed-program)
