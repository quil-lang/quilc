;;;; rewriting-rule-data-type.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

;;; We define the REWRITING-RULE data structure which is made use of
;;; frequently. Actual rewriting rules are defined in
;;; rewriting-rules.lisp.

(defstruct (rewriting-rule (:constructor %make-rewriting-rule))
  "An individual rewriting rule for use by the compressor.

READABLE-NAME is a string used to identify the rewriting rule during debugging.

COUNT is an integer that specifies the number of adjacent instructions the rule takes as input.
 
CONSUMER is a function that consumes a COMPRESSOR-CONTEXT followed by a list of instructions.  Its possible results are:
   * The error condition COMPILER-DOES-NOT-APPLY if the rule is inapplicable.
   * a LIST of instructions if the rule applies.
   * Some other error condition if compilation unexpectedly fails."
  (readable-name "Unnamed Rewriting Rule" :read-only t)
  (count 1 :read-only t :type (and fixnum unsigned-byte))
  (consumer (lambda (context item)
              (declare (ignore context item))
              (give-up-compilation))
   :read-only t))

(defmacro make-rewriting-rule (readable-name (context-var &rest bind-clauses) &body body)
  (let* ((context-var-p (not (wildcard-pattern-p context-var)))
         (context-var-name (if context-var-p
                               context-var
                               (gensym "CONTEXT-VAR-"))))
    `(%make-rewriting-rule
      :readable-name ,readable-name
      :count ,(length bind-clauses)
      :consumer (lambda (,context-var-name ,@(mapcar #'second bind-clauses))
                  ,@(unless context-var-p
                      `((declare (ignore ,context-var-name))))
                  (operator-match
                    (,bind-clauses
                     ,@body)
                    (_
                     (give-up-compilation)))))))
