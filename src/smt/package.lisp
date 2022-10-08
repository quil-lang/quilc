(defpackage #:cl-quil.smt
  (:use #:alexandria #:cl #:cl-quil #:cl-quil.resource)

  ;; solver.lisp
  (:export
   #:*smt-debug-stream*
   #:*constraint-solver-command*
   #:smt-debug-line
   #:initiate-smt-solver
   #:write-smt-forms
   )

  ;; constraint-addresser.lisp
  (:export
   #:*default-constraint-scheme*
   #:addressing-failed
   #:do-constraint-based-addressing
   ))
