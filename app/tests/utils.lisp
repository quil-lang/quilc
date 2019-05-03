(in-package #:quilc-tests)

(defmacro with-mocked-function-definitions (defs &body body)
  "Dynamically re-define global functions named in DEFS within BODY.

DEFS are LET-like bindings (not FLET) whose binding keys are symbols
that name global functions, and whose binding values are evaluated and
return functions. These functions should have conforming lambda lists,
types, and return values, though this is not enforced by the macro.

e.g.
(defun testfunc (a b) (+ a b))
(with-mocked-function-definitions
    ((testfunc (lambda (a b) (* a b))))
  (testfunc 3 4)) ;; => 12
"
  (let* ((names (mapcar #'first defs))
         (gnames (mapcar (alexandria:compose #'gensym #'symbol-name) names)))
    `(let (,@(loop :for name :in names
                   :for gname :in gnames
                   :collect `(,gname (fdefinition ',name))))
       (unwind-protect
            (progn
              (setf ,@(loop :for (name value) :in defs
                            :collect `(fdefinition ',name)
                            :collect `,value))
              ,@body)
         (setf ,@(loop :for name :in names
                       :for gname :in gnames
                       :collect `(fdefinition ',name)
                       :collect `,gname))))))
