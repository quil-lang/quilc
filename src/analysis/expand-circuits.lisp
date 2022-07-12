;;;; analysis/expand-circuits.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil/frontend)

(define-transform expand-circuits (expand-circuits)
  "This transform expands all circuits to create a circuit-free program.")

(defun expand-circuits (parsed-program)
  "Mutate the code slot of PARSED-PROGRAM with all known circuits recursively expanded."
  (flet ((always-error (x)
           (declare (ignore x))
           (error "Not in a circuit expansion context.")))
    (let ((*expansion-context* ':DEFCIRCUIT)
          (*expansion-depth* 0))
      (let ((flat-instrs
              (loop :for instr :across (parsed-program-executable-code parsed-program)
                    :for instantiated := (instantiate-instruction instr #'always-error #'always-error)
                    :append (a:ensure-list instantiated))))
        (setf (parsed-program-executable-code parsed-program)
              (coerce flat-instrs 'vector)))))
  parsed-program)
