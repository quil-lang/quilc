;;;; initialize-standard-gates.lisp
;;;;
;;;; Author: Parker Williams

(in-package #:cl-quil/frontend)

;; Some standard gates may require specialized processing that
;; prohibits them from being defined much earlier. (For example,
;; SEQUENCE-GATEs.) As such, the variable which refers to the standard
;; gates is forward-declared, but only now do we supply its value.

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun read-standard-gates-from-file (&optional (stdgates-file (asdf:system-relative-pathname                                                                "cl-quil" "src/quil/stdgates.quil")))
    "Produces a table of default gate definitions, mapping string name to a GATE-DEFINITION object."
    (let* ((gate-defs
             (remove-if-not (lambda (obj) (typep obj 'gate-definition))
                            (parse-quil-into-raw-program
                             (a:read-file-into-string stdgates-file))))
           (parsed-program (make-instance 'parsed-program :executable-code #()
                                          :memory-definitions '()
                                          :circuit-definitions '()
                                          :gate-definitions gate-defs)))

      (resolve-objects parsed-program)
      (validate-defgate-loops parsed-program)
      (parsed-program-gate-definitions parsed-program)))

  (defun initialize-standard-gates ()
    (unless (boundp '**default-gate-definitions**)
      (let ((stdgates-file (asdf:system-relative-pathname
                            "cl-quil" "src/quil/stdgates.quil")))
        (format t "~&; loading standard gates from ~A~%"
                stdgates-file)
        (setf **default-gate-definitions**
              (let ((table (make-hash-table :test 'equal)))
                (dolist (gate-def (read-standard-gates-from-file stdgates-file) table)
                  (setf (gethash (gate-definition-name gate-def) table)
                        gate-def)))))))

  (initialize-standard-gates)
  
) ; eval-when



