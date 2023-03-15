;;;; extern-pragma-tests.lisp
;;;;
;;;; Author: Colin O'Keefe

(in-package #:cl-quil-tests)

(deftest test-extern-pragmas ()
  (let ((quil
          "PRAGMA EXTERN MOO; PRAGMA EXTERN FOO; X 1; Y 2; MOO 1 2; CNOT 0 2; FOO 1")
        parsed)
    (not-signals error
     (setf parsed (cl-quil:parse quil)))
    ;; there should be two externs in the externed-operators
    (let ((table
            (cl-quil.frontend::parsed-program-externed-operations parsed))) 
      (is (gethash "FOO" table))
      (is (gethash "MOO" table)))
    ;; there should be two instances of extern-applications
    (is (= 2
           (count-if (a:rcurry 'typep 'cl-quil.frontend:extern-application)
                     (cl-quil::parsed-program-executable-code parsed))))
    (let ((chip
            (cl-quil::build-8q-chip))
          compiled)
      ;; we should be able to compile programs with extern-applications in them
      (not-signals error
        (setf compiled
              (cl-quil::compiler-hook
               parsed
               chip)))
      ;; There should still be two instances of extern-application,
      ;; which may have been rewired but not otherwise altered.
      (is (= 2
             (count-if (a:rcurry 'typep 'cl-quil.frontend:extern-application)
                       (cl-quil::parsed-program-executable-code parsed)))))))

