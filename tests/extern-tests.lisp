;;;; extern-tests.lisp
;;;;
;;;; Author: Colin O'Keefe

(in-package #:cl-quil-tests)

(deftest test-externs ()
  ;; We supply Quil with a mixture of "standard" and "totally phony"
  ;; gates.  Here we mark both MOO and CNOT as externed
  (let ((quil
          "EXTERN MOO; EXTERN CNOT; X 1; Y 2; MOO 1; CNOT 0 2; CZ 1 0")
        parsed)
    (not-signals error
     (setf parsed (cl-quil:parse quil)))
    ;; there should be two externs in the extern-operators
    (let ((table
            (cl-quil.frontend::parsed-program-extern-operations parsed))) 
      (is (gethash "CNOT" table))
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
                       (cl-quil::parsed-program-executable-code parsed))))

      ;; One of the EXTERN-APPLICATION instances should be a MOO and
      ;; the other should be a CNOT
      (flet ((extern-named? ( name)
               (lambda (instr)
                 (and (typep instr 'cl-quil.frontend::extern-application)
                      (equal name (cl-quil.frontend:application-operator-name instr))))))
        (let ((instructions
                (cl-quil:parsed-program-executable-code compiled)))
          (is (= 1 (count-if (extern-named? "MOO") instructions)))
          (is (= 1 (count-if (extern-named? "CNOT") instructions)))))

      ;; The extern table should have been duplicated on compiled program
      (let ((pp-externs
              (cl-quil.frontend:parsed-program-extern-operations parsed))
            (comp-externs
              (cl-quil.frontend:parsed-program-extern-operations compiled)))

        (is (= (hash-table-count pp-externs)
               (hash-table-count comp-externs))) 
        ;; And they should contain the same members
        (loop :for key :being :the :hash-keys :of pp-externs
              :do (is (gethash key comp-externs)))
        (loop :for key :being :the :hash-keys :of comp-externs
              :do (is (gethash key pp-externs)))))))

