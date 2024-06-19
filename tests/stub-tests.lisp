;;;; stub-tests.lisp
;;;;
;;;; Author: Colin O'Keefe

(in-package #:cl-quil-tests)

(deftest test-stubs ()
  ;; We supply Quil with a mixture of "standard" and "totally phony"
  ;; gates.  Here we mark both MOO and CNOT as stubed
  (let ((quil
          "STUB MOO; STUB CNOT; X 1; Y 2; MOO 1; X 0; CNOT 0 2; CZ 1 0; Y 0")
        parsed)
    (not-signals error
     (setf parsed (cl-quil:parse quil)))
    ;; there should be two stubs in the stub-operators
    (let ((table
            (cl-quil.frontend::parsed-program-stub-operations parsed))) 
      (is (gethash "CNOT" table))
      (is (gethash "MOO" table)))
    ;; there should be two instances of stub-applications
    (is (= 2
           (count-if (a:rcurry 'typep 'cl-quil.frontend:stub-application)
                     (cl-quil::parsed-program-executable-code parsed))))
    (let ((chip
            (cl-quil::build-8q-chip))
          compiled)
      ;; we should be able to compile programs with stub-applications in them
      (not-signals error
        (setf compiled
              (cl-quil::compiler-hook
               parsed
               chip)))
      ;; There should still be two instances of stub-application,
      ;; which may have been rewired but not otherwise altered.
      (is (= 2
             (count-if (a:rcurry 'typep 'cl-quil.frontend:stub-application)
                       (cl-quil::parsed-program-executable-code parsed))))

      ;; One of the STUB-APPLICATION instances should be a MOO and
      ;; the other should be a CNOT
      (flet ((stub-named? (name)
               (lambda (instr)
                 (and (typep instr 'cl-quil.frontend::stub-application)
                      (equal name (cl-quil.frontend:application-operator-name instr))))))
        (let ((instructions
                (cl-quil:parsed-program-executable-code compiled)))
          (is (= 1 (count-if (stub-named? "MOO") instructions)))
          (is (= 1 (count-if (stub-named? "CNOT") instructions)))

          ;; All of the Xs Ys should have been compiled to other gates and
          ;; should no longer be present in the code
          (is (zerop
               (loop :for instr :across instructions
                     :for name = (and (typep instr 'quil::application)
                                      (quil::application-operator-name instr))
                     :when (member name '("X" "Y") :test #'equal)
                       :count 1)))))

      ;; The stub table should have been duplicated on compiled program
      (let ((pp-stubs
              (cl-quil.frontend:parsed-program-stub-operations parsed))
            (comp-stubs
              (cl-quil.frontend:parsed-program-stub-operations compiled)))

        (is (= (hash-table-count pp-stubs)
               (hash-table-count comp-stubs))) 
        ;; And they should contain the same members
        (loop :for key :being :the :hash-keys :of pp-stubs
              :do (is (gethash key comp-stubs)))
        (loop :for key :being :the :hash-keys :of comp-stubs
              :do (is (gethash key pp-stubs)))))))

