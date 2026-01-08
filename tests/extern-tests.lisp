;;;; extern-tests.lisp
;;;;
;;;; Author: Colin O'Keefe

(in-package #:cl-quil-tests)

(defun rando (realcell) (setf (aref realcell 0) (random 1.0)))

;; you can give functions a name for use in Quil programs
(cl-quil::register-classical-function "randomize" 'rando)

(deftest test-extern-and-call ()
  (let (parsed
        compiled
        (quil
          "
EXTERN randomize
DECLARE x REAL
RX(x) 0
CALL randomize x
CPHASE(pi*x/2) 0 1"))

    ;; we can parse programs like the above without error
    (not-signals error
      (setf parsed (parse-quil quil)))

    ;; an can compile such programs 
    (not-signals error
      (setf compiled (cl-quil::compiler-hook parsed (cl-quil:build-8q-chip))))

    ;; we want to ensure that the call to randomize happens after the
    ;; first instruction to reference x and before any other
    ;; instruction that references x.
    (flet ((is-dexpr (e) (cl-quil.frontend::delayed-expression-p e)))
      (let* ((instrs
               (parsed-program-executable-code compiled))
             (pos-rx
               (position-if (lambda (instr)
                              (and (typep instr 'application)
                                   (find-if #'is-dexpr (application-parameters instr))))
                            instrs))
             (pos-call
               (position-if (lambda (instr) (typep instr 'cl-quil.frontend::call))
                            instrs))
             (pos-ref-after-rx
               (and pos-rx
                    (position-if (lambda (instr)
                                   (and (typep instr 'application)
                                        (find-if #'is-dexpr (application-parameters instr))))
                                 instrs
                                 :start (1+ pos-rx)))))
        (is (and pos-rx pos-call pos-ref-after-rx
                 (< pos-rx pos-call pos-ref-after-rx)))))))

(defun add2 (x y) (+ x y))
(cl-quil::register-classical-function "add2" 'add2)

(deftest test-extern-in-expressions ()
  (let (parsed rx)
    ;; parsing works
    (not-signals error
      (setf parsed (parse-quil "
EXTERN add2;
PRAGMA EXTERN add2 \"REAL (x:REAL, y:REAL)\";
RX(add2(3,pi)/4) 0")))

    ;; because the expression involved no memory references, it is
    ;; actually evaluated
    (setf rx (elt (parsed-program-executable-code parsed)
                   (1- (length (parsed-program-executable-code parsed)))))
    
    (is (is-constant
         (elt (application-parameters rx) 0)))

    ;; but this one will involve memory refs, and so will involve a delayed expression
    (setf parsed (parse-quil "
EXTERN add2;
PRAGMA EXTERN add2 \"REAL (x:REAL, y:REAL)\";
DECLARE x REAL;
RX(add2(x,pi)/4) 0"))
    (setf rx (elt (parsed-program-executable-code parsed)
                  (1- (length (parsed-program-executable-code parsed)))))
    (is (cl-quil.frontend::delayed-expression-p
         (elt (application-parameters rx) 0)))))




