;;;; src/discrete/compilers/clifford-t.lisp
;;;;
;;;; Author: A.J. Nyquist

(in-package #:cl-quil/discrete)

;;; Compilers targeting Clifford+T (i.e. H, S, T)

;; Pauli to clifford expansion rules

(q::define-compiler X-to-HZH
    ((x ("X" () q)))
  (q:inst "H" () q)
  (q:inst "Z" () q)
  (q:inst "H" () q))

(q::define-compiler expand-Z-to-Ss
    ((z ("Z" () q)))
  (q:inst "S" () q)
  (q:inst "S" () q))

(q::define-compiler expand-Y-to-ZX
    ((z ("Y" () q)))
  (q:inst "Z" () q)
  (q:inst "X" () q))

;; RZ to Clifford+T

(q::define-compiler discretize-RZ
    ((rz ("RZ" (theta) q))
     :output-gateset `((:operator ,(q:named-operator "H")
                        :arguments (_))
                       100
                       (:operator ,(q:named-operator "S")
                        :arguments (_))
                       100
                       (:operator ,(q:named-operator "T")
                        :arguments (_))
                       100))
  (unless (realp theta)
    (q::give-up-compilation))

  (let* ((reduced-theta (mod theta (* 2 pi)))
         ;; Check RZ(n pi / 4)
         (n (find-if (lambda (x)
                       (q::double= (* (/ x 4) pi) reduced-theta))
                     (alexandria:iota 8)))
         ;; Only valid up to 0.5 epsilon
         (epsilon (min 0.5d0 *tolerance*))
         ;; Less candidates are available for larger epsilons
         (trials (max 1 (- 16 (floor (log epsilon 1/10)))))
         ;; Prime checks have a 50% success rate, so 2 checks is reasonable
         (prime-checks 2)
         (sequence
           (if n
               (coalton-prelude:Some (output-T^ n))
               (generate-maform-output-with-double
                trials prime-checks epsilon reduced-theta))))
    ;; Brute force if no sequence was found (extremely unlikely, but /possible/)
    (etypecase sequence
      (coalton-library/classes::Optional/None
       (setf sequence (generate-maform-output-with-double
                       64 (* 2 prime-checks) (* epsilon 99/100) reduced-theta)))
      (coalton-library/classes::Optional/Some nil))

    (etypecase sequence
      (coalton-library/classes::Optional/None
       ;; Almost certainly a bug if no solution is found
       (cl:error "No RZ solution was found within ~,2E of an angle ~E "
                 epsilon theta))
      (coalton-library/classes::Optional/Some
       (dolist (x (coalton-library/optional:fromSome
                   "Could not descructure SOME." sequence))
         (ecase x
           (OutputGate1/Discrete-H (q:inst "H" () q))
           (OutputGate1/Discrete-S (q:inst "S" () q))
           (OutputGate1/Discrete-T (q:inst "T" () q))))))))
