;;;; rewriting-rules.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)


;;; rewriting rules in general

(define-compiler sort-CZs
    ((x ("CZ" () p q))
     (y ("CZ" () r s)
        :where (< (+ (ash 1 p) (ash 1 q))
                  (+ (ash 1 r) (ash 1 s)))))
  (list (build-gate "CZ" () r s)
        (build-gate "CZ" () p q)))

(define-compiler sort-CPHASEs
    ((x ("CPHASE" (theta) p q))
     (y ("CPHASE" (phi)   r s)
        :where (< (+ (ash 1 p) (ash 1 q))
                  (+ (ash 1 r) (ash 1 s)))))
  (list (build-gate "CPHASE" `(,phi)   r s)
        (build-gate "CPHASE" `(,theta) p q)))

(define-compiler elide-applications-on-eigenvectors
    ((instr :acting-on (psi qubit-indices)
            :where (collinearp psi (nondestructively-apply-instr-to-wf instr psi qubit-indices))))
  (list))

(defun global-rewriting-rules ()
  "Rewriting rules that may be useful in manipulating instruction sequences that do not have an underlying notion of 'native hardware'.  In particular, this includes canonicalizing the ordering of commuting instructions."
  (list (make-rewriting-rule "CZs commute" #'sort-CZs)
        (make-rewriting-rule "CPHASEs commute" #'sort-CPHASEs)
        (make-rewriting-rule "Eigenvectors take no action" #'elide-applications-on-eigenvectors)))


(define-compiler agglutinate-RXes
    ((x ("RX" (theta) q))
     (y ("RX" (phi)   q)))
  (list (build-gate "RX" `(,(param-+ theta phi)) q)))

(define-compiler eliminate-full-RX-rotations
    ((x ("RX" (theta) _)
        :where (and (typep theta 'double-float)
                    (double= (/ theta (* 2 pi)) (round (/ theta (* 2 pi)))))))
  (list))

(define-compiler normalize-RX-rotations
    ((x ("RX" (theta) q)
        :where (typep theta 'double-float)))
  (let ((reduced-theta (- (mod (+ pi theta) (* 2 pi)) pi)))
    (give-up-compilation-unless
        (and (< pi (abs theta))
             (< (abs reduced-theta) (abs theta)))
      (list (build-gate "RX" (list reduced-theta) q)))))

(define-compiler prefer-RXpi-to-RXnegpi
    ((x ("RX" (#.(- pi)) q)))
  (list (build-gate "RX" '(#.pi) q)))

(define-compiler agglutinate-RZes
    ((x ("RZ" (theta) q))
     (y ("RZ" (phi)   q)))
  (list (build-gate "RZ" `(,(param-+ theta phi)) q)))

(define-compiler eliminate-full-RZ-rotations
    ((x ("RZ" (theta) _)
        :where (and (typep theta 'double-float)
                    (double= (/ theta (* 2 pi)) (round (/ theta (* 2 pi)))))))
  (list))

(define-compiler normalize-RZ-rotations
    ((x ("RZ" (theta) q)
        :where (typep theta 'double-float)))
  (let ((reduced-theta (- (mod (+ pi theta) (* 2 pi)) pi)))
    (give-up-compilation-unless
        (and (< pi (abs theta))
             (< (abs reduced-theta) (abs theta)))
      (list (build-gate "RZ" (list reduced-theta) q)))))

(define-compiler prefer-RZpi-to-RZnegpi
    ((x ("RZ" (#.(- pi)) q)))
  (list (build-gate "RZ" '(#.pi) q)))

;; rewriting rules specialized to qubit types
(defun rewriting-rules-for-roll-RX ()
  "Generates a list of rewriting rules for simplifying expressions involving RX."
  (list (make-rewriting-rule "RX(a) RX(b) -> RX(a+b)" #'agglutinate-RXes)
        (make-rewriting-rule "RX(2 pi k) -> " #'eliminate-full-RX-rotations)
        (make-rewriting-rule "RX(x + 2 pi k) -> RX(x)" #'normalize-RX-rotations)
        (make-rewriting-rule "RX(-pi) -> RX(pi)" #'prefer-RXpi-to-RXnegpi)))

(defun rewriting-rules-for-roll-RZ ()
  "Generates a list of rewriting rules for simplifying expressions involving RZ."
  (list (make-rewriting-rule "RZ(a) RZ(b) -> RZ(a+b)" #'agglutinate-RZes)
        (make-rewriting-rule "RZ(2 pi k) -> " #'eliminate-full-RZ-rotations)
        (make-rewriting-rule "RZ(x + 2 pi k) -> RZ(x)" #'normalize-RZ-rotations)
        (make-rewriting-rule "RZ(-pi) -> RZ(pi)" #'prefer-RZpi-to-RZnegpi)))


(define-compiler sort-RX-after-Z
    ((x ("RX" (theta) q))
     (y ("RZ" (#.pi)  q)))
  (list (build-gate "RZ" '(#.pi)       q)
        (build-gate "RX" `(,(- theta)) q)))

(define-compiler sort-X-after-RZ
    ((x ("RX" (#.pi)  q))
     (y ("RZ" (theta) q)))
  (list (build-gate "RZ" `(,(- theta)) q)
        (build-gate "RX" '(#.pi)       q)))

(define-compiler rewrite-XZX-as-ZXZ
    ((x ("RX" (theta) q)
        :where (double= (/ pi 2) (abs theta)))
     (y ("RZ" (phi)   q)
        :where (double= (/ pi 2) (abs phi)))
     (z ("RX" (psi)   q)
        :where (double= (/ pi 2) (abs psi))))
  (list (build-gate "RZ" `(,psi)   q)
        (build-gate "RX" `(,phi)   q)
        (build-gate "RZ" `(,theta) q)))

(defun rewriting-rules-preferring-RZ-to-RX ()
  "Generates a list of rewriting rules for simplifying expressions involving RZ and RX, with RZ preferred."
  (list (make-rewriting-rule "X RZ(theta) -> RZ(-theta) X" #'sort-RX-after-Z)
        (make-rewriting-rule "RX(theta) Z -> Z RX(-theta)" #'sort-X-after-RZ)
        (make-rewriting-rule "-X/2 Z/2 X/2 -> Z/2 X/2 -Z/2, etc" #'rewrite-XZX-as-ZXZ)))


;; rewriting rules specialized to link type

(define-compiler collapse-ISWAPs
    ((x ("ISWAP" () p q))
     (y ("ISWAP" () r s)
        :where (subsetp (list p q) (list r s))))
  (list (build-gate "RZ" '(#.pi) p)
        (build-gate "RZ" '(#.pi) q)))

(define-compiler commute-RZ-after-ISWAP
    ((x ("RZ"    (theta) q))
     (y ("ISWAP" ()      q1 q2)
        :where (or (= q q1) (= q q2))))
  (list (build-gate "ISWAP" ()       q1 q2)
        (build-gate "RZ"   `(,theta) (if (= q q1) q2 q1))))

(defun rewriting-rules-for-link-of-ISWAP-type ()
  "Generates a list of rewriting rules for simplifying expressions involving ISWAP and standard single-qubit operations."
  (list (make-rewriting-rule "ISWAP ISWAP -> Z (x) Z" #'collapse-ISWAPs)
        (make-rewriting-rule "(RZ (x) I) ISWAP -> ISWAP (I (x) RZ)" #'commute-RZ-after-ISWAP)))


(define-compiler agglutinate-PISWAPs
    ((x ("PISWAP" (theta) p q))
     (y ("PISWAP" (phi)   r s)
        :where (subsetp (list p q) (list r s))))
  (list (build-gate "PISWAP" `(,(param-+ theta phi)) p q)))

(define-compiler normalize-PISWAP
    ((x ("PISWAP" (theta) p q)
        :where (typep theta 'double-float)))
  (let ((reduced-theta (mod theta (* 4 pi))))
    (give-up-compilation-unless
        (not (double= theta reduced-theta))
      (list (build-gate "PISWAP" (list reduced-theta) p q)))))

(define-compiler eliminate-half-PISWAP
    ((x ("PISWAP" (#.(* 2 pi)) p q)))
  (list (build-gate "RZ" '(#.pi) p)
        (build-gate "RZ" '(#.pi) q)))

(define-compiler eliminate-full-PISWAP
    ((x ("PISWAP" (#.(* 4 pi)) _ _)))
  (list))

;; TODO: add a variant of this for symmetric applications of RZ
(define-compiler commute-RZ-after-quarter-PISWAP
    ((x ("RZ"     (theta) q))
     (y ("PISWAP" (phi)   q1 q2)
        :where (and (typep phi 'double-float)
                    (double= 0d0 (mod (abs phi) pi))
                    (or (= q q1) (= q q2)))))
  (list (build-gate "PISWAP" (list phi)   q1 q2)
        (build-gate "RZ"     (list theta) (if (= q q1) q2 q1))))

(defun rewriting-rules-for-link-of-PISWAP-type ()
  "Generates a list of rewriting rules for simplifying expressions involving PISWAP and standard single-qubit operations."
  (list (make-rewriting-rule "PISWAP(phi) PISWAP(theta) -> PISWAP(phi + theta)" #'agglutinate-PISWAPs)
        (make-rewriting-rule "PISWAP(phi) -> PISWAP(phi mod 4 pi)" #'normalize-PISWAP)
        (make-rewriting-rule "PISWAP(2pi) -> Z (x) Z" #'eliminate-half-PISWAP)
        (make-rewriting-rule "PISWAP(4pi) -> " #'eliminate-full-PISWAP)
        (make-rewriting-rule "(RZ (x) I) PISWAP(pi) -> PISWAP(pi) (I (x) RZ)" #'commute-RZ-after-quarter-PISWAP)))


(define-compiler factor-ISWAP-out-of-PISWAP
    ((x ("PISWAP" (theta) p q)
        :where (and (typep theta 'double-float)
                    (double= theta (* pi (floor theta pi))))))
  (list (build-gate "PISWAP" `(,(- theta pi)) p q)
        (build-gate "ISWAP"   ()              p q)))

(define-compiler agglutinate-ISWAP-on-left-into-PISWAP
    ((x ("ISWAP"  ()      p q))
     (y ("PISWAP" (theta) r s)
        :where (subsetp (list p q) (list r s))))
  (list (build-gate "PISWAP" `(,(param-+ theta pi)) p q)))

(define-compiler agglutinate-ISWAP-on-right-into-PISWAP
    ((x ("PISWAP" (theta) p q))
     (y ("ISWAP"  ()      r s)
        :where (subsetp (list p q) (list r s))))
  (list (build-gate "PISWAP" `(,(param-+ theta pi)) p q)))

(defun rewriting-rules-preferring-ISWAP-to-PISWAP ()
  "Generates a list of rewriting rules that simplify expressions involving ISWAP and PISWAP, with a preference for rewriting into ISWAP over PISWAP."
  (list (make-rewriting-rule "PISWAP(odd * pi) -> PISWAP(even * pi) ISWAP" #'factor-ISWAP-out-of-PISWAP)
        (make-rewriting-rule "ISWAP PISWAP(t) -> PISWAP(t + pi)" #'agglutinate-ISWAP-on-left-into-PISWAP)
        (make-rewriting-rule "PISWAP(t) ISWAP -> PISWAP(t + pi)" #'agglutinate-ISWAP-on-right-into-PISWAP)))


(define-compiler ISWAP-to-PISWAP
    ((x ("ISWAP" () p q)))
  (list (build-gate "PISWAP" '(#.pi) p q)))

(defun rewriting-rules-preferring-PISWAP-to-ISWAP ()
  "Generates a list of rewriting rules that simplify expressions involving ISWAP and PISWAP, with a preference for rewriting into PISWAP over ISWAP."
  (list (make-rewriting-rule "ISWAP -> PISWAP" #'ISWAP-to-PISWAP)))


(define-compiler collapse-CNOTs
    ((x ("CNOT" () p q))
     (y ("CNOT" () p q)))
  (list))

(define-compiler commute-control-RZ-after-CNOT
    ((x ("RZ"   (theta) control))
     (y ("CNOT" ()      control target)))
  (list (build-gate "CNOT" () control target)
        (build-gate "RZ"  `(,theta) control)))

(define-compiler commute-target-RX-after-CNOT
    ((x ("RX"   (theta) target))
     (y ("CNOT" ()      control target)))
  (list (build-gate "CNOT" ()       control target)
        (build-gate "RX"  `(,theta) target)))

(defun rewriting-rules-for-link-of-CNOT-type ()
  (list (make-rewriting-rule "CNOT CNOT ->" #'collapse-CNOTs)
        (make-rewriting-rule "(RZ (x) I) CNOT -> CNOT (RZ (x) I)" #'commute-control-RZ-after-CNOT)
        (make-rewriting-rule "(I (x) RX) CNOT -> CNOT (I (x) RX)" #'commute-target-RX-after-CNOT)))


(define-compiler collapse-CZs
    ((x ("CZ" () p q))
     (y ("CZ" () r s)
        :where (subsetp (list p q) (list r s))))
  (list))

(define-compiler commute-RZ-after-CZ
    ((x ("RZ" (theta) q))
     (y ("CZ" ()      q1 q2)
        :where (or (= q q1) (= q q2))))
  (list (build-gate "CZ"  () q1 q2)
        (build-gate "RZ" `(,theta) q)))

(define-compiler commute-X-after-CZ
    ((x ("RX" (#.pi) q))
     (y ("CZ" ()     q1 q2)
        :where (or (= q q1) (= q q2))))
  (list (build-gate "CZ" ()        q1 q2)
        (build-gate "RZ" (list pi) (if (= q q1) q2 q1))
        (build-gate "RX" (list pi) (if (= q q1) q1 q2))))

(define-compiler CZ-on-wf-with-partial-support
    ((x ("CZ" () control target)
        :acting-on (psi qubit-indices)))
  (let* ((zero-position (- (length qubit-indices) 1
                           (position target qubit-indices)))
         (one-position (- (length qubit-indices) 1
                          (position control qubit-indices)))
         (wf-components (make-array 4 :initial-element 0d0)))
    (flet ((position-type (i)
             (+ (* 2 (ldb (byte 1 one-position) i))
                (ldb (byte 1 zero-position) i))))
      (dotimes (i (array-total-size psi))
        (incf (aref wf-components (position-type i))
              (expt (abs (aref psi i)) 2)))
      (let ((wf-signature
              (+ (if (double= 0d0 (aref wf-components 0))
                     0 1)
                 (if (double= 0d0 (aref wf-components 1))
                     0 2)
                 (if (double= 0d0 (aref wf-components 2))
                     0 4)
                 (if (double= 0d0 (aref wf-components 3))
                     0 8))))
        (case wf-signature
          ((#b0000 #b0001 #b0010 #b0011 #b0100 #b0101 #b0110 #b0111 #b1000)
           (list))
          ((#b1001 #b1101 #b1100)     ; -+-+ masks to -+++
           (list (build-gate "RZ" (list pi) target)))
          ((#b1010 #b1011)            ; --++ masks to -+++
           (list (build-gate "RZ" (list pi) control)))
          (#b1110                     ; -++- masks to -+++
           (list (build-gate "RZ" (list pi) control)
                 (build-gate "RZ" (list pi) target)))
          (otherwise
           (give-up-compilation)))))))

(defun rewriting-rules-for-link-of-CZ-type ()
  "Generates a list of rewriting rules for simplifying expressions involving CZ and standard single-qubit operations."  (list (make-rewriting-rule "CZ CZ -> " #'collapse-CZs)
        (make-rewriting-rule "RZ CZ -> CZ RZ" #'commute-RZ-after-CZ)
        (make-rewriting-rule "(X (x) I) CZ -> CZ (X (x) Z)" #'commute-X-after-CZ)
        (make-rewriting-rule "CZ with partially supported wf" #'CZ-on-wf-with-partial-support)))


(define-compiler agglutinate-CPHASEs
    ((x ("CPHASE" (theta) p q))
     (y ("CPHASE" (phi)   r s)
        :where (subsetp (list p q) (list r s))))
  (list (build-gate "CPHASE" `(,(param-+ theta phi)) p q)))

(define-compiler eliminate-full-CPHASE
    ((x ("CPHASE" (theta) _ _)
        :where (and (typep theta 'double-float)
                    (double= 0d0 (mod theta (* 2 pi))))))
  (list))

(define-compiler normalize-CPHASE
    ((x ("CPHASE" (theta) p q)
        :where (and (typep theta 'double-float)
                    (not (double= theta (mod theta (* 2 pi)))))))
  (list (build-gate "CPHASE" `(,(mod theta (* 2 pi))) p q)))

(define-compiler commute-RZ-after-CPHASE
    ((x ("RZ"     (theta) q))
     (y ("CPHASE" (phi)   q1 q2)))
  (list (build-gate "CPHASE" `(,phi)   q1 q2)
        (build-gate "RZ"     `(,theta) q)))

(define-compiler CPHASE-on-wf-with-partial-support
    ((x ("CPHASE" (theta) control target)
        :acting-on (psi qubit-indices)))
  (let* ((zero-position (- (length qubit-indices) 1
                           (position target qubit-indices)))
         (one-position (- (length qubit-indices) 1
                          (position control qubit-indices)))
         (wf-components (make-array 4 :initial-element 0d0)))
    (flet ((position-type (i)
             (+ (* 2 (ldb (byte 1 one-position) i))
                (ldb (byte 1 zero-position) i))))
      (dotimes (i (array-total-size psi))
        (incf (aref wf-components (position-type i))
              (expt (abs (aref psi i)) 2)))
      (let ((wf-signature
              (+ (if (double= 0d0 (aref wf-components 0))
                     0 1)
                 (if (double= 0d0 (aref wf-components 1))
                     0 2)
                 (if (double= 0d0 (aref wf-components 2))
                     0 4)
                 (if (double= 0d0 (aref wf-components 3))
                     0 8))))
        (case wf-signature
          ((#b0000 #b0001 #b0010 #b0011 #b0100 #b0101 #b0110 #b0111 #b1000)
           (list))
          ((#b1001 #b1101 #b1100)     ; t0t0 masks to t000
           (list (build-gate "RZ" `(,theta) target)))
          ((#b1010 #b1011)            ; tt00 masks to t000
           (list (build-gate "RZ" `(,theta) control)))
          (#b1110                     ; t00t masks to t000
           (list (build-gate "RZ" `(,theta) control)
                 (build-gate "RZ" `(,theta) target)))
          (otherwise
           (give-up-compilation)))))))

(defun rewriting-rules-for-link-of-CPHASE-type ()
  "Generates a list of rewriting rules for simplifying expressions involving CPHASE and standard single-qubit operations."
  (list* (make-rewriting-rule "CPHASE(theta) CPHASE(phi) -> CPHASE(theta + phi)" #'agglutinate-CPHASEs)
         (make-rewriting-rule "CPHASE(2 pi k) ->" #'eliminate-full-CPHASE)
         (make-rewriting-rule "CPHASE(t + 2 pi k) -> CPHASE(t)" #'normalize-CPHASE)
         (make-rewriting-rule "RZ CPHASE -> CPHASE RZ" #'commute-RZ-after-CPHASE)
         (make-rewriting-rule "CPHASE on partially supported wf" #'CPHASE-on-wf-with-partial-support)
         (rewriting-rules-for-link-of-CZ-type)))


(define-compiler CZ-to-CPHASE
    ((x ("CZ" () p q)))
  (list (build-gate "CPHASE" '(#.pi) p q)))

(defun rewriting-rules-preferring-CPHASE-to-CZ ()
  "Generates a list of rewriting rules that simplify expressions involving CZ and CPHASE, with a preference for rewriting into CPHASE over CZ."
  (list (make-rewriting-rule "CZ -> CPHASE" #'CZ-to-CPHASE)))


(define-compiler agglutinate-CZ-on-right-into-CPHASE
    ((x ("CPHASE" (phi) p q))
     (y ("CZ"     ()    r s)
        :where (subsetp (list p q) (list r s))))
  (list (build-gate "CPHASE" `(,(param-+ phi pi)) p q)))

(define-compiler agglutinate-CZ-on-left-into-CPHASE
    ((x ("CZ"     ()    r s))
     (y ("CPHASE" (phi) p q)
        :where (subsetp (list p q) (list r s))))
  (list (build-gate "CPHASE" `(,(param-+ phi pi)) p q)))

(define-compiler half-CPHASE-to-CZ
    ((x ("CPHASE" (#.pi) p q)))
  (list (build-gate "CZ" () p q)))

(defun rewriting-rules-preferring-CZ-to-CPHASE ()
  "Generates a list of rewriting rules that simplify expressions involving CZ and CPHASE, with a preference for rewriting into CZ over CPHASE."
  (list (make-rewriting-rule "CPHASE CZ -> CPHASE" #'agglutinate-CZ-on-right-into-CPHASE)
        (make-rewriting-rule "CZ CPHASE -> CPHASE" #'agglutinate-CZ-on-left-into-CPHASE)
        (make-rewriting-rule "CPHASE(pi) -> CZ" #'half-CPHASE-to-CZ)))
