;;;; rewriting-rules.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)


;;; rewriting rules in general

(define-compiler eliminate-identities
    ((x ("I" () _)))
  "Eliminate I gates."
  nil)

(define-compiler sort-CZs
    ((x ("CZ" () p q))
     (y ("CZ" () r s)
        :where (< (+ (ash 1 p) (ash 1 q))
                  (+ (ash 1 r) (ash 1 s)))))
  "Sorted adjacent CZs into a canonical order, with the intention of bringing canceling pairs next to one another."
  (inst "CZ" () r s)
  (inst "CZ" () p q))

(define-compiler sort-CPHASEs
    ((x ("CPHASE" (theta) p q))
     (y ("CPHASE" (phi)   r s)
        :where (< (+ (ash 1 p) (ash 1 q))
                  (+ (ash 1 r) (ash 1 s)))))
  (inst "CPHASE" `(,phi)   r s)
  (inst "CPHASE" `(,theta) p q))

(define-compiler elide-applications-on-eigenvectors
    ((instr :acting-on (psi qubit-indices)
            :where (collinearp psi (nondestructively-apply-instr-to-wf instr psi qubit-indices))))
  nil)

(defun global-rewriting-rules ()
  "Rewriting rules that may be useful in manipulating instruction sequences that do not have an underlying notion of 'native hardware'.  In particular, this includes canonicalizing the ordering of commuting instructions."
  (list #'eliminate-identities
        #'sort-CZs
        #'sort-CPHASEs
        #'elide-applications-on-eigenvectors))


;;; rewriting rules specialized to qubit types

(defun full-rotation-p (param &key (full 2pi))
  "Return T if PARAM is an integer multiple of FULL. PARAM may be a DOUBLE-FLOAT, or a CONSTANT."
  (check-type param (or constant double-float))
  (let ((val (or (and (is-constant param)
                      (constant-value param))
                 param)))
    (double= (/ val full) (round (/ val full)))))

(define-compiler eliminate-full-RX-rotations
    ((x ("RX" (theta) _)
        :where (full-rotation-p theta)))
  nil)

(define-compiler normalize-RX-rotations
    ((x ("RX" (theta) q)
        :where (typep theta 'double-float)))
  (let ((reduced-theta (- (mod (+ pi theta) 2pi) pi)))
    (give-up-compilation-unless
        (and (< pi (abs theta))
             (< (abs reduced-theta) (abs theta)))
      (inst "RX" (list reduced-theta) q))))

(define-compiler agglutinate-RXs
    ((x ("RX" (theta) q))
     (y ("RX" (phi)   q)))
  (inst "RX" `(,(param-mod (param-+ theta phi) 2pi)) q))

(define-compiler prefer-RXpi-to-RXnegpi
    ((x ("RX" (#.-pi) q)))
  (inst "RX" '(#.pi) q))

(define-compiler agglutinate-RZs
    ((x ("RZ" (theta) q))
     (y ("RZ" (phi)   q)))
  (inst "RZ" `(,(param-+ theta phi)) q))

(define-compiler eliminate-full-RZ-rotations
    ((x ("RZ" (theta) _)
        :where (full-rotation-p theta)))
  nil)

(define-compiler normalize-RZ-rotations
    ((x ("RZ" (theta) q)
        :where (typep theta 'double-float)))
  (let ((reduced-theta (- (mod (+ pi theta) 2pi) pi)))
    (give-up-compilation-unless
        (and (< pi (abs theta))
             (< (abs reduced-theta) (abs theta)))
      (inst "RZ" (list reduced-theta) q))))

(define-compiler prefer-RZpi-to-RZnegpi
    ((x ("RZ" (#.-pi) q)))
  (inst "RZ" '(#.pi) q))


(define-compiler sort-RX-after-Z
    ((x ("RX" (theta) q))
     (y ("RZ" (#.pi)  q)))
  (inst "RZ" '(#.pi)                  q)
  (inst "RX" `(,(param-* -1d0 theta)) q))

(define-compiler sort-X-after-RZ
    ((x ("RX" (#.pi)  q))
     (y ("RZ" (theta) q)))
  (inst "RZ" `(,(param-* -1d0 theta)) q)
  (inst "RX" '(#.pi)                  q))

(defmacro rewrite-GHG-as-HGH (g h)
  `(progn
     ,@(a:map-product
        (lambda (sign1 sign2 sign3)
          (let ((name (intern (format nil "REWRITE-~A~A~A-AS-~A~A~A-~A"
                                      g h g h g h
                                      (+ (if (plusp sign1) 4 0)
                                         (if (plusp sign2) 2 0)
                                         (if (plusp sign3) 1 0))))))
            `(define-compiler ,name
                 ((x (,g (,(* sign1 pi 1/2)) q))
                  (y (,h (,(* sign2 pi 1/2)) q))
                  (z (,g (,(* sign3 pi 1/2)) q)))
               (inst ,h '(,(* sign3 pi 1/2)) q)
               (inst ,g '(,(* sign2 pi 1/2)) q)
               (inst ,h '(,(* sign1 pi 1/2)) q))))
        '(1 -1) '(1 -1) '(1 -1))))

(rewrite-GHG-as-HGH "RX" "RZ")

;; the above macro expands into a bunch of embodiments of this old rule. the
;; compiler matching stuff isn't smart enough to impose the k pi / 2 constraint
;; on the middle RZ input, so the macro manually imposes it instead. c'est la vie
#+#:ignore
(define-compiler rewrite-XZX-as-ZXZ
    ((x ("RX" (theta) q)
        :where (double= pi/2 (abs theta)))
     (y ("RZ" (phi)   q)
        :where (double= pi/2 (abs phi)))
     (z ("RX" (psi)   q)
        :where (double= pi/2 (abs psi))))
  (inst "RZ" `(,psi)   q)
  (inst "RX" `(,phi)   q)
  (inst "RZ" `(,theta) q))


;;; rewriting rules specialized to link type
;; ISWAPs

(define-compiler collapse-ISWAPs
    ((x ("ISWAP" () p q))
     (y ("ISWAP" () r s)
        :where (subsetp (list p q) (list r s))))
  (inst "RZ" '(#.pi) p)
  (inst "RZ" '(#.pi) q))

(define-compiler commute-RZ-after-ISWAP
    ((x ("RZ"    (theta) q))
     (y ("ISWAP" ()      q1 q2)
        :where (or (= q q1) (= q q2))))
  (inst "ISWAP" ()       q1 q2)
  (inst "RZ"   `(,theta) (if (= q q1) q2 q1)))

;; PISWAPs

(define-compiler agglutinate-PISWAPs
    ((x ("PISWAP" (theta) p q))
     (y ("PISWAP" (phi)   r s)
        :where (subsetp (list p q) (list r s))))
  (inst "PISWAP" `(,(param-+ theta phi)) p q))

(define-compiler normalize-PISWAP
    ((x ("PISWAP" (theta) p q)
        :where (typep theta 'double-float)))
  (let ((reduced-theta (mod theta 4pi)))
    (give-up-compilation-unless
        (not (double= theta reduced-theta))
      (inst "PISWAP" (list reduced-theta) p q))))

(define-compiler eliminate-half-PISWAP
    ((x ("PISWAP" (theta) p q)
        :where (and (full-rotation-p theta :full 2pi)
                    (not (full-rotation-p theta :full 4pi)))))
  (inst "RZ" '(#.pi) p)
  (inst "RZ" '(#.pi) q))

(define-compiler eliminate-full-PISWAP
    ((x ("PISWAP" (theta) _ _)
        :where (full-rotation-p theta :full 4pi)))
  nil)

;; TODO: add a variant of this for symmetric applications of RZ
(define-compiler commute-RZ-after-quarter-PISWAP
    ((x ("RZ"     (theta) q))
     (y ("PISWAP" (phi)   q1 q2)
        :where (and (typep phi 'double-float)
                    (double= 0d0 (mod (abs phi) pi))
                    (or (= q q1) (= q q2)))))
  (inst "PISWAP" (list phi)   q1 q2)
  (inst "RZ"     (list theta) (if (= q q1) q2 q1)))

(define-compiler factor-ISWAP-out-of-PISWAP
    ((x ("PISWAP" (theta) p q)
        :where (and (typep theta 'double-float)
                    (double= theta (* pi (floor theta pi))))))
  (inst "PISWAP" `(,(param-+ theta -pi)) p q)
  (inst "ISWAP"   ()                     p q))

(define-compiler agglutinate-ISWAP-on-left-into-PISWAP
    ((x ("ISWAP"  ()      p q))
     (y ("PISWAP" (theta) r s)
        :where (subsetp (list p q) (list r s))))
  (inst "PISWAP" `(,(param-+ theta pi)) p q))

(define-compiler agglutinate-ISWAP-on-right-into-PISWAP
    ((x ("PISWAP" (theta) p q))
     (y ("ISWAP"  ()      r s)
        :where (subsetp (list p q) (list r s))))
  (inst "PISWAP" `(,(param-+ theta pi)) p q))

;; CNOTs

(define-compiler collapse-CNOTs
    ((x ("CNOT" () p q))
     (y ("CNOT" () p q)))
  nil)

(define-compiler commute-control-RZ-after-CNOT
    ((x ("RZ"   (theta) control))
     (y ("CNOT" ()      control target)))
  (inst "CNOT" ()       control target)
  (inst "RZ"  `(,theta) control))

(define-compiler commute-target-RX-after-CNOT
    ((x ("RX"   (theta) target))
     (y ("CNOT" ()      control target)))
  (inst "CNOT" ()       control target)
  (inst "RX"  `(,theta) target))

;; CZs

(define-compiler collapse-CZs
    ((x ("CZ" () p q))
     (y ("CZ" () r s)
        :where (subsetp (list p q) (list r s))))
  nil)

(define-compiler commute-RZ-after-CZ
    ((x ("RZ" (theta) q))
     (y ("CZ" ()      q1 q2)
        :where (or (= q q1) (= q q2))))
  (inst "CZ"  ()       q1 q2)
  (inst "RZ" `(,theta) q))

(define-compiler commute-X-after-CZ
    ((x ("RX" (#.pi) q))
     (y ("CZ" ()     q1 q2)
        :where (or (= q q1) (= q q2))))
  (inst "CZ" ()        q1 q2)
  (inst "RZ" (list pi) (if (= q q1) q2 q1))
  (inst "RX" (list pi) (if (= q q1) q1 q2)))

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
           nil)
          ((#b1001 #b1101 #b1100)     ; -+-+ masks to -+++
           (inst "RZ" (list pi) target))
          ((#b1010 #b1011)            ; --++ masks to -+++
           (inst "RZ" (list pi) control))
          (#b1110                     ; -++- masks to -+++
           (inst "RZ" (list pi) control)
           (inst "RZ" (list pi) target))
          (otherwise
           (give-up-compilation)))))))

;; CPHASEs

(define-compiler agglutinate-CPHASEs
    ((x ("CPHASE" (theta) p q))
     (y ("CPHASE" (phi)   r s)
        :where (subsetp (list p q) (list r s))))
  (inst "CPHASE" `(,(param-+ theta phi)) p q))

(define-compiler eliminate-full-CPHASE
    ((x ("CPHASE" (theta) _ _)
        :where (full-rotation-p theta)))
  nil)

(define-compiler normalize-CPHASE
    ((x ("CPHASE" (theta) p q)
        :where (and (typep theta 'double-float)
                    (not (double= theta (mod theta 2pi))))))
  (inst "CPHASE" `(,(mod theta 2pi)) p q))

(define-compiler commute-RZ-after-CPHASE
    ((x ("RZ"     (theta) q))
     (y ("CPHASE" (phi)   q1 q2)))
  (inst "CPHASE" `(,phi)   q1 q2)
  (inst "RZ"     `(,theta) q))

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
           nil)
          ((#b1001 #b1101 #b1100)     ; t0t0 masks to t000
           (inst "RZ" `(,theta) target))
          ((#b1010 #b1011)            ; tt00 masks to t000
           (inst "RZ" `(,theta) control))
          (#b1110                     ; t00t masks to t000
           (inst "RZ" `(,theta) control)
           (inst "RZ" `(,theta) target))
          (otherwise
           (give-up-compilation)))))))

(define-compiler agglutinate-CZ-on-right-into-CPHASE
    ((x ("CPHASE" (phi) p q))
     (y ("CZ"     ()    r s)
        :where (subsetp (list p q) (list r s))))
  (inst "CPHASE" `(,(param-+ phi pi)) p q))

(define-compiler agglutinate-CZ-on-left-into-CPHASE
    ((x ("CZ"     ()    r s))
     (y ("CPHASE" (phi) p q)
        :where (subsetp (list p q) (list r s))))
  (inst "CPHASE" `(,(param-+ phi pi)) p q))

(define-compiler half-CPHASE-to-CZ
    ((x ("CPHASE" (#.pi) p q)))
  (inst "CZ" () p q))
