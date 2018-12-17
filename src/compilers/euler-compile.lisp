;;;; euler-compile.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

;; a different implementation of this might call csc-compile and then convert
;; the resulting uncontrolled UCRs into just rolls.

(deftype euler-target ()
  "A valid target for Euler compilation."
  '(member :zxz :zyz :xyx :xzx :yzy :yxy))

(defun euler-compiler (instr &key (target ':zyz))
  "Compiles a 1-Q gate definition to a circuit definition by ZYZ Euler decomposition."
  (check-type target euler-target)
  (when (< 1 (length (application-arguments instr)))
    (give-up-compilation))
  (let ((matrix (gate-matrix instr)))
    (unless matrix
      (give-up-compilation))
    (let* (;; read out what gates we're targetting
           (gate-types (ecase target
                         (:zyz '("RZ" "RY" "RZ"))
                         (:xyx '("RX" "RY" "RX"))
                         (:xzx '("RX" "RZ" "RX"))
                         (:zxz '("RZ" "RX" "RZ"))
                         (:yzy '("RY" "RZ" "RY"))
                         (:yxy '("RY" "RX" "RY"))))
           (gate-scalars (ecase target
                           (:zyz '( 1  1  1))
                           (:xyx '(-1  1 -1))
                           (:xzx '(-1 -1 -1))
                           (:zxz '( 1  1  1))
                           (:yzy '( 1 -1  1))
                           (:yxy '( 1  1  1))))
           ;; CSD is naturally valued in ZYZ Euler decomposition. so, if we conjugate
           ;; the matrix according to the target gate type and run CSD on it, we
           ;; will get something that conjugates to the desired Euler decomposition
           ;; of the original gate.
           (m (ecase target
                (:zyz matrix)
                (:xyx (reduce #'magicl:multiply-complex-matrices
                              (list
                               (gate-matrix (lookup-standard-gate "RY") (/ pi 2))
                               matrix
                               (gate-matrix (lookup-standard-gate "RY") (/ pi -2)))))
                (:xzx (reduce #'magicl:multiply-complex-matrices
                              (list
                               (gate-matrix (lookup-standard-gate "RY") (/ pi 2))
                               (gate-matrix (lookup-standard-gate "RX") (/ pi 2))
                               matrix
                               (gate-matrix (lookup-standard-gate "RX") (/ pi -2))
                               (gate-matrix (lookup-standard-gate "RY") (/ pi -2)))))
                (:zxz (reduce #'magicl:multiply-complex-matrices
                              (list
                               (gate-matrix (lookup-standard-gate "RZ") (/ pi 2))
                               matrix
                               (gate-matrix (lookup-standard-gate "RZ") (/ pi -2)))))
                (:yzy (reduce #'magicl:multiply-complex-matrices
                              (list
                               (gate-matrix (lookup-standard-gate "RX") (/ pi 2))
                               matrix
                               (gate-matrix (lookup-standard-gate "RX") (/ pi -2)))))
                (:yxy (reduce #'magicl:multiply-complex-matrices
                              (list
                               (gate-matrix (lookup-standard-gate "RX") (/ pi 2))
                               (gate-matrix (lookup-standard-gate "RY") (/ pi 2))
                               matrix
                               (gate-matrix (lookup-standard-gate "RY") (/ pi -2))
                               (gate-matrix (lookup-standard-gate "RX") (/ pi -2))))))))
      ;; perform CSD on m, which in this case writes m as a product u s v, where
      ;; s is an RY gate and u and v are, up to rescaling, RZ gates.
      ;;
      ;; more specifically, u = [u0, 0 ; 0, u1]; v = [v0, 0 ; 0, v1], and
      ;; s = [cos angles(0), -sin angles(0); sin angles(0), cos angles(0)].
      (multiple-value-bind (u0 u1 v0 v1 angles) (magicl:lapack-csd m 1 1)
        ;; 'up to rescaling' here means that u and v aren't guaranteed to be
        ;; special unitary. so, we correct their global phase.
        (let ((udet-fixer (sqrt (* (magicl:ref u0 0 0) (magicl:ref u1 0 0))))
              (vdet-fixer (sqrt (* (magicl:ref v0 0 0) (magicl:ref v1 0 0)))))
          (setf u0 (/ (magicl:ref u0 0 0) udet-fixer))
          (setf u1 (/ (magicl:ref u1 0 0) udet-fixer))
          (setf v0 (/ (magicl:ref v0 0 0) vdet-fixer))
          (setf v1 (/ (magicl:ref v1 0 0) vdet-fixer)))
        ;; now deduce the Z-rotation angles and write the CSD out as instructions.
        ;; remember that instruction order is the reverse of matrix order.
        (list
         (build-gate (first gate-types)
                     (list (* (realpart (/ (log v1) #C(0 1/2)))
                              (first gate-scalars)))
                     (first (application-arguments instr)))
         (build-gate (second gate-types)
                     (list (* 2
                              (nth 0 angles)
                              (second gate-scalars)))
                     (first (application-arguments instr)))
         (build-gate (third gate-types)
                     (list (* (realpart (/ (log u1) #C(0 1/2)))
                              (third gate-scalars)))
                     (first (application-arguments instr))))))))
