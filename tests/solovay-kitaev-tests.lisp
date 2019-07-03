;;;; solovay-kitaev-tests.lisp
;;;;
;;;; Author: Andrew Shi

(in-package #:cl-quil-tests)

(deftest test-pauli-to-bloch-vector ()
  (let ((x-axis #(1.0d0 0.0d0 0.0d0))
        (y-axis #(0.0d0 1.0d0 0.0d0))
        (z-axis #(0.0d0 0.0d0 1.0d0)))
    (is (loop :for i :below 4 :for phase := (expt #C(0 1) i) :for test-bloch := (cl-quil::matrix-to-bloch-vector (magicl:scale phase cl-quil::+PX+))
              :always (every #'quil::double~
                             (cl-quil::bloch-vector-axis test-bloch)
                             x-axis)))
    (is (loop :for i :below 4 :for phase := (expt #C(0 1) i) :for test-bloch := (cl-quil::matrix-to-bloch-vector (magicl:scale phase cl-quil::+PY+))
              :always (every #'quil::double~
                             (cl-quil::bloch-vector-axis test-bloch)
                             y-axis)))
    (is (loop :for i :below 4 :for phase := (expt #C(0 1) i) :for test-bloch := (cl-quil::matrix-to-bloch-vector (magicl:scale phase cl-quil::+PZ+))
              :always (every #'quil::double~
                             (cl-quil::bloch-vector-axis test-bloch)
                             z-axis)))))

(deftest test-random-bloch-matrix-conversions ()
  (dotimes (i 1000)
    (let ((u (magicl:random-unitary 2)))
      (is (cl-quil::matrix-equality
           (cl-quil::scale-out-matrix-phases (cl-quil::bloch-vector-to-matrix (cl-quil::matrix-to-bloch-vector u)) u)
           u)))))

(deftest test-find-transformation-matrix ()
  (dotimes (i 100)
    (let ((bv1 (cl-quil::matrix-to-bloch-vector (magicl:random-unitary 2)))
          (bv2 (cl-quil::matrix-to-bloch-vector (magicl:random-unitary 2))))
      (setf (cl-quil::bloch-vector-theta bv1) (cl-quil::bloch-vector-theta bv2))
      (let* ((u1 (cl-quil::bloch-vector-to-matrix bv1))
             (u2 (cl-quil::bloch-vector-to-matrix bv2))
             (s (cl-quil::find-transformation-matrix u1 u2))
             (bvs (cl-quil::matrix-to-bloch-vector s)))
        (format t "~%bv1: ~A~%bv2: ~A~%bvS: ~A~%~%u1: ~A~%u2: ~A~%s: ~A~%"
                bv1 bv2 bvs u1 u2 s)
        (is (cl-quil::matrix-equality
             (magicl:multiply-complex-matrices s (magicl:multiply-complex-matrices u2 (magicl:dagger s)))
             u1))))))
