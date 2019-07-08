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
  (dotimes (i 100)
    (let ((u (magicl:random-unitary 2)))
      (fiasco-assert-matrices-are-equal (cl-quil::bloch-vector-to-matrix (cl-quil::matrix-to-bloch-vector u))
                                        u))))

(deftest test-find-transformation-matrix ()
  (dotimes (i 100)
    (let ((bv1 (cl-quil::matrix-to-bloch-vector (magicl:random-unitary 2)))
          (bv2 (cl-quil::matrix-to-bloch-vector (magicl:random-unitary 2))))
      (setf (cl-quil::bloch-vector-theta bv1) (cl-quil::bloch-vector-theta bv2))
      (let* ((u1 (cl-quil::bloch-vector-to-matrix bv1))
             (u2 (cl-quil::bloch-vector-to-matrix bv2))
             (s (cl-quil::find-transformation-matrix u1 u2)))
        (fiasco-assert-matrices-are-equal (magicl:multiply-complex-matrices s (magicl:multiply-complex-matrices u2 (magicl:dagger s)))
                                          u1)))))

(deftest test-gc-decompose-x-rotation ()
  (dotimes (i 100)
    (let ((x-rot (cl-quil::bloch-vector-to-matrix (cl-quil::make-bloch-vector :theta (- (random (* 2 pi)) pi)
                                                                              :axis #(1 0 0)))))
      (multiple-value-bind (b c) (cl-quil::gc-decompose-x-rotation x-rot)
        ;; (format t "~%X-ROT: ~A~%B: ~A~%C: ~A~%" x-rot b c)
        (fiasco-assert-matrices-are-equal (magicl:multiply-complex-matrices b (magicl:multiply-complex-matrices c (magicl:multiply-complex-matrices (magicl:dagger b) (magicl:dagger c))))
                                          x-rot)))))

(deftest test-gc-decompose ()
  (dotimes (i 100)
    (let ((u (magicl:random-unitary 2)))
      (multiple-value-bind (v w) (cl-quil::gc-decompose u)
        (fiasco-assert-matrices-are-equal (magicl:multiply-complex-matrices v (magicl:multiply-complex-matrices w (magicl:multiply-complex-matrices (magicl:dagger v) (magicl:dagger w))))
                                          u)))))
