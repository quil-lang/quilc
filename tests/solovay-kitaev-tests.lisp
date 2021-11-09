;;;; solovay-kitaev-tests.lisp
;;;;
;;;; Author: Andrew Shi

(in-package #:cl-quil-tests)

(defparameter *px* (cl-quil:gate-matrix (cl-quil:gate-definition-to-gate (cl-quil:lookup-standard-gate "X"))))
(defparameter *py* (cl-quil:gate-matrix (cl-quil:gate-definition-to-gate (cl-quil:lookup-standard-gate "Y"))))
(defparameter *pz* (cl-quil:gate-matrix (cl-quil:gate-definition-to-gate (cl-quil:lookup-standard-gate "Z"))))

(deftest test-pauli-to-bloch-vector ()
  (let ((x-axis #(1.0d0 0.0d0 0.0d0))
        (y-axis #(0.0d0 1.0d0 0.0d0))
        (z-axis #(0.0d0 0.0d0 1.0d0)))
    (is (loop :for i :below 4
              :for phase := (expt #C(0 1) i)
              :for test-bloch := (cl-quil::matrix-to-bloch-vector (magicl:scale *px* phase))
              :always (every #'quil::double~
                             (cl-quil::bloch-vector-axis test-bloch)
                             x-axis)))
    (is (loop :for i :below 4
              :for phase := (expt #C(0 1) i)
              :for test-bloch := (cl-quil::matrix-to-bloch-vector (magicl:scale *py* phase))
              :always (every #'quil::double~
                             (cl-quil::bloch-vector-axis test-bloch)
                             y-axis)))
    (is (loop :for i :below 4
              :for phase := (expt #C(0 1) i)
              :for test-bloch := (cl-quil::matrix-to-bloch-vector (magicl:scale *pz* phase))
              :always (every #'quil::double~
                             (cl-quil::bloch-vector-axis test-bloch)
                             z-axis)))))

(deftest test-random-bloch-matrix-conversions ()
  (dotimes (i 100)
    (let ((u (magicl:random-unitary '(2 2))))
      (fiasco-assert-matrices-are-equal
       u
       (cl-quil::bloch-vector-to-matrix
        (cl-quil::matrix-to-bloch-vector u))))))

(deftest test-find-transformation-matrix ()
  (dotimes (i 100)
    (let ((bv1 (cl-quil::matrix-to-bloch-vector (magicl:random-unitary '(2 2))))
          (bv2 (cl-quil::matrix-to-bloch-vector (magicl:random-unitary '(2 2)))))
      (setf (cl-quil::bloch-vector-theta bv1) (cl-quil::bloch-vector-theta bv2))
      (let* ((u1 (cl-quil::bloch-vector-to-matrix bv1))
             (u2 (cl-quil::bloch-vector-to-matrix bv2))
             (s (cl-quil::find-transformation-matrix u1 u2)))
        (fiasco-assert-matrices-are-equal (magicl:@ s (magicl:@ u2 (magicl:dagger s)))
                                          u1)))))

(deftest test-gc-decompose-x-rotation ()
  (dotimes (i 100)
    (let ((x-rot (cl-quil::bloch-vector-to-matrix (cl-quil::make-bloch-vector :theta (random pi)
                                                                              :axis #(1 0 0)))))
      (multiple-value-bind (b c) (cl-quil::gc-decompose-x-rotation x-rot)
        ;; (format t "~%X-ROT: ~A~%B: ~A~%C: ~A~%" x-rot b c)
        (fiasco-assert-matrices-are-equal (magicl:@ b (magicl:@ c (magicl:@ (magicl:dagger b) (magicl:dagger c))))
                                          x-rot)))))

(deftest test-gc-decompose ()
  (dotimes (i 100)
    (let ((u (magicl:random-unitary '(2 2))))
      (multiple-value-bind (v w) (cl-quil::gc-decompose u)
        (fiasco-assert-matrices-are-equal (magicl:@ v (magicl:@ w (magicl:@ (magicl:dagger v) (magicl:dagger w))))
                                          u)))))

(deftest test-gc-decompose-alt ()
  (dotimes (i 100)
    (let ((u (magicl:random-unitary '(2 2))))
      (multiple-value-bind (v w) (cl-quil::gc-decompose-alt u)
        (fiasco-assert-matrices-are-equal (magicl:@ v (magicl:@ w (magicl:@ (magicl:dagger v) (magicl:dagger w))))
                                          u)))))
