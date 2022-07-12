;;;; ansatz-search-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil-tests)

(deftest test-ansatz-search-euler ()
  "Can we reproduce simple Euler decompositions?"
  (flet ((test-rz ()
           (let* ((a (random pi))
                  (zyz (cl-quil::example-matrix->zyz (cl-quil::rz-matrix a))))

             ;; We double-noodle these zero-equal tests because ansatz
             ;; stuff isn't terribly accurate.
             ;;
             ;; Y-component should be 0.
             (is (cl-quil::double~ 0.0d0 (aref zyz 1)))
             ;; Z-components should add to 0.
             (is (cl-quil::double~ a (mod (+ (aref zyz 0)
                                          (aref zyz 2))
                                       (* 2 pi))))
             ))
         (test-random ()
           (let* ((a (cl-quil::random-unitary '(2 2)))
                  (zyz (cl-quil::example-matrix->zyz a)))
             (is (cl-quil::matrix-equals-dwim a (magicl:@ (cl-quil::rz-matrix (aref zyz 2))
                                                       (cl-quil::ry-matrix (aref zyz 1))
                                                       (cl-quil::rz-matrix (aref zyz 0))))))))
    (dotimes (i 100)
      (test-rz))
    (dotimes (i 100)
      (test-random))))
