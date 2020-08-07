;;;; ansatz-search-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil-tests)

(deftest test-ansatz-search-euler ()
  "Can we reproduce simple Euler decompositions?"
  (flet ((test-rz ()
           (let* ((a (random pi))
                  (zyz (quil::example-matrix->zyz (quil::rz-matrix a))))

             ;; We double-noodle these zero-equal tests because ansatz
             ;; stuff isn't terribly accurate.
             ;;
             ;; Y-component should be 0.
             (is (quil::double~ 0.0d0 (aref zyz 1)))
             ;; Z-components should add to 0.
             (is (quil::double~ a (mod (+ (aref zyz 0)
                                          (aref zyz 2))
                                       (* 2 pi))))
             ))
         (test-random ()
           (let* ((a (quil::random-unitary '(2 2)))
                  (zyz (quil::example-matrix->zyz a)))
             (is (quil::matrix-equals-dwim a (magicl:@ (quil::rz-matrix (aref zyz 2))
                                                       (quil::ry-matrix (aref zyz 1))
                                                       (quil::rz-matrix (aref zyz 0))))))))
    (dotimes (i 100)
      (test-rz))
    (dotimes (i 100)
      (test-random))))
