;;;; tests/quilec/stabilizer-group-tests.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:cl-quil/quilec-tests)

(deftest test-make-group ()
  (let* ((symbols '((i x y z)
                    (z i x y)
                    (y z i x)
                    (x y z i)))
         (code (make-group symbols)))
    (let ((generators (qec::generators code)))
      (is (= (length generators) (length symbols)))
      (loop :for generator :across generators
            :for symbol :in symbols :do
              (is (equalp generator (cl-quil/clifford::pauli-from-symbols symbol)))))))

(deftest test-group-matrices ()
  (let ((input-code (make-group '((i x y z i x x i)
                                  (z i x y i x x i)
                                  (y z i x i x x i)
                                  (x y z i i x x i)))))
    (multiple-value-bind (x z)
        (qec::group-to-matrices input-code)
      (let ((output-code (qec::group-from-matrices x z)))
        (is (equalp (qec::generators input-code)
                    (qec::generators output-code)))))))
