;;;; tests/quilec/code.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:cl-quil.quilec-tests)

(deftest test-make-code ()
  (let* ((symbols '((i x y z)
                    (z i x y)
                    (y z i x)
                    (x y z i)))
         (code (make-code symbols)))
    (let ((generators (qec::generators code)))
      (is (= (length generators) (length symbols)))
      (loop :for generator :across generators
            :for symbol :in symbols :do
              (is (equalp generator
                          (cl-quil.clifford::pauli-from-symbols symbol)))))))

(deftest test-code-matrices ()
  (let ((input-code (make-code '((i x y z i x x i)
                                 (z i x y i x x i)
                                 (y z i x i x x i)
                                 (x y z i i x x i)))))
    (multiple-value-bind (x z)
        (qec::code-to-matrices input-code)
      (let ((output-code (qec::code-from-matrices x z)))
        (is (equalp (qec::generators input-code)
                    (qec::generators output-code)))))))
