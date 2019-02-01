;;;; utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil-tests)

;;; Cribbed from QVM-TESTS
(defmacro with-output-to-quil (&body body)
  `(let ((quil:*allow-unresolved-applications* t))
     (quil:parse-quil-string
      (with-output-to-string (*standard-output*)
        ,@(loop :for form :in body
                :if (stringp form)
                  :collect `(write-line ,form)
                :else
                  :collect form)))))

(defun fiasco-assert-matrices-are-equal (m u)
  (is (= (magicl:matrix-rows u) (magicl:matrix-rows m)))
  (is (= (magicl:matrix-cols u) (magicl:matrix-cols m)))
  (setf u (quil::scale-out-matrix-phases u m))
  (is (loop :for i :below (magicl:matrix-rows m) :always
         (loop :for j :below (magicl:matrix-cols m) :always
            (< (abs (- (magicl:ref m i j) (magicl:ref u i j))) 0.01)))
      (with-output-to-string (s)
        (format s "Matrix comparison failed.~%Input matrix:")
        (magicl::pprint-matrix s m)
        (format s "~%Output matrix:~%")
        (magicl::pprint-matrix s u))))

(defun build-anonymous-gate (matrix &rest qubit-indices)
  (make-instance 'cl-quil::gate-application
                 :operator (cl-quil::named-operator "TEST")
                 :arguments (mapcar #'cl-quil::qubit qubit-indices)
                 :gate matrix))

(defun first-column-operator= (mat1 mat2)
  (multiple-value-bind (mat1 mat2) (quil::matrix-rescale mat1 mat2)
    (setf mat1 (quil::scale-out-matrix-phases mat1 mat2))
    (quil::matrix-first-column-equality mat1 mat2)))

(defun operator= (mat1 mat2)
  (multiple-value-bind (mat1 mat2) (quil::matrix-rescale mat1 mat2)
    (setf mat1 (quil::scale-out-matrix-phases mat1 mat2))
    (quil::matrix-equality mat1 mat2)))

(defun matrix-equals-dwim (mat1 mat2)
  "Returns true if mat1 is equal to mat2, with the specific notion of equality
depending on whether *ENABLE-STATE-PREP-COMPRESSION* is enabled."
  (funcall (if quil::*enable-state-prep-compression*
               #'first-column-operator=
               #'operator=)
           mat1
           mat2))
