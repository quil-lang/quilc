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
