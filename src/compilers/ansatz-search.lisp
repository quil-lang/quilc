;;;; ansatz-search.lisp
;;;;
;;;; Author: Robert Smith
;;;;         Cole Scott

(in-package #:cl-quil)

;;; This file contains a simple procedure for taking a "template"
;;; parametric circuit and using Nelder-Mead to find parameters to
;;; fill in that circuit with.
;;;
;;; This function may be useful to build compilers with.

(defun solve-for-ansatz-inputs (matrix k ansatz &key initial-guess)
  "Let MATRIX be a unitary matrix. Let ANSATZ be a function taking a vector of K reals (as DOUBLE-FLOATs) as input and producing a unitary matrix whose dimension is equal to MATRIX.

SOLVE-FOR-ANSATZ-INPUTS will attempt to find a vector of values v such that MATRIX == (FUNCALL ANSATZ v).

The keyword argument INITIAL-GUESS is optional to allow control over the optimization process. It should be a vector of K double floats, or NIL. (By default, the a random initial guess will be generated.)

If it is found, a vector #(t1 ... tK) will be returned.

If it is not found (with enough accuracy to satisfy DOUBLE=), return NIL.
"
  (assert (magicl:square-matrix-p matrix))
  (check-type k (integer 1))
  (check-type ansatz (or symbol function))
  (let* ((1/dim (coerce (/ (elt (magicl:shape matrix) 0)) 'double-float))
         (matrix-dagger (magicl:dagger matrix)))
    (flet ((cost (thetas)
             (- 1.0d0 (* 1/dim
                         (realpart
                          (abs
                           (magicl:trace
                            (magicl:@ matrix-dagger
                                      (funcall ansatz thetas)))))))))
      (declare (dynamic-extent #'cost))
      (let* ((guess (or initial-guess
                        (make-array k :element-type 'double-float
                                      :initial-element (random 0.01d0))))
             (answer (cl-grnm:nm-optimize #'cost guess))
             (final-cost (cost answer)))
        (cond
          ((double= 0.0d0 final-cost)
           answer)
          (t
           nil))))))

(defun function-list-ansatz (list)
  "Let LIST be a list of n>0 functions A1, ..., An each mapping a real number (double float) to a unitary matrix (each of the same dimension). Return an ansatz function taking a vector of parameters #(t1 ... tn) as a single argument and returning

    An(tn) * ... * A1(t1)

as a single matrix."
  (assert (not (null list)))
  (let ((n (length list)))
    (lambda (thetas)
      (assert (= n (length thetas)))
      (let ((matrices nil))
        (loop :for f :in list
              :for theta :across thetas
              :do (push (funcall f theta) matrices))
        (reduce #'magicl:@ matrices)))))


;;; Below are examples purely for illustrative purposes.
;;;
;;; They're also used in CL-QUIl-TESTS!

;;; Example #1: Euler decomposition

(defun ry-matrix (theta)
  (let* ((cos (cos (/ theta 2.0d0)))
         (sin (sin (/ theta 2.0d0)))
         (entries (list cos (- sin)
                        sin cos)))
    (declare (dynamic-extent entries))
    (magicl:from-list
     entries
     '(2 2)
     :type '(complex double-float))))

(defun rz-matrix (theta)
  (let ((entries (list (cis (- (/ theta 2.0d0))) 0
                       0                         (cis (/ theta 2.0d0)))))
    (declare (dynamic-extent entries))
    (magicl:from-list
     entries
     '(2 2)
     :type '(complex double-float))))

(defun example-matrix->zyz (matrix)
  ;; QUIL> (example-matrix->zyz (rz-matrix 0.2))
  ;; #(0.07756948907697571d0 7.905827293715612d-9 0.12243051125234047d0)
  ;; QUIL> (example-matrix->zyz (rz-matrix 0.0))
  ;; #(-4.5812693316469753d-4 9.02959081120493d-9 4.5812983104571173d-4)
  ;; QUIL> (example-matrix->zyz (random-unitary '(2 2)))
  ;; #(1.122230993309917d0 -1.8985151823268946d0 1.8010987789367303d0)
  (let ((ansatz (function-list-ansatz '(rz-matrix
                                        ry-matrix
                                        rz-matrix))))
    (solve-for-ansatz-inputs matrix 3 ansatz)))

;;; Example #2: Find an exotic non-orthogonal decomposition
;;;
;;; Given the matrices RZ(theta) and 
;;;
;;;    U(theta) = T * RY(theta) * T^-1,
;;;
;;; how might we express a matrix as U * RZ * U * RZ?

(defun example-matrix->weird (matrix)
;; QUIL> (example-matrix->weird (rz-matrix 0))
;; #(7.887363276812386d-4 0.0017777576605988899d0 -7.887385870853589d-4 -0.0017777637051886305d0)
;; QUIL> (example-matrix->weird (rz-matrix 0.5))
;; #(0.326465810854041d0 1.0697749430445004d-7 0.17353417761594397d0 -1.3000717067791296d-7)
;; QUIL> (example-matrix->weird (random-unitary '(2 2)))
;; #(2.3763850129347244d0 -0.25785073423800253d0 -0.7510310245749852d0 -0.05059685813635351d0)
  (let* ((t-gate  (gate-matrix (gate-definition-to-gate (lookup-standard-gate "T"))))
         (t-gate* (magicl:dagger t-gate))
         (U (lambda (theta)
              (magicl:@ t-gate
                        (ry-matrix theta)
                        t-gate*)))
         (ansatz (function-list-ansatz (list #'rz-matrix U #'rz-matrix U))))
    (solve-for-ansatz-inputs matrix 4 ansatz)))
