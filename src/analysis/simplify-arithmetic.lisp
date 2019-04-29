;;;; simplify-arithmetic.lisp
;;;;
;;;; Author: Peter Karalekas

(in-package #:cl-quil)

(define-transform simplify-arithmetic (simplify-arithmetic)
  "A transform which converts a parsed program with potentially complicated arithmetic to one that has simplified arithmetic expressions")

(defstruct affine-representation
  ""
  (constant 0d0 :type double-float)
  (coefficients (make-hash-table :test #'equalp) :type hash-table))

(defun expression->affine-representation (de)
  ""
  (etypecase de
    (double-float
     (make-affine-representation :constant de))
    (memory-ref
     (let ((rep (make-affine-representation)))
       (setf (gethash de (affine-representation-coefficients rep)) 1.0)
       rep))
    (cons
     (let ((left (expression->affine-representation (second de)))
           (right (expression->affine-representation (third de))))
       (ecase (car de)
         (+
          (let ((rep (make-affine-representation)))
            (setf (affine-representation-constant rep)
                  (+ (affine-representation-constant left)
                     (affine-representation-constant right)))
            (dohash ((ref coefficient) (affine-representation-coefficients left))
              (setf (gethash ref (affine-representation-coefficients rep)) coefficient))
            (dohash ((ref coefficient) (affine-representation-coefficients right))
              (if (gethash ref (affine-representation-coefficients rep))
                  (incf (gethash ref (affine-representation-coefficients rep)) coefficient)
                  (setf (gethash ref (affine-representation-coefficients rep)) coefficient)))
            rep))
         (-
          (let ((rep (make-affine-representation)))
            (setf (affine-representation-constant rep)
                  (- (affine-representation-constant left)
                     (affine-representation-constant right)))
            (dohash ((ref coefficient) (affine-representation-coefficients left))
              (setf (gethash ref (affine-representation-coefficients rep)) coefficient))
            (dohash ((ref coefficient) (affine-representation-coefficients right))
              (if (gethash ref (affine-representation-coefficients rep))
                  (decf (gethash ref (affine-representation-coefficients rep)) coefficient)
                  (setf (gethash ref (affine-representation-coefficients rep)) (- coefficient))))
            rep))
         (*
          (let ((rep (make-affine-representation)))
            (unless (or (zerop (hash-table-count (affine-representation-coefficients left)))
                        (zerop (hash-table-count (affine-representation-coefficients right))))
              (error ""))
            (setf (affine-representation-constant rep)
                  (* (affine-representation-constant left)
                     (affine-representation-constant right)))
            (dohash ((ref coefficient) (affine-representation-coefficients left))
              (setf (gethash ref (affine-representation-coefficients rep))
                    (* (affine-representation-constant right) coefficient)))
            (dohash ((ref coefficient) (affine-representation-coefficients right))
              (setf (gethash ref (affine-representation-coefficients rep))
                    (* (affine-representation-constant left) coefficient)))
            rep))
         (/
          (let ((rep (make-affine-representation)))
            (unless (zerop (hash-table-count (affine-representation-coefficients right)))
              (error ""))
            (setf (affine-representation-constant rep)
                  (/ (affine-representation-constant left)
                     (affine-representation-constant right)))
            (dohash ((ref coefficient) (affine-representation-coefficients left))
              (setf (gethash ref (affine-representation-coefficients rep))
                    (/ coefficient (affine-representation-constant right))))
            rep)))))))

(defgeneric simplify-arithmetic (thing)
  (:method ((thing t))
    thing)
  (:method ((thing gate-application))
    (map-into (application-parameters thing)
              (lambda (param)
                (etypecase param
                  (constant
                   param)
                  (delayed-expression
                   ;; TODO: extract expression and use expression->affine-representation
                   param)))
              (application-parameters thing))))

(defun simplify-arithmetics (parsed-prog)
  ""
  (map nil #'simplify-arithmetic (parsed-program-executable-code parsed-prog))
  parsed-prog)
