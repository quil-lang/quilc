;;;; simplify-arithmetic.lisp
;;;;
;;;; Author: Peter Karalekas
;;;;
;;;; This file defines a SIMPLIFY-ARITHMETIC function which gets used
;;;; in the SIMPLIFY-INDIVIDUAL-INSTRUCTIONS transform, which when
;;;; applied to a PARSED-PROGRAM will iterate through all the
;;;; instructions in its executable code, attempting to reduce the
;;;; complexity of the arithmetic expressions contained in each
;;;; instruction's set of parameters. For example, the following is a
;;;; valid Quil program:
;;;;
;;;;    DECLARE theta REAL[2]
;;;;    RX(2.0 + theta[0] - 2.0) 0
;;;;    RZ(3.0*theta[1] - theta[1]/2.0 + 5.0) 1
;;;;
;;;; However, clearly some of the arithmetic in the parameters of the
;;;; RX and RZ gates could be made simpler. SIMPLIFY-ARITHMETIC takes
;;;; these expressions and stores them in their
;;;; AFFINE-REPRESENTATIONs, which give a canonical form for
;;;; equivalent linear arithmetic expressions. Then, these structures
;;;; can be unpacked once again into (potentially) simpler arithmetic
;;;; expressions. For example, after applying the simplifications to
;;;; the above Quil program, we get the following (simpler) program:
;;;;
;;;;    DECLARE theta REAL[2]
;;;;    RX(theta[0]) 0
;;;;    RZ(2.5*theta[1] + 5.0) 1
;;;;
;;;; This implementation is certainly not without its limitations. For
;;;; example, we can only simplify a particular subset of arithmetic
;;;; expressions (ones that are linear). However, for the majority of
;;;; our use cases at this time, this subset is sufficient.

(in-package #:cl-quil.frontend)

(define-condition expression-not-simplifiable (serious-condition)
  ()
  (:documentation "A condition that is signaled any time an expression cannot be simplified. In general, a more specific conditions should be preferred over signaling this one."))

(define-condition expression-not-linear (expression-not-simplifiable)
  ()
  (:documentation "A condition that is signaled any time an expression cannot be simplified due to being non-linear."))

(defstruct (affine-representation (:constructor %affine-representation (constant coefficients)))
  "This data structure represents linear arithmetic expressions of the form

   CONSTANT + COEFF_0 * MREF_0 + COEFF_1 * MREF_1 + ... + COEFF_n * MREF_n

 where

   CONSTANT is the constant offset (of type NUMBER) of the expression
   COEFF_i is the coefficient (of type NUMBER) for MREF_i
   MREF_i is the MEMORY-REF with coefficient COEFF_i
"
  (constant nil :type number)
  (coefficients nil :type hash-table :read-only t))

(defun make-affine-representation (constant &rest keyval)
  "Make an AFFINE-REPRESENTATION object from a CONSTANT and an optional collection of MEMORY-REF -> COEFFICIENT key-value pairs for building the COEFFICIENTS hash table. Thus, the function would be used by running

   (make-affine-representation CONSTANT)

which produces an AFFINE-REPRESENTATION with an empty COEFFICIENTS hash table, or

   (make-affine-representation CONSTANT MREF_0 COEFF_0 ... MREF_N COEFF_N)

which fills in the COEFFICIENTS hash table with the memory references and their corresponding coefficients."
  (assert (evenp (length keyval)))
  (let ((coefficients (make-hash-table :test 'memory-ref=
                                       :hash-function 'memory-ref-hash)))
    (loop :for (ref coefficient) :on keyval :by #'cddr
          :do (assert (numberp coefficient))
              (setf (gethash ref coefficients) coefficient))
    (%affine-representation constant coefficients)))

(defun combine-affine-representations (operator left right)
  "Given an operator (e.g. +, -, *, /) and two operand expressions (LEFT and RIGHT, in AFFINE-REPRESENTATION form), combine them via the rules of arithmetic, enforcing the linearity of the output AFFINE-REPRESENTATION.  When the OPERATOR is + or -, there are no restrictions on the contents of LEFT or RIGHT. If the OPERATOR is *, one of either LEFT or RIGHT must be simply a CONSTANT value, meaning that its COEFFICIENTS hash table is empty (otherwise we give up simplifying). If the OPERATOR is /, the RIGHT operand must be a CONSTANT value, meaning that its COEFFICIENTS hash table must be empty (otherwise we give up simplifying)."
  (check-type operator (member + - * /))
  (let ((rep (make-affine-representation
              (funcall operator
                       (affine-representation-constant left)
                       (affine-representation-constant right)))))
    (ecase operator
      (+
       (dohash ((ref coefficient) (affine-representation-coefficients left))
           (setf (gethash ref (affine-representation-coefficients rep)) coefficient))
       (dohash ((ref coefficient) (affine-representation-coefficients right))
           (if (gethash ref (affine-representation-coefficients rep))
               (incf (gethash ref (affine-representation-coefficients rep)) coefficient)
               (setf (gethash ref (affine-representation-coefficients rep)) coefficient)))
       rep)
      (-
       (dohash ((ref coefficient) (affine-representation-coefficients left))
           (setf (gethash ref (affine-representation-coefficients rep)) coefficient))
       (dohash ((ref coefficient) (affine-representation-coefficients right))
           (if (gethash ref (affine-representation-coefficients rep))
               (decf (gethash ref (affine-representation-coefficients rep)) coefficient)
               (setf (gethash ref (affine-representation-coefficients rep)) (- coefficient))))
       rep)
      (*
       (unless (or (zerop (hash-table-count (affine-representation-coefficients left)))
                   (zerop (hash-table-count (affine-representation-coefficients right))))
         (error 'expression-not-linear))
       (dohash ((ref coefficient) (affine-representation-coefficients left))
           (setf (gethash ref (affine-representation-coefficients rep))
                 (* (affine-representation-constant right) coefficient)))
       (dohash ((ref coefficient) (affine-representation-coefficients right))
           (setf (gethash ref (affine-representation-coefficients rep))
                 (* (affine-representation-constant left) coefficient)))
       rep)
      (/
       (unless (zerop (hash-table-count (affine-representation-coefficients right)))
         (error 'expression-not-linear))
       (dohash ((ref coefficient) (affine-representation-coefficients left))
           (setf (gethash ref (affine-representation-coefficients rep))
                 (/ coefficient (affine-representation-constant right))))
       rep))))

(defun expression->affine-representation (de)
  "Recursively construct an AFFINE-REPRESENTATION from DE. In each step of the recursion, DE is either a NUMBER, MEMORY-REF, or a tree of arithmetic expressions written in prefix notation. If DE is a NUMBER or MEMORY-REF, that branch of the recursion terminates. Otherwise, the expression tree is broken up into its operator, the left operand, and the right operand. The function acts recursively upon each of the left and right operands, and the resulting AFFINE-REPRESENTATIONs are combined per the operator. The following is a visualization of the recursion:

            de: (+ 2.0 (* 2.0 theta[0]))
           //                          \\
   left: 2.0                           right: (* 2.0 theta[0])
                                       //                    \\
                               left: 2.0                     right: theta[0]
"
  (typecase de
    (number
     (make-affine-representation de))
    (memory-ref
     (make-affine-representation 0d0 de 1.0))
    (cons
     (destructuring-bind (op left &optional right) de
       (if (and (null right) (eq op '-))
           ;; Special case for negative-prefixed memory references
           ;; e.g. RX(-theta[0] + theta[0] + 2.0) 0 -> RX(2.0) 0
           (expression->affine-representation `(* -1.0 ,left))
           (combine-affine-representations
            op
            (expression->affine-representation left)
            ;; If right is null, this will signal expression-not-simplifiable
            (expression->affine-representation right)))))
    (otherwise (error 'expression-not-simplifiable))))

(defun affine-representation->expression (rep)
  "Build a tree of arithmetic expressions written in prefix notation by iterating through the COEFFICIENTS hash table of an AFFINE-REPRESENTATION, and finally adding the CONSTANT at the end. The following is a visualization of how an example AFFINE-REPRESENTATION (left) is built up to an increasingly more complicated tree of expressions (right):

AFFINE-REPRESENTATION                       (* 2.0 theta[0])
   CONSTANT: 3.0                                    |
                                                    v
                       ====>      (+ (* 5.0 theta[1]) (* 2.0 theta[0]))
   COEFFICIENTS:                                    |
      theta[0]   2.0                                v
      theta[1]   5.0          (+ 3.0 (+ (* 5.0 theta[1]) (* 2.0 theta[0])))
"
  (let ((expr nil))
    (dohash ((ref coefficient) (affine-representation-coefficients rep))
      (cond
        ((double= 0 coefficient)        ; skip
         nil)
        ((double= 1 coefficient)        ; don't add '1 *` factor
         (setf expr (if expr
                       `(+ ,ref ,expr)
                       ref)))
        (t
         (setf expr (if expr
                       `(+ (* ,coefficient ,ref) ,expr)
                       `(* ,coefficient ,ref))))))
    (cond
      ((null expr)
       (affine-representation-constant rep))
      ((double= 0 (affine-representation-constant rep))
       expr)
      (t
       `(+ ,(affine-representation-constant rep) ,expr)))))

(defun canonicalize-expression (de)
  "Given DE (a DELAYED-EXPRESSION), canonicalize it by converting it into its AFFINE-REPRESENTATION form and back into a DELAYED-EXPRESSION once again. Refer to the detailed documentation for the expression->affine-representation and affine-representation->expression functions for more info."
  (evaluate-delayed-expression
   (make-delayed-expression (delayed-expression-params de)
                            (delayed-expression-lambda-params de)
                            (affine-representation->expression
                             (expression->affine-representation
                              (delayed-expression-expression de))))))

(defgeneric simplify-arithmetic (thing)
  (:documentation "Generic function that defines the underlying mechanics for the SIMPLIFY-INDIVIDUAL-INSTRUCTIONS transform. If this function is given a PARSED-PROGRAM, it recursively applies itself to the program's exectuable code. Otherwise, if this function is given a GATE-APPLICATION, it attempts to simplify the gate parameters by canonicalizing the arithmetic expressions they (potentially) contain.")
  (:method ((thing t))
    thing)
  (:method ((thing constant))
    thing)
  (:method ((thing delayed-expression))
    (handler-case
        (canonicalize-expression thing)
      (expression-not-linear () thing)
      (expression-not-simplifiable () thing)))
  (:method ((thing gate-application))
    (let ((new-gate (copy-instance thing)))
      (setf (application-parameters new-gate)
            (mapcar #'simplify-arithmetic
                    (application-parameters new-gate)))
      new-gate))
  (:method ((thing parsed-program))
    (map-into (parsed-program-executable-code thing)
              #'simplify-arithmetic
              (parsed-program-executable-code thing))
    thing))
