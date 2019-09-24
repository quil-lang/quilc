;;;; simplify-arithmetic.lisp
;;;;
;;;; Author: Peter Karalekas
;;;;
;;;; This file defines the SIMPLIFY-ARITHMETIC transform, which when applied to a PARSED-PROGRAM will
;;;; iterate through all the instructions in its executable code, attempting to reduce the complexity
;;;; of the arithmetic expressions contained in each instruction's set of parameters. For example,
;;;; the following is a valid Quil program:
;;;;
;;;;    DECLARE theta REAL[2]
;;;;    RX(2.0 + theta[0] - 2.0) 0
;;;;    RZ(3.0*theta[1] - theta[1]/2.0 + 5.0) 1
;;;;
;;;; However, clearly some of the arithmetic in the parameters of the RX and RZ gates could be made
;;;; simpler. The SIMPLIFY-ARITHMETIC transform takes these expressions and stores them in their
;;;; AFFINE-REPRESENTATIONs, which give a canonical form for equivalent linear arithmetic
;;;; expressions. Then, these structures can be unpacked once again into (potentially) simpler
;;;; arithmetic expressions. For example, after applying the transform to the above Quil program,
;;;; we get the following (simpler) program:
;;;;
;;;;    DECLARE theta REAL[2]
;;;;    RX(theta[0]) 0
;;;;    RZ(2.5*theta[1] + 5.0) 1
;;;;
;;;; This implementation is certainly not without its limitations. For example, we can only simplify
;;;; a particular subset of arithmetic expressions (ones that are linear). However, for the majority
;;;; of our use cases at this time, this subset is sufficient.

(in-package #:cl-quil)

(define-condition expression-not-simplifiable (serious-condition)
  ()
  (:documentation "A condition that is signalled any time an expression cannot be simplified.
In general, a more specific conditionxs should be preferred over signalling this one."))

(define-condition expression-not-linear (expression-not-simplifiable)
  ()
  (:documentation "A condition that is signalled any time an expression cannot be simplified due to
being non-linear."))

(define-transform simplify-arithmetic (simplify-arithmetic)
  "A transform which converts a parsed program with potentially complicated arithmetic to one that
has simplified arithmetic expressions.")

(defstruct (affine-representation (:constructor %affine-representation))
  "This data structure represents linear arithmetic expressions of the form

   CONSTANT + COEFFICIENTS_0 * MEMORY-REF_0 + COEFFICIENTS_1 * MEMORY-REF_1 + ... + COEFFICIENTS_n * MEMORY-REF_n

 where

   CONSTANT is the constant offset (of type NUMBER) of the expression
   COEFFICIENTS_i is the coefficient (of type NUMBER) for MEMORY-REF_i
   MEMORY-REF_i is the MEMORY-REF with coefficient COEFFICIENTS_i
"
  (constant 0 :type number)
  ;; This builds a hash table with MEMORY-REFs as keys and their coefficients (of type NUMBER) as values
  (coefficients (make-hash-table :test 'memory-ref= :hash-function 'memory-ref-hash) :type hash-table))

(defun make-affine-representation (constant &rest keyval)
  "Make an AFFINE-REPRESENTATION object from a CONSTANT and an optional collection of MEMORY-REF ->
COEFFICIENT key-value pairs for building the COEFFICIENTS hash table. Thus, the function would be
used by running

   (make-affine-representation CONSTANT)

which produces an AFFINE-REPRESENTATION with an empty COEFFICIENTS hash table, or

   (make-affine-representation CONSTANT MEMORY-REF_0 COEFFICIENTS_0 ... MEMORY-REF_N COEFFICIENTS_N)

which fills in the COEFFICIENTS hash table with the memory references and their corresponding coefficients."
  (let ((coefficients (make-hash-table :test 'memory-ref= :hash-function 'memory-ref-hash)))
    (loop :for (ref coefficient) :on keyval :by #'cddr :do (setf (gethash ref coefficients) coefficient))
    (%affine-representation :constant constant
                            :coefficients coefficients)))

(defun combine-affine-representations (operator left right)
  "Given an operator (e.g. +, -, *, /) and two operand expressions (LEFT and RIGHT, in AFFINE-REPRESENTATION
form), combine them via the rules of arithmetic, enforcing the linearity of the output AFFINE-REPRESENTATION.
When the OPERATOR is + or -, there are no restrictions on the contents of LEFT or RIGHT. If the OPERATOR
is *, one of either LEFT or RIGHT must be simply a CONSTANT value, meaning that its COEFFICIENTS hash table
is empty (otherwise we give up simplifying). If the OPERATOR is /, the RIGHT operand must be a CONSTANT
value, meaning that its COEFFICIENTS hash table must be empty (otherwise we give up simplifying)."
  (ecase operator
    (+
     (let ((rep (make-affine-representation
                 (+ (affine-representation-constant left)
                    (affine-representation-constant right)))))
       (dohash ((ref coefficient) (affine-representation-coefficients left))
         (setf (gethash ref (affine-representation-coefficients rep)) coefficient))
       (dohash ((ref coefficient) (affine-representation-coefficients right))
         (if (gethash ref (affine-representation-coefficients rep))
             (incf (gethash ref (affine-representation-coefficients rep)) coefficient)
             (setf (gethash ref (affine-representation-coefficients rep)) coefficient)))
       rep))
    (-
     (let ((rep (make-affine-representation
                 (- (affine-representation-constant left)
                    (affine-representation-constant right)))))
       (dohash ((ref coefficient) (affine-representation-coefficients left))
         (setf (gethash ref (affine-representation-coefficients rep)) coefficient))
       (dohash ((ref coefficient) (affine-representation-coefficients right))
         (if (gethash ref (affine-representation-coefficients rep))
             (decf (gethash ref (affine-representation-coefficients rep)) coefficient)
             (setf (gethash ref (affine-representation-coefficients rep)) (- coefficient))))
       rep))
    (*
     (unless (or (zerop (hash-table-count (affine-representation-coefficients left)))
                 (zerop (hash-table-count (affine-representation-coefficients right))))
       (error 'expression-not-linear))
     (let ((rep (make-affine-representation
                 (* (affine-representation-constant left)
                    (affine-representation-constant right)))))
       (dohash ((ref coefficient) (affine-representation-coefficients left))
         (setf (gethash ref (affine-representation-coefficients rep))
               (* (affine-representation-constant right) coefficient)))
       (dohash ((ref coefficient) (affine-representation-coefficients right))
         (setf (gethash ref (affine-representation-coefficients rep))
               (* (affine-representation-constant left) coefficient)))
       rep))
    (/
     (unless (zerop (hash-table-count (affine-representation-coefficients right)))
       (error 'expression-not-linear))
     (let ((rep (make-affine-representation
                 (/ (affine-representation-constant left)
                    (affine-representation-constant right)))))
       (dohash ((ref coefficient) (affine-representation-coefficients left))
         (setf (gethash ref (affine-representation-coefficients rep))
               (/ coefficient (affine-representation-constant right))))
       rep))))

(defun expression->affine-representation (de)
  "Recursively construct an AFFINE-REPRESENTATION from DE. In each step of the recursion, DE is either
a NUMBER, MEMORY-REF, or a tree of arithmetic expressions written in prefix notation. If DE is a NUMBER
or MEMORY-REF, that branch of the recursion terminates. Otherwise, the expression tree is broken up into
its operator, the left operand, and the right operand. The function acts recursively upon each of the left
and right operands, and the resulting AFFINE-REPRESENTATIONs are combined per the operator. The following
is a visualization of the recursion:

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
     (make-affine-representation 0 de 1.0))
    (cons
     (let ((left (expression->affine-representation (second de)))
           (right (expression->affine-representation (third de))))
       (combine-affine-representations (car de) left right)))
    (otherwise (error 'expression-not-simplifiable))))

(defun affine-representation->expression (rep)
  "Build a tree of arithmetic expressions written in prefix notation by iterating through the
COEFFICIENTS hash table of an AFFINE-REPRESENTATION, and finally adding the CONSTANT at the end.
The following is a visualization of how an example AFFINE-REPRESENTATION (left) is built up
to an increasingly more complicated tree of expressions (right):

AFFINE-REPRESENTATION                                    (* 2.0 theta[0])
   CONSTANT: 3.0                                                |
                                                                v
                              ====>           (+ (* 5.0 theta[1]) (* 2.0 theta[0]))
   COEFFICIENTS:                                                |
      theta[0]   2.0                                            v
      theta[1]   5.0                      (+ 3.0 (+ (* 5.0 theta[1]) (* 2.0 theta[0])))
"
  (let ((expr nil))
    (dohash ((ref coefficient) (affine-representation-coefficients rep))
      (unless (double= 0 coefficient)
        (setf expr (if expr
                       `(+ (* ,coefficient ,ref) ,expr)
                       `(* ,coefficient ,ref)))))
    (cond
      ((null expr)
       (affine-representation-constant rep))
      ((double= 0 (affine-representation-constant rep))
       expr)
      (t
       `(+ ,(affine-representation-constant rep) ,expr)))))

(defun canonicalize-expression (de)
  "Given DE (a DELAYED-EXPRESSION), canonicalize it by converting it into its AFFINE-REPRESENTATION
form and back into a DELAYED-EXPRESSION once again. Refer to the detailed documentation for the
expression->affine-representation and affine-representation->expression functions for more info."
  (make-delayed-expression (delayed-expression-params de)
                           (delayed-expression-lambda-params de)
                           (affine-representation->expression
                            (expression->affine-representation
                             (delayed-expression-expression de)))))

(defgeneric simplify-arithmetic (thing)
  (:documentation "Generic function that defines the underlying mechanics for the SIMPLIFY-ARITHMETIC
transform. If this function is given a PARSED-PROGRAM, it recursively applies itself to the program's
exectuable code. Otherwise, if this function is given a GATE-APPLICATION, it attempts to simplify
the gate parameters by canonicalizing the arithmetic expressions they (potentially) contain.")
  (:method ((thing t))
    thing)
  (:method ((thing gate-application))
    (let ((new-gate (copy-instance thing)))
      (setf (application-parameters new-gate)
            (map 'cons (lambda (de)
                         (handler-case
                             (typecase de
                               (constant
                                de)
                               (delayed-expression
                                (canonicalize-expression de))
                               (otherwise de))
                           (expression-not-linear () de)
                           (expression-not-simplifiable () de)))
                 (application-parameters new-gate)))
      new-gate))
  (:method ((thing parsed-program))
    (map-into (parsed-program-executable-code thing) #'simplify-arithmetic (parsed-program-executable-code thing))
    thing))
