;;;; build-gate.lisp
;;;; 
;;;; Author: Eric Peterson
;;;;
;;;; This file collects the gate-construction routines in CL-QUIL which are
;;;; used as internal sugar for otherwise ugly constructions like
;;;;
;;;; (make-instance 'gate-application :blah 'blah-blah ...) .

(in-package #:cl-quil)


;;; first, some simple utilities for constructing gate objects

(defun build-gate (operator params &rest qubits)
  "Shorthand function for constructing a GATE-APPLICATION object from QUIL-like input. OPERATOR must be a standard gate name."
  (check-type params list)
  (check-type qubits list)
  (assert (not (null qubits)))
  (flet ((capture-param (param)
           (typecase param
             (double-float
              (constant param))
             (memory-ref
              (make-delayed-expression nil nil param))
             (otherwise
              param)))
         (capture-arg (arg)
           (typecase arg
             (integer
              (qubit arg))
             (symbol
              (formal (string-downcase (symbol-name arg))))
             (otherwise
              arg))))
    (let* ((operator-adt
             (etypecase operator
               (string (named-operator operator))
               (operator-description operator)))
           (gate-def
             (etypecase operator
               (string (lookup-standard-gate operator))
               (operator-description
                (labels ((recurse (o)
                           (adt:match operator-description o
                             ((named-operator str)
                              (lookup-standard-gate str))
                             ((dagger-operator o)
                              (recurse o))
                             ((controlled-operator o)
                              (recurse o))
                             ((forked-operator o)
                              (recurse o)))))
                  (recurse operator))))))
      (assert (not (null gate-def)) (operator) "BUILD-GATE only takes standard gate names.")
      (make-instance 'gate-application
                     :operator operator-adt
                     :name-resolution gate-def
                     :parameters (mapcar #'capture-param params)
                     :arguments (mapcar #'capture-arg qubits)))))

(define-global-counter **anonymous-gate-counter** get-anonymous-gate-counter)

(defun anon-gate (operator matrix &rest qubits)
  "Variant of BUILD-GATE for constructing anonymous gate applications."
  (check-type operator string)
  (check-type matrix magicl:matrix)
  (assert (not (endp qubits)))
  (flet ((capture-arg (arg)
           (typecase arg
             (integer
              (qubit arg))
             (symbol
              (formal (string-downcase (symbol-name arg))))
             (otherwise
              arg))))
    (make-instance 'gate-application
                   :operator (named-operator (format nil "~a-~a" operator (get-anonymous-gate-counter)))
                   :gate matrix
                   :arguments (mapcar #'capture-arg qubits))))

(defun build-UCR (roll-name params &rest args)
  (let ((op (named-operator roll-name)))
    (dolist (x (rest args))
      (declare (ignore x))
      (setf op (forked-operator op)))
    (apply #'build-gate op params args)))

;;; functions for dealing with mixed constant vs delayed-expression types

(defun param-binary-op (op arg1 arg2)
  "Binary operator that safely applies to possibly mixed arguments of NUMBER / DELAYED-EXPRESSION objects."
  (flet ((decompose (arg)
           "Returns a values tuple: PARAMS LAMBDA-PARAMS EXPRESSION DELAYEDP"
           (optima:match arg
             ((delayed-expression (params arg-params)
                                  (lambda-params arg-lambda-params)
                                  (expression arg-expression))
              (values
               arg-params
               arg-lambda-params
               arg-expression
               t))
             ((constant (value val))
              (values nil nil val nil))
             (_
              (values nil nil arg nil)))))
    (multiple-value-bind (params-1 lambda-params-1 expression-1 delayedp-1) (decompose arg1)
      (multiple-value-bind (params-2 lambda-params-2 expression-2 delayedp-2) (decompose arg2)
        (let ((params-zipped (union (mapcar #'list params-1 lambda-params-1)
                                    (mapcar #'list params-2 lambda-params-2)
                                    :test #'equal)))
          (loop :with zipped-tail := params-zipped
                :for head :in params-zipped
                :do (setf zipped-tail (rest zipped-tail))
                :when (find (second head) zipped-tail :key #'second :test #'equal)
                  :do (error "Duplicate definition of lambda-param: ~a" (second head)))
          (if (or delayedp-1 delayedp-2)
              (make-delayed-expression (mapcar #'first params-zipped)
                                       (mapcar #'second params-zipped)
                                       `(,op ,expression-1 ,expression-2))
              (funcall op expression-1 expression-2)))))))

(defun param-+ (arg1 arg2)
  (param-binary-op '+ arg1 arg2))

(defun param-* (arg1 arg2)
  (param-binary-op '* arg1 arg2))
