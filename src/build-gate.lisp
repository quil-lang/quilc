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

(defun %capture-arg (arg)
  (typecase arg
    (integer
     (qubit arg))
    (symbol
     (formal (string-downcase (symbol-name arg))))
    (otherwise
     arg)))

(defun %capture-param (param)
  (typecase param
    (double-float
     (constant param))
    (memory-ref
     (make-delayed-expression nil nil param))
    (otherwise
     param)))

(defun build-gate (operator params qubit &rest qubits)
  "Shorthand function for constructing a GATE-APPLICATION object from Quil-like input. OPERATOR must be a standard gate name, PARAMS is a list of numbers or FORMALs to be used as parameters, and (LIST* QUBIT QUBITS) is a list of units or FORMALs to be used as qubit arguments.

EXAMPLE: The Quil line \"CPHASE(pi) 2 3\" corresponds to the S-expression (build-gate \"CPHASE\" '(#.pi) 2 3)."
  (check-type params a:proper-list)
  (check-type qubits a:proper-list)
  (check-type operator (or string operator-description symbol))
  (push qubit qubits)
  (let* ((operator-adt
           (etypecase operator
             (symbol (named-operator (string operator)))
             (string (named-operator operator))
             (operator-description operator)))
         (gate-def
           (lookup-standard-gate (operator-description-root-name operator-adt))))
    (assert (not (null gate-def)) (operator) "BUILD-GATE only takes standard gates.")
    (make-instance 'gate-application
                   :operator operator-adt
                   :name-resolution gate-def
                   :parameters (mapcar #'%capture-param params)
                   :arguments (mapcar #'%capture-arg qubits))))

(define-global-counter **anonymous-gate-counter** get-anonymous-gate-counter)

(defun anon-gate (operator gate qubit &rest qubits)
  "Variant of BUILD-GATE for constructing anonymous gate applications."
  (check-type operator string)
  (push qubit qubits)
  (let* ((name (format nil "~A-~A" operator (get-anonymous-gate-counter)))
         (gate
           (etypecase gate
             (gate gate)
             (magicl:matrix (make-instance 'simple-gate :matrix gate :name name)))))
    (make-instance 'gate-application
                   :operator (named-operator name)
                   :gate gate
                   :arguments (mapcar #'%capture-arg qubits))))

(defun repeatedly-fork (op n)
  (loop :repeat n
        :do (setf op (forked-operator op))
        :finally (return op)))

(defun build-UCR (roll-name params qubit &rest qubits)
  (apply #'build-gate (repeatedly-fork (named-operator roll-name) (length qubits))
         params qubit qubits))

(defun pauli-gate (operator &rest pauli-))

;;; functions for dealing with mixed constant vs delayed-expression types

(defun param-binary-op (op arg1 arg2)
  "Binary operator that safely applies to possibly mixed arguments of NUMBER / DELAYED-EXPRESSION objects. Returns a NUMBER when possible, and a DELAYED-EXPRESSION otherwise."
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
                  :do (error "Duplicate definition of lambda-param: ~A" (second head)))
          (if (or delayedp-1 delayedp-2)
              (simplify-arithmetic
               (make-delayed-expression (mapcar #'first params-zipped)
                                        (mapcar #'second params-zipped)
                                        `(,op ,expression-1 ,expression-2)))
              (funcall op expression-1 expression-2)))))))

(defun param-+ (arg1 arg2)
  (param-binary-op '+ arg1 arg2))

(defun param-* (arg1 arg2)
  (param-binary-op '* arg1 arg2))

(defun param-mod (arg1 arg2)
  "Wraps arg1 into (-arg2 / 2, arg2 / 2] when arg1 is a determined value."
  (typecase arg1
    (real
     (- (mod (+ (/ arg2 2) arg1) arg2) (/ arg2 2)))
    (otherwise arg1)))
