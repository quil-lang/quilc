;;;; build-gate.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; This file collects the gate-construction routines in CL-QUIL which are
;;;; used as internal sugar for otherwise ugly constructions like
;;;;
;;;; (make-instance 'gate-application :blah 'blah-blah ...) .

(in-package #:cl-quil/frontend)


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
    (real
     (constant (coerce param 'double-float)))
    (complex
     (error "Complex-valued gate parameter ~S not permitted" param))
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

(defun anon-gate (operator-or-gate gate-or-parameters qubit &rest qubits)
  "Variant of BUILD-GATE for constructing anonymous gate applications.

Comes in two flavors:
  (1) (anon-gate string-name magicl-matrix &rest qubits) builds an anonymous gate application with human-readable name STRING-NAME, behavior indicated by MAGICL-MATRIX, and acting on QUBITS.
  (2) (anon-gate gate-definition parameter-list &rest qubits) builds an anonymous gate application with definition set by GATE-DEFINITION, human-readable name inferred from the defintiion, PARAMETER-LIST fed to the definition, and acting on QUBITS."
  (push qubit qubits)
  (let* ((name
           (etypecase operator-or-gate
             (string
              (format nil "~A-~A" operator-or-gate (get-anonymous-gate-counter)))
             (gate
              (format nil "~A-~A" (gate-name operator-or-gate) (get-anonymous-gate-counter)))))
         (gate
           (cond
             ((typep operator-or-gate 'gate)
              operator-or-gate)
             ((typep gate-or-parameters 'magicl:matrix)
              (make-instance 'simple-gate :matrix gate-or-parameters :name name))
             (t
              (error "Cannot find gate definition."))))
         (parameters
           (typecase gate-or-parameters
             (cons gate-or-parameters)
             (otherwise nil))))
    (make-instance 'gate-application
                   :operator (named-operator name)
                   :gate gate
                   :arguments (mapcar #'%capture-arg qubits)
                   :parameters parameters)))

(defun repeatedly-fork (op n)
  (loop :repeat n
        :do (setf op (forked-operator op))
        :finally (return op)))

(defun build-UCR (roll-name params qubit &rest qubits)
  (apply #'build-gate (repeatedly-fork (named-operator roll-name) (length qubits))
         params qubit qubits))

(defun repeatedly-control (op n)
  (loop :repeat n
        :do (setf op (controlled-operator op))
        :finally (return op)))

;;; Build a multiple controlled gate.
(defun build-multiple-controlled-gate (gate params qubit &rest qubits)
  (apply #'build-gate (repeatedly-control (named-operator gate) (length qubits))
         params qubit qubits))

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
             ((memory-ref)
              (values
               nil
               nil
               arg
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

(defun kq-gate-on-lines (gate-mat n lines)
  "Writes the gate GATE-MAT as an N-qubit gate by applying it to the qubit lines in LINES."
  (check-type gate-mat magicl:matrix)
  (check-type n integer)
  (let* ((width (expt 2 n))
         (mask (- -1 (loop :for l :in lines :sum (expt 2 l))))
         (out-mat (zeros (list width width))))
    (dotimes (i width)
      (dotimes (j width)
        (if (= (logand mask i) (logand mask j))
            (setf (magicl:tref out-mat i j)
                  (magicl:tref gate-mat
                               (loop :for r :below (length lines)
                                     :sum (if (logbitp (nth r lines) i)
                                              (ash 1 (- (length lines) 1 r))
                                              0))
                               (loop :for s :below (length lines)
                                     :sum (if (logbitp (nth s lines) j)
                                              (ash 1 (- (length lines) 1 s))
                                              0)))))))
    out-mat))
