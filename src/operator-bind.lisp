;;;; operator-bind.lisp
;;;;
;;;; Authors: Eric Peterson, Zach Beane

(in-package #:cl-quil)

(defun build-gate (operator params &rest qubits)
  "Shorthand function for constructing a GATE-APPLICATION object from QUIL-like input. OPERATOR must be a standard gate name."
  (check-type operator string)
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
    (let ((gate (lookup-standard-gate operator)))
      (assert (not (null gate)) (operator) "BUILD-GATE only takes standard gate names.")
      (make-instance 'gate-application
                     :operator (named-operator operator)
                     :gate gate
                     :parameters (mapcar #'capture-param params)
                     :arguments (mapcar #'capture-arg qubits)))))


;; a typical use of operator-bind looks like
;;
;; (operator-bind ((("CNOT"   ()    p q) x)
;;                 (("CPHASE" (phi) q _) y))
;;   ...)
;;
;; the individual bindings are of the form (gate-var op (&rest params) &rest args)

(defun binding-gate-var (binding)
  (unless (second binding)
    (error "Binding ~a is missing a source." binding))
  (second binding))

(defun binding-op (binding)
  (first (first binding)))

(defun binding-parameters (binding)
  (second (first binding)))

(defun binding-arguments (binding)
  (nthcdr 2 (first binding)))

;; if we ever want to expand the list of "reserved symbols", this is the place
;; to do it. previously, `'pi` lived here, but it seems better to require users
;; to write `#.pi`, since it's more visually offsetting.
(defun match-symbol-p (x)
  (and (symbolp x)
       (not (wildcard-pattern-p x))))

(defun wildcard-pattern-p (x)
  (and (symbolp x)
       (string= "_" x)))

(defun binding-environment (binding)
  "Return an alist of (name . t) for each variable element in
  BINDING."
  (nconc
   (when (match-symbol-p (binding-op binding))
     (list (cons (binding-op binding) t)))
   (unless (wildcard-pattern-p (binding-parameters binding))
     (mapcan (lambda (param)
               (when (match-symbol-p param)
                 (list (cons param t))))
             (binding-parameters binding)))
   (mapcan (lambda (arg)
             (when (match-symbol-p arg)
               (list (cons arg t))))
           (binding-arguments binding))))

(defun extend-environment (binding environment)
  (setf environment
        (append (binding-environment binding) environment)))

(defun lookup (name env)
  (cdr (assoc name env)))

(defun operator-bind-form (bindings body)
  (labels ((expand-bindings (bindings env)
             (cond
               ((endp bindings)
                `(progn ,@body))
               ((wildcard-pattern-p (first (first bindings)))
                (expand-bindings (rest bindings)
                                 env))
               (t
                (let ((binding (first bindings)))
                  (expand-binding binding
                                  env
                                  (expand-bindings (rest bindings)
                                                   (extend-environment binding env)))))))
           
           (expand-binding (binding env body)
             (expand-op binding env
                        (expand-parameters binding env
                                           (expand-arguments binding env body))))
           
           (expand-sequence (seq env rest &key gensym-name seq-accessor gate-name ele-accessor ele-type eq-predicate)
             (let* ((target-length (length seq))
                    (seq-var (gensym (format nil "~aS" gensym-name)))
                    (ele-vars (loop :repeat target-length
                                    :collect (gensym gensym-name)))
                    (dead-vars (loop :for v :in ele-vars
                                     :for p :in seq
                                     :when (wildcard-pattern-p p)
                                       :collect v)))
               `(let ((,seq-var (,seq-accessor ,gate-name)))
                  (when (= (length ,seq-var) ,target-length)
                    (destructuring-bind ,ele-vars
                        ,seq-var
                      ,@(when dead-vars
                          (list `(declare (ignore ,@dead-vars))))
                      ,(expand-each-element seq
                                            ele-vars
                                            env
                                            rest
                                            :ele-accessor ele-accessor
                                            :ele-type ele-type
                                            :eq-predicate eq-predicate))))))
           
           (expand-each-element (seq ele-vars env rest &key ele-accessor ele-type eq-predicate)
             (when (endp seq)
               (return-from expand-each-element rest))
             (let ((ele (first seq))
                   (var (first ele-vars))
                   (new-rest (expand-each-element (rest seq)
                                                  (rest ele-vars)
                                                  env
                                                  rest
                                                  :ele-accessor ele-accessor
                                                  :ele-type ele-type
                                                  :eq-predicate eq-predicate)))
               (cond
                 ((wildcard-pattern-p ele)
                  ;; ignore
                  new-rest)
                 ((and (match-symbol-p ele)
                       (not (lookup ele env)))
                  ;; fresh binding
                  `(let ((,ele (if (typep ,var ',ele-type)
                                   (,ele-accessor ,var)
                                   ,var)))
                     ,new-rest))
                 (t
                  ;; existing binding / data. insert eq-predicate check
                  `(when (and (typep ,var ',ele-type)
                              (,eq-predicate ,ele (,ele-accessor ,var)))
                     ,new-rest)))))
           
           (expand-parameters (binding env rest)
             (if (wildcard-pattern-p (binding-parameters binding))
                 rest
                 (expand-sequence (binding-parameters binding)
                                  env
                                  rest
                                  :gensym-name "PARAM"
                                  :seq-accessor 'application-parameters
                                  :gate-name (binding-gate-var binding)
                                  :ele-accessor 'constant-value
                                  :ele-type 'constant
                                  :eq-predicate 'double=)))
           
           (expand-arguments (binding env rest)
             (expand-sequence (binding-arguments binding)
                              env
                              rest
                              :gensym-name "ARG"
                              :seq-accessor 'application-arguments
                              :gate-name (binding-gate-var binding)
                              :ele-accessor 'qubit-index
                              :ele-type 'qubit
                              :eq-predicate '=))
           
           (expand-op (binding env body)
             (let ((op (binding-op binding)))
               (cond
                 ((wildcard-pattern-p op)
                  ;; ignore
                  body)
                 ((and (match-symbol-p op)
                       (not (lookup op env)))
                  ;; fresh binding
                  `(let ((,op (application-operator-name ,(binding-gate-var binding))))
                     ,body))
                 (t
                  ;; existing binding / data. insert string check
                  `(when (string= ,op (operator-description-string
                                       (application-operator ,(binding-gate-var binding))))
                     ,body))))))
    (expand-bindings bindings nil)))

(defmacro operator-bind ((&rest bindings) &body body)
  "For a sequence of OPERATOR objects, performs a match and destructuring bind over the lexical environment of BODY.

Example usage:
(let ((g (build-gate \"CZ\" () 0 1)))
  (operator-bind   ((\"CZ\" _  _ q) g)
    ;; ... forms that use Q, which has been bound to 1 ...
    ))

NOTE: Returns NIL without executing BODY if match fails."
  (operator-bind-form bindings body))

(defmacro lambda-on-operators (binding-list &body body)
  "Defines an anonymous function on operator arguments that automatically performs a destructuring bind+match on its arguments.

Example usage:
(let ((g (build-gate \"CZ\" () 0 1))
      (fn (lambda-on-operators (((\"CZ\" () p q) h))
            ;; defines a function with argument h,
            ;; and matches h against the pattern (\"CZ\" () p q)
            )))
  (funcall fn g))"
  `(lambda ,(mapcar #'second binding-list)
     (operator-bind ,binding-list
      ,@body)))


(defun operator-match-p (gate pattern)
  "Tests whether a single GATE matches PATTERN, without performing any binding."
  (or (and (typep gate 'application)
           (or (not (typep (first pattern) 'string))
               (adt:match operator-description (application-operator gate)
                 ((named-operator name) (string= name (first pattern)))
                 (_ nil)))
           (loop :for param :in (second pattern)
                 :for g-param :in (application-parameters gate)
                 :always (or (not (typep param 'double-float))
                             (and (typep g-param 'constant)
                                  (double= param (constant-value g-param)))))
           (loop :for arg :in (nthcdr 2 pattern)
                 :for g-arg :in (application-arguments gate)
                 :always (or (not (typep arg '(integer 0)))
                             (= arg (qubit-index g-arg)))))
      (and (string= "MEASURE" (first pattern))
           (typep gate 'measurement)
           (or (not (typep (second pattern) 'integer))
               (= (second pattern) (qubit-index (measurement-qubit gate))))
           (or (and (not (third pattern))
                    (not (typep gate 'measure)))
               (and (typep gate 'measure)
                    (or (wildcard-pattern-p (third pattern))
                        (equal (measure-address gate) (third pattern))))
               (assert "operator-match-p on MEASURE patterns can only use _ for the measurement address"
                       nil)))))


(define-condition operator-match-fell-through (serious-condition)
  ())


(defmacro operator-match (&body binding-clauses)
  "Performs a sequence of OPERATOR-BINDs. The first that does not fall through has its body executed and its result returned. If all the clauses provided fall through, this results in an OPERATOR-MATCH-FELL-THROUGH error. The special binding clause \"_\" will always match.

Syntax:
(operator-match
 (((binding-clause-1-1)
   (binding-clause-1-2)
   ...)
  body-1)
 (((binding-clause-2-1)
   (binding-clause-2-2)
   ...)
  body-2)
 ...)"
  (let ((match-tag (gensym "OPERATOR-MATCH-TAG-")))
    `(block ,match-tag
       ,@(loop :for (match-clauses . code) :in binding-clauses
               :if (wildcard-pattern-p match-clauses)
                 :collect `(return-from ,match-tag (progn ,@code))
               :else
                 :collect `(operator-bind ,match-clauses
                             (return-from ,match-tag
                               (progn
                                 ,@code))))
       (error 'operator-match-fell-through))))
