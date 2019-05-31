;;;; operator-bind.lisp
;;;;
;;;; Authors: Eric Peterson, Zach Beane
;;;;
;;;; The bulk of this file concerns itself with syntactic sugar for
;;;; destructuring gate objects and otherwise extracting information from them.
;;;; The main use of these routines is to support `define-compiler`, which does
;;;; a lot of clever destructuring of its arguments.

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


;;; this data structure captures a compilation routine, annotated with various
;;; information about how and when to apply it.

(global-vars:define-global-var **compilers-available** nil)

(defclass compiler ()
  ((name
    :initarg :name
    :reader compiler-name
    :documentation "A human-readable name for the compiler.")
   (instruction-count
    :initarg :instruction-count
    :reader compiler-instruction-count
    :documentation "The number of instructions that this compiler consumes at a time.")
   (bindings
    :initarg :bindings
    :reader compiler-bindings
    :documentation "Raw list of bindings that the compiler matches against.")
   (body
    :initarg :body
    :reader compiler-body
    :documentation "Raw source of the compiler.")
   (output-gates
    :initarg :output-gates
    :reader compiler-output-gates
    :documentation "Information automatically extracted about the target gate set.")
   (%function
    :initarg :function
    :reader compiler-%function
    :documentation "A funcallable that does the compilation. Signature (compilation-context &rest instructions) -> instructions."))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((c compiler) &key)
  ;; We could generate the function dynamically, but BOOLEAN-FORMULA
  ;; will typically be made with a macro below.
  (closer-mop:set-funcallable-instance-function c (compiler-%function c)))

(defmethod print-object ((obj compiler) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (compiler-name obj))))


;;;

(defun get-output-gates-from-raw-code (body)
  (unless (typep body 'cons)
    (return-from get-output-gates-from-raw-code (make-hash-table)))
  (case (first body)
    (build-gate
     (destructuring-bind (head name param-list &rest qubit-list) body
       (declare (ignore head qubit-list))
       (let ((table (make-hash-table :test #'equalp)))
         (cond
           ((and (typep param-list 'cons)
                 (eql 'quote (first param-list))
                 (every #'numberp (second param-list)))
            (setf (gethash name table) (list (cons (second param-list) 1))))
           (t
            (setf (gethash name table) (list (cons '_ 1)))))
         table)))
    (otherwise
     (let ((param-table (make-hash-table :test #'equalp)))
       (dolist (subbody body param-table)
         (let ((incoming-table (get-output-gates-from-raw-code subbody)))
           (dohash ((key val) incoming-table)
             (cond
               ((gethash key param-table)
                ;; merge the two alists
                (loop :for (param-list . count) :in val
                      :do (cond
                            ((assoc param-list (gethash key param-table) :test #'equalp)
                             (let ((cons-cell (assoc param-list (gethash key param-table) :test #'equalp)))
                               (incf (cdr cons-cell) count)))
                            (t
                             (push (cons param-list count)
                                   (gethash key param-table))))))
               (t
                (setf (gethash key param-table) val))))))))))

(defun add-single-entry-to-occurrence-table (table gate-name parameters count &optional (scalar 1))
  (cond
    ((assoc parameters (gethash gate-name table) :test #'equalp)
     (let ((target-cons (assoc parameters (gethash gate-name table) :test #'equalp)))
       (incf (cdr target-cons) (* count scalar))))
    (t
     (push (cons parameters (* count scalar))
           (gethash gate-name table)))))

(defun add-entry-list-to-occurrence-table (table gate-name entry-list &optional (scalar 1))
  (dolist (entry entry-list)
    (destructuring-bind (params . count) entry
      (add-single-entry-to-occurrence-table table gate-name params count scalar))))

(defun copy-occurrence-table (table)
  (let ((new-table (make-hash-table :test #'equalp)))
    (dohash ((key val) table)
      (add-entry-list-to-occurrence-table new-table key val))
    new-table))

(defun add-occurrence-tables (table1 table2 &optional (scalar2 1))
  (let ((new-table (copy-occurrence-table table1)))
    (dohash ((key val) table2)
      (add-entry-list-to-occurrence-table new-table key val scalar2))
    new-table))

(defun binding-matches-p (binding gate-name params)
  ;; case: INSTR
  (when (typep binding 'symbol)
    (return-from binding-matches-p t))
  ;; case: any options present
  (when (binding-options (rest binding))
    (return-from binding-matches-p nil))
  ;; case: (INSTR (BINDING-NAME BINDING-PARAMS &REST BINDING-ARGS))
  (let* ((binding-name (binding-op binding))
         (binding-params (binding-parameters binding)))
    ;; OP matches or subsumes
    (unless (or (wildcard-pattern-p binding-name)
                (typep binding-name 'symbol)
                (and (typep gate-name 'string)
                     (string= binding-name gate-name)))
      (return-from binding-matches-p nil))
    ;; ditto for each parameter
    (when (wildcard-pattern-p binding-params)
      (return-from binding-matches-p t))
    (when (wildcard-pattern-p params)
      ;; need to check that binding-params is EQUIVALENT to a wildcard.
      (return-from binding-matches-p
        (loop :for binding-param :in binding-params
              :always (or (wildcard-pattern-p binding-param)
                          (typep binding-param 'symbol)))))
    (unless (= (length params) (length binding-params))
      (return-from binding-matches-p nil))
    (loop :for param :in params
          :for binding-param :in binding-params
          :always (or (wildcard-pattern-p binding-param)
                      (typep binding-param 'symbol) ; what about parameter lists like (alpha, alpha)?
                      (and (typep binding-param 'double-float)
                           (typep param 'double-float)
                           (double= binding-param param))))))

(defun update-occurrence-table (table compiler)
  (assert (= 1 (length (compiler-bindings compiler))))
  (let ((binding (first (compiler-bindings compiler)))
        (replacement (compiler-output-gates compiler))
        (new-table (make-hash-table :test #'equalp))
        (binding-applied? nil))
    ;; for each existing entry in the table...
    (dohash ((key val) table)
      ;; ... and for each parameter entry in the list...
      (dolist (cons-cell val)
        (destructuring-bind (params . count) cons-cell
          ;; ... do we match the binding?
          (cond
            ;; if so, merge the replacement into the new table
            ((binding-matches-p binding key params)
             (setf binding-applied? t)
             (setf new-table (add-occurrence-tables new-table replacement count)))
            ;; if not, retain the old entry
            (t
             (add-single-entry-to-occurrence-table new-table key params count))))))
    (values new-table binding-applied?)))

(defun get-1Q-compilers ()
  (remove-if-not
   (lambda (compiler)
     (and (or (= 1 (length (compiler-bindings compiler))) ; specialize to 1 input
              (keywordp (second (compiler-bindings compiler)))) ; don't count guards as inputs
          (every (lambda (binding)
                   (and (not (wildcard-pattern-p binding)) ; specialize to 1Q inputs
                        (= 1 (length (binding-arguments binding)))))
                 (compiler-bindings compiler))))
   **compilers-available**))

(defun occurrence-table-cost (table gate-cost-table &optional (default-value 0.5d0))
  (let ((ret 1d0))
    (dohash ((key val) table ret)
      (dolist (cons-cell val)
        (destructuring-bind (params . count) cons-cell
          (let ((true-cost (gethash (cons key params) gate-cost-table)))
            (setf ret (* ret (expt (if true-cost true-cost default-value) count)))))))))

(defun occurrence-table-in-gateset-p (table gateset)
  (dohash ((key val) table t)
    (dolist (cons-cell val)
      (destructuring-bind (params . count) cons-cell
        (declare (ignore count))
        (unless (or (gethash (cons key params) gateset)
                    (gethash (cons key '_) gateset))
          (return-from occurrence-table-in-gateset-p nil))))))

(defun find-shortest-compiler-path (compilers target-gateset)
  ;; target-gateset is an equalp hash (name . params) -> fidelities
  (let ((queue (make-instance 'cl-heap:priority-queue :sort-fun #'>)))
    ;; initial contents: arbitrary gate, no history
    (loop :for (task . history) := (list (alexandria:plist-hash-table
                                          (list "_" (list (cons nil 1)))))
            :then (cl-heap:dequeue queue)
          :when (occurrence-table-in-gateset-p task target-gateset)
            :return (cons task history)
          :do (dolist (compiler compilers)
                (multiple-value-bind (new-table updated?)
                    (update-occurrence-table task compiler)
                  (when updated?
                    (cl-heap:enqueue queue
                                     (list* new-table compiler task history)
                                     (occurrence-table-cost new-table target-gateset))))))))


;;; these functions assemble into the macro DEFINE-COMPILER, which constructs
;;; non-specialized instances of the above class COMPILER and installs them into
;;; the function namespace.

(defun binding-gate-var (binding)
  (unless (first binding)
    (error "Binding ~a is missing a source." binding))
  (first binding))

(defun binding-op (binding)
  (if (and (typep binding 'cons)
           (typep (second binding) 'cons))
      (first (second binding))
      nil))

(defun binding-parameters (binding)
  (if (and (typep binding 'cons)
           (typep (second binding) 'cons))
      (second (second binding))
      nil))

(defun binding-arguments (binding)
  (if (and (typep binding 'cons)
           (typep (second binding) 'cons))
      (loop :for arg :in (rest (rest (second binding)))
            :when (keywordp arg)
              :do (return args)
            :collect arg :into args
            :finally (return args))
      nil))

(defun binding-options (binding)
  (cond
    ((endp (rest binding))
     nil)
    ((typep (second binding) 'keyword)
     (rest binding))
    (t
     (rest (rest binding)))))

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
  "Return an alist of (name . t) for each variable element in BINDING."
  (cond
    ((typep binding 'symbol)
     nil)
    ((and (typep binding 'cons)
          (> 2 (length binding)))
     nil)
    ((and (typep binding 'cons)
          (not (typep (second binding) 'cons)))
     nil)
    (t
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
              (binding-arguments binding))))))

(defun extend-environment (binding environment)
  (setf environment
        (append (binding-environment binding) environment)))

(defun lookup (name env)
  (cdr (assoc name env)))

(defun define-compiler-form (bindings body)
  (labels ((expand-bindings (bindings env)
             (cond
               ((endp bindings)
                `(values (progn ,@body)
                         t))
               ((typep (first bindings) 'symbol)
                (expand-bindings (rest bindings)
                                 env))
               (t
                (let ((binding (first bindings)))
                  (expand-binding binding
                                  env
                                  (expand-bindings (rest bindings)
                                                   (extend-environment binding env)))))))
           
           (expand-binding (binding env body)
             (cond
               ;; raw binding w/ no specializations
               ((typep binding 'symbol)
                body)
               ;; specialized binding w/ a destructuring
               ((and (typep binding 'cons)
                     (typep (second binding) 'cons))
                (assert (and (typep (first binding) 'symbol)
                             (not (wildcard-pattern-p (first binding))))
                        ()
                        "Leftmost term in a compiler form binding must be a symbol, but got ~a"
                        binding)
                (expand-op binding env
                           (expand-parameters binding env
                                              (expand-arguments binding env
                                                                (expand-options binding env body)))))
               ;; specialized binding w/o destructuring
               ((typep binding 'cons)
                (assert (and (typep (first binding) 'symbol)
                             (not (wildcard-pattern-p (first binding))))
                        ()
                        "Leftmost term in a compiler form binding must be a symbol, but got ~a"
                        binding)
                (expand-options binding env body))
               (t
                (error "Malformed binding in compiler form: ~a" binding))))
           
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
                   (var (first ele-vars)))
               (cond
                 ((wildcard-pattern-p ele)
                  (expand-each-element (rest seq)
                                           (rest ele-vars)
                                           env
                                           rest
                                           :ele-accessor ele-accessor
                                           :ele-type ele-type
                                           :eq-predicate eq-predicate))
                 ((and (match-symbol-p ele)
                       (not (lookup ele env)))
                  ;; fresh binding
                  `(let ((,ele (if (typep ,var ',ele-type)
                                   (,ele-accessor ,var)
                                   ,var)))
                     ,(expand-each-element (rest seq)
                                           (rest ele-vars)
                                           (list* (cons ele t) env)
                                           rest
                                           :ele-accessor ele-accessor
                                           :ele-type ele-type
                                           :eq-predicate eq-predicate)))
                 (t
                  ;; existing binding / data. insert eq-predicate check
                  `(when (and (typep ,var ',ele-type)
                              (,eq-predicate ,ele (,ele-accessor ,var)))
                     ,(expand-each-element (rest seq)
                                           (rest ele-vars)
                                           env
                                           rest
                                           :ele-accessor ele-accessor
                                           :ele-type ele-type
                                           :eq-predicate eq-predicate))))))
           
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
           
           (fast-forward-to-options (binding)
             (unless (endp binding)
               (cond
                 ((typep (first binding) 'keyword)
                  binding)
                 (t
                  (fast-forward-to-options (rest binding))))))
           
           (expand-options (binding env rest)
             (declare (ignore env))
             (let ((option-plist (fast-forward-to-options binding))
                   (rest rest))
               (loop :for (val key) :on (reverse option-plist) :by #'cddr
                     :do (setf rest
                               (case key
                                 (:where
                                  `(when ,val
                                     ,rest))
                                 (:acting-on
                                  (destructuring-bind (wf qc) val
                                    `(unpack-wf ,(first binding) context (,wf ,qc)
                                       ,rest)))
                                 (otherwise
                                  (error "Illegal OPERATOR-BIND option: ~a" key)))))
               rest))
           
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

(defmacro define-compiler (name (&rest bindings) &body body)
  (multiple-value-bind (body decls docstring) (alexandria:parse-body body :documentation t)
    (labels ((cleave-options (bindings-and-options &optional backlog)
               (cond
                 ((or (endp bindings-and-options)
                      (keywordp (first bindings-and-options)))
                  (values (reverse backlog) bindings-and-options))
                 (t
                  (cleave-options (rest bindings-and-options)
                                  (cons (first bindings-and-options) backlog)))))
             (collect-variable-names (bindings)
               (when (endp bindings) (return-from collect-variable-names nil))
               (let ((binding (first bindings)))
                 (etypecase binding
                   (symbol (cons binding
                                 (collect-variable-names (rest bindings))))
                   (cons   (cons (first binding)
                                 (collect-variable-names (rest bindings))))))))
      (multiple-value-bind (bindings options) (cleave-options bindings)
        (let ((variable-names (collect-variable-names bindings)))
          (alexandria:when-let (pos (position "CONTEXT" variable-names :key #'string :test #'string=))
            (warn "DEFINE-COMPILER reserves the variable name CONTEXT, but the ~dth binding of ~a has that name."
                  (1+ pos) (string name)))
          (alexandria:with-gensyms (ret-val ret-bool struct-name old-record)
            ;; TODO: do the alexandria destructuring to catch the docstring or whatever
            `(labels ((,name (,@variable-names &key context)
                        (declare (ignorable context))
                        (multiple-value-bind (,ret-val ,ret-bool)
                            ,(define-compiler-form bindings (append decls body))
                          (if ,ret-bool ,ret-val (give-up-compilation)))))
               (let ((,old-record (find ,(string name) **compilers-available**
                                        :key #'compiler-name))
                     (,struct-name
                       (make-instance ',(getf options :class 'compiler)
                                      :name ,(string name)
                                      :instruction-count ,(length variable-names)
                                      :bindings (quote ,bindings)
                                      :body (quote (progn ,@decls ,@body))
                                      :output-gates (get-output-gates-from-raw-code (quote (progn ,@body)))
                                      :function #',name)))
                 (setf (fdefinition ',name) ,struct-name)
                 (setf (documentation ',name 'function) ,docstring)
                 (cond
                   (,old-record
                    (setf **compilers-available**
                          (substitute ,struct-name ,old-record **compilers-available**)))
                   (t
                    (push ,struct-name **compilers-available**)))
                 ',name))))))))

(defmacro with-compilation-context ((&rest bindings-plist) &body body)
  (setf body `(progn ,@body))
  (loop :for (val key) :on (reverse bindings-plist) :by #'cddr
        :do (setf body
                  (ecase key
                    (:full-context
                     `(let ((,val context))
                        ,body))
                    (:chip-specification
                     `(progn
                        (unless context (give-up-compilation))
                        (let ((,val (compilation-context-chip-specification context)))
                          (unless ,val (give-up-compilation))
                          ,body)))))
        :finally (return body)))

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
