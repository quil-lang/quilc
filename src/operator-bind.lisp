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


;;; also some utilities for defining compiler bodies generally

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


;;; dumb little macros for cond guards that we were repeating a million times

(defmacro give-up-compilation-unless (test &body body)
  "If TEST passes, proceed to execute BODY (and return the value of the final expression). If TEST fails, call GIVE-UP-COMPILATION."
  `(cond
     (,test
      ,@body)
     (t
      (give-up-compilation))))

(defmacro unpack-wf (instr context (psi qubits) &body body)
  "Extracts from CONTEXT the wavefunction component related to the instruction INSTR.  Upon success, bind PSI to the wavefunction contents, bind QUBITS to the qubit ordering convention of PSI, execute BODY, and return the result of the final expression.  Upon failure, call GIVE-UP-COMPILATION."
  `(destructuring-bind (,psi ,qubits)
       (aqvm-extract-state (compilation-context-aqvm ,context)
                           (mapcar #'qubit-index (application-arguments ,instr)))
     (give-up-compilation-unless
         (not (eql ':not-simulated ,psi))
       ,@body)))


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
   (gateset-reducer
    :initarg :gateset-reducer
    :initform t
    :reader compiler-gateset-reducer-p
    :documentation "Is this compiler intended to transition from one gate set to another?")
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

(defun cleave-options (bindings-and-options &optional backlog)
  (cond
    ((or (endp bindings-and-options)
         (keywordp (first bindings-and-options)))
     (values (reverse backlog) bindings-and-options))
    (t
     (cleave-options (rest bindings-and-options)
                     (cons (first bindings-and-options) backlog)))))

(defun get-binding-from-instr (instr)
  (typecase instr
    (measure-discard
     (list "MEASURE" '_))
    (measurement
     (list "MEASURE" '_ '_))
    (application
     (adt:match operator-description (application-operator instr)
       ((named-operator s)
        (list* s
               (loop :for p :in (application-parameters instr)
                     :when (typep p 'constant)
                       :collect (constant-value p)
                     :when (symbolp p)
                       :collect '_)
               (loop :for q :in (application-arguments instr)
                     :when (typep q 'qubit)
                       :collect (qubit-index q)
                     :when (symbolp q)
                       :collect '_)))
       (_
        nil)))))

(defun get-output-gates-from-raw-code (body)
  (unless (typep body 'cons)
    (return-from get-output-gates-from-raw-code (make-occurrence-table)))
  (case (first body)
    (build-gate
     (destructuring-bind (head name param-list &rest qubit-list) body
       (declare (ignore head))
       (let ((table (make-hash-table :test #'equalp))
             (param-list (cond
                           ((endp param-list)
                            nil)
                           ((and (typep param-list 'list)
                                 (= 2 (length param-list))
                                 (typep (second param-list) 'list))
                            (loop :for item :in (second param-list)
                                  :if (typep item 'number)
                                    :collect item
                                  :else
                                    :collect '_))
                           (t
                            '_)))
             (qubit-list (mapcar (lambda (x)
                                   (typecase x
                                     (number x)
                                     (otherwise '_)))
                                 qubit-list)))
         (setf (gethash (list* name param-list qubit-list) table) 1)
         table)))
    (anon-gate
     (destructuring-bind (head name matrix &rest qubit-list) body
       (declare (ignore head name matrix))
       (let ((table (make-hash-table :test #'equalp))
             (qubit-list (mapcar (lambda (x)
                                   (typecase x
                                     (number x)
                                     (otherwise '_)))
                                 qubit-list)))
         (setf (gethash (list* '_ '_ qubit-list) table) 1)
         table)))
    (apply
     (destructuring-bind (head fn &rest args) body
       (declare (ignore head))
       (case fn
         (#'build-gate
          (destructuring-bind (name param-list qubit-list) args
            (declare (ignore qubit-list))
            (get-output-gates-from-raw-code
             `(build-gate ,name ,param-list
                          ,@(loop :for j :below (gate-definition-qubits-needed (lookup-standard-gate name))
                                  :collect '_)))))
         (otherwise
          (make-occurrence-table)))))
    (otherwise
     (let ((table (make-occurrence-table)))
       (dolist (subbody body table)
         (let ((incoming-table (get-output-gates-from-raw-code subbody)))
           (setf table (add-occurrence-tables table incoming-table))))))))

(defun make-occurrence-table ()
  (make-hash-table :test #'equalp))

(defun add-entry-to-occurrence-table (table binding count &optional (scalar 1))
  (incf (gethash binding table 0) (* count scalar)))

(defun copy-occurrence-table (table)
  (let ((new-table (make-hash-table :test #'equalp)))
    (dohash ((key val) table)
      (add-entry-to-occurrence-table new-table key val))
    new-table))

(defun add-occurrence-tables (table1 table2 &optional (scalar2 1))
  (let ((new-table (copy-occurrence-table table1)))
    (dohash ((key val) table2)
      (add-entry-to-occurrence-table new-table key val scalar2))
    new-table))

(defun binding-subsumes-p (big-binding small-binding)
  "If SMALL-BINDING matches, will BIG-BINDING necessarily match?"
  (flet ((wildcard-pattern-p (x)
           (or (wildcard-pattern-p x)
               (and (typep x 'cons)
                    (every (lambda (xp)
                             (or (wildcard-pattern-p xp)
                                 (typep xp 'symbol)))
                           x)))))
    (when (symbolp big-binding)
      (return-from binding-subsumes-p t))
    (when (symbolp small-binding)
      (return-from binding-subsumes-p nil))
    ;; both are destructurable
    (multiple-value-bind (big-binding big-options) (cleave-options big-binding)
      (multiple-value-bind (small-binding small-options) (cleave-options small-binding)
        (cond
          ((and (= 1 (length big-binding))
                (or (endp big-options)
                    (equalp big-options small-options)))
           t)
          ((and (= 1 (length small-binding))
                (endp small-options))
           nil)
          ((and big-options small-options)
           nil)
          ;; big-binding and small-binding are now actual bindings
          ;; check the operators for failing both _ <= _ and "literal" <= "literal"
          ((or (and (wildcard-pattern-p (binding-op small-binding))
                    (not (wildcard-pattern-p (binding-op big-binding))))
               (and (not (wildcard-pattern-p (binding-op small-binding)))
                    (not (wildcard-pattern-p (binding-op big-binding)))
                    (not (string= (binding-op big-binding) (binding-op small-binding)))))
           nil)
          ;; check the parameters for failing both _ <= _ and literal <= literal
          ((or (and (wildcard-pattern-p (binding-parameters small-binding))
                    (not (wildcard-pattern-p (binding-parameters big-binding))))
               (and (not (wildcard-pattern-p (binding-parameters small-binding)))
                    (not (wildcard-pattern-p (binding-parameters big-binding)))
                    (loop :for big-param :in (binding-parameters big-binding)
                          :for small-param :in (binding-parameters small-binding)
                            :thereis (or (and (or (wildcard-pattern-p small-param)
                                                  (symbolp small-param))
                                              (not (wildcard-pattern-p big-param))
                                              (not (symbolp big-param)))
                                         (and (not (wildcard-pattern-p small-param))
                                              (not (wildcard-pattern-p big-param))
                                              (not (double= big-param small-param)))))))
           nil)
          ;; check the arguments for failing both _ <= _ and literal <= literal
          ((or (not (= (length (binding-arguments big-binding))
                       (length (binding-arguments small-binding))))
               (loop :for big-arg :in (binding-arguments big-binding)
                     :for small-arg :in (binding-arguments small-binding)
                       :thereis (or (and (or (wildcard-pattern-p small-arg)
                                             (symbolp small-arg))
                                         (not (wildcard-pattern-p big-arg))
                                         (not (symbolp big-arg)))
                                    (and (not (or (wildcard-pattern-p small-arg)
                                                  (symbolp small-arg)))
                                         (not (or (wildcard-pattern-p big-arg)
                                                  (symbolp big-arg)))
                                         (not (= big-arg small-arg))))))
           nil)
          ;; otherwise, i guess everything checks out
          (t
           t))))))

(defun filter-by-qubit-count (table qubit-count)
  (let ((new-table (make-occurrence-table)))
    (dohash ((key val) table new-table)
      (when (= qubit-count
               (length (binding-arguments (list '_ key))))
        (setf (gethash key new-table) val)))))

(defun update-occurrence-table (table compiler &optional qubit-count)
  (assert (= 1 (length (compiler-bindings compiler))))
  (let ((binding (first (compiler-bindings compiler)))
        (replacement (if qubit-count
                         (filter-by-qubit-count (compiler-output-gates compiler) qubit-count)
                         (compiler-output-gates compiler)))
        (new-table (make-hash-table :test #'equalp))
        (binding-applied? nil))
    ;; for each existing entry in the table...
    (dohash ((key val) table)
      ;; ... do we match the binding?
      (cond
        ;; if so, merge the replacement into the new table
        ((binding-subsumes-p binding (list '_ key))
         (setf binding-applied? t)
         (setf new-table (add-occurrence-tables new-table replacement val)))
        ;; if not, retain the old entry
        (t
         (add-entry-to-occurrence-table new-table key val))))
    (values new-table binding-applied?)))

(defun get-compilers (&optional (qubit-bound 1))
  (remove-if-not
   (lambda (compiler)
     (and (compiler-gateset-reducer-p compiler)
          (every (lambda (binding)
                   (and (not (or (wildcard-pattern-p binding)
                                 (symbolp binding)))
                        (>= qubit-bound (length (binding-arguments binding)))
                        (endp (binding-options binding))))
                 (compiler-bindings compiler))))
   **compilers-available**))

(defun occurrence-table-cost (table gate-cost-table &optional (default-value 0.5d0))
  (let ((ret 1d0))
    (dohash ((key val) table ret)
      (let ((true-cost default-value))
        (dohash ((cost-key cost-val) gate-cost-table)
          (when (binding-subsumes-p (list '_ cost-key) (list '_ key))
            (setf true-cost (gate-record-fidelity cost-val))))
        (setf ret (* ret (expt true-cost val)))))))

(defun occurrence-table-in-gateset-p (table gateset)
  (loop :for table-key :being :the :hash-keys :of table
        :always (loop :for gateset-key :being :the :hash-keys :of gateset
                        :thereis (and (or (gate-record-duration (gethash gateset-key gateset))
                                          (gate-record-fidelity (gethash gateset-key gateset)))
                                      (binding-subsumes-p (list '_ gateset-key)
                                                          (list '_ table-key))))))

(defun generate-blank-binding (qubit-count)
  (list* '_ ()
         (loop :for i :below qubit-count
               :collect '_)))

(defun find-shortest-compiler-path (compilers target-gateset source-gateset &optional qubit-count)
  ;; target-gateset is an equalp hash binding -> gate-record
  (let ((queue (make-instance 'cl-heap:priority-queue :sort-fun #'>)))
    (flet ((collect-bindings (occurrence-table)
             (loop :for key :being :the :hash-keys :of occurrence-table
                   :collect key)))
      ;; initial contents: arbitrary gate, no history
      (loop :with visited-nodes := ()
            :for (task . history) := (list source-gateset)
              :then (cl-heap:dequeue queue)
            :unless task
              :do (error "Exhausted task queue without finding a route to the target gateset.")
            :when (occurrence-table-in-gateset-p task target-gateset)
              :return (cons task history)
            :unless (member (sort (collect-bindings task)
                                  (lambda (a b)
                                    (binding-precedes-p
                                     (list (list '_ a))
                                     (list (list '_ b)))))
                            visited-nodes :test #'equalp)
              :do (push (sort (collect-bindings task)
                              (lambda (a b)
                                (binding-precedes-p
                                 (list (list '_ a))
                                 (list (list '_ b)))))
                        visited-nodes)
                  (dolist (compiler compilers)
                    (multiple-value-bind (new-table updated?)
                        (update-occurrence-table task compiler qubit-count)
                      (when updated?
                        (cl-heap:enqueue queue
                                         (list* new-table compiler task history)
                                         (occurrence-table-cost new-table target-gateset)))))))))

(defun binding-precedes-p (a b)
  (labels
      ((qubit-count (binding)
         (if (or (symbolp binding)
                 (keywordp (second binding)))
             most-positive-fixnum
             (length (binding-arguments binding)))))
    (multiple-value-bind (a-bindings a-options)
        (cleave-options a)
      (multiple-value-bind (b-bindings b-options)
          (cleave-options b)
        (assert (= 1 (length a-bindings)))
        (assert (= 1 (length b-bindings)))
        (let* ((a-binding (first a-bindings))
               (b-binding (first b-bindings))
               (arg-count-a (qubit-count a-binding))
               (arg-count-b (qubit-count b-binding)))
          ;; fewer qubits means it comes earlier
          (cond
            ((< arg-count-a arg-count-b)
             (print (list arg-count-a arg-count-b 1))
             t)
            ((> arg-count-a arg-count-b)
             nil)
            ;; from here on: (and (= arg-count-a arg-count b) ...)
            ;; cluster by name
            ((and (or (wildcard-pattern-p (binding-op a-binding))
                      (symbolp (binding-op a-binding)))
                  (stringp (binding-op b-binding)))
             nil)
            ((and (stringp (binding-op a-binding))
                  (or (wildcard-pattern-p (binding-op b-binding))
                      (symbolp (binding-op b-binding))))
             t)
            ((and (stringp (binding-op a-binding))
                  (stringp (binding-op b-binding))
                  (string< (binding-op a-binding) (binding-op b-binding)))
             t)
            ;; option predicates make it come earlier
            ((and (or a-options (binding-options a-binding))
                  (not (or b-options (binding-options b-binding))))
             t)
            ;; restrictive parameter matching makes it come earlier
            ((and (not (wildcard-pattern-p (binding-parameters a-binding)))
                  (wildcard-pattern-p (binding-parameters b-binding)))
             t)
            ((and (wildcard-pattern-p (binding-parameters a-binding))
                  (not (wildcard-pattern-p (binding-parameters b-binding))))
             nil)
            ;; does A need to specialize before B?
            ((and (not (wildcard-pattern-p (binding-parameters a-binding)))
                  (not (wildcard-pattern-p (binding-parameters b-binding)))
                  (loop :for param-a :in (binding-parameters a-binding)
                        :for param-b :in (binding-parameters b-binding)
                          :thereis (and (not (or (wildcard-pattern-p param-a)
                                                 (symbolp param-a)))
                                        (or (wildcard-pattern-p param-b)
                                            (symbolp param-b)))))
             t)
            (t
             nil)))))))

(defun sort-compilers-by-specialization (compilers)
  (stable-sort (copy-seq compilers)
               (lambda (a b)
                 (binding-precedes-p (compiler-bindings a)
                                     (compiler-bindings b)))))

(defun sort-compilers-by-output-friendliness (compilers gateset qubit-count)
  (sort (copy-seq compilers)
        (lambda (a b)
          (occurrence-table-cost (filter-by-qubit-count (compiler-output-gates a)
                                                        qubit-count)
                                 gateset)
          (occurrence-table-cost (filter-by-qubit-count (compiler-output-gates b)
                                                        qubit-count)
                                 gateset))))

(defun blank-out-qubits (gateset)
  (let ((new-gateset (make-hash-table :test #'equalp)))
    (dohash ((key val) gateset new-gateset)
      (setf (gethash (list* (first key)
                            (second key)
                            (mapcar (constantly '_) (rest (rest key))))
                     new-gateset)
            val))))

(defun compute-applicable-compilers (target-gateset qubit-count) ; h/t lisp
  (flet ((discard-tables (path)
           (loop :for item :in path
                 :when (typep item 'compiler)
                   :collect item))
         (path-has-a-loop-p (path start)
           (and (< 1 (length path))
                (loop :for (gateset compiler) :on path :by #'cddr
                      :when (loop :for gate :being :the :hash-keys :of gateset
                                  :always (loop :for binding :in start
                                                  :thereis (binding-subsumes-p (first start)
                                                                               (list '_ gate))))
                        :do (return-from path-has-a-loop-p t)
                      :finally (return nil))))
         (print-path (path)
           (dolist (item path)
             (typecase item
               (compiler (format t "~&is the output from applying ~a, coming from...~%" item))
               (hash-table
                (dohash ((key val) item)
                  (format t "~a -> ~a~%" key val)))))))
    (declare (ignorable #'print-path))
    (let* ((compilers (get-compilers qubit-count))
           (target-gateset (blank-out-qubits target-gateset))
           (unconditional-compilers
             (remove-if (lambda (x)
                          (multiple-value-bind (bindings options) (compiler-bindings x)
                            (or options
                                (some (lambda (b) (and (not (symbolp b))
                                                       (binding-options b)))
                                      bindings))))
                        compilers))
           (generic-path (find-shortest-compiler-path unconditional-compilers
                                                      target-gateset
                                                      (alexandria:plist-hash-table
                                                       (list (generate-blank-binding qubit-count) 1)
                                                       :test #'equalp)
                                                      qubit-count))
           (generic-cost (occurrence-table-cost (first generic-path) target-gateset))
           (reduced-compilers (discard-tables generic-path)))
      (let ((candidate-entry-points
              (remove-if (lambda (x)
                           (or (/= qubit-count (length (binding-arguments (first (compiler-bindings x)))))
                               (loop :for b :being :the :hash-keys :of target-gateset
                                       :thereis (binding-subsumes-p (list '_ b)
                                                                    (first (compiler-bindings x))))))
                         compilers)))
        (dolist (compiler candidate-entry-points)
          (let* ((special-path
                   (find-shortest-compiler-path
                    unconditional-compilers target-gateset
                    (filter-by-qubit-count (compiler-output-gates compiler) qubit-count)
                    qubit-count))
                 (special-cost (occurrence-table-cost (first special-path) target-gateset)))
            (when (and (not (path-has-a-loop-p special-path (compiler-bindings compiler)))
                       (> special-cost generic-cost))
              (setf reduced-compilers
                    (append reduced-compilers
                            (list compiler)
                            (discard-tables special-path)))))))
      (let ((sorted-compilers
              ;; TODO: figure out a better way to dispatch this highly feature-specific stuff
              (append (cond
                        ((= 1 qubit-count)
                         (list #'state-prep-1q-compiler))
                        ((= 2 qubit-count)
                         (list #'state-prep-2q-compiler)))
                      #+ignore
                      (cond
                        ((= 2 qubit-count)
                         (list
                          (make-instance 'compiler
                                         :name 'APPROXIMATE-2Q-COMPILER
                                         :instruction-count 1
                                         :bindings '((instr (_ _ _ _)))
                                         :output-gates (a:plist-hash-table
                                                        (list '("CAN" (_ _ _) _ _) 1)
                                                        :test #'equalp)
                                         :function (a:curry #'approximate-2q-compiler
                                                            (remove-if-not (lambda (c)
                                                                             (typep c 'approximate-compiler))
                                                                           reduced-compilers))))))
                      (sort-compilers-by-output-friendliness
                       (remove-duplicates reduced-compilers)
                       target-gateset qubit-count))))
        (append (remove-if-not (lambda (c)
                                 (occurrence-table-in-gateset-p (filter-by-qubit-count
                                                                 (compiler-output-gates c)
                                                                 qubit-count)
                                                                target-gateset))
                               sorted-compilers)
                (remove-if (lambda (c)
                             (occurrence-table-in-gateset-p (filter-by-qubit-count
                                                             (compiler-output-gates c)
                                                             qubit-count)
                                                            target-gateset))
                           sorted-compilers))))))

(defun compute-applicable-reducers (gateset)
  (let* ((gateset-bindings (loop :for g :being :the :hash-keys :of gateset
                                 :collect (list '_ g))))
    (remove-if-not (lambda (c)
                     (let* ((in-fidelity 1d0)
                            (out-fidelity (occurrence-table-cost (compiler-output-gates c) gateset)))
                       (and (occurrence-table-in-gateset-p (compiler-output-gates c) gateset)
                            (every (lambda (b)
                                     (some (lambda (g)
                                             (let ((gate-fidelity (gate-record-fidelity
                                                                   (gethash (second g) gateset))))
                                               (and (binding-subsumes-p g b)
                                                    (setf in-fidelity (* in-fidelity gate-fidelity)))))
                                           gateset-bindings))
                                   (compiler-bindings c))
                            (>= out-fidelity in-fidelity))))
                   **compilers-available**)))


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

(defun define-compiler-form (bindings body &key permit-binding-mismatches-when)
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
                  `(when (or ,permit-binding-mismatches-when
                             (and (typep ,var ',ele-type)
                                  (,eq-predicate ,ele (,ele-accessor ,var))))
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

(defun parse-compiler-options (options body)
  (when (endp options)
    (return-from parse-compiler-options body))
  (destructuring-bind (key val &rest remaining-options) options
    (case key
      (:full-context
       (parse-compiler-options remaining-options
                               `(let ((,val context))
                                  ,body)))
      (:chip-specification
       (parse-compiler-options remaining-options
                               `(progn
                                  (unless context (give-up-compilation))
                                  (let ((,val (compilation-context-chip-specification context)))
                                    (unless ,val (give-up-compilation))
                                    ,body))))
      ((:gateset-reducer :class :permit-binding-mismatches-when)
       (parse-compiler-options remaining-options body))
      (otherwise
       (error "Unknown compiler option: ~a." (first options))))))

(defmacro define-compiler (name (&rest bindings) &body body)
  (multiple-value-bind (body decls docstring) (alexandria:parse-body body :documentation t)
    (labels ((collect-variable-names (bindings)
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
                            ,(parse-compiler-options
                              options
                              (define-compiler-form bindings (append decls body)
                                :permit-binding-mismatches-when (getf options ':permit-binding-mismatches-when)))
                          (if ,ret-bool ,ret-val (give-up-compilation)))))
               (let ((,old-record (find ',name **compilers-available**
                                        :key #'compiler-name))
                     (,struct-name
                       (make-instance ',(getf options :class 'compiler)
                                      :name ',name
                                      :instruction-count ,(length variable-names)
                                      :bindings (quote ,bindings)
                                      :body (quote (progn ,@decls ,@body))
                                      :output-gates (get-output-gates-from-raw-code (quote (progn ,@body)))
                                      :function #',name
                                      :gateset-reducer ,(and (= 1 (length bindings))
                                                             (getf options :gateset-reducer t)))))
                 (setf (fdefinition ',name) ,struct-name)
                 (setf (documentation ',name 'function) ,docstring)
                 (cond
                   (,old-record
                    (setf **compilers-available**
                          (substitute ,struct-name ,old-record **compilers-available**)))
                   (t
                    (push ,struct-name **compilers-available**)))
                 ',name))))))))

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
