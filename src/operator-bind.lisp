;;;; operator-bind.lisp
;;;;
;;;; Authors: Eric Peterson, Zach Beane
;;;;
;;;; The bulk of this file concerns itself with syntactic sugar for
;;;; destructuring gate objects and otherwise extracting information from them.
;;;; The main use of these routines is to support `define-compiler`, which does
;;;; a lot of clever destructuring of its arguments.

(in-package #:cl-quil)


;;; here are two utility macros for building compiler bodies. these are typically
;;; **not** used by the average programmer, but they are used in the expansion
;;; of define-compiler.

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

(defstruct compiler-binding
  "Represents a generic compiler argument binding.

NAME: Symbol to be bound in the compiler definition.
OPTIONS: plist of options governing applicability of the compiler binding."
  (name    nil :type symbol)
  (options nil :type list))

(defstruct (wildcard-binding
            (:include compiler-binding)))

(defstruct (gate-binding
            (:include compiler-binding))
  "Represents a destructuring compiler argument binding.

OPERATOR: Describes the APPLICATION-OPERATOR field of the instruction.
PARAMETERS: A list of descriptors for the APPLICATION-PARAMETER field of the instruction.
ARGUMENTS: A list of descriptions for the APPLICATION-ARGUMENTS field of the instruction."
  (operator   nil)             ; Type of symbol or operator-description.
  (parameters nil)             ; Type of symbol or list of symbols and numeric literals.
  (arguments  nil :type list)) ; Type of list of symbols and numeric literals.

(defstruct (measure-binding
            (:include compiler-binding))
  "Represents a destructuring compiler argument binding for a MEASURE instruction."
  (qubit  nil)                     ; Type of symbol or numeric literal
  (target nil))                    ; Type of symbol or mref

(defun get-binding-from-instr (instr)
  "Constructs a COMPILER-BINDING object from an INSTRUCTION object, in such a way that if some auxiliary COMPILER-BINDING subsumes the output of this routine, then it will match when applied to the original INSTRUCTION object."
  (typecase instr
    (measure-discard
     (make-measure-binding :qubit '_))
    (measurement
     (make-measure-binding :qubit '_ :target '_))
    (application
     (let ((parameters (loop :for p :in (application-parameters instr)
                             :when (typep p 'constant)
                               :collect (constant-value p)
                             :when (symbolp p)
                               :collect '_))
           (arguments (loop :for q :in (application-arguments instr)
                            :when (typep q 'qubit)
                              :collect (qubit-index q)
                            :when (symbolp q)
                              :collect '_)))
       (make-gate-binding :operator (application-operator instr)
                          :parameters parameters
                          :arguments arguments)))))

(defun compiler-gateset-reducer-p (compiler)
  (getf (compiler-options compiler) ':gateset-reducer t))

(defun get-compilers (qubit-bound)
  "Returns the sublist of **COMPILERS-AVAILABLE** which match on no more than QUBIT-BOUND qubits and which function as gateset reducers."
  (remove-if-not
   (lambda (compiler)
     (and (compiler-gateset-reducer-p compiler)
          (= 1 (length (compiler-bindings compiler)))
          (gate-binding-p (first (compiler-bindings compiler)))
          (>= qubit-bound (length (gate-binding-arguments (first (compiler-bindings compiler)))))
          (endp (compiler-binding-options (first (compiler-bindings compiler))))))
   **compilers-available**))

(defun generate-blank-binding (qubit-count)
  "Constructs a wildcard COMPILER-BINDING of a fixed QUBIT-COUNT."
  (make-gate-binding :operator '_
                     :parameters nil
                     :arguments (loop :for i :below qubit-count
                                      :collect '_)))

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
   (options
    :initarg :options
    :reader compiler-options
    :documentation "plist of global options supplied to this compiler definition.")
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
  (closer-mop:set-funcallable-instance-function c (compiler-%function c)))

(defmethod print-object ((obj compiler) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (compiler-name obj))))


;;; in what follows, we're very interested in two kinds of maps keyed on bindings:
;;;  + an "occurrence table" is valued in integers, and it counts frequency
;;;  + a "gate set" is valued in GATE-RECORD objects, and it tracks e.g. fidelity

(defun make-occurrence-table ()
  "Construct a fresh occurrence table."
  (make-hash-table :test #'equalp))

(defun add-entry-to-occurrence-table (table binding count &optional (scalar 1))
  "Destructively increment a binding's value in an occurrence table."
  (incf (gethash binding table 0) (* count scalar)))

(defun copy-occurrence-table (table)
  "Create a shallow copy of an occurrence table."
  (let ((new-table (make-hash-table :test #'equalp)))
    (dohash ((key val) table)
      (add-entry-to-occurrence-table new-table key val))
    new-table))

(defun add-occurrence-tables (table1 table2 &optional (scalar2 1))
  "Construct a new occurrence table whose values are the sums of the values of the two input tables."
  (let ((new-table (copy-occurrence-table table1)))
    (dohash ((key val) table2)
      (add-entry-to-occurrence-table new-table key val scalar2))
    new-table))

(defun filter-by-qubit-count (table qubit-count)
  (let ((new-table (make-occurrence-table)))
    (dohash ((key val) table new-table)
      (when (= qubit-count
               (length (gate-binding-arguments key)))
        (setf (gethash key new-table) val)))))


;;; a panoply of routines for dissecting compiler bodies and bindings

(defun cleave-options (bindings-and-options &optional backlog)
  "Separates a list of the form (binding1 binding2 ... bindingn :option1 val1 ... :optionm valm) into the values pair (binding1 ... bindingn) (:option1 ... valm)."
  (cond
    ((or (endp bindings-and-options)
         (keywordp (first bindings-and-options)))
     (values (nreverse backlog) bindings-and-options))
    (t
     (cleave-options (rest bindings-and-options)
                     (cons (first bindings-and-options) backlog)))))

(defun get-output-gates-from-raw-code (body)
  "Walks the body of a compiler definition and extracts (or, at least attempts to extract) frequency data about the gates emitted by the compiler.  Results in an occurrence table.

N.B.: This routine is somewhat fragile, and highly creative compiler authors will want to supply a manually-constructed frequency table as an option to DEFINE-COMPILER rather than rely on the success of this helper."
  (unless (typep body 'cons)
    (return-from get-output-gates-from-raw-code (make-occurrence-table)))
  (case (first body)
    (build-gate
     (destructuring-bind (head name param-list &rest qubit-list) body
       (declare (ignore head))
       (let ((table (make-hash-table :test #'equalp))
             (operator (if (stringp name)
                           (named-operator name)
                           name))
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
         (setf (gethash (make-gate-binding :operator operator
                                           :parameters param-list
                                           :arguments qubit-list)
                        table)
               1)
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
         (setf (gethash (make-gate-binding :operator '_
                                           :parameters '_ 
                                           :arguments qubit-list)
                        table)
               1)
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

#+ignore
(defgeneric binding-subsumes-p (big-binding small-binding)
  (:method ((big-binding wildcard-binding) (small-binding wildcard-binding))
    (or (endp (wildcard-binding-options big-binding))
        (equalp (wildcard-binding-options big-binding)
                (wildcard-binding-options small-binding))))
  (:method ((big-binding compiler-binding) (small-binding wildcard-binding))
    nil)
  (:method ((big-binding gate-binding) (small-binding measure-binding))
    nil)
  (:method ((big-binding measure-binding) (small-binding gate-binding))
    nil)
  (:method ((big-binding measure-binding) (small-binding measure-binding))
    
    )
  (:method ((big-binding gate-binding) (small-binding gate-binding))
    
    )
  )

(defun binding-subsumes-p (big-binding small-binding)
  "If this routine returns T and BIG-BINDING matches on an INSTRUCTION, then SMALL-BINDING will necessarily also match on it."
  (flet ((wildcard-pattern-p (x)
           (or (symbolp x)
               (and (consp x) (every #'symbolp x)))))
    (cond
      ((and (wildcard-binding-p big-binding)
            (or (endp (compiler-binding-options big-binding))
                (equalp (compiler-binding-options big-binding)
                        (compiler-binding-options small-binding))))
       t)
      ((wildcard-binding-p small-binding)
       nil)
      ((and (compiler-binding-options big-binding)
            (compiler-binding-options small-binding))
       nil)
      ;; deal with the case of measure bindings
      ((or (and (measure-binding-p big-binding)
                (not (measure-binding-p small-binding)))
           (and (not (measure-binding-p big-binding))
                (measure-binding-p small-binding)))
       nil)
      ((and (measure-binding-p big-binding)
            (measure-binding-p small-binding))
       (or (symbolp (measure-binding-qubit big-binding))
           (equalp (measure-binding-qubit big-binding)
                   (measure-binding-qubit small-binding))))
      ;; big-binding and small-binding cannot either satisfy wildcard-binding-p.
      ;; check the operators for failing both _ <= _ and "literal" <= "literal"
      ((or (and (symbolp (gate-binding-operator small-binding))
                (not (symbolp (gate-binding-operator big-binding))))
           (and (not (symbolp (gate-binding-operator small-binding)))
                (not (symbolp (gate-binding-operator big-binding)))
                (not (equalp (gate-binding-operator big-binding)
                             (gate-binding-operator small-binding)))))
       nil)
      ;; check the parameters for failing both _ <= _ and literal <= literal
      ((or (and (wildcard-pattern-p (gate-binding-parameters small-binding))
                (not (wildcard-pattern-p (gate-binding-parameters big-binding))))
           (and (not (wildcard-pattern-p (gate-binding-parameters small-binding)))
                (not (wildcard-pattern-p (gate-binding-parameters big-binding)))
                (loop :for big-param :in (gate-binding-parameters big-binding)
                      :for small-param :in (gate-binding-parameters small-binding)
                        :thereis (or (and (symbolp small-param)
                                          (not (symbolp big-param)))
                                     (and (not (symbolp small-param))
                                          (not (symbolp big-param))
                                          (not (double= big-param small-param)))))))
       nil)
      ;; check the arguments for failing both _ <= _ and literal <= literal
      ((or (not (= (length (gate-binding-arguments big-binding))
                   (length (gate-binding-arguments small-binding))))
           (loop :for big-arg :in (gate-binding-arguments big-binding)
                 :for small-arg :in (gate-binding-arguments small-binding)
                   :thereis (or (and (symbolp small-arg)
                                     (not (symbolp big-arg)))
                                (and (not (symbolp small-arg))
                                     (not (symbolp big-arg))
                                     (not (= big-arg small-arg))))))
       nil)
      ;; otherwise, i guess everything checks out
      (t
       t))))

(defun update-occurrence-table (table compiler &optional qubit-count)
  "Treats a COMPILER as a replacement rule: each binding in the occurrence table TABLE on which COMPILER matches is replaced by the output occurrence table of COMPILER (appropriately scaled by the frequency of the binding in TABLE).

Optionally constrains the output to include only those bindings of a particular QUBIT-COUNT."
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
        ((binding-subsumes-p binding key)
         (setf binding-applied? t)
         (setf new-table (add-occurrence-tables new-table replacement val)))
        ;; if not, retain the old entry
        (t
         (add-entry-to-occurrence-table new-table key val))))
    (values new-table binding-applied?)))

(defun occurrence-table-cost (occurrence-table gateset &optional (default-value 0.5d0))
  "Given an OCCURRENCE-TABLE (i.e., a map from GATE-BINDINGs to frequencies) and a GATESET (i.e., a map from GATE-BINDINGs to GATE-RECORDs, which include fidelities), estimate the overall fidelity cost of all the gates in OCCURRENCE-TABLE."
  (let ((ret 1d0))
    (dohash ((key val) occurrence-table ret)
      (let ((true-cost default-value))
        (dohash ((cost-key cost-val) gateset)
          (when (binding-subsumes-p cost-key key)
            (setf true-cost (gate-record-fidelity cost-val))))
        (setf ret (* ret (expt true-cost val)))))))

(defun occurrence-table-in-gateset-p (occurrence-table gateset)
  "Are all of the entires in OCCURRENCE-TABLE subsumed by entries in GATESET?  This is the stopping condition for recognizing that a table consists of 'native gates'."
  (loop :for table-key :being :the :hash-keys :of occurrence-table
        :always (loop :for gateset-key :being :the :hash-keys :of gateset
                        :thereis (and (or (gate-record-duration (gethash gateset-key gateset))
                                          (gate-record-fidelity (gethash gateset-key gateset)))
                                      (binding-subsumes-p gateset-key table-key)))))

(defun find-shortest-compiler-path (compilers target-gateset occurrence-table &optional qubit-count)
  "Produces a path of compilers, drawn from COMPILERS, which convert all of the entries of OCCURRENCE-TABLE into \"native gates\" according to TARGET-GATESET.  Returns an alternating chain of occurrence tables and compilers: (On C(n-1) O(n-1) ... C1 O1), where O(j+1) is the result of substituting Cj through Oj and where On is subsumed by TARGET-GATESET.

Optionally restricts to considering only those gates and compilers which involve QUBIT-COUNT many qubits.

N.B.: The word \"shortest\" here is a bit fuzzy.  In practice it typically means \"of least length\", but in theory this invariant could be violated by pathological compiler output frequency + fidelity pairings that push the least-length path further down the priority queue."
  (let ((queue (make-instance 'cl-heap:priority-queue :sort-fun #'>)))
    (flet ((collect-bindings (occurrence-table)
             (loop :for key :being :the :hash-keys :of occurrence-table
                   :collect key)))
      ;; initial contents: arbitrary gate, no history
      (loop :with visited-nodes := ()
            :for (task . history) := (list occurrence-table)
              :then (cl-heap:dequeue queue)
            :unless task
              :do (error "Exhausted task queue without finding a route to the target gateset.")
            :when (occurrence-table-in-gateset-p task target-gateset)
              :return (cons task history)
            :unless (member (sort (collect-bindings task) #'binding-precedes-p)
                            visited-nodes :test #'equalp)
              :do (push (sort (collect-bindings task) #'binding-precedes-p)
                        visited-nodes)
                  (dolist (compiler compilers)
                    (multiple-value-bind (new-table updated?)
                        (update-occurrence-table task compiler qubit-count)
                      (when updated?
                        (cl-heap:enqueue queue
                                         (list* new-table compiler task history)
                                         (occurrence-table-cost new-table target-gateset)))))))))

(defun print-compiler-path (path)
  "Pretty-prints the output of FIND-SHORTEST-COMPILER-PATH. Useful for debugging."
  (dolist (item path)
    (typecase item
      (compiler (format t "~&is the output from applying ~a, coming from...~%" item))
      (hash-table
       (dohash ((key val) item)
         (format t "~a -> ~a~%" key val))))))

(defun binding-precedes-p (a-binding b-binding &optional a-options b-options)
  "If BINDING-PRECEDES-P and if A fails to match on an INSTRUCTION, then B fails to match later."
  (let* ((arg-count-a (if (gate-binding-p a-binding)
                          (length (gate-binding-arguments a-binding))
                          most-positive-fixnum))
         (arg-count-b (if (gate-binding-p b-binding)
                          (length (gate-binding-arguments b-binding))
                          most-positive-fixnum)))
    ;; fewer qubits means it comes earlier
    (cond
      ((< arg-count-a arg-count-b)
       t)
      ((> arg-count-a arg-count-b)
       nil)
      ;; from here on: (and (= arg-count-a arg-count b) ...)
      ;; cluster by name
      ((and (wildcard-binding-p a-binding)
            (not (symbolp (gate-binding-operator b-binding))))
       nil)
      ((and (not (symbolp (gate-binding-operator a-binding)))
            (wildcard-binding-p b-binding))
       t)
      ((and (typep (gate-binding-operator a-binding) 'named-operator)
            (typep (gate-binding-operator b-binding) 'named-operator)
            (adt:with-data (named-operator a-str) (gate-binding-operator a-binding)
              (adt:with-data (named-operator b-str) (gate-binding-operator b-binding)
                (string< a-str b-str))))
       t)
      ;; option predicates make it come earlier
      ((and (or a-options (compiler-binding-options a-binding))
            (not (or b-options (compiler-binding-options b-binding))))
       t)
      ;; restrictive parameter matching makes it come earlier
      ((and (not (symbolp (gate-binding-parameters a-binding)))
            (symbolp (gate-binding-parameters b-binding)))
       t)
      ((and (symbolp (gate-binding-parameters a-binding))
            (not (symbolp (gate-binding-parameters b-binding))))
       nil)
      ;; does A need to specialize before B?
      ((and (not (symbolp (gate-binding-parameters a-binding)))
            (not (symbolp (gate-binding-parameters b-binding)))
            (loop :for param-a :in (gate-binding-parameters a-binding)
                  :for param-b :in (gate-binding-parameters b-binding)
                    :thereis (and (not (symbolp param-a))
                                  (symbolp param-b))))
       t)
      (t
       nil))))

(defun sort-compilers-by-specialization (compilers)
  (sort (copy-seq compilers) #'binding-precedes-p))

(defun sort-compilers-by-output-friendliness (compilers gateset &optional qubit-count)
  (sort (copy-seq compilers)
        (lambda (a b)
          (let ((a (if qubit-count
                       (filter-by-qubit-count (compiler-output-gates a) qubit-count)
                       (compiler-output-gates a)))
                (b (if qubit-count
                       (filter-by-qubit-count (compiler-output-gates b) qubit-count)
                       (compiler-output-gates b))))
            (> (occurrence-table-cost a gateset)
               (occurrence-table-cost b gateset))))))

(defun blank-out-qubits (gateset)
  (let ((new-gateset (make-hash-table :test #'equalp)))
    (dohash ((key val) gateset new-gateset)
      (let ((new-key (copy-instance key)))
        (etypecase new-key
          (gate-binding
           (setf (gate-binding-arguments new-key)
                 (mapcar (constantly '_)
                         (gate-binding-arguments new-key))))
          (measure-binding
           (setf (measure-binding-qubit new-key) '_))
          (wildcard-binding
           t))
        (setf (gethash new-key new-gateset) val)))))

(defun compute-applicable-compilers (target-gateset qubit-count) ; h/t lisp
  (flet ( ;; Strips the occurrence tables from a run of FIND-SHORTEST-COMPILER-PATH.
         (discard-tables (path)
           (loop :for item :in path
                 :when (typep item 'compiler)
                   :collect item))
         ;; Checks whether PATH doubles back through the occurrence table START.
         (path-has-a-loop-p (path start)
           (and (< 1 (length path))
                (loop :for (gateset compiler) :on path :by #'cddr
                      :when (loop :for gate :being :the :hash-keys :of gateset
                                  :always (loop :for binding :in start
                                                  :thereis (binding-subsumes-p (first start) gate)))
                        :do (return-from path-has-a-loop-p t)
                      :finally (return nil)))))
    
    (let* ((target-gateset (blank-out-qubits target-gateset))
           (compilers (get-compilers qubit-count))
           (unconditional-compilers
             (remove-if (lambda (x)
                          (some (lambda (b) (and (not (symbolp b))
                                                 (compiler-binding-options b)))
                                (compiler-bindings x)))
                        compilers))
           (candidate-special-compilers
             (remove-if (lambda (x)
                          (or (/= qubit-count (length (gate-binding-arguments (first (compiler-bindings x)))))
                              (loop :for b :being :the :hash-keys :of target-gateset
                                      :thereis (binding-subsumes-p b (first (compiler-bindings x))))))
                        compilers))
           generic-path
           generic-cost
           compilers-to-save)
      
      ;; start by computing a fast route from the generic gate to the target gate set
      (setf generic-path (find-shortest-compiler-path unconditional-compilers
                                                      target-gateset
                                                      (alexandria:plist-hash-table
                                                       (list (generate-blank-binding qubit-count) 1)
                                                       :test #'equalp)
                                                      qubit-count))
      (setf generic-cost (occurrence-table-cost (first generic-path) target-gateset))
      (setf compilers-to-save (discard-tables generic-path))
      
      ;; it may be that non-generic gates have shorter routes to the target gate set.
      ;; each possible such route begins with a specialized compiler.
      ;; so, iterate over specialized compilers and see if they lead anywhere nice.
      (dolist (compiler candidate-special-compilers)
        (let* ((special-path
                 (find-shortest-compiler-path
                  unconditional-compilers target-gateset
                  (filter-by-qubit-count (compiler-output-gates compiler) qubit-count)
                  qubit-count))
               (special-cost (occurrence-table-cost (first special-path) target-gateset)))
          ;; did we in fact beat out the generic machinery?
          (when (and (not (path-has-a-loop-p special-path (compiler-bindings compiler)))
                     (> special-cost generic-cost))
            ;; then store it!
            (setf compilers-to-save
                  (append compilers-to-save (list compiler) (discard-tables special-path))))))
      
      ;; these are basically all the compilers we care to use; now we need to
      ;; sort them into preference order.
      (let* ((sorted-compilers
               (sort-compilers-by-output-friendliness
                        (remove-duplicates compilers-to-save)
                        target-gateset qubit-count))
             ;; additionally, we install a couple extra compilers as hax to make
             ;; the whole machine work. each such hak comes with an explanation,
             ;; and it would be preferable to work to make each hak unnecessary.
             (compilers-with-features
               (append
                ;; STATE-PREP-APPLICATION doesn't have a GATE-MATRIX, which causes
                ;; some havoc with all this new automation. so, instead, we prefix
                ;; with a compiler that catches S-P-As early.
                (cond
                  ((= 1 qubit-count)
                   (list #'state-prep-1q-compiler))
                  ((= 2 qubit-count)
                   (list #'state-prep-2q-compiler)))
                sorted-compilers)))
        (loop :for c :in compilers-with-features
              :if (occurrence-table-in-gateset-p (filter-by-qubit-count
                                                  (compiler-output-gates c)
                                                  qubit-count)
                                                 target-gateset)
                :collect c :into compilers-t
              :else
                :collect c :into compilers-nil
              :finally (return (append compilers-t compilers-nil)))))))

(defun compute-applicable-reducers (gateset)
  (let* ((gateset-bindings (loop :for g :being :the :hash-keys :of gateset
                                 :collect g)))
    (remove-if-not (lambda (c)
                     (let* ((in-fidelity 1d0)
                            (out-fidelity (occurrence-table-cost (compiler-output-gates c) gateset)))
                       (and (occurrence-table-in-gateset-p (compiler-output-gates c) gateset)
                            (every (lambda (b)
                                     (some (lambda (g)
                                             (let ((gate-fidelity (gate-record-fidelity
                                                                   (gethash g gateset))))
                                               (and (binding-subsumes-p g b)
                                                    (setf in-fidelity (* in-fidelity gate-fidelity)))))
                                           gateset-bindings))
                                   (compiler-bindings c))
                            (>= out-fidelity in-fidelity))))
                   **compilers-available**)))


;;; these functions assemble into the macro DEFINE-COMPILER, which constructs
;;; non-specialized instances of the above class COMPILER and installs them into
;;; the function namespace.

(defun make-binding-from-source (source)
  (when (symbolp source)
    (return-from make-binding-from-source
      (make-wildcard-binding :name source)))
  (assert (listp source))
  (multiple-value-bind (source options) (cleave-options source)
    (cond
      ((endp (cdr source))
       (make-wildcard-binding :name (first source)
                              :options options))
      (t
       (make-gate-binding :name (first source)
                          :options options
                          :operator (let ((op (first (second source))))
                                      (etypecase op
                                        (symbol
                                         op)
                                        (string
                                         (named-operator op))
                                        (operator-description
                                         op)))
                          :parameters (second (second source))
                          :arguments (rest (rest (second source))))))))

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
  (when (wildcard-binding-p binding)
    (return-from binding-environment nil))
  (nconc (when (match-symbol-p (gate-binding-operator binding))
           (list (cons (binding-op binding) t)))
         (unless (wildcard-pattern-p (gate-binding-parameters binding))
           (mapcan (lambda (param)
                     (when (match-symbol-p param)
                       (list (cons param t))))
                   (gate-binding-parameters binding)))
         (mapcan (lambda (arg)
                   (when (match-symbol-p arg)
                     (list (cons arg t))))
                 (gate-binding-arguments binding))))

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
               ((wildcard-binding-p binding)
                (if (wildcard-binding-options binding)
                    (expand-options binding env body)
                    body))
               ((gate-binding-p binding)
                (expand-op binding env
                           (expand-parameters binding env
                                              (expand-arguments binding env
                                                                (expand-options binding env body)))))
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
             (if (wildcard-pattern-p (gate-binding-parameters binding))
                 rest
                 (expand-sequence (gate-binding-parameters binding)
                                  env
                                  rest
                                  :gensym-name "PARAM"
                                  :seq-accessor 'application-parameters
                                  :gate-name (compiler-binding-name binding)
                                  :ele-accessor 'constant-value
                                  :ele-type 'constant
                                  :eq-predicate 'double=)))
           
           (expand-arguments (binding env rest)
             (expand-sequence (gate-binding-arguments binding)
                              env
                              rest
                              :gensym-name "ARG"
                              :seq-accessor 'application-arguments
                              :gate-name (compiler-binding-name binding)
                              :ele-accessor 'qubit-index
                              :ele-type 'qubit
                              :eq-predicate '=))
           
           (expand-options (binding env rest)
             (declare (ignore env))
             (let ((option-plist (compiler-binding-options binding)))
               (loop :for (val key) :on (reverse option-plist) :by #'cddr
                     :do (setf rest
                               (case key
                                 (:where
                                  `(when ,val
                                     ,rest))
                                 (:acting-on
                                  (destructuring-bind (wf qc) val
                                    `(unpack-wf ,(compiler-binding-name binding) context (,wf ,qc)
                                       ,rest)))
                                 (otherwise
                                  (error "Illegal OPERATOR-BIND option: ~a" key)))))
               rest))
           
           (expand-op (binding env body)
             (let ((op (gate-binding-operator binding)))
               (cond
                 ((wildcard-pattern-p op)
                  ;; ignore
                  body)
                 ((and (match-symbol-p op)
                       (not (lookup op env)))
                  ;; fresh binding
                  `(let ((,op ,(compiler-binding-name binding)))
                     ,body))
                 (t
                  ;; existing binding / data. insert string check
                  `(when (equalp ,op (application-operator ,(compiler-binding-name binding)))
                     ,body))))))
    (expand-bindings bindings nil)))

(defun enact-compiler-options (options body)
  (when (endp options)
    (return-from enact-compiler-options body))
  (destructuring-bind (key val &rest remaining-options) options
    (case key
      (:full-context
       (enact-compiler-options remaining-options
                               `(let ((,val context))
                                  ,body)))
      (:chip-specification
       (enact-compiler-options remaining-options
                               `(progn
                                  (unless context (give-up-compilation))
                                  (let ((,val (compilation-context-chip-specification context)))
                                    (unless ,val (give-up-compilation))
                                    ,body))))
      ((:gateset-reducer :class :permit-binding-mismatches-when)
       (enact-compiler-options remaining-options body))
      (otherwise
       (error "Unknown compiler option: ~a." (first options))))))

(defmacro define-compiler (name (&rest bindings) &body body)
  (multiple-value-bind (body decls docstring) (alexandria:parse-body body :documentation t)
    (multiple-value-bind (bindings options) (cleave-options bindings)
      (let* ((parsed-bindings (mapcar #'make-binding-from-source bindings))
             (variable-names (mapcar #'compiler-binding-name parsed-bindings)))
        (alexandria:when-let (pos (position "CONTEXT" variable-names :key #'string :test #'string=))
          (warn "DEFINE-COMPILER reserves the variable name CONTEXT, but the ~dth binding of ~a has that name."
                (1+ pos) (string name)))
        (alexandria:with-gensyms (ret-val ret-bool struct-name old-record)
          ;; TODO: do the alexandria destructuring to catch the docstring or whatever
          `(labels ((,name (,@variable-names &key context)
                      (declare (ignorable context))
                      (multiple-value-bind (,ret-val ,ret-bool)
                          ,(enact-compiler-options
                            options
                            (define-compiler-form parsed-bindings (append decls body)
                              :permit-binding-mismatches-when (getf options ':permit-binding-mismatches-when)))
                        (if ,ret-bool ,ret-val (give-up-compilation)))))
             (let ((,old-record (find ',name **compilers-available**
                                      :key #'compiler-name))
                   (,struct-name
                     (make-instance ',(getf options :class 'compiler)
                                    :name ',name
                                    :instruction-count ,(length variable-names)
                                    :bindings (mapcar #'make-binding-from-source ',bindings)
                                    :options ',options
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
               ',name)))))))
