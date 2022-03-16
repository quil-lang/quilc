;;;; qasm.lisp

(in-package #:cl-quil.qasm)

(defvar *line-start-position*)
(defvar *line-number*)

(defvar *gate-names*)
(setf (documentation '*gate-names* 'variable)
      "A HASH-TABLE of user-defined gates. The table is keyed on the gate name (a case-sensitive string), and the value is simply T.")

(defvar *gate-applications-are-formal*)
(setf (documentation '*gate-applications-are-formal* 'variable)
      "When parsing the body of a gate declaration, this is T, and is generally used to enable/disable certain parsing constraints.")

(defvar *gate-qregs*)
(setf (documentation '*gate-qregs* 'variable)
      "When parsing the body of a gate declaration, this is a list of the qubit register names (strings).")

(defvar *gate-params*)
(setf (documentation '*gate-params* 'variable)
      "When parsing the body of a gate declaration, this is a list of the parameter names (strings).")

(defvar *creg-names*)
(setf (documentation '*creg-names* 'variable)
      "Maps a creg name to its size (number of bits).")

(defvar *qreg-names*)
(setf (documentation '*qreg-names* 'variable)
      "Maps a qreg name to the pair (offset size). A qreg defined with `qreg q[size];` maps to Quil qubits (offset, offset + 1, offset + 2, ..., offset + size - 1). This complication maintains unique qubits in the Quil translation.")

(defvar *qubit-count*)
(setf (documentation '*qubit-count* 'variable)
      "The total number of qubits registered by qregs in the program.")

(defstruct (qasm-register (:conc-name reg-)
                          (:constructor nil))    ; :constructor nil ensures this thing can't be created
  "Abstract structure for ..."
  (name nil :type string :read-only t)
  (index nil :type (or null unsigned-byte)))

(defstruct (qreg (:include qasm-register)
                 (:constructor qreg (name &optional index)))
  "An OpenQASM qregister object.")

(defstruct (creg (:include qasm-register)
                 (:constructor creg (name &optional index)))
  "An OpenQASM cregister object.")

#+sbcl (declaim (sb-ext:freeze-type qasm-register qreg creg))

(defun reg-fmt (stream reg &optional colon-modifier at-modifier)
  (declare (ignore colon-modifier at-modifier))
  (let ((name (reg-name reg))
        (index (reg-index reg)))
    (if (null index)
        (format stream "~A" name)
        (format stream "~A[~A]" name index))))

(defgeneric register-to-quil-object (register)
  (:method ((qreg qreg))
    (with-slots (name index) qreg
      (if *gate-applications-are-formal*
          (quil:formal name)
          (destructuring-bind (offset size)
              (gethash name *qreg-names*)
            (assert (< index size) ()
                    "The index ~s is out-of-bounds for qreg ~/quil.qasm::reg-fmt/."
                    index qreg)
            (quil:qubit (+ offset index))))))
  (:method ((creg creg))
    (with-slots (name index) creg
      (let ((size (gethash name *creg-names*)))
        (assert (< index size) ()
                "The index ~s is out-of-bounds for creg ~/quil.qasm::reg-fmt/."
                index creg)
        (quil::mref name index)))))

(defun register-namespace (register)
  (etypecase register
    (qreg *qreg-names*)
    (creg *creg-names*)))

(defun find-register (register &key (error-if-undefined t))
  "Find the associated register data (in the appropriate namespace) for the given REGISTER.  If ERROR-IF-UNDEFINED, signal error if REGISTER is not defined."
  (let ((register-val (gethash (reg-name register)
                               (register-namespace register))))
    (cond
      ((not (null register-val))
       register-val)
      ((and (null register-val)
            error-if-undefined)
       (qasm-parse-error "Undefined register ~A." (reg-name register)))
      (t
       nil))))

(defun register-offset (register)
  (check-type register qreg)
  (first (find-register register :error-if-undefined t)))

(defgeneric register-size (register)
  (:method ((qreg qreg))
    (second (find-register qreg :error-if-undefined t)))
  (:method ((creg creg))
    (find-register creg :error-if-undefined t)))

(alexa:define-string-lexer line-lexer
  "A lexical analyzer for lines of OpenQASM 2.0."
  ((:int    "\\d+")
   (:real  "(?=\\d*[.eE])(?=\\.?\\d)\\d*\\.?\\d*(?:[eE][+-]?\\d+)?")
   (:ident  "[a-zA-Z](?:[A-Za-z0-9_]*[A-Za-z0-9_])?")
   (:string "\\\"(?:[^\\\"]|\\\\\\\")*\\\"")
   (:newline "(?:\\r\\n?|\\n)"))
  ("//([^\\n\\r]*)"
   nil
   )
  ((eager "{{NEWLINE}}")
   (incf *line-number*)
   (setf *line-start-position* $>)
   (return ':NEWLINE))
  ((eager "\\;")
   (return (tok :SEMI-COLON)))
  ((eager "\\(")
   (return (tok :LEFT-PAREN)))
  ((eager "\\)")
   (return (tok :RIGHT-PAREN)))
  ((eager "\\[")
   (return (tok :LEFT-SQUARE-BRACKET)))
  ((eager "\\]")
   (return (tok :RIGHT-SQUARE-BRACKET)))
  ((eager "\\{")
   (return (tok :LEFT-CURLY-BRACKET)))
  ((eager "\\}")
   (return (tok :RIGHT-CURLY-BRACKET)))
  ((eager "\\<")
   (return (tok :LEFT-ANGLE-BRACKET)))
  ((eager "\\>")
   (return (tok :RIGHT-ANGLE-BRACKET)))
  ((eager "\\,")
   (return (tok :COMMA)))
  ((eager "->")
   (return (tok :ARROW)))
  ("OPENQASM|qreg|creg|barrier|measure|reset|opaque|include|if|gate"
   (return (tok (intern (string-upcase $@) :keyword))))
  ("#pragma"
   (return (tok :PRAGMA)))
  ("pi"
   (return (tok ':PI (quil:constant quil::pi))))
  ((eager "\\+") (return (tok :PLUS "+")))
  ((eager "\\-") (return (tok :MINUS "-")))
  ((eager "\\*") (return (tok :TIMES "*")))
  ((eager "\\/") (return (tok :DIVIDE "/")))
  ((eager "\\^") (return (tok :EXPT "^")))
  ((eager "==")  (return (tok :EQUALSEQUALS)))
  ((eager "{{STRING}}")
   (return (tok :STRING (read-from-string $@))))
  ((eager "{{REAL}}")
   (return (tok :REAL (parse-float:parse-float $@))))
  ((eager "{{INT}}")
   (return (tok :NNINTEGER (parse-integer $@))))
  ("{{IDENT}}"
   (return (tok :NAME $@)))
  ("[^\\S\\n\\r]+"
   nil))

;;; SPLIT-SEQUENCE was too slow for 10k+ tokens.
(defun nsplit (wedge list)
  "Split LIST into non-empty sublists that were separated by WEDGE."
  (declare (type symbol wedge)
           (type list list)
           (optimize speed (space 0)))
  (let* ((pieces      (cons nil nil))
         (pieces-last pieces))
    (declare (type cons pieces pieces-last))
    (labels ((doit (start last end)
               (declare (type list start last end))
               (cond
                 ;; Done
                 ((null end)
                  (unless (eq start end)
                    (rplacd pieces-last (cons start nil))
                    (setf pieces-last (cdr pieces-last)))
                  pieces)
                 ;; Found a wedge.
                 ((eql wedge (car end))
                  (cond
                    ((eq start end)
                     (let ((next (cdr start)))
                       (doit next next next)))
                    (t
                     (rplacd pieces-last (cons start nil))
                     (setf pieces-last (cdr pieces-last))
                     (setf start (cdr end))
                     (rplacd last nil)
                     (doit start start start))))
                 ;; Keep on truckin'.
                 (t
                  (doit start end (cdr end))))))
      (cdr (doit list list list)))))

(defun tokenize-line (lexer string)
  "Given a lexer (as defined by DEFINE-STRING-LEXER) and a string, return a list of tokens represented by that string."
  (let ((f (funcall lexer string))
        (*line-number* 1)
        (*line-start-position* 0))
    (loop :for tok := (funcall f)
          :until (null tok)
          :collect tok)))

(defun tokenize (string)
  (flet ((more-newlines (string)
           ;; Forcefully swap semi-colons for newlines, to ease
           ;; parsing down-the-line. Likewise, ensure that newlines
           ;; both precede and follow curly braces.
           (with-output-to-string (s)
             (loop :for ch :across string
                   :do (case ch
                         ((#\{ #\}) (format s "~%~C~%" ch))
                         ((#\;) (terpri s))
                         (t (write-char ch s)))))))
    (nsplit ':NEWLINE (tokenize-line 'line-lexer
                                     (more-newlines string)))))

(define-condition qasm-parse-error (a:simple-parse-error)
  ()
  (:documentation "Representation of an error parsing QASM."))

(defun qasm-parse-error (format-control &rest format-args)
  "Signal a QASM-PARSE-ERROR with a descriptive error message described by FORMAT-CONTROL and FORMAT-ARGS."
  (error 'qasm-parse-error :format-control format-control
                           :format-arguments format-args))

(defun check-qasm-token-type (token type)
  (unless (eql (token-type token) type)
    (qasm-parse-error "Expected a token of type ~A but got a token of type ~A."
                      type
                      (token-type token))))

(defun check-qasm-unexpected-eof (tokens expected)
  (when (null tokens)
    (qasm-parse-error "Unexpectedly reached end of program expected ~A."
                      expected)))

(defun parse-program-lines (tok-lines)
  "Parse the next AST object from the list of token lists. Returns two values:

1. The next AST object.
2. A list of lines that remain unparsed."
  (let* ((line (first tok-lines))
         (tok (first line))
         (tok-type (token-type tok)))
    (case tok-type
      ((:OPENQASM)
       (parse-openqasm tok-lines))

      ((:INCLUDE)
       (parse-include tok-lines))

      ((:PRAGMA)
       (quil::parse-pragma tok-lines))

      ((:QREG)
       (parse-qreg-definition tok-lines))

      ((:CREG)
       (parse-creg-definition tok-lines))

      ((:BARRIER)
       ;; TODO Prettier pragma
       (values (quil::make-pragma
                (list "QASM_BARRIER")
                (format nil "~{~/quil::instruction-fmt/~^, ~}"
                        (let ((*gate-applications-are-formal* t))
                          (mapcar (lambda (qreg) (register-to-quil-object qreg))
                                  (parse-qregisters (rest line))))))
               (rest tok-lines)))

      ((:GATE)
       (parse-gate-decl tok-lines))

      ((:OPAQUE)
       (parse-opaque tok-lines))

      ((:MEASURE)
       (parse-measure tok-lines))

      ((:RESET)
       (values
        (map-registers (lambda (qub) (make-instance 'quil:reset-qubit :target qub))
                       (parse-qregister (rest line)))
        (rest tok-lines)))

      ((:NAME)
       (parse-application tok-lines))

      ((:IF)
       (parse-if tok-lines))

      (otherwise
       (qasm-parse-error "Got an unexpected token of type ~S ~
                         when trying to parse a program." tok-type)))))

(defmacro destructuring-token-bind (token-idents token-line &body body)
  ;; TODO Throw some lib-specific condition if destructuring-bind
  ;; fails.
  ;;
  ;; TODO Allow for "fall-through" cases. i.e. if the first
  ;; TOKEN-IDENTS doesn't match, try the next one, etc. I'm thinking
  ;; this might be helpful when matching `measure a -> b` vs `measure
  ;; a[0] -> b[0]`.
  ;;
  ;; TODO I wrote this while very tired. Untested.
  (let ((ts (mapcar #'a:ensure-list (remove '_ token-idents))))
    `(progn
       (unless (= ,(length token-idents) (length ,token-line))
         (qasm-parse-error "Expected ~d tokens but parsed ~d."
                           ,(length token-idents) (length ,token-line)))
       (loop :for (tok-name tok-type) :in ',ts
             :for tok :in ,token-line
             :when tok-type :do
               (assert (eql tok-type (token-type tok)) ()
                       "Expected a token of type ~S but got a token (~S) of type ~S."
                       tok-type tok (token-type tok)))
       ;; Some ugliness follows to remove ignored elements (those
       ;; whose token name is the underscore).
       (destructuring-bind ,(mapcar #'first (remove-if (lambda (tok) (eql '_ (car tok))) ts))
           (loop :for (tok-name tok-type) :in ',ts
                 :for tok :in ,token-line
                 :unless (eql tok-name '_)
                   :collect tok)
         ,@body))))

(defun parse-creg-definition (tok-lines)
  (check-qasm-unexpected-eof tok-lines "creg")

  (destructuring-bind (creg-toks . rest-toks)
      tok-lines
    (destructuring-token-bind ((_ :CREG) (name-tok :NAME) (_ :LEFT-SQUARE-BRACKET)
                               (length-tok :NNINTEGER) (_ :RIGHT-SQUARE-BRACKET))
        creg-toks
      (let ((name (token-payload name-tok))
            (length (token-payload length-tok)))
        (setf (gethash name *creg-names*) length)
        (values (quil::make-memory-descriptor
                 :name name
                 :type quil::quil-bit
                 :length length)
                rest-toks)))))

(defun parse-qreg-definition (tok-lines)
  (check-qasm-unexpected-eof tok-lines "qreg")

  (destructuring-bind (qreg-toks . rest-toks)
      tok-lines
    (destructuring-token-bind ((_ :QREG) (name :NAME) (_ :LEFT-SQUARE-BRACKET)
                               (length :NNINTEGER) (_ :RIGHT-SQUARE-BRACKET))
        qreg-toks
      (let ((name (token-payload name))
            (length (token-payload length)))
        ;; While a qreg doesn't directly map to a quil instruction, we
        ;; do need to keep track of named qubit registers so that we
        ;; can properly translate instructions that reference qubit
        ;; registers.
        ;;
        ;; e.g. the translation of "measure q[3] -> r[0];" might
        ;; simply be "MEASURE 3 r[0]". But what about "measure q[3] ->
        ;; r[0]; measure p[3] -> r[1];"? In OpenQASM the qubits "q[3]"
        ;; and "p[3]" are not the same.
        (when (gethash name *qreg-names*)
          (qasm-parse-error "Attempting to redefine qreg ~A." name))
        (setf (gethash name *qreg-names*) (list *qubit-count* length))
        (incf *qubit-count* length)
        (values nil
                rest-toks)))))

(defun parse-include (tok-lines)
  ;; TODO Use cl-quil's include?
  (check-qasm-unexpected-eof tok-lines "include")

  (destructuring-bind (include-toks . rest-toks)
      tok-lines
    (destructuring-token-bind ((_ :INCLUDE)
                               (path-tok :STRING))
        include-toks
      ;; TODO Some error checking.
      (let ((file (uiop:read-file-string
                   (if quil::*safe-include-directory*
                       (quil::resolve-safely (token-payload path-tok))
                       (token-payload path-tok)))))
        (values (parse-qasm-body file)
                rest-toks)))))

(defun parse-measure (tok-lines)
  (check-qasm-unexpected-eof tok-lines "measure")

  (let ((measure-toks (first tok-lines)))
    (check-qasm-token-type (first measure-toks) ':MEASURE)

    (multiple-value-bind (qreg rest-toks)
        (parse-qregister (rest measure-toks))
      (check-qasm-token-type (first rest-toks) ':ARROW)

      (multiple-value-bind (creg rest-toks)
          (parse-cregister (rest rest-toks))
        (assert (null rest-toks))

        (values (map-registers (lambda (src dest)
                                 (make-instance 'quil:measure
                                                :qubit src
                                                :address dest))
                               qreg creg)
                (rest tok-lines))))))

(defun parse-openqasm (tok-lines)
  (check-qasm-unexpected-eof tok-lines "OPENQASM")

  (destructuring-bind (openqasm-toks . rest-toks)
      tok-lines
    (destructuring-token-bind ((_ :OPENQASM) (version :REAL))
        openqasm-toks
      (values (quil::make-pragma (list "OPENQASM"
                                       (format nil "~A" (token-payload version))))
              rest-toks))))

(defun parse-register (tokens reg-ctor)
  (let* ((id (first tokens))
         (id-type (token-type id)))
    (unless (eql id-type ':NAME)
      (qasm-parse-error "Expected a token of type :NAME, got ~A." id-type))
    (let ((next (second tokens)))
      (if (and next
               (eql (token-type next) ':LEFT-SQUARE-BRACKET))
          (destructuring-token-bind ((index :NNINTEGER) (_ :RIGHT-SQUARE-BRACKET))
              (subseq tokens 2 4)
            (values (funcall reg-ctor (token-payload id) (token-payload index))
                    (subseq tokens 4)))
          (values (funcall reg-ctor (token-payload id))
                  (rest tokens))))))

;; TODO parse-cregister and parse-qregister have a common theme. For
;; D.R.Y.'s sake, factor out.
(defun parse-cregister (tokens)
  "Parse a single qasm creg from TOKENS. Returns the parsed register (of type QASM-REGISTER), and a second value which is the remaining tokens."
  (parse-register tokens #'creg))

(defun parse-qregister (tokens)
  "Parse a single qasm qreg from TOKENS. Returns the parsed register (of type QASM-REGISTER), and a second value which is the remaining tokens."
  (parse-register tokens #'qreg))

(defun parse-qregisters (tokens)
  "Parse qasm registers from TOKENS until a no more valid tokens are available. Returns the list of parsed registers (of type QASM-REGISTER)."
  (check-qasm-unexpected-eof tokens "registers")

  (loop :for (register rest-toks) := (multiple-value-list (parse-qregister tokens))
          :then (multiple-value-list (parse-qregister rest-toks))
        :for next-tok := (first rest-toks)
        :collect register :into registers
        :if (null next-tok) :do
          (return-from parse-qregisters registers)
        :if (and (not (null next-tok))
                 (eql (token-type next-tok) ':COMMA)) :do
                   (setf rest-toks (rest rest-toks))))

(defun %stringify-token-payload (token)
  (let ((payload (token-payload token)))
    (cond ((and *gate-params*
                (find payload *gate-params* :test #'equalp :key #'quil:param-name))
           (format nil "(%~A)" payload))
          ((eql (token-type token) ':LEFT-PAREN)
           "(")
          ((eql (token-type token) ':RIGHT-PAREN)
           ")")
          ((typep payload 'quil:constant)
           (let ((*read-default-float-format* (type-of (quil:constant-value payload))))
             (format nil "~F" (quil:constant-value payload))))
          (t
           (format nil "~A" payload)))))

(defun parse-param (tokens)
  (if (= 1 (length tokens))
      ;; We have a single token, and it must be either an ID (a name)
      ;; or a number.  So cast it, or return its name.
      (let* ((param (first tokens))
             (payload (token-payload param)))
        (cond ((member (token-type param) '(:NNINTEGER :REAL))
               (quil:constant (coerce payload 'double-float)))
              ((eql (token-type param) ':PI)
               payload)
              ((and *gate-params*
                    (find payload *gate-params* :test #'equalp))
               (%formalize payload))
              (t
               (quil::param payload))))
      ;; Otherwise we have a compound expression like lambda/2, in
      ;; which case we need to recombine that into a single string,
      ;; and then pass it through cl-quil's arithmetic parsing stuff.
      ;; This is a bit hacky, but I don't want to duplicate the
      ;; arithmetic stuff in cl-quil.
      (let* ((str (with-output-to-string (s)
                    (dolist (tok tokens)
                      (write-string (%stringify-token-payload tok) s))))
             (quil-tokens (first (quil::tokenize str)))
             (quil::*parse-context* ':DEFCIRCUIT)
             (quil::*formal-arguments-allowed* t))
        (declare (special quil::*parse-context* quil::*formal-arguments-allowed*))
        (quil::simplify-arithmetic (quil::parse-parameter-or-expression quil-tokens)))))

(defun parse-params (tokens)
  "Parse a list of qasm params (e.g. in the instruction  rx(0.5) q;). Returns a list of parameter values (type float), and a second value which is the remaining tokens (not including closing parenthesis)."
  (check-qasm-unexpected-eof tokens "parameters")
  (check-qasm-token-type (first tokens) :LEFT-PAREN)

  (let ((rp-pos (position ':RIGHT-PAREN tokens :key 'token-type :from-end t)))
    (unless rp-pos
      (qasm-parse-error "Could not find token of type ':RIGHT-PAREN in parameter list ~A." tokens))
    (let* ((subseq (subseq tokens 1 rp-pos))
           (param-list (split-sequence:split-sequence ':COMMA subseq :key #'token-type)))
      (when (and (some #'null param-list)
                 (> (length param-list) 1))
        (qasm-parse-error "Found a malformed parameter list ~A." tokens))
      (values (mapcar #'parse-param
                      (remove nil param-list))
              (subseq tokens (1+ rp-pos))))))

(defun maybe-parse-params (tokens)
  (if (eql (token-type (first tokens)) ':LEFT-PAREN)
      (parse-params tokens)
      (values nil tokens)))

(defun collect-single-application-from-tokens (tokens)
  ;; gate-name((p1, ...))? reg,+
  (check-qasm-unexpected-eof tokens "gate application")

  (let ((name-tok (first tokens)))
    (check-qasm-token-type name-tok :NAME)
    (multiple-value-bind (params rest-toks)
        (maybe-parse-params (rest tokens))
      (multiple-value-bind (registers rest-toks)
          (parse-qregisters rest-toks)
        (declare (ignore rest-toks))
        ;; TODO Check rest-toks is nil, or refactor to not require
        ;; that check.
        (list name-tok params registers)))))

(defun check-number-of-parameters (params number)
  (unless (= number (length params))
    (qasm-parse-error "Expected ~A parameters but found ~A."
                      number (length params))))

(defun build-u-gate (θ ϕ λ qubit)
  "As per the OpenQASM spec: U(θ, ϕ, λ) = RZ(ϕ) . RY(θ) . RZ(λ)."
  (list
   (quil::build-gate "RZ" `(,λ) qubit)
   (quil::build-gate "RY" `(,θ) qubit)
   (quil::build-gate "RZ" `(,ϕ) qubit)))

(defun map-registers (function register &rest more-registers)
  "Apply FUNCTION to successive sets of registers.

In OpenQASM a gate can be applied to an indexed register like so: rx(pi) q[0].  Alternatively, a gate can also be applied to an unindexed register: rx(pi) q.  In this case, the gate is applied to all the qubits in that register.  More complex cases occur where multiple registers are involved, and some subset of those are unindexed and so the gate must be expanded into multiple gate applications over the appropriate registers.

For example, consider

  qreg p[3]; qreg q[2]; qreg r[3];

  ccx p, q[1], r;

This should be expanded into

  qreg p[3]; qreg q[2]; qreg r[3];

  ccx p[0], q[1], r[0];
  ccx p[1], q[1], r[1];
  ccx p[2], q[1], r[2];

It is an error to have unindexed registers of unequal size.

Note: the above \"expansion\" is not performed when in a gate body."
  (let* ((registers (cons register more-registers))
         (unindexed-registers (remove-if-not #'null registers :key #'reg-index)))
    (cond ((and unindexed-registers
                ;; This prevents us from expanding, e.g., `x q` in
                ;; `gate a q { x q; };`. Perhaps this guard should be
                ;; performed by the caller however, as it makes this
                ;; function less general than its name would imply.
                (not *gate-applications-are-formal*))
           (let* ((register-size (reduce #'min unindexed-registers :key #'register-size))
                  (registers
                    (mapcar (lambda (register)
                              (a:if-let ((index (reg-index register)))
                                (loop :repeat register-size :collect (register-to-quil-object register))
                                (loop :for i :below register-size
                                      :for reg := (copy-qasm-register register)
                                      :do (setf (reg-index reg) i)
                                      :collect (register-to-quil-object reg))))
                            registers)))
             (apply #'mapcar function registers)))
          (t
           (apply function (mapcar #'register-to-quil-object registers))))))

(defun %qasm-gate-name (name)
  "Prefixes NAME with \"QASM_\".  Prevents a QASM gate naming collision with Quil names."
  (check-type name string)
  (concatenate 'string "QASM_" name))

(defun parse-application (tok-lines)
  (let ((application-toks (collect-single-application-from-tokens (first tok-lines)))
        (rest-toks (rest tok-lines)))
    (destructuring-bind (name (&rest params) (&rest registers))
        application-toks
      (declare (ignorable params registers))
      (let ((name (token-payload name)))
        (cond
          ;; TODO Valid number of params, registers
          ;;
          ;; "QE hardware primitives"
          ((string= name "CX")
           (check-number-of-parameters params 0)
           (values (apply #'map-registers (lambda (ctl tgt)
                                            (quil::build-gate "CNOT" nil ctl tgt))
                          registers)
                   rest-toks))

          ((string= name "U")
           (check-number-of-parameters params 3)
           (destructuring-bind (θ ϕ λ) params
             (values (apply #'map-registers (lambda (tgt) (build-u-gate θ ϕ λ tgt))
                            registers)
                     rest-toks)))

          (t
           (a:if-let ((gate (gethash (%qasm-gate-name name) *gate-names*)))
             (values
              (if (eql gate ':opaque)
                  (let* ((params-constant-values (mapcar #'quil:constant-value params))
                         (*read-default-float-format* (or (and (null params) 'single-float)
                                                          (type-of (car params-constant-values)))))
                    (quil::make-pragma (list "QASM_OPAQUE_APPLICATION" name)
                                       (format nil "(~{~F~^, ~}) ~{~/quil:instruction-fmt/~^, ~}"
                                               params-constant-values
                                               (mapcar #'register-to-quil-object
                                                       registers))))
                  (apply #'map-registers (lambda (&rest qubits)
                                           (make-instance 'quil:unresolved-application
                                                          :operator (quil:named-operator (%qasm-gate-name name))
                                                          :parameters params
                                                          :arguments qubits))
                         registers))
              rest-toks)
             (qasm-parse-error "Found unknown gate application '~A'."
                               name))))))))

(defun parse-gate-body (tok-lines)
  (a:flatten (mapcar #'parse-application (mapcar #'list tok-lines))))

(defun collect-gate-decl (tok-line)
  (let ((name (token-payload (second tok-line))))
    (multiple-value-bind (params rest-toks)
        (maybe-parse-params (subseq tok-line 2))
      (dolist (p params) (push p *gate-params*))
      (let ((qregs (mapcar #'reg-name (parse-qregisters rest-toks))))
        (dolist (q qregs) (push q *gate-qregs*))
        (values name params qregs)))))

(defun line-position-of-token-type (token-type sequence)
  (position token-type sequence
            :key (a:compose #'token-type #'first)))

(defun %formalize (param)
  (quil:param param))

(defun parse-gate-decl (tok-lines)
  (check-qasm-unexpected-eof tok-lines "gate")
  (check-qasm-token-type (first (first tok-lines)) ':GATE)

  (let* ((close-pos (line-position-of-token-type ':RIGHT-CURLY-BRACKET tok-lines))
         (*gate-applications-are-formal* t)
         (*gate-params* nil)
         (*gate-qregs* nil))
    (multiple-value-bind (gate-name gate-params gate-qargs)
        (collect-gate-decl (first tok-lines))
      ;; TODO Store more info about the gate, for later validating an
      ;; application (num params, qubits, etc).
      (setf (gethash (%qasm-gate-name gate-name) *gate-names*) t)
      (values (quil::make-circuit-definition
               (%qasm-gate-name gate-name)
               gate-params
               (mapcar #'quil::formal gate-qargs)
               (parse-gate-body (subseq tok-lines 2 close-pos))
               :context nil)
              (subseq tok-lines (1+ close-pos))))))

(defun parse-opaque (tok-lines)
  (check-qasm-unexpected-eof tok-lines "opaque")
  (check-qasm-token-type (first (first tok-lines)) ':OPAQUE)
  (check-qasm-token-type (second (first tok-lines)) ':GATE)

  (destructuring-bind ((opaque gate name-tok &rest rest-toks) &rest rest-lines)
      tok-lines
    (declare (ignore opaque gate))
    (multiple-value-bind (params rest-toks)
        (maybe-parse-params rest-toks)
      (let ((*gate-applications-are-formal* t)
            (qregs (parse-qregisters rest-toks)))
        (setf (gethash (%qasm-gate-name (token-payload name-tok)) *gate-names*) ':opaque)
        (values (quil::make-pragma
                 (list "QASM_OPAQUE_DEFINITION" (token-payload name-tok))
                 (format nil "(~{~A~^, ~}) ~{~/quil:instruction-fmt/~^, ~}"
                         (mapcar #'quil:param-name params)
                         (mapcar #'register-to-quil-object qregs)))
                rest-lines)))))

(defun parse-if (tok-lines)
  (check-qasm-unexpected-eof tok-lines "if")
  (check-qasm-token-type (first (first tok-lines)) ':IF)

  (destructuring-bind (if-toks . rest-lines)
      tok-lines
    (pop if-toks)
    (check-qasm-token-type (pop if-toks) ':LEFT-PAREN)
    (multiple-value-bind (register rest-toks)
        (parse-cregister if-toks)
      (check-qasm-token-type (pop rest-toks) ':EQUALSEQUALS)
      (let ((val (pop rest-toks)))
        (check-qasm-token-type val ':NNINTEGER)
        (check-qasm-token-type (pop rest-toks) ':RIGHT-PAREN)
        (multiple-value-bind (gate-application rest-toks)
            (parse-application (list rest-toks))
          (assert (null rest-toks))
          (let* ((cmp-name (string (gensym "CMP-")))
                 (cmp-desc (quil::make-memory-descriptor
                            :name cmp-name
                            :type quil::quil-bit
                            :length 1))
                 (jmp-label (quil::label (string (gensym "JMP-")))))
            (unless (< (token-payload val)
                       (expt 2 (register-size register)))
              (qasm-parse-error "Cannot compare the integer ~A to the creg ~A of size ~A."
                                (token-payload val)
                                (creg-name register)
                                (register-size register)))
            (values
             (list
              ;; Build a unique intermediary classical address to store
              ;; the comparison result.
              cmp-desc
              ;; This is admittedly a bit ugly. OpenQASM's "if" is a
              ;; little strange at first glance.  You might expect
              ;; that you would compare bits of classical memory with
              ;; something like `if(c[0] == 1) ...` but actually you
              ;; compare the entire classical register against a
              ;; number (bitwise) `if(c == 5) ...` tests
              ;;
              ;;     c[0] == 1 & c[1] == 0 & c[2] == 1 & c[3] == 0
              ;;
              ;; etc.
              ;;
              ;; TODO Clean this up, possibly using Quil's SHARING.
              (loop :for i :below (register-size register)
                    :for reg := (copy-qasm-register register)
                    :do (setf (reg-index reg) i)
                    :collect
                    ;; TODO Make this more efficient by using AND. See
                    ;; https://github.com/rigetti/quilc/pull/495/files#r353990886
                    (make-instance 'quil::classical-equality-bit/bit/immediate
                                   :left (register-to-quil-object reg)
                                   :right (quil::constant
                                           (if (logbitp i (token-payload val)) 1 0)
                                           quil::quil-bit)
                                   :target (quil::mref cmp-name 0))
                    :collect
                    (make-instance 'quil::jump-unless
                                   :address (quil::mref cmp-name 0)
                                   :label jmp-label))
              gate-application
              (make-instance 'quil::jump-target
                             :label jmp-label))
             rest-lines)))))))

(defun parse-qasm-body (string)
  "Parse STRING into a list of instructions."
  (check-type string string)

  (loop :with tok-lines := (tokenize string)
        :until (null tok-lines)
        :for (program-entity rest-toks) := (multiple-value-list
                                            (parse-program-lines tok-lines))
        :when program-entity
          :append (a:flatten (a:ensure-list program-entity)) :do
            (setf tok-lines rest-toks)))

(defun parse-qasm-into-raw-program (string)
  "Parse STRING into a raw, untransformed PARSED-PROGRAM object."

  (let* ((*creg-names* (make-hash-table :test #'equalp))
         (*qreg-names* (make-hash-table :test #'equalp))
         (*qubit-count* 0)
         (*gate-applications-are-formal* nil)
         (*gate-params* nil)
         (*gate-names* (make-hash-table :test #'equalp))
         (code (parse-qasm-body string)))
    (setf code (quil::process-includes code))
    ;; Return the parsed sequence of objects.
    (values
     (quil::raw-quil-to-unresolved-program code)
     *creg-names*
     *qreg-names*
     *qubit-count*)))

(defun parse-qasm (string)
  "Parse STRING into a PARSED-PROGRAM object, applying all transforms."
  (let ((pp (quil::resolve-objects (parse-qasm-into-raw-program string))))
    (setf pp (quil::transform 'quil::expand-circuits pp))
    (setf pp (quil::transform 'quil::type-check pp))
    (setf pp (quil::transform 'quil::simplify-individual-instructions pp))))
