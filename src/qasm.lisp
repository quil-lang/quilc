 ;;;; qasm.lisp

(in-package #:cl-quil.qasm)
(use-package :abstract-classes)

(deftype qasm-keywords ()
  '(member
    :OPENQASM :QREG :CREG :GATE :BARRIER
    :MEASURE :RESET :OPAQUE :INCLUDE
    :IF))

(deftype token-type ()
  '(or
    qasm-keywords
    (member :SEMI-COLON :LEFT-PAREN :RIGHT-PAREN :COMMA
            :ARROW :LEFT-SQUARE-BRACKET :RIGHT-SQUARE-BRACKET :LEFT-CURLY-BRACKET
            :RIGHT-CURLY-BRACKET :LEFT-ANGLE-BRACKET :RIGHT-ANGLE-BRACKET :PLUS
            :MINUS :TIMES :DIVIDE :EXPT :EQUALSEQUALS :NNINTEGER :REAL :ID
            :STRING :KEYWORD :OPENQASM :QREG :CREG :GATE :COMMENT :PI)))

(defparameter *valid-functions*
    '(("sin"  cl:sin)
      ("cos"  cl:cos)
      ("sqrt" cl:sqrt)
      ("exp"  cl:exp)
      ("ln"   cl:log)
      ("tan"  cl:tan)))

(defvar *line-start-position*)
(defvar *line-number*)

(defparameter *gate-names* (make-hash-table :test #'equal)
  "A table of user-defined gates.")

(defparameter *gate-applications-are-formal* nil
  "When parsing the body of a gate declaration, this is T, and is generally used to enable/disable certain parsing constraints.")

(defparameter *gate-qregs* nil
  "When parsing the body of a gate declaration, this is a list of the qubit register names.")

(defparameter *gate-params* nil
  "When parsing the body of a gate declaration, this is a list of the parameter names.")

(defparameter *creg-names* (make-hash-table :test #'equal)
  "Maps a creg name to its size.")

(defparameter *qreg-names* (make-hash-table :test #'equal)
  "Maps a qreg name to the pair (offset . size). A qreg defined with `qreg q[size];` maps to Quil qubits (offset, offset + 1, offset + 2, ..., offset + size - 1). This complication maintains unique qubits in the Quil translation.")

(defparameter *qubit-count* 0
  "The number of qubits registered by qreg.")

(defclass qasm-reg ()
  ((name :initarg :name :accessor reg-name
         :type string)
   (index :initarg :index :accessor reg-index
          :type (or null (integer 0 *))))
  (:metaclass abstract-class))

(defclass qasm-qreg (qasm-reg)
  ())

(defun qreg (name &optional index)
  (make-instance 'qasm-qreg :name name :index index))

(defclass qasm-creg (qasm-reg)
  ())

(defun creg (name &optional index)
  (make-instance 'qasm-creg :name name :index index))

(defun print-reg (reg &optional stream)
  (let ((name (reg-name reg))
        (index (reg-index reg)))
    (if index
        (format stream "~A[~A]" name index)
        (format stream "~A" name))))

(defgeneric register-to-quil-object (register)
  (:method ((qreg qasm-qreg))
    (with-slots (name index) qreg
      (if *gate-applications-are-formal*
          (quil:formal name)
          (destructuring-bind (offset size)
              (gethash name *qreg-names*)
            (assert (< index size) ()
                    "The index ~s is out-of-bounds for qreg ~s."
                    index qreg)
            (quil::qubit (+ offset index))))))
  (:method ((creg qasm-creg))
    (with-slots (name index) creg
      (destructuring-bind (offset size)
          (gethash name *creg-names*)
        (assert (< index size) ()
                "The index ~s is out-of-bounds for creg ~s."
                index creg)
        (quil::mref name (+ offset index))))))

(defgeneric check-register-is-defined (register)
  (:method ((qreg qasm-qreg))
    (unless (gethash (reg-name qreg) *qreg-names*)
      (q2q-parse-error "Undefined qregister ~A." (reg-name qreg))))
  (:method ((creg qasm-creg))
    (unless (gethash (reg-name creg) *creg-names*)
      (q2q-parse-error "Undefined cregister ~A." (reg-name creg)))))

(defun register-offset (register)
  (check-type register qasm-qreg)
  (check-register-is-defined register)
  (first (gethash (reg-name register) *qreg-names*)))

(defgeneric register-size (register)
  (:method ((qreg qasm-qreg))
    (second (gethash (reg-name qreg) *qreg-names*)))
  (:method ((creg qasm-creg))
    (second (gethash (reg-name creg) *creg-names*))))

(defstruct (token (:constructor tok (type &optional payload)))
  "A lexical token."
  (line nil :type (or null (integer 1)))
  (type nil :type token-type)
  (payload nil))

(alexa:define-string-lexer line-lexer
  "A lexical analyzer for lines of Quil."
  ((:int    "\\d+")
   (:real  "(?=\\d*[.eE])(?=\\.?\\d)\\d*\\.?\\d*(?:[eE][+-]?\\d+)?")
   (:ident  "[a-zA-Z](?:[A-Za-z0-9_\\-]*[A-Za-z0-9_])?")
   (:string "\\\"(?:[^\\\"]|\\\\\\\")*\\\"")
   (:newline "(?:\\r\\n?|\\n)"))
  ;; TODO Do I want eager here?
  ("//([^\\n\\r]*)"
   (return (tok :COMMENT $@)))
  ((eager "{{NEWLINE}}")
   ;; We return a keyword because they get split out and
   ;; removed. They're not actually logical tokens of the Quil
   ;; language.
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
  ("pi"
   (return (tok ':PI (quil:constant quil::pi))))
  ("\\+" (return (tok :PLUS "+")))
  ("\\-" (return (tok :MINUS "-")))
  ("\\*" (return (tok :TIMES "*")))
  ("\\/" (return (tok :DIVIDE "/")))
  ("\\^" (return (tok :EXPT "^")))
  ("=="  (return (tok :EQUALSEQUALS)))
  ((eager "{{STRING}}")
   (return (tok :STRING (read-from-string $@))))
  ((eager "{{REAL}}")
   (return (tok :REAL (parse-float:parse-float $@))))
  ((eager "{{INT}}")
   (return (tok :NNINTEGER (parse-integer $@))))
  ("{{IDENT}}"
   (return (tok :ID $@)))
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
  (let* (;; Forcefully swap semi-colons for newlines, to ease parsing
         ;; down-the-line.
         (string (substitute #\Newline #\; string))
         ;; Similarly to aid parsing, ensure that newlines both
         ;; precede and follow curly braces.
         (string (ppcre:regex-replace-all #\{ string "
{
"))
         (string (ppcre:regex-replace-all #\} string "
}
"))
         (lines (nsplit ':NEWLINE (tokenize-line 'line-lexer string))))
    lines))

;; parse

(define-condition q2q-parse-error (alexandria:simple-parse-error)
  ()
  (:documentation "Representation of an error parsing QASM."))

(defun q2q-parse-error (format-control &rest format-args)
  "Signal a Q2Q-PARSE-ERROR with a descriptive error message described by FORMAT-CONTROL and FORMAT-ARGS."
  (error 'q2q-parse-error :format-control format-control
         :format-arguments format-args))

(defmacro q2q-check-token-type (token type)
  `(unless (eql (token-type ,token) ,type)
     (q2q-parse-error "Expected a token of type ~A but got a token of type ~A."
                      ,type
                      (token-type ,token))))

(defmacro q2q-check-unexpected-eof (tokens expected)
  `(when (null ,tokens)
     (q2q-parse-error "Unexpectedly reached end of program, expected ~A."
                      ,expected)))

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

      ((:COMMENT)
       (parse-comment tok-lines))

      ((:INCLUDE)
       (parse-include tok-lines))

      ((:QREG)
       (parse-qreg-definition tok-lines))

      ((:CREG)
       (parse-creg-definition tok-lines))

      ((:BARRIER)
       (values (quil::make-pragma
                (list "QASM_BARRIER")
                (format nil "~{~A~^, ~}"
                        (mapcar (lambda (qreg)
                                  (quil::print-instruction
                                   (let ((*gate-applications-are-formal* t))
                                     (register-to-quil-object qreg))
                                   nil))
                                (parse-qregisters (rest (first tok-lines))))))
               (rest tok-lines)))
      
      ((:GATE)
       (parse-gate-decl tok-lines))

      ((:OPAQUE)
       (parse-opaque tok-lines))
      
      ((:MEASURE)
       (parse-measure tok-lines))
      
      ((:RESET)
       (values
        (map-registers (lambda (qub) (make-instance 'quil::reset-qubit :target qub))
                       (parse-qregister (rest (first tok-lines))))
        (rest tok-lines)))
      
      ((:ID)
       (parse-application tok-lines))

      ((:IF)
       (parse-if tok-lines))

      (otherwise
       (q2q-parse-error "Got an unexpected token of type ~S ~
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
  (let ((ts (mapcar #'alexandria:ensure-list (remove '_ token-idents))))
    `(progn
       (unless (= ,(length token-idents) (length ,token-line))
         (q2q-parse-error "Expected ~d tokens but parsed ~d."
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

;; u1(alpha) qubit;

(defun parse-creg-definition (tok-lines)
  (when (null tok-lines)
    (q2q-parse-error "Unexpectedly reached end of program, expecting creg."))

  (destructuring-bind (creg-toks . rest-toks)
      tok-lines
    (destructuring-token-bind ((_ :CREG) (name-tok :ID) (_ :LEFT-SQUARE-BRACKET)
                               (length-tok :NNINTEGER) (_ :RIGHT-SQUARE-BRACKET))
        creg-toks
      (let ((name (token-payload name-tok))
            (length (token-payload length-tok)))
        (setf (gethash name *creg-names*) (list 0 length))
        (values (quil::make-memory-descriptor
                 :name name
                 :type quil::quil-bit
                 :length length
                 :lexical-context nil)
                rest-toks)))))

(defun parse-qreg-definition (tok-lines)
  (q2q-check-unexpected-eof tok-lines "qreg")

  (destructuring-bind (qreg-toks . rest-toks)
      tok-lines
    (destructuring-token-bind ((_ :QREG) (name :ID) (_ :LEFT-SQUARE-BRACKET)
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
        (setf (gethash name *qreg-names*) (list *qubit-count* length))
        (incf *qubit-count* length)
        (values nil
                rest-toks)))))

(defun parse-include (tok-lines)
  (q2q-check-unexpected-eof tok-lines "include")

  (destructuring-bind (include-toks . rest-toks)
      tok-lines
    (destructuring-token-bind ((_ :INCLUDE)
                               (path-tok :STRING))
        include-toks
      ;; TODO Some error checking.
      (let ((file (uiop:read-file-string (token-payload path-tok))))
        (values (parse-qasm-body file)
                rest-toks)))))

(defun parse-measure (tok-lines)
  (q2q-check-unexpected-eof tok-lines "measure")

  (let ((measure-toks (first tok-lines)))
    (q2q-check-token-type (first measure-toks) ':MEASURE)

    (multiple-value-bind (qreg rest-toks)
        (parse-qregister (rest measure-toks))
      (q2q-check-token-type (first rest-toks) ':ARROW)
      (check-register-is-defined qreg)

      (multiple-value-bind (creg rest-toks)
          (parse-cregister (rest rest-toks))
        (assert (null rest-toks))
        (check-register-is-defined creg)
        
        (values (map-registers (lambda (src dest)
                                 (make-instance 'quil:measure
                                                :qubit src
                                                :address dest))
                               qreg creg)
                (rest tok-lines))))))

(defun quilify-qasm-comment (str)
  (concatenate 'string "#" (string-left-trim "/" str)))

(defun parse-comment (tok-lines)
  (q2q-check-unexpected-eof tok-lines "comment")
  
  (values nil
          (rest tok-lines)))

(defun parse-openqasm (tok-lines)
  (q2q-check-unexpected-eof tok-lines "OPENQASM")

  (destructuring-bind (openqasm-toks . rest-toks)
      tok-lines
    (destructuring-token-bind ((_ :OPENQASM) (version :REAL))
        openqasm-toks
      (values (quil::make-pragma (list "OPENQASM"
                                       (format nil "~A" (token-payload version))))
              rest-toks))))

(defun parse-cregister (tokens)
  "Parse a single qasm creg from TOKENS. Returns the parsed register (of type QASM-REGISTER), and a second value which is the remaining tokens."
  (let* ((id (first tokens))
         (id-type (token-type id)))
    (unless (eql id-type ':ID)
      (q2q-parse-error "Expected a token of type :ID, got ~A." id-type))
    (let ((next (second tokens)))
      (if (and next
               (eql (token-type next) ':LEFT-SQUARE-BRACKET))
          (destructuring-token-bind ((index :NNINTEGER) (_ :RIGHT-SQUARE-BRACKET))
              (subseq tokens 2 4)
            (values (creg (token-payload id)
                          (token-payload index))
                    (subseq tokens 4)))
          (values (creg (token-payload id))
                  (rest tokens))))))

(defun parse-qregister (tokens)
  "Parse a single qasm qreg from TOKENS. Returns the parsed register (of type QASM-REGISTER), and a second value which is the remaining tokens."
  (let* ((id (first tokens))
         (id-type (token-type id)))
    (unless (eql id-type ':ID)
      (q2q-parse-error "Expected a token of type :ID, got ~A." id-type))
    (let ((next (second tokens)))
      (if (and next
               (eql (token-type next) ':LEFT-SQUARE-BRACKET))
          (destructuring-token-bind ((index :NNINTEGER) (_ :RIGHT-SQUARE-BRACKET))
              (subseq tokens 2 4)
            (values (qreg (token-payload id)
                          (token-payload index))
                    (subseq tokens 4)))
          (values (qreg (token-payload id))
                  (rest tokens))))))

(defun parse-qregisters (tokens)
  "Parse qasm registers from TOKENS until a no more valid tokens are available. Returns the list of parsed registers (of type QASM-REGISTER)."
  (q2q-check-unexpected-eof tokens "registers")
  
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
           (format nil "%~A" payload))
          ((typep payload 'quil:constant)
           (format nil "~F" (quil:constant-value payload)))
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
      (let* ((str (format nil "(~A)"
                          (apply #'concatenate 'string (mapcar #'%stringify-token-payload tokens))))
             (quil-tokens (first (quil::tokenize str)))
             (quil::*parse-context* ':DEFCIRCUIT)
             (quil::*formal-arguments-allowed* t))
        (quil::parse-parameter-or-expression quil-tokens))))

(defun parse-params (tokens)
  "Parse a list of qasm params (e.g. in the instruction  rx(0.5) q;). Returns a list of parameter values (type float), and a second value which is the remaining tokens (not including closing parenthesis)."
  (q2q-check-unexpected-eof tokens "parameters")
  (q2q-check-token-type (first tokens) :LEFT-PAREN)
  
  (let ((rp-pos (position ':RIGHT-PAREN tokens :key 'token-type)))
    (unless rp-pos
      (q2q-parse-error "Could not find token of type ':RIGHT-PAREN in parameter list ~A." tokens))
    (let* ((subseq (subseq tokens 1 rp-pos))
           ;; TODO Raise an error when parsing something like "(0, 0,)"
           (param-list (split-sequence:split-sequence ':COMMA subseq :key #'token-type)))
      (values (mapcar #'parse-param
                      param-list)
              (subseq tokens (1+ rp-pos))))))

(defun maybe-parse-params (tokens)
  (if (eql (token-type (first tokens)) ':LEFT-PAREN)
      (parse-params tokens)
      (values nil tokens)))

(defun collect-single-application-from-tokens (tokens)
  ;; gate-name((p1, ...))? reg,+
  (q2q-check-unexpected-eof tokens "gate application")

  (let ((name-tok (first tokens)))
    (q2q-check-token-type name-tok :ID)
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
    (q2q-parse-error "Expected ~A parameters but found ~A."
                     number (length params))))

(defun build-u-gate (θ ϕ λ qubit)
  (list
   (quil::build-gate "RZ" `(,λ) qubit)
   (quil::build-gate "RY" `(,ϕ) qubit)
   (quil::build-gate "RZ" `(,θ) qubit)))

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
  (let ((registers (append (list register) more-registers)))
    (let ((unindexed-registers (remove-if-not #'null registers :key #'reg-index)))
      (cond ((and unindexed-registers
                  ;; This prevents us from expanding, e.g., `x q` in
                  ;; `gate a q { x q; };`. Perhaps this guard should be
                  ;; performed by the caller however, as it makes this
                  ;; function less general than its name would imply.
                  (not *gate-applications-are-formal*))
             (map nil #'check-register-is-defined registers)
             (let* ((register-size (reduce #'min (mapcar #'register-size unindexed-registers)))
                    (registers
                      (mapcar (lambda (register)
                                (alexandria:if-let ((index (reg-index register)))
                                  (loop :repeat register-size :collect (register-to-quil-object register))
                                  (loop :for i :below register-size
                                        :for reg := (quil:copy-instance register)
                                        :do (setf (reg-index reg) i)
                                        :collect (register-to-quil-object reg))))
                              registers)))
               (apply #'mapcar function registers)))
            (t
             (apply function (mapcar #'register-to-quil-object registers)))))))

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
          ;; TODO Allow application of gates to multiple registers,
          ;; e.g. cx q, r; where q and r can be register arrays.
          ;;
          ;; TODO I think there is an opportunity to delete everything
          ;; except CX and U, since the remaining functions are all
          ;; defined in terms of just those two gates.  Typically how
          ;; this works in OpenQASM is that a program would `include
          ;; "qelib.inc"` which contains QASM definitions of most of
          ;; the stuff below.  That would simplify this a good deal.
          ;; The issue I see is that some of those gates use names
          ;; that are already reserved in Quil (e.g. rx), and so QASM
          ;; gate names would not (could not) be preserved (and would
          ;; require e.g. a prefix "qasm_gate_rx").

          ;; "QE hardware primitives"
          ((member name '("CX" "cx") :test 'string=)
           (check-number-of-parameters params 0)
           (values (apply #'map-registers (lambda (ctl tgt)
                                            (quil::build-gate "CNOT" nil ctl tgt))
                          registers)
                   rest-toks))
          
          ((member name '("U" "u" "u3") :test 'string=)
           (check-number-of-parameters params 3)
           (destructuring-bind (θ ϕ λ) params
             (values (apply #'map-registers (lambda (tgt) (build-u-gate θ ϕ λ tgt))
                            registers) 
                     rest-toks)))
          
          ((member name '("u2") :test 'string=)
           (check-number-of-parameters params 2)
           (destructuring-bind (ϕ λ) params
             (values (apply #'map-registers (lambda (tgt) (build-u-gate quil::pi/2 ϕ λ tgt))
                            registers)
                     rest-toks)))
          
          ((member name '("u1") :test 'string=)
           (check-number-of-parameters params 1)
           (destructuring-bind (λ) params
             (values (apply #'map-registers (lambda (tgt) (build-u-gate 0d0 0d0 λ tgt))
                            registers)
                     rest-toks)))
          
          ((member name '("id") :test 'string=)
           (check-number-of-parameters params 0)
           (values nil
                   ;; Bit redundant.  Maybe just nil.
                   (append (tokenize (format nil "U(0,0,0) ~A;" (print-reg (first registers))))
                           rest-toks)))

          ;; "QE standard gates"
          
          ((member name '("x" "y" "z" "h" "s" "t") :test 'string=)
           (check-number-of-parameters params 0)
           (values (apply #'map-registers (lambda (tgt) (quil::build-gate (string-upcase name) nil tgt))
                          registers)
                   rest-toks))
          
          ((member name '("rx" "ry" "rz") :test 'string=)
           (check-number-of-parameters params 1)
           (values (apply #'map-registers
                          (lambda (tgt) (quil::build-gate (string-upcase name) params tgt))
                          registers)
                   rest-toks))
          
          ((string= name "sdg")
           (check-number-of-parameters params 0)
           (values nil
                   (append (tokenize (format nil "u1(-pi/2) ~A;" (print-reg (first registers))))
                           rest-toks)))
          
          ((string= name "tdg")
           (check-number-of-parameters params 0)
           (values nil
                   (append (tokenize (format nil "u1(-pi/4) ~A;" (print-reg (first registers))))
                           rest-toks)))

          ;; "QE standard user-defined gates"
          
          ((string= name "cz")
           (check-number-of-parameters params 0)
           (values (apply #'map-registers (lambda (ctl tgt)
                                            (quil:build-gate "CZ" nil ctl tgt))
                          registers)
                   rest-toks))
          
          ((string= name "cy")
           (check-number-of-parameters params 0)
           (values (apply #'map-registers
                          (lambda (ctl tgt)
                            (quil:build-gate (quil:controlled-operator (quil:named-operator "Y"))
                                             nil ctl tgt))
                          registers) 
                   rest-toks))
          
          ((string= name "ccx")
           (check-number-of-parameters params 0)
           (values (apply #'map-registers (lambda (ctl1 ctl2 tgt)
                                            (quil:build-gate "CCNOT" nil ctl1 ctl2 tgt))
                          registers)
                   rest-toks))
          
          ((string= name "crx")
           (check-number-of-parameters params 1)
           (values (apply #'map-registers
                          (lambda (ctl tgt)
                            (quil:build-gate (quil:controlled-operator (quil:named-operator "RX"))
                                             params ctl tgt))
                          registers)
                   rest-toks))
          
          ;; CPHASE?

          ((string= name "cu1")
           (check-number-of-parameters params 1)
           (values (apply #'map-registers
                          (lambda (ctl tgt)
                            (list (build-u-gate 0d0 0d0 (quil::param-* (first params) 1/2) ctl)
                                  (quil:build-gate "CNOT" nil ctl tgt)
                                  (build-u-gate 0d0 0d0 (quil::param-* (first params) -1/2) tgt)
                                  (quil:build-gate "CNOT" nil ctl tgt)
                                  (build-u-gate 0d0 0d0 (quil::param-* (first params) 1/2) ctl)))
                          registers)
                   rest-toks))
          
          ((string= name "cu3")
           (check-number-of-parameters params 3)
           (destructuring-bind (θ ϕ λ) params
             (values (apply #'map-registers
                            (lambda (ctl tgt)
                              (list (build-u-gate 0d0 0d0
                                                  (quil::param-* (quil::param-+ λ (quil::param-* ϕ -1))
                                                                 1/2)
                                                  tgt)
                                    (quil:build-gate "CNOT" nil ctl tgt)
                                    (build-u-gate (quil::param-* θ -1/2)
                                                  0d0
                                                  (quil::param-* (quil::param-+ ϕ θ) -1/2)
                                                  tgt)
                                    (quil:build-gate "CNOT" nil ctl tgt)
                                    (build-u-gate (quil::param-* θ 1/2) ϕ 0d0 ctl)))
                            registers)
                     rest-toks)))
          
          (t
           (alexandria:if-let ((gate (gethash name *gate-names*)))
             (values
              (if (eql gate ':opaque)
                  (quil::make-pragma (list "QASM_OPAQUE_APPLICATION" name)
                                     (format nil "(~{~F~^, ~}) ~{~/quil:instruction-fmt/~^, ~}"
                                             (mapcar #'quil:constant-value params)
                                             (mapcar #'register-to-quil-object
                                                     registers)))
                  (apply #'map-registers (lambda (&rest qubits)
                                           (make-instance 'quil:unresolved-application
                                                          :operator (quil:named-operator name)
                                                          :parameters params
                                                          :arguments qubits))
                         registers))
              rest-toks)
             (q2q-parse-error "Found unknown gate application '~A'."
                              name))))))))

(defun parse-gate-body (tok-lines)
  (alexandria:flatten (mapcar #'parse-application (mapcar #'list tok-lines))))

(defun collect-gate-decl (tok-line)
  (let ((name (token-payload (second tok-line))))
    (multiple-value-bind (params rest-toks)
        (maybe-parse-params (subseq tok-line 2))
      (dolist (p params) (push p *gate-params*))
      (let ((qregs (mapcar #'reg-name (parse-qregisters rest-toks))))
        (dolist (q qregs) (push q *gate-qregs*))
        (list name params qregs)))))

(defun line-position-of-token-type (token-type sequence)
  (position token-type sequence
            :key (alexandria:compose #'token-type #'first)))

(defun %formalize (param)
  (quil:param param))

(defun parse-gate-decl (tok-lines)
  (q2q-check-unexpected-eof tok-lines "gate")
  (q2q-check-token-type (first (first tok-lines)) ':GATE)
  
  (let* ((close-pos (line-position-of-token-type ':RIGHT-CURLY-BRACKET tok-lines))
         (*gate-applications-are-formal* t)
         (*gate-params* nil)
         (*gate-qregs* nil))
    (destructuring-bind (gate-name gate-params gate-qargs)
        (collect-gate-decl (first tok-lines))
      ;; TODO Store more info about the gate, for later validating an
      ;; application (num params, qubits, etc).
      (setf (gethash gate-name *gate-names*) t)
      (values (quil::make-circuit-definition
               gate-name
               gate-params
               (mapcar #'quil::formal gate-qargs)
               (parse-gate-body (subseq tok-lines 2 close-pos))
               :context nil)
              (subseq tok-lines (1+ close-pos))))))

(defun parse-opaque (tok-lines)
  (q2q-check-unexpected-eof tok-lines "opaque")
  (q2q-check-token-type (first (first tok-lines)) ':OPAQUE)

  (destructuring-bind ((opaque gate name-tok &rest rest-toks) &rest rest-lines)
      tok-lines
    (declare (ignore opaque gate))
    (multiple-value-bind (params rest-toks)
        (maybe-parse-params rest-toks)
      (let ((*gate-applications-are-formal* t)
            (qregs (parse-qregisters rest-toks)))
        (setf (gethash (token-payload name-tok) *gate-names*) ':opaque)
        (values (quil::make-pragma
                 (list "QASM_OPAQUE_DEFINITION" (token-payload name-tok))
                 (format nil "(~{~A~^, ~}) ~{~/quil:instruction-fmt/~^, ~}"
                         (mapcar #'quil:param-name params)
                         (mapcar #'register-to-quil-object qregs)))
                rest-lines)))))

(defun parse-if (tok-lines)
  (q2q-check-unexpected-eof tok-lines "if")

  (destructuring-bind (if-toks . rest-lines)
      tok-lines
    (pop if-toks)
    (q2q-check-token-type (pop if-toks) ':LEFT-PAREN)
    (multiple-value-bind (register rest-toks)
        (parse-cregister if-toks)
      (q2q-check-token-type (pop rest-toks) ':EQUALSEQUALS)
      (let ((val (pop rest-toks)))
        (q2q-check-token-type val ':NNINTEGER)
        (q2q-check-token-type (pop rest-toks) ':RIGHT-PAREN)
        (multiple-value-bind (gate-application rest-toks)
            (parse-application (list rest-toks))
          (assert (null rest-toks))
          (let* ((cmp-name (string (gensym "CMP-")))
                 (cmp-desc (quil::make-memory-descriptor
                            :name cmp-name
                            :type quil::quil-bit
                            :length 1
                            ;; TODO Figure out what to do here. Ask erik
                            :lexical-context (quil::tok ':name)))
                 (jmp-label (quil::label (string (gensym "JMP-")))))
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
              (loop :for i :below (1+ (ceiling (log (token-payload val) 2)))
                    :for reg := (quil:copy-instance register)
                    :do (setf (reg-index reg) i)
                    :collect
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
          :append (alexandria:flatten
                   (alexandria:ensure-list program-entity)) :do
            (setf tok-lines rest-toks)))

(defun parse-qasm (string)
  "Parse STRING into a raw, untransformed PARSED-PROGRAM object."
  
  (let* ((*creg-names* (make-hash-table :test #'equal))
         (*qreg-names* (make-hash-table :test #'equal))
         (*qubit-count* 0)
         (code (parse-qasm-body string)))
    (setf code (quil::process-includes code))
    ;; Return the parsed sequence of objects.
    (values
     (quil::raw-quil-to-unresolved-program code)
     *creg-names*
     *qreg-names*
     *qubit-count*)))

(defun parse-qasm-string (string &optional originating-file)
  "Parse STRING into a PARSED-PROGRAM object, applying all transforms."
  (declare (ignore originating-file))
  (let ((pp (quil::resolve-objects (parse-qasm string))))
    (setf pp (quil::transform 'quil::expand-circuits pp))
    (setf pp (quil::transform 'quil::type-check pp))
    (setf pp (quil::transform 'quil::simplify-arithmetic pp))))
