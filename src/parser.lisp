;;;; src/parser.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

;;;;;;;;;;;;;;;;;;;;;;;;;; Lexical Analysis ;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype quil-keyword ()
  '(member
    :JUMP :JUMP-WHEN :JUMP-UNLESS :INCLUDE :MEASURE
    :NEG :NOT :AND :IOR :XOR :MOVE :EXCHANGE :CONVERT :ADD :SUB :MUL :DIV
    :LOAD :STORE :EQ :GT :GE :LT :LE :DEFGATE :DEFCIRCUIT :RESET
    :HALT :WAIT :LABEL :NOP :CONTROLLED :DAGGER :DECLARE :SHARING :OFFSET
    :PRAGMA))

(deftype token-type ()
  '(or
    quil-keyword
    (member
     :COLON :LEFT-PAREN :RIGHT-PAREN :COMMA :LABEL-NAME :PARAMETER
     :STRING :INDENTATION :INDENT :DEDENT :COMPLEX :PLUS
     :MINUS :TIMES :DIVIDE :EXPT :INTEGER :NAME :AREF)))

(defvar *line-start-position* nil)
(defvar *line-number* nil)

(defstruct (token (:constructor tok (type &optional payload (line *line-number*))))
  "A lexical token."
  (line nil :type (or null (integer 1)))
  (type nil :type token-type)
  (payload nil))

(defmethod print-object ((obj token) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~A~@[*~*~]~@[@L~D~]"
            (token-type obj)
            (token-payload obj)
            (token-line obj))))

(defun parse-indent-string (str)
  "Count the number of tabs/4-space indentations of the string STR."
  (let* ((total-length (length str))
         (tabs (count #\Tab str))
         (spaces (- total-length tabs)))
    (assert (zerop (mod spaces 4)))
    (+ tabs (floor spaces 4))))

(defun parse-complex (real imag)
  "Parse the complex number with real and imaginary components REAL, IMAG represented as NIL or strings."
  (check-type real (or null string))
  (check-type imag (or null string))
  (flet ((parse-number (thing)
           (declare (type (or null string) thing))
           (if (null thing)
               0.0d0
               (parse-float thing :type 'double-float))))
    (declare (inline parse-number))
    (let ((re (parse-number real))
          (im (parse-number imag)))
      (declare (type double-float re im))
      (if (zerop im)
          re
          (complex re im)))))

(alexa:define-string-lexer line-lexer
  "A lexical analyzer for lines of Quil."
  ((:int    "\\d+")
   (:float  "(?=\\d*[.eE])(?=\\.?\\d)\\d*\\.?\\d*(?:[eE][+-]?\\d+)?")
   (:ident  "[A-Za-z_](?:[A-Za-z0-9_\\-]*[A-Za-z0-9_])?")
   (:string "\\\"(?:[^\\\"]|\\\\\\\")*\\\"")
   (:newline "(?:\\r\\n?|\\n)"))
  ((eager "\\#[^\\n\\r]*")
   nil)
  ((eager "{{NEWLINE}}")
   ;; We return a keyword because they get split out and
   ;; removed. They're not actually logical tokens of the Quil
   ;; language.
   (incf *line-number*)
   (setf *line-start-position* $>)
   (return ':NEWLINE))
  ((eager "\\:")
   (return (tok :COLON)))
  ((eager "\\(")
   (return (tok :LEFT-PAREN)))
  ((eager "\\)")
   (return (tok :RIGHT-PAREN)))
  ((eager "\\,")
   (return (tok :COMMA)))
  ((eager "\\@({{IDENT}})")
   (assert (not (null $1)))
   (return (tok :LABEL-NAME (label $1))))
  ((eager "\\%({{IDENT}})")
   (assert (not (null $1)))
   (return (tok :PARAMETER (param $1))))
  ((eager "{{STRING}}")
   (return (tok :STRING (read-from-string $@))))
  ((eager #.(string #\DAGGER))
   (return (tok ':DAGGER)))
  ((eager #.(string #\HELM_SYMBOL))
   (return (tok ':CONTROLLED)))
  ("INCLUDE|DEFCIRCUIT|DEFGATE|MEASURE|LABEL|WAIT|NOP|HALT|RESET|JUMP\\-WHEN|JUMP\\-UNLESS|JUMP|PRAGMA|NOT|AND|IOR|MOVE|EXCHANGE|SHARING|DECLARE|OFFSET|XOR|NEG|LOAD|STORE|CONVERT|ADD|SUB|MUL|DIV|EQ|GT|GE|LT|LE|CONTROLLED|DAGGER"
   (return (tok (intern $@ :keyword))))
  ((eager "(?<NAME>{{IDENT}})\\[(?<OFFSET>{{INT}})\\]")
   (assert (not (null $NAME)))
   (assert (not (null $OFFSET)))
   (return (tok :AREF (cons $NAME (parse-integer $OFFSET)))))
  ((eager "\\[{{INT}}\\]")
   (quil-parse-error "Old-style classical memory syntax, like ~A, isn't supported." $@))
  ("(?<NUM>{{FLOAT}})(?<IMAG>i)?"
   (return
     (if $IMAG
         (tok :COMPLEX (constant (parse-complex nil $NUM)))
         (tok :COMPLEX (constant (parse-complex $NUM nil))))))
  ("\\+" (return (tok :PLUS)))
  ("\\-" (return (tok :MINUS)))
  ("\\*" (return (tok :TIMES)))
  ("\\/" (return (tok :DIVIDE)))
  ("\\^" (return (tok :EXPT)))
  ("{{INT}}"
   (return (tok :INTEGER (parse-integer $@))))
  ("{{IDENT}}"
   (return
     (cond
       ((string= "pi" $@) (tok :COMPLEX (constant pi)))
       ((string= "i" $@)  (tok :COMPLEX (constant #C(0.0d0 1.0d0))))
       (t                 (tok :NAME $@)))))
  ((eager "(?:    |\\t)+")
   (when (= *line-start-position* $<)
     (return (tok :INDENTATION (parse-indent-string $@)))))
  ;; Non-newline whitespace. In newer Perl, you can just use \h, for
  ;; "horizontal space", but this isn't supported in CL-PPCRE.
  ("[^\\S\\n\\r]+"
   nil))

(defun tokenize-line (lexer string)
  "Given a lexer (as defined by DEFINE-STRING-LEXER) and a string, return a list of tokens represented by that string."
  (let ((f (funcall lexer string))
        (*line-number* 1)
        (*line-start-position* 0))
    (loop :for tok := (funcall f)
          :until (null tok)
          :collect tok)))

(defun process-indentation (toks)
  "Given a list of token lines TOKS, map all changes to :INDENTATION levels to :INDENT and :DEDENT tokens."
  (let ((indent (tok :INDENT))
        (dedent (tok :DEDENT)))
    (labels ((indent (delta-level)
               (cond
                 ((zerop delta-level) nil)
                 ((plusp delta-level) (make-list delta-level :initial-element indent))
                 ((minusp delta-level) (make-list (abs delta-level) :initial-element dedent)))))
      (loop :with new-toks := nil
            :with level := 0
            :for tok-line :in toks
            :for type := (token-type (first tok-line))
            :for payload := (token-payload (first tok-line))
            :do (case type
                  ((:INDENTATION)
                   (let ((delta (- payload level)))
                     (unless (endp (rest tok-line))
                       (push (append (indent delta) (rest tok-line)) new-toks)
                       (setf level payload))))
                  (otherwise
                   (push tok-line new-toks)))
            :finally (return (nreverse new-toks))))))

(defun ensure-indentation (line)
  (cond
    ((endp line)                                  (list (tok ':INDENTATION 0)))
    ((eq ':INDENTATION (token-type (first line))) line)
    (t                                            (cons (tok ':INDENTATION 0) line))))

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

(defun tokenize (string)
  "Tokenize the Quil string STRING into a list of token lines with indentation resolved. Each token line is itself a list of tokens."
  (let* ((lines (nsplit ':NEWLINE (tokenize-line 'line-lexer string))))
    (map-into lines #'ensure-indentation lines)
    (process-indentation lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition quil-parse-error (alexandria:simple-parse-error)
  ()
  (:documentation "Representation of an error parsing Quil."))

(define-condition quil-parse-error-at-line (quil-parse-error)
  ((line :initarg :line :reader line-of-quil-parse-error))
  (:report (lambda (condition stream)
             (format stream "At line ~D: "
                     (line-of-quil-parse-error condition))
             (apply #'format stream (simple-condition-format-control condition)
                    (simple-condition-format-arguments condition)))))

(defun quil-parse-error (format-control &rest format-args)
  "Signal a QUIL-PARSE-ERROR with a descriptive error message described by FORMAT-CONTROL and FORMAT-ARGS."
  ;; There are callers of this function where *line-number* is not set
  ;; appropriately (e.g. in expand-circuits.lisp) and where it may be impossible
  ;; to set due to missing context. Most callers of this function however are
  ;; right here in parser.lisp where *line-number* is more than likely set.
  (if *line-number*
      (error 'quil-parse-error-at-line :line *line-number*
                                       :format-control format-control
                                       :format-arguments format-args)
      (error 'quil-parse-error :format-control format-control
                               :format-arguments format-args)))

(defvar *definitions-allowed* t
  "Dynamic variable to control whether DEF* forms are allowed in the current parsing context.")

(defvar *formal-arguments-allowed* nil
  "Dynamic variable to control whether formal parameters and arguments are allowed in the current parsing context.")

(defun parse-program-lines (tok-lines)
  "Parse the next AST object from the list of token lists. Returns two values:

1. The next AST object.
2. A list of lines that remain unparsed.
"
  (let* ((line (first tok-lines))
         (tok (first line))
         (tok-type (token-type tok))
         (*line-number* (token-line tok)))
    (case tok-type
      ;; Gate/Circuit Applications
      ((:CONTROLLED :DAGGER)
       (parse-modified-application tok-lines))

      ((:NAME)
       (parse-application tok-lines))

      ;; Circuit Definition
      ((:DEFCIRCUIT)
       (unless *definitions-allowed*
         (quil-parse-error "Found DEFCIRCUIT where it's not allowed."))

       (let ((*definitions-allowed* nil)
             (*formal-arguments-allowed* t))
         (parse-circuit-definition tok-lines)))

      ;; Gate Definition
      ((:DEFGATE)
       (unless *definitions-allowed*
         (quil-parse-error "Found DEFGATE where it's not allowed."))

       (let ((*definitions-allowed* nil)
             (*formal-arguments-allowed* t))
         (parse-gate-definition tok-lines)))

      ;; Memory Declaration
      ((:DECLARE)
       (unless *definitions-allowed*
         (quil-parse-error "Found DECLARE where it's not allowed."))

       (let ((*formal-arguments-allowed* t))
         (parse-memory-descriptor tok-lines)))

      ;; Pragma
      ((:PRAGMA)
       (parse-pragma tok-lines))

      ;; Measurement
      ((:MEASURE)
       (parse-measurement tok-lines))

      ;; Halt, No-op, Wait
      ((:HALT :NOP :WAIT)
       (parse-simple-instruction (token-type tok) tok-lines))

      ((:RESET)
       (parse-reset-instruction tok-lines))

      ;; Include
      ((:INCLUDE)
       (parse-include tok-lines))

      ;; Label
      ((:LABEL)
       (parse-label tok-lines))

      ;; Jump
      ((:JUMP)
       (parse-jump tok-lines))

      ;; Conditional Jump
      ((:JUMP-WHEN :JUMP-UNLESS)
       (parse-conditional-jump tok-type tok-lines))

      ;; Unary classical function
      ((:NEG :NOT)
       (parse-unary-classical tok-type tok-lines))

      ;; Binary classical function
      ((:AND :IOR :XOR :MOVE :EXCHANGE :CONVERT :ADD :SUB :MUL :DIV)
       (parse-binary-classical tok-type tok-lines))

      ;; Trinary classical function
      ((:LOAD :STORE :EQ :GT :GE :LT :LE)
       (parse-trinary-classical tok-type tok-lines))

      (otherwise
       (quil-parse-error "Got an unexpected token of type ~S ~
                          when trying to parse a program." tok-type)))))

(defun parse-simple-instruction (type tok-lines)
  ;; Check that we have a line with exactly one token.
  (when (or (null tok-lines)
            (/= 1 (length (first tok-lines))))
    (quil-parse-error "Invalid line format for ~S" type))
  (let ((tok (first (first tok-lines))))
    ;; Check that we actually have the token we expect.
    (unless (eql type (token-type tok))
      (quil-parse-error "Not a ~S token" type))
    ;; Return the objects.
    (values (ecase (token-type tok)
              ((:HALT)  (make-instance 'halt))
              ((:RESET) (make-instance 'reset))
              ((:NOP)   (make-instance 'no-operation))
              ((:WAIT)  (make-instance 'wait)))
            (rest tok-lines))))

(defun parse-reset-instruction (tok-lines)
  "Parse either a 'RESET' or 'RESET <q>' instruction where <q> is a :NAME or :INTEGER into a RESET or RESET-QUBIT instruction, respectively."
  (let ((reset-line (first tok-lines))
        (rest-lines (rest tok-lines)))
    ;; Check that we have a line with either exactly one (full reset) or exactly two (targeted reset) tokens.
    (when (or (null tok-lines)
              (not (<= 1 (length reset-line) 2)))
      (quil-parse-error "Invalid line format for RESET"))
    (let ((reset-tok (first reset-line))
          (target-tok (second reset-line)))

      ;; Check that we actually have the token we expect.
      (unless (eql :RESET (token-type reset-tok))
        (quil-parse-error "Expected a :RESET token."))

      (if (null target-tok)
          ;; global RESET
          (values
           (make-instance 'reset)
           rest-lines)

          ;; targeted RESET
          (values
           (make-instance
            'reset-qubit
            ;; target can be explicit qubit index or name (if inside DEFCIRCUIT)
            :target (ecase (token-type target-tok)
                      ((:NAME)
                       (unless *formal-arguments-allowed*
                         (quil-parse-error "Found formal parameter in RESET where not allowed."))
                       (formal (token-payload target-tok)))
                      ((:INTEGER) (qubit (token-payload target-tok)))))
           rest-lines)))))

(defun take-until (f list)
  "Take elements of LIST until the boolean function F is satisfied. More specifically, split LIST into two lists A and B (as two values) such that

    * (append A B) is equal to LIST

    * (notany f A) => T

    * B is NIL, or (funcall f (first B)) => T.
"
  (let ((head nil))
    (loop :named driver :do
      (cond
        ((or (endp list)
             (funcall f (first list)))
         (return-from driver (values (nreverse head) list)))
        (t
         (push (pop list) head))))))

(defun take-while-from-end (f list)
  "Same as TAKE-WHILE, but operates starting at the end of the list."
  (let ((p (position-if f list :from-end t)))
    (if (null p)
        (values list nil)
        (values (subseq list 0 p)
                (subseq list p)))))

(defun parse-parameter (arg-tok)
  "Parse a token/number/parameter ARG-TOK intended to be a parameter."
  (typecase arg-tok
    (number (constant arg-tok))
    (param
     (unless *formal-arguments-allowed*
       (quil-parse-error "Found formal parameter where not allowed: ~S"
                         arg-tok))
     arg-tok)
    (token
     (let ((type (token-type arg-tok)))
       (case type
         ((:PARAMETER)
          (unless *formal-arguments-allowed*
            (quil-parse-error "Found formal parameter where not allowed: ~S"
                              (token-payload arg-tok)))
          (token-payload arg-tok))
         ((:COMPLEX)
          (token-payload arg-tok))
         ((:INTEGER)
          (constant (coerce (token-payload arg-tok) 'double-float)))
         (otherwise
          (quil-parse-error "Token doesn't represent a parameter where expected: ~S" arg-tok)))))
    (t (quil-parse-error "Unexpected token type ~S: ~S"
                         (type-of arg-tok)
                         arg-tok))))

(defun parse-argument (arg-tok)
  "Parse a token ARG-TOK intended to be an argument."
  (declare (special *memory-region-names*)) ; Forward declaration
  (case (token-type arg-tok)
    ((:NAME)
     (let ((names-memory-region-p (find (token-payload arg-tok) *memory-region-names*
                                        :test #'string=)))
       (unless (or names-memory-region-p
                   *formal-arguments-allowed*)
         (quil-parse-error "Found formal argument where not allowed: ~S"
                           (token-payload arg-tok)))
       (if names-memory-region-p
           (mref (token-payload arg-tok) 0)
           (formal (token-payload arg-tok)))))
    ((:INTEGER)
     (qubit (token-payload arg-tok)))
    ((:COMPLEX)
     (token-payload arg-tok))
    ((:AREF)
     (mref (car (token-payload arg-tok))
           (cdr (token-payload arg-tok))))
    (otherwise
     (quil-parse-error "Token doesn't represent an argument where expected: ~S" arg-tok))))

;; Dear reader:
;;
;;     I must apologize to you in advance for a petty crime I've
;; committed. The following special variables make use of "spooky action at a
;; distance".
;;
;;  * *arithmetic-parameters* is used in the arithmetic parser to collect
;;    parameters encountered while parsing, and also act as a map from those
;;    parameters to symbols generated.
;;
;;  * *segment-encountered* is used to track whether an expression contains
;;    compile-time-unevaluable classical memory references.
;;
;;  * *memory-region-names* is used to store a list of allowable memory region
;;    names, to distinguish references to them from other parameter types.
;;
;; Sorry.
;;
;;                                   Regards,
;;
;;                                   Robert (& Eric)
(defvar *arithmetic-parameters*)
(setf (documentation '*arithmetic-parameters* 'variable)
      "A special variable to detect and collect the parameters found in an arithmetic expression when parsing. An alist mapping formal parameters to generated symbols.")

(defvar *segment-encountered*)
(setf (documentation '*segment-encountered* 'variable)
      "A special variable to detect the presence of a segment address found in an arithmetic expression when parsing.  A simple boolean.")

(defvar *memory-region-names*)
(setf (documentation '*memory-region-names* 'variable)
      "A special variable to collect the names of declared memory regions.")

(defun gate-modifier-token-p (tok)
  (member (token-type tok) '(:CONTROLLED :DAGGER)))

(defun apply-modifiers-to-operator (processed-modifiers base-operator)
  (if (endp processed-modifiers)
      base-operator
      (destructuring-bind (modifier-name &rest modifier-args)
          (first processed-modifiers)
        (declare (ignore modifier-args))
        (ecase modifier-name
          (:CONTROLLED
           (apply-modifiers-to-operator (rest processed-modifiers)
                                        (controlled-operator base-operator)))
          (:DAGGER
           (apply-modifiers-to-operator (rest processed-modifiers)
                                        (involutive-dagger-operator base-operator)))))))

(defun parse-modified-application (tok-lines)
  (multiple-value-bind (modifiers rest-line)
      (take-until (complement #'gate-modifier-token-p) (pop tok-lines))
    (multiple-value-bind (app rest-lines)
        (parse-application (cons rest-line tok-lines))
      ;; It will be an unresolved application, but that's OK. We can
      ;; still parse it out.
      (loop :for modifier :in modifiers
            :collect (ecase (token-type modifier)
                       (:CONTROLLED (list ':CONTROLLED))
                       (:DAGGER     (list ':DAGGER)))
              :into processed-modifiers
            :finally (progn
                       (setf (application-operator app)
                             (apply-modifiers-to-operator
                              processed-modifiers
                              (application-operator app)))
                       (return (values app rest-lines)))))))

(defun parse-application (tok-lines)
  "Parse a gate or circuit application out of the lines of tokens TOK-LINES, returning an UNRESOLVED-APPLICATION."
  (let ((line (pop tok-lines))
        params)
    (destructuring-bind (op . args) line
      ;; Check that we are starting out with a :NAME, representing the
      ;; name of the gate or circuit.
      (unless (eql ':NAME (token-type op))
        (quil-parse-error "Gate/circuit application needs a name. Got ~S"
                          (token-type op)))

      ;; Check for anything following the name.
      (when (endp args)
        ;; This is almost surely a circuit application.
        (return-from parse-application
          (values (make-instance 'unresolved-application
                                 :operator (named-operator (token-payload op)))
                  tok-lines)))

      ;; Check for parenthesis in first position to indicate
      ;; parameters.
      (when (eql ':LEFT-PAREN (token-type (first args)))
        ;; Remove :LEFT-PAREN
        (pop args)
        ;; Time to parse out everything in between the parentheses...
        (multiple-value-bind (found-params rest-line)
            ;; ... but first we have to find the right paren.
            (take-while-from-end (lambda (x) (eql ':RIGHT-PAREN (token-type x)))
                                 args)

          ;; But if we didn't find it, then we have an unmatched
          ;; parenthesis.
          (when (endp rest-line)
            (quil-parse-error "No matching right parenthesis in the ~
                               application ~S."
                              (token-payload op)))

          ;; If we did find it, pop it off from where the arguments
          ;; will lie.
          (pop rest-line)
          (setf args rest-line)

          ;; Parse the params. Separate them by comma, evaluate them
          ;; if necessary.
          (setf params (let ((entries (split-sequence:split-sequence-if
                                        (lambda (tok)
                                          (eq ':comma (token-type tok)))
                                        found-params)))
                         (mapcar (lambda (toks)
                                   (let ((*arithmetic-parameters* nil)
                                         (*segment-encountered* nil))
                                     (let ((result (parse-arithmetic-tokens toks :eval t)))
                                       (cond
                                         ((and (null *arithmetic-parameters*)
                                               (null *segment-encountered*))
                                          (cond
                                            ((numberp result)
                                             (constant result))
                                            ((typep result 'memory-ref)
                                             result)
                                            (t
                                             (quil-parse-error "A number was expected from arithmetic evaluation. Got something of type ~S."
                                                               (type-of result)))))

                                         ((or *formal-arguments-allowed*
                                              (and *segment-encountered*
                                                   (null *arithmetic-parameters*)))
                                          (if (and (= 1 (length toks))
                                                   (eql ':PARAMETER (token-type (first toks)))
                                                   (= 1 (length *arithmetic-parameters*)))
                                              (token-payload (first toks))
                                              (make-delayed-expression
                                               (mapcar #'first *arithmetic-parameters*)
                                               (mapcar #'second *arithmetic-parameters*)
                                               result)))

                                         (t
                                          (quil-parse-error "Formal parameters found in a place they're not allowed."))))))
                                 entries)))))

      ;; Parse out the rest of the arguments and return.
      (return-from parse-application
        (values
         (make-instance 'unresolved-application
                        :operator (named-operator (token-payload op))
                        :parameters params
                        :arguments (mapcar #'parse-argument args))
         tok-lines)))))

(defun parse-measurement (tok-lines)
  "Parse a measurement out of the lines of tokens TOK-LINES."
  ;; Make sure we have a line to parse.
  (when (null tok-lines)
    (quil-parse-error "Unexpectedly reached end of program, expecting MEASURE."))

  ;; Next we need to discriminate between
  ;;
  ;;    MEASURE x
  ;;
  ;; and
  ;;
  ;;    MEASURE x y
  (destructuring-bind (measure-line . rest-lines) tok-lines
    ;; Entire line must be two or three tokens long.
    (unless (or (= 2 (length measure-line))
                (= 3 (length measure-line)))
      (quil-parse-error "Malformed MEASURE line."))

    ;; Now we can destructure it.
    (destructuring-bind (measure-tok qubit &optional address)
        measure-line

      ;; First token must obviously be MEASURE.
      (unless (eql ':MEASURE (token-type measure-tok))
        (quil-parse-error "Expected MEASURE token, got ~S" (token-type measure-tok)))

      ;; Convert token to either a formal or a QUBIT object.
      (case (token-type qubit)
        ((:INTEGER)
         (setf qubit (qubit (token-payload qubit))))
        ((:NAME)
         (unless *formal-arguments-allowed*
           (quil-parse-error "Found a formal argument where it's not allowed."))
         (setf qubit (formal (token-payload qubit))))
        (t
         (quil-parse-error "Expected qubit after MEASURE.")))

      ;; Parse the address.
      (cond
        ;; No address provided.
        ((null address)
         (values (make-instance 'measure-discard :qubit qubit)
                 rest-lines))

        ;; Actual address to measure into.
        ((eql ':AREF (token-type address))
         (unless (find (car (token-payload address)) *memory-region-names* :test #'string=)
           (quil-parse-error "Bad memory region name ~a in MEASURE instruction" (car (token-payload address))))
         (values (make-instance 'measure
                                :qubit qubit
                                :address (mref (car (token-payload address))
                                               (cdr (token-payload address))))
                 rest-lines))

        ;; Implicit address to measure into.
        ((and (eql ':NAME (token-type address))
              (find (token-payload address) *memory-region-names* :test #'string=))

         (values (make-instance 'measure
                                :qubit qubit
                                :address (mref (token-payload address) 0))
                 rest-lines))

        ;; Formal argument.
        ((eql ':NAME (token-type address))
         (unless *formal-arguments-allowed*
           (quil-parse-error "Found formal argument where it's not allowed."))

         (values (make-instance 'measure
                                :qubit qubit
                                :address (formal (token-payload address)))
                 rest-lines))

        (t
         (quil-parse-error "Expected address after MEASURE"))))))

(defun parse-pragma (tok-lines)
  "Parse a PRAGMA out of the lines of tokens TOK-LINES."
  ;; Check that we have something to parse.
  (when (null tok-lines)
    (quil-parse-error "Unexpectedly reached end of program, expecting PRAGMA."))

  (let ((pragma-line (first tok-lines)))
    ;; Check that our line are at least two tokens, INCLUDE and
    ;; whatever follows.
    (unless (<= 2 (length pragma-line))
      (quil-parse-error "PRAGMA line must have at least one identifier"))

    ;; Destructure it into components.
    (destructuring-bind (pragma-tok &rest word-toks) pragma-line
      ;; Check that it's actually a PRAGMA instruction.
      (unless (eql ':PRAGMA (token-type pragma-tok))
        (quil-parse-error "Unexpected token ~S, expected PRAGMA" (token-type pragma-tok)))
      (unless (eql ':NAME (token-type (car word-toks)))
        (quil-parse-error "Unexpected token ~S, expected identifier after PRAGMA." (token-type (car word-toks))))
      (multiple-value-bind (words non-words)
          (take-until (lambda (tok) (not (member (token-type tok) '(:NAME :INTEGER)))) word-toks)
        (setf words (mapcar #'token-payload words))

        (cond
          ((null non-words)
           (values (make-pragma words)
                   (rest tok-lines)))

          ((endp (cdr non-words))
           (let ((last-tok (first non-words)))
             (unless (eql ':STRING (token-type last-tok))
               (quil-parse-error "Expected string at end of PRAGMA (or nothing), got ~S."
                                 (token-type last-tok)))
             (values (make-pragma words (token-payload last-tok))
                     (rest tok-lines))))

          (t
           (quil-parse-error "Unexpected tokens near the end of a PRAGMA.")))))))

(defun parse-include (tok-lines)
  "Parse an INCLUDE out of the lines of tokens TOK-LINES."
  ;; Check that we have something to parse.
  (when (null tok-lines)
    (quil-parse-error "Unexpectedly reached end of program, expecting INCLUDE."))

  (let ((include-line (first tok-lines)))
    ;; Check that our line is exactly two tokens for INCLUDE str
    (unless (= 2 (length include-line))
      (quil-parse-error "Unexpected tokens in INCLUDE line"))

    ;; Destructure it into components.
    (destructuring-bind (include-tok path-tok) include-line
      ;; Check that it's actually an INCLUDE instruction.
      (unless (eql ':INCLUDE (token-type include-tok))
        (quil-parse-error "Unexpected token ~S, expected INCLUDE" (token-type include-tok)))

      ;; Check that we are including a string (though we don't check if it's a valid path yet).
      (unless (eql ':STRING (token-type path-tok))
        (quil-parse-error "Expecting a pathname string to INCLUDE"))

      ;; Return the object.
      (values
       (make-instance 'include :pathname (token-payload path-tok))
       (rest tok-lines)))))

(defun validate-gate-matrix-size (gate-name num-entries)
  "For the gate named GATE-NAME, check that NUM-ENTRIES is a valid number of entries for a gate matrix."
  (flet ((perfect-square-p (n)
           (= n (expt (isqrt n) 2)))
         (power-of-two-p (n)
           (zerop (logand n (1- n)))))
    (unless (<= 4 num-entries)
      (quil-parse-error "There must be at least 4 entries (a one-qubit ~
                         operator) for the gate ~A being defined. Got ~D."
                        gate-name
                        num-entries))
    (unless (perfect-square-p num-entries)
      (quil-parse-error "The number of entries of the gate ~A being defined ~
                         must be a perfect square (i.e., it must be a square ~
                         matrix. I got ~D entries."
                        gate-name
                        num-entries))
    (unless (power-of-two-p (isqrt num-entries))
      (quil-parse-error "The gate ~A being defined isn't a square 2^n x 2^n ~
                         matrix. In particular, it is a ~D x ~D matrix."
                        gate-name
                        (isqrt num-entries)
                        (isqrt num-entries)))
    t))

(defun parse-gate-definition (tok-lines)
  "Parse a gate definition from the token lines TOK-LINES."
  ;; Check that we have tokens left
  (when (null tok-lines)
    (quil-parse-error "EOF reached when gate definition expected"))

  ;; Get the parameter and body lines
  (let (name params)
    (destructuring-bind (parameter-line &rest body-lines) tok-lines
      (destructuring-bind (op . params-args) parameter-line
        ;; Check that we are dealing with a DEFGATE.
        (unless (eql ':DEFGATE (token-type op))
          (quil-parse-error "DEFGATE expected. Got ~S"
                            (token-type op)))

        ;; Check that something is following the DEFGATE.
        (when (null params-args)
          (quil-parse-error "Expected more after DEFGATE token"))

        ;; Check for a name.
        (unless (eql ':NAME (token-type (first params-args)))
          (quil-parse-error "Expected a name for the DEFGATE"))

        ;; We have a name. Stash it away.
        (setf name (token-payload (pop params-args)))

        ;; Check for parenthesis in first position to indicate
        ;; parameters.
        (when (eql ':LEFT-PAREN (token-type (first params-args)))
          ;; Remove :LEFT-PAREN
          (pop params-args)

          ;; Parse out until the right paren.
          (multiple-value-bind (found-params rest-line)
              (take-until (lambda (x) (eql ':RIGHT-PAREN (token-type x)))
                          params-args)
            ;; ... or error if it doesn't exist.
            (when (null rest-line)
              (quil-parse-error "No matching right paren in DEFGATE params"))

            ;; Remove right paren.
            (pop rest-line)

            ;; Some sanity checks for the parameter list. Must be of odd length,
            ;; and every other token should be a :COMMA
            (unless (and (oddp (length found-params))
                         (loop :for c :in (rest found-params) :by #'cddr
                               :always (eql ':COMMA (token-type c))))
              ;; TODO Some printer for tokens?
              (quil-parse-error "Malformed parameter list in DEFGATE: ~A" found-params))
            ;; Go through the supposed parameters, checking that they
            ;; are, and parsing them out.
            (setf params (loop :for p :in (remove ':comma found-params :key #'token-type)
                               :when (not (eql ':PARAMETER (token-type p)))
                                 :do (quil-parse-error "Found something other than a parameter in a DEFGATE parameter list: ~A" p)
                               :collect (parse-parameter p)))

            ;; Ensure the colon (and nothing else) is there.
            (unless (and (= 1 (length rest-line))
                         (eql ':COLON (token-type (first rest-line))))
              (quil-parse-error "Expected a colon in DEFGATE line")))))

      ;; Parse out the gate entries.
      (let ((*arithmetic-parameters* nil)
            (*segment-encountered* nil))
        (multiple-value-bind (parsed-entries rest-lines)
            (parse-gate-entries body-lines)
          ;; Check that we only refered to parameters in our param list.
          (loop :for body-p :in (mapcar #'first *arithmetic-parameters*)
                :unless (find (param-name body-p) params :key #'param-name
                                                         :test #'string=)
                  :do (quil-parse-error
                       "The parameter ~A was found in the body of the gate definition of ~
                        ~A but wasn't in the declared parameter list."
                       (param-name body-p)
                       name))

          ;; Validate the number of gate entries.
          (validate-gate-matrix-size name (length parsed-entries))

          (let ((param-symbols
                  ;; Make sure we have symbols for everything. Collect
                  ;; a list of them in the same order as PARAMS.
                  (loop :for p :in params
                        :for found-p := (assoc (param-name p) *arithmetic-parameters*
                                               :key #'param-name
                                               :test #'string=)
                        :if (null found-p)
                          :collect (gensym (concatenate 'string (param-name p) "-UNUSED"))
                        :else
                          :collect (second found-p))))

            ;; Return the gate definition.
            (values (make-gate-definition name param-symbols parsed-entries)
                    rest-lines)))))))

(defun parse-arithmetic-entries (entries)
  "Given a list of entries, each of which is a list of tokens, return the entries evaluated, unless there are formal parameters. Return the simplified entries.
"

  (labels ((simplify (entry)
             (parse-arithmetic-tokens entry :eval t)))
    (let ((simple-entries (mapcar #'simplify entries)))
      simple-entries)))

(defun parse-gate-entries (tok-lines)
  ;; Do we have any lines to parse?
  (when (null tok-lines)
    (warn "End of program each when indented gate entries was expected.")
    (return-from parse-gate-entries (values nil nil)))

  ;; Check for indentation.
  (multiple-value-bind (indented? modified-line)
      (indented-line (first tok-lines))
    ;; Is the first line indented?
    (unless indented?
      (warn "Expected indented gate entries but alas, they weren't found.")
      (return-from parse-gate-entries
        (values nil tok-lines)))
    ;; Remove the indentation from the top line.
    (setf tok-lines (cons modified-line (rest tok-lines))))

  ;; Parse until we reach a :DEDENT token.
  ;;
  ;; TODO: check the second value of PARSE-ARITHMETIC-ENTRIES. We want
  ;; to return this upward so that we can check for free variables in
  ;; the gate definition.
  (loop :with gate-entries := nil :do
    ;; Reached EOF
    (when (null tok-lines)
      (return-from parse-gate-entries
        (values (parse-arithmetic-entries gate-entries)
                tok-lines)))

    ;; Check for :DEDENT
    (multiple-value-bind (dedented? modified-line)
        (dedented-line-p (first tok-lines))
      (cond
        ;; DEDENT found, return.
        (dedented?
         (let ((new-lines (cons modified-line
                                (rest tok-lines))))
           (return-from parse-gate-entries
             (values (parse-arithmetic-entries gate-entries)
                     new-lines))))
        ;; DEDENT not found, continue parsing entries.
        (t
         (multiple-value-bind (parsed rest-lines)
             (parse-gate-line tok-lines)
           (setf gate-entries (append gate-entries parsed))
           (setf tok-lines rest-lines)))))))

(defun split-by (f list)
  (labels ((rec (list parts)
             (if (null list)
                 (nreverse parts)
                 (multiple-value-bind (left right)
                     (take-until f list)
                   (rec (rest right) (cons left parts))))))
    (rec list nil)))

(defun parse-gate-line (tok-lines)
  (values
   (split-by (lambda (tok)
               (eql ':COMMA (token-type tok)))
             (first tok-lines))
   (rest tok-lines)))

(let ((counter 0))
  (defun genlabel (&optional (name "L"))
    "Generate a label derived from NAME which is internally unique."
    (check-type name string)
    ;; The @ symbol is invalid in normal syntax, but we've already
    ;; parsed everything out, so we use it in order to provide a
    ;; unique name.
    (label (format nil "__~A_~D" name (incf counter)))))

(defun parse-circuit-definition (tok-lines)
  "Parse out a circuit definition from the lines of tokens TOK-LINES."

  ;; Check that we have tokens left to parse.
  (when (null tok-lines)
    (quil-parse-error "EOF reached when circuit definition expected"))

  (let (name params args)
    ;; Split off the header line from the body lines.
    (destructuring-bind (parameter-line &rest body-lines) tok-lines
      ;; Check that we have a well-formed header line.
      (destructuring-bind (op . params-args) parameter-line
        ;; We must be working with a DEFCIRCUIT.
        (unless (eql ':DEFCIRCUIT (token-type op))
          (quil-parse-error "DEFCIRCUIT expected. Got ~S"
                            (token-type op)))

        ;; Check that there are tokens following DEFCIRCUIT.
        (when (null params-args)
          (quil-parse-error "Expected more after DEFCIRCUIT token"))

        ;; Check for name.
        (unless (eql ':NAME (token-type (first params-args)))
          (quil-parse-error "Expected a name for the DEFCIRCUIT"))

        ;; Stash it away.
        (setf name (token-payload (pop params-args)))

        ;; Check for parenthesis in first position to indicate
        ;; parameters.
        (when (eql ':LEFT-PAREN (token-type (first params-args)))
          ;; Remove :LEFT-PAREN
          (pop params-args)

          ;; Parse out the parameters enclosed.
          (multiple-value-bind (found-params rest-line)
              (take-until (lambda (x) (eql ':RIGHT-PAREN (token-type x)))
                          params-args)

            ;; Error if we didn't find a right parenthesis.
            (when (null rest-line)
              (quil-parse-error "No matching right paren in DEFCIRCUIT params"))

            ;; Remove right paren and stash away params.
            (pop rest-line)

            ;; Some sanity checks for the parameter list. Must be of odd length,
            ;; and every other token should be a :COMMA
            (unless (and (oddp (length found-params))
                         (loop :for c :in (rest found-params) :by #'cddr
                               :always (eql ':COMMA (token-type c))))
              ;; TODO Some printer for tokens?
              (quil-parse-error "Malformed parameter list in DEFCIRCUIT: ~A" found-params))
            ;; Parse out the parameters.
            (setf params (loop :for p :in (remove ':COMMA found-params :key #'token-type)
                               :when (not (eql ':PARAMETER (token-type p)))
                                 :do (quil-parse-error "Found something other than a parameter in a DEFCIRCUIT parameter list: ~A" p)
                               :collect (parse-parameter p))
                  params-args rest-line)))

        ;; Check for colon and incise it.
        (let ((maybe-colon (last params-args)))
          (when (or (null maybe-colon)
                    (not (eql ':COLON (token-type (first maybe-colon)))))
            (quil-parse-error "Expected a colon in DEFCIRCUIT"))
          (setf params-args (butlast params-args)))

        ;; Collect arguments and stash them away.
        (loop :for arg :in params-args
              :when (not (eql ':NAME (token-type arg)))
                :do (quil-parse-error "Invalid formal argument in DEFCIRCUIT line.")
              :collect (parse-argument arg) :into formal-args
              :finally (setf args formal-args)))

      (multiple-value-bind (parsed-body rest-lines)
          (parse-indented-body body-lines)
        (values (make-circuit-definition
                 name              ; Circuit name
                 params            ; formal parameters
                 args              ; formal arguments
                 parsed-body)
                rest-lines)))))

(defun parse-quil-type (string)
  (check-type string string)
  (or (string-to-quil-type string)
      (quil-parse-error "Expected a valid Quil type in DECLARE: ~
                         BIT, OCTET, INTEGER, or REAL. Got ~S."
                        string)))

(defun parse-memory-descriptor (tok-lines)
  "Parse a memory location declaration out of the lines of tokens TOK-LINES."
  (when (null tok-lines)
    (quil-parse-error "Expected DECLARE, reached EOF"))

  (let ((line (rest (first tok-lines)))
        name
        type
        (length 1)
        sharing-parent
        (sharing-offset-alist nil))
    ;; Get the name.
    (setf name (token-payload (pop line)))
    ;; Check that we haven't seen name before.
    (when (member name *memory-region-names* :test #'string=)
      (quil-parse-error "The name ~S has been DECLAREd more than once." name))

    ;; Get the type
    (case (token-type (first line))
      (:name
       (setf type (parse-quil-type (token-payload (pop line)))))
      (:aref
       (let ((top-token (pop line)))
         (setf type (parse-quil-type (car (token-payload top-token))))
         (setf length (cdr (token-payload top-token))))))

    ;; Check for SHARING
    (unless (endp line)
      (unless (eql ':sharing (token-type (first line)))
        (quil-parse-error "Expected SHARING in DECLARE for ~S, found ~A."
                          name
                          (token-type (first line))))
      ;; Pop SHARING
      (pop line)
      ;; Get the parent.
      (let ((parent-name (token-payload (pop line))))
        (unless (member parent-name *memory-region-names* :test #'string=)
          (quil-parse-error "Unknown parent ~a of ~a" parent-name name))
        (setf sharing-parent parent-name))
      ;; Check if there's an OFFSET.
      (unless (endp line)
        (unless (eql ':offset (token-type (first line)))
          (quil-parse-error "Expected OFFSET in DECLARE for ~S, found ~A."
                            name
                            (token-type (first line))))
        ;; Pop it off.
        (pop line)
        ;; Process the offset pairs.
        (loop :for (offset-amount offset-type) :on line :by #'cddr
              :do (progn
                    ;; Check that we have the right types.
                    (unless (and (not (null offset-amount))
                                 (not (null offset-type))
                                 (eql ':integer (token-type offset-amount))
                                 (eql ':name    (token-type offset-type)))
                      (quil-parse-error "Expected integer-type pairs in DECLARE ~
                                         for ~S."
                                        name))
                    ;; Push our newfound knowledge onto an alist.
                    (let ((type   (token-payload offset-type))
                          (amount (token-payload offset-amount)))
                      (push (cons (parse-quil-type type) amount)
                            sharing-offset-alist))))))
    ;; store this name as visited
    (push name *memory-region-names*)
    ;; return
    (values
     (make-memory-descriptor :name name
                             :type type
                             :length length
                             :sharing-parent sharing-parent
                             :sharing-offset-alist (reverse sharing-offset-alist))
     (rest tok-lines))))

(defun parse-label (tok-lines)
  "Parse a label out of the lines of tokens TOK-LINES."

  ;; Check that we have something to parse.
  (when (null tok-lines)
    (quil-parse-error "Expected LABEL, reached EOF"))

  (let ((line (first tok-lines)))
    ;; Make sure the line is exactly two tokens long.
    (unless (= 2 (length line))
      (quil-parse-error "Malformed line when LABEL was expected."))

    (destructuring-bind (label-tok label-name-tok)
        line
      ;; Check that the first thing is a LABEL token.
      (unless (eql ':LABEL (token-type label-tok))
        (quil-parse-error "Expected LABEL, got ~S" (token-type label-tok)))

      ;; Check that the second thing is a label name.
      (unless (eql ':LABEL-NAME (token-type label-name-tok))
        (quil-parse-error "Expecting a label name, got ~S" (token-type label-name-tok)))

      ;; Return the label.
      (values
       (make-instance 'jump-target :label (token-payload label-name-tok))
       (rest tok-lines)))))

(defun parse-jump (tok-lines)
  (when (null tok-lines)
    (quil-parse-error "Expected JUMP, reached EOF"))
  (let ((line (first tok-lines)))
    (unless (= 2 (length line))
      (quil-parse-error "Malformed line when JUMP was expected."))
    (destructuring-bind (jump-tok label-tok)
        line
      (unless (eql ':JUMP (token-type jump-tok))
        (quil-parse-error "Expected JUMP, got ~S" (token-type jump-tok)))
      (unless (eql ':LABEL-NAME (token-type label-tok))
        (quil-parse-error "Expecting a label reference after JUMP got ~S" (token-type label-tok)))
      (values (make-instance 'unconditional-jump :label (token-payload label-tok))
              (rest tok-lines)))))

(defun parse-memory-or-formal-token (tok)
  (cond
    ((eql ':AREF (token-type tok))
     (mref (car (token-payload tok))
           (cdr (token-payload tok))))
    ((and (eql ':NAME (token-type tok))
          (find (token-payload tok) *memory-region-names* :test #'string=))
     (mref (token-payload tok) 0))
    ((and (eql ':NAME (token-type tok))
          *formal-arguments-allowed*)
     (formal (token-payload tok)))
    (t
     (quil-parse-error "Expected an address~:[~; or formal argument~], got ~S"
                       *formal-arguments-allowed*
                       (token-type tok)))))

(defun parse-classical-argument (toks)
  "Given a list of tokens that represent an argument to a classical instruction, parse it into something valid, including evaluating arithmetic tokens to produce a number."
  (if (= 1 (length toks))
      (let ((tok (first toks)))
        (cond
          ((eql (token-type tok) ':INTEGER)
           (constant (token-payload tok) quil-integer))
          ((eql (token-type tok) ':COMPLEX)
           (token-payload tok))
          (t
           (parse-memory-or-formal-token tok))))
      ;; We must have arithmetic tokens.
      (let ((result (eval-arithmetic-tokens-in-null-environment toks)))
        (etypecase result
          (integer (constant result quil-integer))
          (real (constant result quil-real))
          (complex (quil-parse-error "Unexpected complex number: ~A" result))))))

(defun parse-memory-offset (tok)
  (cond
    ((eql (token-type tok) ':INTEGER)
     (memory-offset (token-payload tok)))
    (t
     (parse-memory-or-formal-token tok))))

(defun parse-conditional-jump (tok-type tok-lines)
  (when (null tok-lines)
    (quil-parse-error "Expected conditional jump, reached EOF"))
  (let ((line (first tok-lines)))
    (unless (= 3 (length line))
      (quil-parse-error "Malformed line when conditional jump was expected."))
    (destructuring-bind (jump-tok label-tok addr-tok)
        line
      (unless (or (eql ':JUMP-WHEN (token-type jump-tok))
                  (eql ':JUMP-UNLESS (token-type jump-tok)))
        (quil-parse-error "Expected conditional jump, got ~S" (token-type jump-tok)))
      (unless (eql ':LABEL-NAME (token-type label-tok))
        (quil-parse-error "Expecting a label reference after JUMP got ~S" (token-type label-tok)))
      (unless (or (eql ':AREF (token-type addr-tok))
                  (eql ':NAME (token-type addr-tok)))
        (quil-parse-error "Expected an address~:[~; or formal argument~] for conditional jump, got ~S" *formal-arguments-allowed* (token-type addr-tok)))
      (values
       (ecase tok-type
         ((:JUMP-WHEN)
          (make-instance 'jump-when
                         :label (token-payload label-tok)
                         :address (parse-memory-or-formal-token addr-tok)))
         ((:JUMP-UNLESS)
          (make-instance 'jump-unless
                         :label (token-payload label-tok)
                         :address (parse-memory-or-formal-token addr-tok))))
       (rest tok-lines)))))

(defun parse-unary-classical (tok-type tok-lines)
  (check-type tok-type (member :NEG :NOT))
  (when (null tok-lines)
    (quil-parse-error "Expected unary instruction, but reached EOF"))
  (let ((line (first tok-lines)))
    (unless (= 2 (length line))
      (quil-parse-error "Malformed line when unary instruction was expected."))
    (destructuring-bind (instr-tok addr-tok)
        line
      (unless (member (token-type instr-tok) '(:NEG :NOT))
        (quil-parse-error "Expected unary instruction token, got ~S" (token-type instr-tok)))
      (let ((target (parse-memory-or-formal-token addr-tok)))
        (values (ecase tok-type
                  (:NEG (make-instance 'classical-negate :target target))
                  (:NOT (make-instance 'classical-not :target target)))
                (rest tok-lines))))))

(defun parse-binary-classical (tok-type tok-lines)
  (check-type tok-type (member :AND :IOR :XOR :MOVE :EXCHANGE :CONVERT :ADD :SUB
                               :MUL :DIV))
  (when (null tok-lines)
    (quil-parse-error "Expected binary instruction but reached EOF"))
  (let ((line (first tok-lines)))
    (unless (<= 3 (length line))
      (quil-parse-error "Malformed line when binary instruction was expected."))
    (destructuring-bind (instr-tok left-addr-tok &rest right-addr-toks)
        line
      (unless (find (token-type instr-tok) '(:AND :IOR :XOR :MOVE :EXCHANGE
                                             :CONVERT :ADD :SUB :MUL :DIV))
        (quil-parse-error "Expected binary instruction token, got ~S" (token-type instr-tok)))
      (let ((left (parse-memory-or-formal-token left-addr-tok))
            (right (parse-classical-argument right-addr-toks)))
        (when (and (member (token-type instr-tok) '(:EXCHANGE :CONVERT))
                   (not (or (typep right 'memory-ref)
                            (typep right 'formal))))
          (quil-parse-error "Second argument of EXCHANGE/CONVERT command expected to be a memory address, but got ~S"
                            (type-of right)))
        (values (ecase tok-type
                  (:AND (make-instance 'classical-and :left left :right right))
                  (:IOR (make-instance 'classical-inclusive-or :left left :right right))
                  (:XOR (make-instance 'classical-exclusive-or :left left :right right))
                  (:MOVE (make-instance 'classical-move :left left :right right))
                  (:EXCHANGE (make-instance 'classical-exchange :left left :right right))
                  (:CONVERT (make-instance 'classical-convert :left left :right right))
                  (:ADD (make-instance 'classical-addition :left left :right right))
                  (:SUB (make-instance 'classical-subtraction :left left :right right))
                  (:MUL (make-instance 'classical-multiplication :left left :right right))
                  (:DIV (make-instance 'classical-division :left left :right right)))
                (rest tok-lines))))))

(defun parse-trinary-classical (tok-type tok-lines)
  (check-type tok-type (member :LOAD :STORE :EQ :GT :GE :LT :LE))
  (when (null tok-lines)
    (quil-parse-error "Expected trinary instruction but reached EOF"))
  (let ((line (first tok-lines)))
    (unless (<= 4 (length line))
      (quil-parse-error "Malformed line when trinary instruction was expected."))
    (destructuring-bind (instr-tok target-tok left-tok &rest right-toks)
        line
      (unless (find (token-type instr-tok) '(:LOAD :STORE :EQ :GT :GE :LT :LE))
        (quil-parse-error "Expected trinary instruction token, got ~S" (token-type instr-tok)))
      (flet ((parse-memory-region-name (tok)
               (unless (eql ':NAME (token-type tok))
                 (quil-parse-error "Expected a memory region name, but got token of type ~S"
                                   (token-type tok)))
               (unless (find (token-payload tok) *memory-region-names* :test #'string=)
                 (quil-parse-error "Unknown memory region name ~S"
                                   (token-payload tok)))
               (memory-name (token-payload tok))))
        (let (target left right result)
          (ecase tok-type
            ((:LOAD)
             (unless (= 1 (length right-toks))
               (quil-parse-error "Invalid right argument to LOAD."))
             (setf target (parse-memory-or-formal-token target-tok))
             (setf left (parse-memory-region-name left-tok))
             (setf right (parse-classical-argument right-toks))
             (setf result (make-instance 'classical-load
                                         :target target
                                         :left left
                                         :right right)))
            ((:STORE)
             (unless (= 1 (length right-toks))
               (quil-parse-error "Malformed STORE. Expected a memory reference ~
                                  as the third argument."))
             (setf target (parse-memory-region-name target-tok))
             (setf left (parse-classical-argument (list left-tok)))
             (setf right (parse-memory-or-formal-token (first right-toks)))
             (setf result (make-instance 'classical-store
                                         :target target
                                         :left left
                                         :right right)))
            ((:EQ :GT :GE :LT :LE)
             (setf target (parse-memory-or-formal-token target-tok))
             (setf left (parse-classical-argument (list left-tok)))
             (setf right (parse-classical-argument right-toks))
             (setf result
                   (make-instance
                    (ecase tok-type
                      (:EQ 'classical-equality)
                      (:GT 'classical-greater-than)
                      (:GE 'classical-greater-equal)
                      (:LT 'classical-less-than)
                      (:LE 'classical-less-equal))
                    :target target
                    :left left
                    :right right))))
          (values result
                  (rest tok-lines)))))))

(defun indented-line (tok-line)
  (if (eql ':INDENT (token-type (first tok-line)))
      (values t (rest tok-line))
      (values nil tok-line)))

(defun dedented-line-p (tok-line)
  "Is the line of tokens TOK-LINE dedented? Return two values:

1. T/NIL depending on if it was.

2. The line excluding the :DEDENT token.
"
  (if (and (not (endp tok-line))
           (eql ':DEDENT (token-type (first tok-line))))
      (values t (rest tok-line))
      (values nil tok-line)))

(defun make-body (list)
  ;; was (list* ':BODY list)
  list)

(defun parse-indented-body (tok-lines)
  ;; Do we have any lines to parse?
  (when (null tok-lines)
    (warn "End of program each when indented code was expected.")
    (return-from parse-indented-body (values nil nil)))

  ;; Check for indentation.
  (multiple-value-bind (indented? modified-line)
      (indented-line (first tok-lines))
    ;; Is the first line indented?
    (unless indented?
      (warn "Expected indented code but alas, it wasn't found.")
      (return-from parse-indented-body
        (values (make-body nil)
                tok-lines)))
    ;; Remove the indentation from the top line.
    (setf tok-lines (cons modified-line (rest tok-lines))))

  ;; Parse until we reach a :DEDENT token.
  (loop :with parsed-body := nil :do
    ;; Reached EOF
    (when (null tok-lines)
      (warn "Reached end of file when parsing indented body.")
      (return-from parse-indented-body
        (values (make-body (nreverse parsed-body))
                tok-lines)))

    ;; Check for :DEDENT
    (multiple-value-bind (dedented? modified-line)
        (dedented-line-p (first tok-lines))
      (cond
        ;; DEDENT found, return.
        (dedented?
         (let ((new-lines (cons modified-line
                                (rest tok-lines))))
           (return-from parse-indented-body
             (values (make-body (nreverse parsed-body))
                     new-lines))))
        ;; DEDENT not found, descend.
        (t
         (multiple-value-bind (parsed rest-lines)
             (parse-program-lines tok-lines)
           (push parsed parsed-body)
           (setf tok-lines rest-lines)))))))


;;;;;;;;;;;;;;;;;;;;;;;;; Arithmetic Parser ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun token-generator (toks)
  "Given a list of tokens, generate a \"lexer\" compatible with CL-YACC."
  (lambda ()
    (if (null toks)
        (values nil nil)
        (let ((tok (pop toks)))
          (values (token-type tok)
                  (token-payload tok))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun binary (head)
    (lambda (a i0 b)
      (declare (ignore i0))
      (list head a b)))

  (defparameter *valid-functions*
    '(("SIN"  cl:sin)
      ("COS"  cl:cos)
      ("SQRT" cl:sqrt)
      ("EXP"  cl:exp)
      ("CIS"  cl:cis))
    "Functions usable from within Quil, and their associated Lisp function symbols.")

  (defun validate-function (func-name)
    "Validate the function named FUNC-NAME against *VALID-FUNCTIONS*. Signal a QUIL-PARSE-ERROR if it's invalid."
    (let ((found (assoc func-name *valid-functions* :test #'string-equal)))
      (if found
          (second found)
          (quil-parse-error "Invalid function name: ~A" func-name))))

  (defun find-or-make-parameter-symbol (param)
    (let ((found (assoc (param-name param)
                        *arithmetic-parameters*
                        :key #'param-name
                        :test #'string=)))
      (cond
        (found (second found))
        (t
         (let ((s (make-symbol (param-name param))))
           (push (list param s) *arithmetic-parameters*)
           s)))))

  (defun parse-arithmetic-aref-token (aref)
    (let ((region-name (car aref))
          (region-position (cdr aref)))
      (unless (find region-name *memory-region-names* :test #'string=)
        (error "Reference to unknown memory region ~a" region-name))
      (setf *segment-encountered* t)
      (mref region-name region-position))))

(yacc:define-parser *arithmetic-grammar*
  (:start-symbol expr)
  (:terminals (:LEFT-PAREN :RIGHT-PAREN
               :NAME :PARAMETER :INTEGER :COMPLEX
               :PLUS :MINUS :TIMES :DIVIDE :EXPT :AREF))
  (:precedence ((:right :EXPT) (:left :TIMES :DIVIDE) (:left :PLUS :MINUS)))

  (expr
   (expr :PLUS expr (binary 'cl:+))
   (expr :MINUS expr (binary 'cl:-))
   (expr :TIMES expr (binary 'cl:*))
   (expr :DIVIDE expr (binary 'cl:/))
   (expr :EXPT expr (binary 'cl:expt))
   term)

  (term
   (:MINUS expr
           (lambda (i0 x)
             (declare (ignore i0))
             (list 'cl:- x)))
   (:NAME :LEFT-PAREN expr :RIGHT-PAREN
          (lambda (f i0 x i1)
            (declare (ignore i0 i1))
            (let ((f (validate-function f)))
              (list f x))))
   (:LEFT-PAREN expr :RIGHT-PAREN
                (lambda (i0 x i1)
                  (declare (ignore i0 i1))
                  x))
   (:INTEGER (lambda (n) (coerce n 'double-float)))
   (:COMPLEX #'constant-value)
   (:PARAMETER #'find-or-make-parameter-symbol)
   (:AREF #'parse-arithmetic-aref-token)
   (:NAME (lambda (name)
            (parse-arithmetic-aref-token (cons name 0))))))

(defun parse-arithmetic-tokens (toks &key eval)
  "Given a list of tokens TOKS that form an arithmetic expression, parse them. If EVAL is T (default: NIL) and if the parsed expression is constant, evaluate it into a number."
  (let ((parsed-expr (yacc:parse-with-lexer
                      (token-generator toks)
                      *arithmetic-grammar*)))
    (if (and eval (null *arithmetic-parameters*) (null *segment-encountered*))
        (eval parsed-expr)
        parsed-expr)))

(defun eval-arithmetic-tokens-in-null-environment (toks)
  (let ((*arithmetic-parameters* nil)
        (*segment-encountered* nil))
    (parse-arithmetic-tokens toks :eval t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Entry Point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-code-sections (code)
  "Partition CODE into three values:

    1. List of gate definitions.

    2. List of circuit definitions.

    3. List of code to execute."
  (let ((gate-defs nil)
        (circ-defs nil)
        (memory-defs nil)
        (exec-code nil))
    (flet ((bin (x)
             (typecase x
               (gate-definition (push x gate-defs))
               (circuit-definition (push x circ-defs))
               (memory-descriptor (push x memory-defs))
               (t (push x exec-code)))))
      (mapc #'bin code)
      (values (nreverse gate-defs)
              (nreverse circ-defs)
              (nreverse memory-defs)
              (nreverse exec-code)))))

(defun parse-quil (string)
  "Parse a string STRING into a raw, untransformed PARSED-PROGRAM object."
  (check-type string string)
  (let* ((tok-lines (tokenize string)))
    (let ((parsed-program nil)
          (*memory-region-names* nil))
      (loop :named parse-loop
            :until (null tok-lines) :do
              (multiple-value-bind (program-entity rest-toks)
                  (parse-program-lines tok-lines)
                (push program-entity parsed-program)
                (setf tok-lines rest-toks)))
      (setf parsed-program (nreverse parsed-program))
      ;; Return the parsed sequence of objects.
      (multiple-value-bind (gate-defs circ-defs memory-defs exec-code)
          (extract-code-sections parsed-program)
        (make-instance 'parsed-program
                       :gate-definitions gate-defs
                       :circuit-definitions circ-defs
                       :memory-definitions memory-defs
                       :executable-code (coerce exec-code 'simple-vector))))))

(defvar *safe-include-directory* nil)

(defun resolve-safely (filename)
  (flet ((contains-up (filename)
           (member-if (lambda (obj)
                        (or (eql ':UP obj)
                            (eql ':BACK obj)))
                      (pathname-directory filename))))
    (cond
      ((uiop:absolute-pathname-p filename)
       (error "Not allowed to specify absolute paths to INCLUDE."))

      ((uiop:relative-pathname-p filename)
       ;; Only files allowed.
       (unless (uiop:file-pathname-p filename)
         (error "INCLUDE requires a pathname to a file."))
       (when (contains-up filename)
         (error "INCLUDE can't refer to files above."))
       (if (null *safe-include-directory*)
           filename
           (merge-pathnames filename *safe-include-directory*)))

      (t
       (error "Invalid pathname: ~S" filename)))))

(defun safely-read-quil (filename)
  "Safely read the Quil file designated by FILENAME."
  (flet ((read-it (file)
           (let ((*allow-unresolved-applications* t))
             (read-quil-file file))))
    (if (null *safe-include-directory*)
        (read-it filename)
        (let ((*resolve-include-pathname* #'resolve-safely))
          (read-it filename)))))

(defun safely-parse-quil-string (string)
  "Safely parse a Quil string STRING."
  (flet ((parse-it (string)
           (let ((*allow-unresolved-applications* t))
             (parse-quil-string string))))
    (if (null *safe-include-directory*)
        (parse-it string)
        (let ((*resolve-include-pathname* #'resolve-safely))
          (parse-it string)))))
