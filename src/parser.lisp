;;;; src/parser.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil/frontend)

;;; There are three main steps on the journey from Quil string to Quil program,
;;; namely lexing, parsing, and analysis.
;;;
;;; The lexing rules are encoded in LINE-LEXER below, with TOKENIZE providing a
;;; higher interface with special management of indentation etc.
;;;
;;; Once lexed, the high-level control flow of parsing is managed by
;;; PARSE-PROGRAM-LINES, which dispatches on token type and hands off to
;;; specialized parsing routines.
;;;
;;; The main entry point provided here is PARSE-QUIL-INTO-RAW-PROGRAM. Most
;;; users would instead do well to use PARSE-QUIL, which performs additional
;;; analysis (like object resolution).
;;;
;;;
;;; A brief note on extensions:
;;;
;;; The parsing code below is written to support some amount of extensibility at
;;; runtime. There are two main hooks in for this:
;;;
;;;   *LEXER-EXTENSIONS* allows for new keywords to be added
;;;   *PARSER-EXTENSIONS* allows for new AST objects to be produced
;;;
;;; For more information on these mechanisms, see their respective documentation.

;;;;;;;;;;;;;;;;;;;;;;;;;; Lexical Analysis ;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype quil-keyword ()
  '(member
    :JUMP :JUMP-WHEN :JUMP-UNLESS :INCLUDE :MEASURE
    :NEG :NOT :AND :IOR :XOR :MOVE :EXCHANGE :CONVERT :ADD :SUB :MUL :DIV
    :LOAD :STORE :EQ :GT :GE :LT :LE :DEFGATE :DEFCIRCUIT :RESET
    :HALT :WAIT :LABEL :NOP :CONTROLLED :DAGGER :FORKED
    :DECLARE :SHARING :OFFSET :PRAGMA
    :AS :MATRIX :PERMUTATION :PAULI-SUM :SEQUENCE))

(deftype token-type ()
  '(or
    quil-keyword
    (member
     :COLON :LEFT-PAREN :RIGHT-PAREN :COMMA :LABEL-NAME :PARAMETER
     :STRING :INDENTATION :INDENT :DEDENT :COMPLEX :PLUS
     :MINUS :TIMES :DIVIDE :EXPT :INTEGER :NAME :AREF)))

(defvar *line-start-position* nil)
(defvar *line-number* nil)
(defvar *current-file* nil
  "The pathname for the file being currently parsed.")

(defstruct (token (:constructor tok (type &optional payload (line *line-number*) (pathname *current-file*))))
  "A lexical token."
  (line nil :type (or null (integer 1))
            :read-only t)
  (pathname nil :type (or null string pathname)
                :read-only t)
  (type nil :type symbol                ; For vanilla Quil this will be
                                        ; TOKEN-TYPE, but for extensions the set
                                        ; of valid symbols may be larger.
            :read-only t)
  (payload nil :read-only t))

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

(defvar *lexer-extensions* '()
  "A list of lexer extensions.

Each lexer extension is a function mapping strings to tokens. They are used to handle keywords in Quil language extensions.")

(defun match-lexer-extension (str)
  "Given a string STR, check whether a lexer extension matches this string, and if so return the corresponding token."
  (dolist (ext *lexer-extensions* nil)
    (a:when-let ((kw (funcall ext str)))
      (return-from match-lexer-extension kw))))

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
  ((eager "\\;")
   (return ':SEMICOLON))
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
  ((eager #.(string #-ccl #\DAGGER #+ccl #\u+2020))
   (return (tok ':DAGGER)))
  ((eager #.(string #-ccl #\HELM_SYMBOL #+ccl #\u+2388))
   (return (tok ':CONTROLLED)))
  ((eager #.(string #\OCR_FORK))
   (return (tok ':FORKED)))
  ("INCLUDE|DEFCIRCUIT|DEFGATE|MEASURE|LABEL|WAIT|NOP|HALT|RESET|JUMP\\-WHEN|JUMP\\-UNLESS|JUMP|PRAGMA|NOT|AND|IOR|MOVE|EXCHANGE|SHARING|DECLARE|OFFSET|XOR|NEG|LOAD|STORE|CONVERT|ADD|SUB|MUL|DIV|EQ|GT|GE|LT|LE|CONTROLLED|DAGGER|FORKED|AS|MATRIX|PERMUTATION|PAULI-SUM|SEQUENCE"
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
       (t (a:if-let ((kw (match-lexer-extension $@)))
            kw
            (tok :NAME $@))))))
  ((eager "(?:    |\\t)+")
   (when (= *line-start-position* $<)
     (return (tok :INDENTATION (parse-indent-string $@)))))
  ;; Non-newline whitespace. In newer Perl, you can just use \h, for
  ;; "horizontal space", but this isn't supported in CL-PPCRE.
  ("[^\\S\\n\\r]+"
   nil))

(defun tokenization-failure-context (input-string condition)
  "Given an ALEXA:LEXER-MATCH-ERROR in CONDITION, return as multiple values the individual line and individual character within INPUT-STRING that triggered the condition."
  (flet ((alexa-failure-position (condition)
           ;; This parses the error string of ALEXA, which looks like
           ;; "Couldn't find match at position 42 ...". This is
           ;; obviously a fragile and silly thing to do, so when ALEXA
           ;; is updated to have an error condition with direct access
           ;; to the failing position, update this code to suit.
           (let* ((text (princ-to-string condition))
                  (start (position-if #'digit-char-p text)))
             (parse-integer text :start start :junk-allowed t))))
    (let* ((pos (alexa-failure-position condition))
           (previous-newline (position #\Newline input-string
                                       :end pos :from-end t))
           (start (if previous-newline (1+ previous-newline) 0))
           (end (position #\Newline input-string
                          :start pos)))
      (values (subseq input-string start end)
              (char input-string pos)))))

(defun tokenize-line (lexer string)
  "Given a lexer (as defined by DEFINE-STRING-LEXER) and a string, return a list of tokens represented by that string."
  (let ((*line-number* 1)
        (*line-start-position* 0))
    (handler-case
        (let ((f (funcall lexer string)))
          (loop :for tok := (funcall f)
                :until (null tok)
                :collect tok))
      (alexa:lexer-match-error (c)
        (multiple-value-bind (context-line failing-char)
            (tokenization-failure-context string c)
          (quil-parse-error "Unexpected input text ~S in ~S."
                            (string failing-char)
                            context-line))))))

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
(defun split-lines (tokens)
  "Split TOKENS into non-empty sublists representing logical lines.

A logical line may be introduced either as the result of a :NEWLINE or a :SEMICOLON token. In the case of a :NEWLINE, the lines are split as written. In the case of :SEMICOLON, the following line is prefixed with the indentation of the immediately preceding line."
  ;; note: something like "H 0 ; X 1" will produce two lines for internal processing,
  ;; but the tokens themselves maintain their original line numbers for error reporting.
  (declare (type list tokens)
           (optimize speed (space 0)))
  (let* ((pieces      (cons nil nil))
         (pieces-last pieces))
    (declare (type cons pieces pieces-last))
    (labels ((doit (start last end)
               ;; This was recursive, but it's been made iterative,
               ;; but we've kept the function around, just to minimize
               ;; change.
               (declare (type list start last end))
               (loop :when (null end)
                       ;; Done
                       :do (unless (eq start end)
                             (rplacd pieces-last (cons start nil))
                             (setq pieces-last (cdr pieces-last))) ; unnec, but consistent
                           (return pieces)
                     :do
                        (cond
                          ;; Split off a newline
                          ((eq ':NEWLINE (car end))
                           (cond
                             ((eq start end)
                              (let ((next (cdr start)))
                                (setq start next)
                                (setq last next)
                                (setq end next)))
                             (t
                              (rplacd pieces-last (cons start nil))
                              (setq pieces-last (cdr pieces-last))
                              (setq start (cdr end))
                              (rplacd last nil)
                              (setq last start)
                              (setq end start))))
                          ;; A semicolon also results in a new line being split, but we
                          ;; also inherit the preceding indentation.
                          ((eq ':SEMICOLON (car end))
                           (cond
                             ((eq start end)
                              (let ((next (cdr start)))
                                (setq start next)
                                (setq last next)
                                (setq end next)))
                             (t
                              (rplacd pieces-last (cons start nil))
                              (setq pieces-last (cdr pieces-last))
                              ;; Prepend appropriate indentation
                              (setq start (if (eq ':INDENTATION (token-type (car start)))
                                              (cons (car start) ; Copy the indentation (line number on this token is irrelevant)
                                                    (cdr end))
                                              (cdr end)))
                              (rplacd last nil)
                              (setq last start)
                              (setq end start))))
                          ;; Keep on truckin'.
                          (t
                           (setq last end)
                           (setq end (cdr end)))))))
      (cdr (doit tokens tokens tokens)))))

(defun tokenize (string)
  "Tokenize the Quil string STRING into a list of token lines with indentation resolved. Each token line is itself a list of tokens."
  (let* ((lines (split-lines (tokenize-line 'line-lexer string))))
    (map-into lines #'ensure-indentation lines)
    (process-indentation lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition quil-parse-error (a:simple-parse-error)
  ()
  (:documentation "Representation of an error parsing Quil."))

(define-condition quil-parse-error-at-line (quil-parse-error)
  ((line :initarg :line :reader line-of-quil-parse-error)
   (pathname :initarg :pathname :reader pathname-of-quil-parse-error))
  (:report (lambda (condition stream)
             (format stream "At line ~D~@[ (~A)~]: "
                     (line-of-quil-parse-error condition)
                     (pathname-of-quil-parse-error condition))
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
                                       :pathname *current-file*
                                       :format-control format-control
                                       :format-arguments format-args)
      (error 'quil-parse-error :format-control format-control
                               :format-arguments format-args)))

(defvar *definitions-allowed* t
  "Dynamic variable to control whether DEF* forms are allowed in the current parsing context.")

(defvar *formal-arguments-allowed* nil
  "Dynamic variable to control whether formal parameters and arguments are allowed in the current parsing context.")

(defvar *parse-context*)
(setf (documentation '*parse-context* 'variable)
      "A Quil keyword (e.g. :DEFGATE) indicating the context in which a portion of a Quil instruction is being parsed.")

(defun disappointing-token-error (found-token expected-msg)
  (quil-parse-error "Expected ~A~@[ in ~A~], but observed a token ~A with value ~A."
                    expected-msg
                    *parse-context*
                    (token-type found-token)
                    (token-payload found-token)))

(defvar *parser-extensions* '()
  "A list of parsers which may be invoked when PARSE-PROGRAM-LINES encounters tokens beyond vanilla Quil.

Each parser is a function mapping a list of lines of tokens to a pair,

1. The next AST object.
2. A list of lines that remain unparsed.

If the parser does not match, then it should return NIL.")

(defun match-parser-extension (tok-lines)
  "Apply any parsers defined in *PARSER-EXTENSIONS* to the list of lines of tokens, TOK-LINES."
  (dolist (parser *parser-extensions* (values nil tok-lines))
    (multiple-value-bind (obj rest-lines)
        (funcall parser tok-lines)
      (when obj
        (return-from match-parser-extension (values obj rest-lines))))))

(defun parse-program-lines (tok-lines)
  "Parse the next AST object from the list of token lists. Returns two values:

1. The next AST object.
2. A list of lines that remain unparsed."
  (let* ((line (first tok-lines))
         (tok (first line))
         (tok-type (token-type tok))
         (*parse-context* tok-type)
         (*line-number* (token-line tok)))
    (case tok-type
      ;; Gate/Circuit Applications
      ((:CONTROLLED :DAGGER :FORKED)
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
       (multiple-value-bind (obj rest-lines)
           (match-parser-extension tok-lines)
         (if obj
             (values obj rest-lines)
             (quil-parse-error "Got an unexpected token of type ~S ~
                               when trying to parse a program." tok-type)))))))

;; Many instructions are given by a single line of a few tokens. The
;; MATCH-LINE macro below just captures some boilerplate common to the
;; parsing of such instructions (namely, checking that there are the right
;; number and type of tokens, destructuring the corresponding token list,
;; and returning a result along with the rest of the lines.
(defmacro match-line (token-specs lines &body body)
  "Bind the tokens in the first line of LINES according to the specified TOKEN-SPECS. These bindings are made available in the BODY.

A TOKEN-SPEC looks like a lambda list. There is support for three sorts of bindings: required, optional, and rest. If specializers (for required) or default values (for optional) are provided, these symbols are used to enforce a given token type.

For example, the spec ((op :RESET) &optional q) would match a single RESET token, or a RESET token followed by a second token.

In accordance with the typical usage here, there are two values returned: the result of BODY, and the (possibly null) list of remaining lines.
"
  (let ((all-lines (gensym))
        (first-line (gensym)))
    (flet ((token-typecheck-clause (spec opt)
             (when (listp spec)
               (let ((spec-name (first spec))
                     (spec-type (second spec)))
                 (when spec-type
                   `((unless
                         ,(if opt
                              `(or (null ,spec-name)
                                   (eql ',spec-type (token-type ,spec-name)))
                              `(eql ,spec-type (token-type ,spec-name)))
                       (quil-parse-error
                        ,(if (keywordp spec-type)  ; If possible, format at expansion time.
                             (format nil "Expected ~S token, but got ~~S." spec-type)
                             `(format nil "Expected ~S token, but got ~~S." ,spec-type))
                        (token-type ,spec-name)))))))))
      (multiple-value-bind (required optional rest)
          (a:parse-ordinary-lambda-list token-specs
                                        :allow-specializers t)
        (let ((stripped-lambda-list (mapcar #'a:ensure-car
                                            token-specs))
              (first-type (second (first required))))
          `(let ((,all-lines ,lines))
             ;; Check that we have a line to consume
             (when (endp ,all-lines)
               (quil-parse-error "Unexpectedly reached end of program~@[ (expected ~A instruction)~]." ,first-type))
             (let ((,first-line (first ,all-lines)))
               ;; Check that we have the right number of tokens
               ,(let ((min-tokens (length required))
                      (max-tokens (unless rest
                                    (+ (length required) (length optional)))))
                  `(unless (<= ,min-tokens
                               (length ,first-line)
                               . ,(when max-tokens (list max-tokens)))
                     (quil-parse-error "Expected at least ~A ~@[ and at most ~A~] tokens~@[ for instruction ~A~]."
                                       ,min-tokens
                                       ,max-tokens
                                       ,first-type)))
               (destructuring-bind ,stripped-lambda-list ,first-line
                 ;; Check the various token types
                 ,@(mapcan (lambda (s) (token-typecheck-clause s nil))
                           required)
                 ,@(mapcan (lambda (s) (token-typecheck-clause s t))
                           optional)
                 (values (progn ,@body)
                         (rest ,all-lines))))))))))

(defun parse-qubit (qubit-tok)
  "Parse a qubit, denoted by a formal argument or an integer index."
  (case (token-type qubit-tok)
    ((:NAME)
     (unless *formal-arguments-allowed*
       (quil-parse-error "Unexpected formal argument \"~A\"~@[ in ~A~]."
                         (token-payload qubit-tok)
                         *parse-context*))
     (formal (token-payload qubit-tok)))
    ((:INTEGER) (qubit (token-payload qubit-tok)))
    (otherwise
     (disappointing-token-error qubit-tok
                                (if *formal-arguments-allowed*
                                    "a name or formal argument"
                                    "a name")))))

(defun parse-memory-or-formal-token (tok &key (ensure-valid t))
  "Parse the token TOK as a memory reference or formal argument.

If ENSURE-VALID is T (default), then a memory reference such as 'foo[0]' will result in an error unless 'foo' has been DECLAREd."
  (declare (special *memory-region-names*)) ; Forward declaration
  (cond
    ;; Actual address to measure into.
    ((eql ':AREF (token-type tok))
     (when (and ensure-valid
                (not (find (car (token-payload tok)) *memory-region-names* :test #'string=)))
       (quil-parse-error "Bad memory region name \"~A\"~@[ in ~A~]. This is probably due to either:
    * a missing DECLARE for this memory,
    * a misspelling of the memory reference, or
    * a misspelling of the DECLAREd memory."
                         (car (token-payload tok))
                         *parse-context*))
     (mref (car (token-payload tok))
           (cdr (token-payload tok))))

    ;; Implicit address to measure into.
    ((and (eql ':NAME (token-type tok))
          (find (token-payload tok) *memory-region-names* :test #'string=))
     (mref (token-payload tok) 0))

    ;; Formal argument.
    ((eql ':NAME (token-type tok))
     (unless *formal-arguments-allowed*
       (quil-parse-error "Found formal argument \"~A\"~@[ in ~A~]."
                         (token-payload tok)
                         *parse-context*))
     (formal (token-payload tok)))

    (t
     (disappointing-token-error tok
                                (if *formal-arguments-allowed*
                                    "an address or formal argument"
                                    "an address")))))

(defun parse-simple-instruction (type tok-lines)
  (match-line ((op type)) tok-lines
    (ecase type
      ((:HALT)  (make-instance 'halt))
      ((:RESET) (make-instance 'reset))
      ((:NOP)   (make-instance 'no-operation))
      ((:WAIT)  (make-instance 'wait)))))

(defun parse-reset-instruction (tok-lines)
  "Parse either a 'RESET' or 'RESET <q>' instruction where <q> is a :NAME or :INTEGER into a RESET or RESET-QUBIT instruction, respectively."
  (match-line ((op :RESET) &optional q) tok-lines
    (if (null q)
        ;; global RESET
        (make-instance 'reset)
        ;; targeted RESET
        (make-instance 'reset-qubit :target (parse-qubit q)))))

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

(defun parse-parameter (tok)
  "Parse a token/number/parameter TOK intended to be a parameter."
  (let ((type (token-type tok)))
    (case type
      ((:PARAMETER)
       (unless *formal-arguments-allowed*
         (quil-parse-error "Found formal parameter~@[ in ~A~] where not allowed: ~S."
                           *parse-context*
                           (token-payload tok)))
       (token-payload tok))
      ((:COMPLEX)
       (token-payload tok))
      ((:INTEGER)
       (constant (coerce (token-payload tok) 'double-float)))
      (otherwise
       (disappointing-token-error tok "a parameter")))))

(defun parse-formal (tok)
  "Parse a token TOK into a formal argument."
  (case (token-type tok)
    ((:NAME)
     ;; Sanity check that we are allowed to savor a formal.
     (unless *formal-arguments-allowed*
       (quil-parse-error "Unexpected formal argument A~@[ in ~A~]."
                         (token-payload tok)
                         *parse-context*))
     (formal (token-payload tok)))
    (otherwise
     (disappointing-token-error tok "a formal"))))

(defun parse-argument (tok)
  "Parse a token TOK intended to be an argument."
  (declare (special *memory-region-names* *shadowing-formals*)) ; Forward declaration
  (case (token-type tok)
    ((:NAME)
     (let ((names-memory-region-p (find (token-payload tok)
                                        *memory-region-names*
                                        :test #'string=))
           (shadowing-formal (find (token-payload tok)
                                   *shadowing-formals*
                                   :test #'string=
                                   :key #'formal-name)))
       (unless (or names-memory-region-p
                   *formal-arguments-allowed*)
         (quil-parse-error "Unexpected formal argument ~A~@[ in ~A~]."
                           (token-payload tok)
                           *parse-context*))
       (cond
         ((and *formal-arguments-allowed*
               shadowing-formal)
          shadowing-formal)
         (names-memory-region-p (mref (token-payload tok) 0))
         (t
          (warn "In PARSE-ARGUMENT, a formal was parsed even ~
                 though it isn't known contextually.")
          (formal (token-payload tok))))))
    ((:INTEGER)
     (qubit (token-payload tok)))
    ((:COMPLEX)
     (token-payload tok))
    ((:AREF)
     (mref (car (token-payload tok))
           (cdr (token-payload tok))))
    (otherwise
     (disappointing-token-error tok "an argument"))))

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

(defvar *shadowing-formals* nil
  "A special variable which indicates formal parameters (as a list of FORMAL objects) which shadow memory names.")

(defun gate-modifier-token-p (tok)
  (member (token-type tok) '(:CONTROLLED :DAGGER :FORKED)))

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
                                        (dagger-operator base-operator)))
          (:FORKED
           (apply-modifiers-to-operator (rest processed-modifiers)
                                        (forked-operator base-operator)))))))

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
                       (:DAGGER     (list ':DAGGER))
                       (:FORKED     (list ':FORKED)))
              :into processed-modifiers
            :finally (progn
                       (setf (application-operator app)
                             (apply-modifiers-to-operator
                              (reverse processed-modifiers)
                              (application-operator app)))
                       (return (values app rest-lines)))))))

(defun parse-parameter-or-expression (toks)
  "Parse a parameter, which may possibly be a compound arithmetic expression. Consumes all tokens given."
  (if (and (= 1 (length toks))
           (not (member (token-type (first toks)) '(:NAME :AREF))))
      (parse-parameter (first toks))
      (let ((*arithmetic-parameters* nil)
            (*segment-encountered* nil))
        (let ((result (parse-arithmetic-tokens toks :eval t)))
          (cond
            ((and (null *arithmetic-parameters*)
                  (null *segment-encountered*))
             (unless (numberp result)
               (quil-parse-error "A parameter or arithmetic expression was expected~@[ in ~A~]. Got something of type ~S."
                                 *parse-context*
                                 (type-of result)))
             (constant result))

            ((or *formal-arguments-allowed*
                 (and *segment-encountered*
                      (null *arithmetic-parameters*)))
             (make-delayed-expression
              (mapcar #'first *arithmetic-parameters*)
              (mapcar #'second *arithmetic-parameters*)
              result))

            (t
             (quil-parse-error "Formal parameters found in a place they're not allowed.")))))))

(defun parse-application (tok-lines)
  "Parse a gate or circuit application out of the lines of tokens TOK-LINES, returning an UNRESOLVED-APPLICATION."
  (match-line ((op :NAME) &rest rest-toks) tok-lines
    (if (endp rest-toks)
        (make-instance 'unresolved-application
                       :operator (named-operator (token-payload op)))
        (multiple-value-bind (params args)
            (parse-parameters rest-toks :allow-expressions t)

          ;; Parse out the rest of the arguments and return.
          (make-instance 'unresolved-application
                         :operator (named-operator (token-payload op))
                         :parameters params
                         :arguments (mapcar #'parse-argument args))))))

(defun parse-measurement (tok-lines)
  "Parse a measurement out of the lines of tokens TOK-LINES."
  (match-line ((op :MEASURE) qubit-tok &optional address-tok) tok-lines
    (let ((qubit (parse-qubit qubit-tok)))
      (if (null address-tok)
          (make-instance 'measure-discard :qubit qubit)
          (make-instance 'measure
                         :qubit qubit
                         :address (parse-memory-or-formal-token address-tok))))))

(defun parse-pragma (tok-lines)
  "Parse a PRAGMA out of the lines of tokens TOK-LINES."
  (match-line ((op :PRAGMA) (word-tok :NAME) &rest word-toks) tok-lines
    (multiple-value-bind (words non-words)
        (take-until (lambda (tok) (not (member (token-type tok) '(:NAME :INTEGER)))) (cons word-tok word-toks))
      (setf words (mapcar #'token-payload words))

      (cond
        ((null non-words)
         (make-pragma words))

        ((endp (cdr non-words))
         (let ((last-tok (first non-words)))
           (unless (eql ':STRING (token-type last-tok))
             (disappointing-token-error last-tok "a terminating string"))
           (make-pragma words (token-payload last-tok))))

        (t
         (quil-parse-error "Unexpected tokens near the end of a PRAGMA."))))))

(defun parse-include (tok-lines)
  "Parse an INCLUDE out of the lines of tokens TOK-LINES."
  (match-line ((op :INCLUDE) (path :STRING)) tok-lines
    (make-instance 'include :pathname (token-payload path))))

(defun validate-gate-matrix-size (gate-name num-entries)
  "For the gate named GATE-NAME, check that NUM-ENTRIES is a valid number of entries for a gate matrix."
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
  (unless (positive-power-of-two-p (isqrt num-entries))
    (quil-parse-error "The gate ~A being defined isn't a square 2^n x 2^n ~
                         matrix. In particular, it is a ~D x ~D matrix."
                      gate-name
                      (isqrt num-entries)
                      (isqrt num-entries)))
  t)

(defun parse-gate-definition (tok-lines)
  "Parse a gate definition from the token lines TOK-LINES."
  ;; Check that we have tokens left
  (when (null tok-lines)
    (quil-parse-error "EOF reached when gate definition expected"))

  ;; Get the parameter and body lines
  (let (name
        (gate-type ':MATRIX))
    (destructuring-bind (parameter-line &rest body-lines) tok-lines
      (destructuring-bind (op . params-args) parameter-line
        ;; Check that we are dealing with a DEFGATE.
        (unless (eql ':DEFGATE (token-type op))
          (quil-parse-error "DEFGATE expected. Got ~S."
                            (token-type op)))

        ;; Check that something is following the DEFGATE.
        (when (null params-args)
          (quil-parse-error "Expected more after DEFGATE token."))

        ;; Check for a name.
        (unless (eql ':NAME (token-type (first params-args)))
          (disappointing-token-error (first params-args) "a name"))

        ;; We have a name. Stash it away.
        (setf name (token-payload (pop params-args)))

        (multiple-value-bind (params rest-line) (parse-parameters params-args)
          (multiple-value-bind (args rest-line) (parse-arguments rest-line)
            (when (eql ':AS (token-type (first rest-line)))
              (pop rest-line)
              (let* ((parsed-gate-tok (first rest-line))
                     (parsed-gate-type (token-type parsed-gate-tok)))
                (unless (find parsed-gate-type '(:MATRIX :PERMUTATION :PAULI-SUM :SEQUENCE))
                  (quil-parse-error "Found unexpected gate type: ~A." (token-payload parsed-gate-tok)))
                (setf gate-type parsed-gate-type)))

            (ecase gate-type
              (:MATRIX
               (when args
                 (quil-parse-error "DEFGATE AS MATRIX cannot carry formal qubit arguments."))
               (parse-gate-entries-as-matrix body-lines params name :lexical-context op))
              (:PERMUTATION
               (when args
                 (quil-parse-error "DEFGATE AS PERMUTATION cannot carry formal qubit arguments."))
               (when params
                 (quil-parse-error "Permutation gate definitions do not support parameters."))
               (parse-gate-entries-as-permutation body-lines name :lexical-context op))
              (:PAULI-SUM
               (let ((*shadowing-formals* args))
                 (parse-gate-definition-body-into-pauli-sum body-lines name
                                                            :lexical-context op
                                                            :legal-arguments args
                                                            :legal-parameters params)))
              (:SEQUENCE
               (let ((*shadowing-formals* args))
                 (parse-gate-definition-body-into-sequence body-lines name args params :context op))))))))))

(defun parse-gate-definition-body-into-sequence (body-lines name args params &key context)
  (multiple-value-bind (parsed-body rest-lines) 
    (parse-indented-body body-lines)
    (values (make-instance 'sequence-gate-definition
              :name name
              :parameters params
              :arguments args
              :sequence parsed-body
              :context context)
            rest-lines)))

(defun parse-gate-definition-body-into-pauli-sum (body-lines name &key lexical-context legal-arguments legal-parameters)
  ;; is the immediate next line indented? if not, error.
  (let ((*segment-encountered* nil)
        (*arithmetic-parameters* nil))
    (multiple-value-bind (indented? modified-line)
        (indented-line (first body-lines))
      (unless indented?
        (quil-parse-error "Declaration DEFGATE ~a ... AS PAULI-SUM has empty body."
                          name))
      ;; strip off the indentation.
      (setf body-lines (list* modified-line (rest body-lines)))
      ;; otherwise, iterate til we hit a dedent token.
      (loop :with parsed-entries := nil
            :with remaining-lines := nil ; If there's no dedent, this
                                         ; will remain empty
            :for line :in body-lines
            :for rest-lines := (rest body-lines) :then (rest rest-lines)
            :do (multiple-value-bind (dedented? modified-line)
                    (dedented-line-p line)
                  ;; if we're done, return the gate definition (and
                  ;; the rest of the lines)
                  (when dedented?
                    (setf remaining-lines (cons modified-line rest-lines))
                    (loop-finish))
                  ;; store this word/qubits pair as part of the gate
                  ;; definition
                  (push (parse-pauli-sum-line line
                                              :lexical-context lexical-context
                                              :legal-parameters legal-parameters
                                              :legal-arguments legal-arguments)
                        parsed-entries))
            :finally (return
                       (values (make-instance 'exp-pauli-sum-gate-definition
                                 :name name
                                 :terms (nreverse parsed-entries)
                                 :context lexical-context
                                 :arguments legal-arguments
                                 :parameters (mapcar (lambda (p)
                                                       (or (cadr (assoc p *arithmetic-parameters* :test #'equalp))
                                                           (make-symbol (format nil "~a-UNUSED" (param-name p)))))
                                                     legal-parameters))
                               remaining-lines))))))

(defun parse-pauli-sum-line (line &key lexical-context legal-arguments legal-parameters)
  "Parses a line inside of a DEFGATE ... AS PAULI-SUM body."
  (declare (ignore lexical-context legal-parameters))
  (let (pauli-word param qubit-list)
    (when (null line)
      (quil-parse-error "Empty line found in DEFGATE AS PAULI-SUM body."))
    ;; PAULI-WORD
    (unless (eql ':NAME (token-type (first line)))
      (quil-parse-error "DEFGATE AS PAULI-SUM body line begins with something other than a Pauli word: ~a" (first line)))
    (setf pauli-word (token-payload (pop line)))
    (unless (every (lambda (c) (member c '(#\I #\X #\Y #\Z))) pauli-word)
      (quil-parse-error "DEFGATE AS PAULI-SUM body line contains Pauli word with letters other than I, X, Y, Z: ~a"
                        pauli-word))
    ;; LPAREN
    (unless (eql ':LEFT-PAREN (token-type (first line)))
      (quil-parse-error "Pauli term requires a parenthesized scalar factor, but found ~a instead of LPAREN" (first line)))
    (pop line)
    ;; PARAMETER
    (multiple-value-bind (param-tokens line)
        (take-until (lambda (x)
                      (pop line)
                      (eql ':RIGHT-PAREN (token-type x)))
                    line)
      (when (null line)
        (quil-parse-error "Pauli term has unmatched left-parenthesis."))
      (setf param (parse-arithmetic-tokens param-tokens :eval t))
      ;; RPAREN
      (unless (eql ':RIGHT-PAREN (token-type (first line)))
        (quil-parse-error "Expected a right parenthesis in parsing this Pauli term, but got ~a" (first line)))
      (pop line)
      ;; QUBIT ... QUBIT
      (setf qubit-list (mapcar (lambda (tok)
                                 (unless (eql ':NAME (token-type tok))
                                   (quil-parse-error "Expected a formal qubit argument, but got ~a" tok))
                                 (let ((ret (parse-argument tok)))
                                   (unless (member ret legal-arguments :test #'equalp)
                                     (quil-parse-error "Found formal qubit argument ~a not present in the argument list." (formal-name ret)))
                                   ret))
                               line))
      ;; some further Sanity Chex:
      ;; there are as many paulis as qubits
      (unless (= (length qubit-list) (length pauli-word))
        (quil-parse-error "Pauli word ~a expected ~a qubit arguments, but got ~a: ~a"
                          pauli-word (length pauli-word) (length qubit-list) qubit-list))
      ;; the parameter body only refers to defined terms
      ;; XXX
      (make-pauli-term :pauli-word pauli-word
                       :prefactor param
                       :arguments qubit-list))))

(defun parse-gate-entries-as-permutation (body-lines name &key lexical-context)
  (multiple-value-bind (parsed-entries rest-lines)
      (parse-permutation-gate-entries body-lines)
    (validate-gate-permutation-size (length parsed-entries))
    (values (make-instance 'permutation-gate-definition
                           :name name
                           :permutation parsed-entries
                           :context lexical-context)
            rest-lines)))

(defun validate-gate-permutation-size (size)
  (unless (positive-power-of-two-p size)
    (quil-parse-error "Permutation gate entries do not represent a square matrix.")))

(defun parse-gate-entries-as-matrix (body-lines params name &key lexical-context)
  ;; Parse out the gate entries.
  (let ((*arithmetic-parameters* nil)
        (*segment-encountered* nil))
    (multiple-value-bind (parsed-entries rest-lines)
        (parse-indented-entries body-lines)
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
        (values (make-gate-definition name param-symbols parsed-entries :context lexical-context)
                rest-lines)))))

(defun parse-arithmetic-entries (entries)
  "Given a list of entries, each of which is a list of tokens, return the entries evaluated, unless there are formal parameters. Return the simplified entries.
"
  (labels ((simplify (entry)
             (parse-arithmetic-tokens entry :eval t)))
    (mapcar #'simplify entries)))

(defun parse-indented-entries (tok-lines &key require-indent)
  "Parse indented, comma separated entries from TOK-LINES. Returns the the parsed entries as well as the remaining lines.

If REQUIRE-INDENT is T, a parse error is signalled if entries are not properly indented."
  ;; Do we have any lines to parse?
  (when (null tok-lines)
    (warn "End of program each when indented gate or waveform entries was expected.")
    (return-from parse-indented-entries (values nil nil)))

  ;; Check for indentation.
  (multiple-value-bind (indented? modified-line)
      (indented-line (first tok-lines))
    ;; Is the first line indented?
    (unless indented?
      (when require-indent
        (quil-parse-error "Expected indented entries when parsing ~A, but alas, ~
                          they weren't found. (Did you indent properly?)"
                          *parse-context*))
      (warn "Expected indented gate or waveform entries but alas, they weren't found.")
      (return-from parse-indented-entries
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
      (return-from parse-indented-entries
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
           (return-from parse-indented-entries
             (values (parse-arithmetic-entries gate-entries)
                     new-lines))))
        ;; DEDENT not found, continue parsing entries.
        (t
         (multiple-value-bind (parsed rest-lines)
             (parse-gate-line tok-lines)
           (setf gate-entries (append gate-entries parsed))
           (setf tok-lines rest-lines)))))))

(defun parse-permutation-gate-entries (tok-lines)
  ;; Do we have any lines to parse?
  (when (null tok-lines)
    (quil-parse-error "End of program each when indented gate entries was expected."))

  ;; Check for indentation.
  (multiple-value-bind (indented? modified-line)
      (indented-line (pop tok-lines))
    ;; Is the first line indented?
    (unless indented?
      (quil-parse-error "Expected indented gate entries but alas, they weren't found."))

    (multiple-value-bind (dedented? dedented-line)
        (dedented-line-p (pop tok-lines))
      ;; AS PERMUTATION expects a single line, followed by a dedented line or no
      ;; lines
      (when (and (not dedented?) dedented-line)
        (quil-parse-error "Expected dedented line following permutation gate entries."))
      (let* ((entries (remove-if (lambda (tok) (eql ':COMMA (token-type tok)))
                                 modified-line)))
        (dolist (entry entries)
          (unless (eql ':INTEGER (token-type entry))
            (disappointing-token-error entry "an integer")))
        ;; Be careful to continue parsing only when a (by definition) dedented
        ;; line follows
        (values (mapcar #'token-payload entries)
                (if dedented?
                    (cons dedented-line tok-lines)
                    nil))))))

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
    (quil-parse-error "EOF reached when circuit definition expected."))

  (let (name args)
    ;; Split off the header line from the body lines.
    (destructuring-bind (parameter-line &rest body-lines) tok-lines
      ;; Check that we have a well-formed header line.
      (destructuring-bind (op . params-args) parameter-line
        ;; We must be working with a DEFCIRCUIT.
        (unless (eql ':DEFCIRCUIT (token-type op))
          (quil-parse-error "DEFCIRCUIT expected. Got ~S."
                            (token-type op)))

        ;; Check that there are tokens following DEFCIRCUIT.
        (when (null params-args)
          (quil-parse-error "Expected more after DEFCIRCUIT token."))

        ;; Check for name.
        (unless (eql ':NAME (token-type (first params-args)))
          (disappointing-token-error (first params-args) "a name"))

        ;; Stash it away.
        (setf name (token-payload (pop params-args)))

        (multiple-value-bind (params rest-line) (parse-parameters params-args)
          ;; Check for colon and incise it.
          (let ((maybe-colon (last rest-line)))
            (when (or (null maybe-colon)
                      (not (eql ':COLON (token-type (first maybe-colon)))))
              (quil-parse-error "Expected a colon in DEFCIRCUIT."))
            (setf rest-line (butlast rest-line)))

          ;; Collect arguments and stash them away.
          (loop :for arg :in rest-line
                :when (not (eql ':NAME (token-type arg)))
                  :do (quil-parse-error "Invalid formal argument in DEFCIRCUIT line.")
                :collect (parse-formal arg) :into formal-args
                :finally (setf args formal-args))

          (multiple-value-bind (parsed-body rest-lines)
              (let ((*shadowing-formals* args))
                (parse-indented-body body-lines))
            (values (make-circuit-definition
                     name              ; Circuit name
                     params            ; formal parameters
                     args              ; formal arguments
                     parsed-body
                     :context op)
                    rest-lines)))))))

(defun parse-quil-type (string)
  (check-type string string)
  (or (string-to-quil-type string)
      (quil-parse-error "Expected a valid Quil type in DECLARE: ~
                         BIT, OCTET, INTEGER, or REAL. Got ~S."
                        string)))

(defun parse-memory-descriptor (tok-lines)
  "Parse a memory location declaration out of the lines of tokens TOK-LINES."
  (when (null tok-lines)
    (quil-parse-error "Expected DECLARE, reached EOF."))

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
          (quil-parse-error "Unknown parent ~A of ~A." parent-name name))
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
                             :sharing-offset-alist (reverse sharing-offset-alist)
                             :lexical-context (first (first tok-lines)))
     (rest tok-lines))))

(defun parse-label (tok-lines)
  "Parse a label out of the lines of tokens TOK-LINES."
  (match-line ((op :LABEL) (label-name :LABEL-NAME)) tok-lines
    (make-instance 'jump-target :label (token-payload label-name))))

(defun parse-jump (tok-lines)
  (match-line ((op :JUMP) (label :LABEL-NAME)) tok-lines
    (make-instance 'unconditional-jump :label (token-payload label))))

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
          (complex (quil-parse-error "Unexpected complex number: ~A." result))))))

(defun parse-memory-offset (tok)
  (cond
    ((eql (token-type tok) ':INTEGER)
     (memory-offset (token-payload tok)))
    (t
     (parse-memory-or-formal-token tok))))

(defun parse-conditional-jump (jump-type tok-lines)
  (match-line ((jump jump-type) (label :LABEL-NAME) addr) tok-lines
    (unless (or (eql ':AREF (token-type addr))
                (eql ':NAME (token-type addr)))
      (disappointing-token-error addr
                                 (if *formal-arguments-allowed*
                                     "an address or formal argument"
                                     "an address")))
    (make-instance (ecase jump-type
                     ((:JUMP-WHEN) 'jump-when)
                     ((:JUMP-UNLESS) 'jump-unless))
                   :label (token-payload label)
                   :address (parse-memory-or-formal-token addr))))

(defun parse-unary-classical (tok-type tok-lines)
  (check-type tok-type (member :NEG :NOT))
  (match-line ((instr tok-type) addr) tok-lines
    (let ((target (parse-memory-or-formal-token addr)))
      (ecase tok-type
        (:NEG (make-instance 'classical-negate :target target))
        (:NOT (make-instance 'classical-not :target target))))))

(defun parse-binary-classical (tok-type tok-lines)
  (check-type tok-type (member :AND :IOR :XOR :MOVE :EXCHANGE :CONVERT :ADD :SUB
                               :MUL :DIV))
  (match-line ((instr tok-type) left-addr right-addr &rest right-addrs) tok-lines
    (let ((left (parse-memory-or-formal-token left-addr))
          (right (parse-classical-argument (cons right-addr right-addrs))))
      (when (and (member (token-type instr) '(:EXCHANGE :CONVERT))
                 (not (or (typep right 'memory-ref)
                          (typep right 'formal))))
        (quil-parse-error "Second argument of EXCHANGE/CONVERT command expected to be a memory address, but got ~S."
                          (type-of right)))
      (ecase tok-type
        (:AND (make-instance 'classical-and :left left :right right))
        (:IOR (make-instance 'classical-inclusive-or :left left :right right))
        (:XOR (make-instance 'classical-exclusive-or :left left :right right))
        (:MOVE (make-instance 'classical-move :left left :right right))
        (:EXCHANGE (make-instance 'classical-exchange :left left :right right))
        (:CONVERT (make-instance 'classical-convert :left left :right right))
        (:ADD (make-instance 'classical-addition :left left :right right))
        (:SUB (make-instance 'classical-subtraction :left left :right right))
        (:MUL (make-instance 'classical-multiplication :left left :right right))
        (:DIV (make-instance 'classical-division :left left :right right))))))

(defun parse-trinary-classical (tok-type tok-lines)
  (check-type tok-type (member :LOAD :STORE :EQ :GT :GE :LT :LE))
  (match-line ((instr tok-type) target left right &rest rest) tok-lines
    (setf right (cons right rest))
    (flet ((parse-memory-region-name (tok)
             (unless (eql ':NAME (token-type tok))
               (disappointing-token-error tok "a memory region name" ))
             (unless (find (token-payload tok) *memory-region-names* :test #'string=)
               (quil-parse-error "Unknown memory region name ~S."
                                 (token-payload tok)))
             (memory-name (token-payload tok))))
      (ecase tok-type
        ((:LOAD)
         (unless (= 1 (length right))
           (quil-parse-error "Invalid right argument to LOAD."))
         (make-instance 'classical-load
                        :target (parse-memory-or-formal-token target)
                        :left (parse-memory-region-name left)
                        :right (parse-classical-argument right)))
        ((:STORE)
         (make-instance 'classical-store
                        :target (parse-memory-region-name target)
                        :left (parse-memory-or-formal-token left)
                        :right (parse-classical-argument right)))
        ((:EQ :GT :GE :LT :LE)
         (make-instance
          (ecase tok-type
            (:EQ 'classical-equality)
            (:GT 'classical-greater-than)
            (:GE 'classical-greater-equal)
            (:LT 'classical-less-than)
            (:LE 'classical-less-equal))
          :target (parse-memory-or-formal-token target)
          :left (parse-classical-argument (list left))
          :right (parse-classical-argument right)))))))

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
      ;; EOF is implicit :DEDENT. This is useful for, say, concatenating on
      ;; DEFCALs at the end of a program
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

(defun parse-parameters (params-args &key allow-expressions)
  "Parse a list of parameters, surrounded by a pair of parentheses. Returns the parsed parameters and the remaining tokens.

When ALLOW-EXPRESSIONS is set, we allow for general arithmetic expressions in a parameter context."
  (unless (eql ':LEFT-PAREN (token-type (first params-args)))
    (return-from parse-parameters (values nil params-args)))

  ;; Remove :LEFT-PAREN
  (pop params-args)

  ;; Parse out the parameters enclosed.
  (multiple-value-bind (found-params rest-line)
      (take-while-from-end (lambda (x) (eql ':RIGHT-PAREN (token-type x)))
                           params-args)

    ;; Error if we didn't find a right parenthesis.
    (when (endp rest-line)
      (quil-parse-error "No matching right paren in~@[ ~A~] parameters" *parse-context*))

    ;; Remove right paren and stash away params.
    (pop rest-line)

    ;; Some sanity checks for the parameter list. Must be of odd length,
    ;; and every other token should be a :COMMA
    (unless (or allow-expressions
                (and (oddp (length found-params))
                     (loop :for c :in (rest found-params) :by #'cddr
                           :always (eql ':COMMA (token-type c)))))
      (quil-parse-error "Malformed parameter list~@[ in ~A~]: ~A" *parse-context* found-params))

    ;; Parse out the parameters.
    (let ((entries
            (split-sequence:split-sequence-if
             (lambda (tok)
               (eq ':COMMA (token-type tok)))
             found-params))
          (parse-op
            (if allow-expressions
                #'parse-parameter-or-expression
                (lambda (toks)
                  (parse-parameter (first toks))))))
      (values (mapcar parse-op entries)
              rest-line))))

(defun parse-arguments (params-args)
  ;; Parse out until we hit :AS or :COLON.
  (multiple-value-bind (found-args rest-line)
      (take-until (lambda (x)
                    (or (eql ':AS (token-type x))
                        (eql ':COLON (token-type x))
                        ;; do this pop last
                        (not (pop params-args))))
                  params-args)
    ;; Make sure we have tokens left over: the line can't end in this state.
    (when (null rest-line)
      (quil-parse-error "Unterminated argument list in DEFGATE."))

    ;; All the intermediate tokens in found-args should be formal qubit names
    (let ((args (loop :for a :in found-args
                      :unless (eql ':NAME (token-type a))
                        :do (quil-parse-error "Found something other than a formal qubit name in a DEFGATE argument list: ~A" a)
                      :collect (parse-formal a))))
      (values args rest-line))))

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
  (a:define-constant +quil<->lisp-functions+
      '(("SIN"  . cl:sin)
        ("COS"  . cl:cos)
        ("SQRT" . cl:sqrt)
        ("EXP"  . cl:exp)
        ("CIS"  . cl:cis))
    :test #'equal
    :documentation
    "Functions usable from within Quil, and their associated Lisp function symbols.")

  ;;; If you add a new arithmetic operator to +QUIL<->LISP-PREFIX-ARITHMETIC-OPERATORS+ or
  ;;; +QUIL<->LISP-INFIX-ARITHMETIC-OPERATORS+, you must also add it to *ARITHMETIC-GRAMMAR*, below.
  (a:define-constant +quil<->lisp-prefix-arithmetic-operators+
      '(("-" . cl:-))
    :test #'equal
    :documentation
    "Prefix arithmetic operators usable from within Quil, and their associated Lisp function symbols.")

  (a:define-constant +quil<->lisp-infix-arithmetic-operators+
      '(("+" . cl:+)
        ("-" . cl:-)
        ("/" . cl:/)
        ("*" . cl:*)
        ("^" . cl:expt))
    :test #'equal
    :documentation
    "Infix arithmetic operators usable from within Quil, and their associated Lisp function symbols.")

  (defun %lisp->quil (lisp-symbol alist)
    (check-type lisp-symbol symbol)
    (a:when-let ((found (rassoc lisp-symbol alist :test #'eq)))
      (car found)))

  (defun %quil->lisp (quil-string alist)
    (check-type quil-string string)
    (a:when-let ((found (assoc quil-string alist :test #'string-equal)))
      (cdr found)))

  ;;; The following functions handle conversion between Quil's arithmetic operators/functions and
  ;;; the corresponding lisp symbols (fbound to lisp functions) that are used in CL-QUIL for
  ;;; evaluating Quil's arithmetic expressions. The mapping from lisp->Quil and Quil->lisp is
  ;;; determined by the above tables, namely: +QUIL<->LISP-FUNCTIONS+,
  ;;; +QUIL<->LISP-PREFIX-ARITHMETIC-OPERATORS+, and +QUIL<->LISP-INFIX-ARITHMETIC-OPERATORS+.
  ;;;
  ;;; For example, the Quil infix operator "/" in an expression like "pi/8" maps to the Common Lisp
  ;;; symbol CL:/ and vice versa. Likewise for the prefix operator "-" in "-%theta" which maps to
  ;;; CL:-.
  ;;;
  ;;; The purpose of the following functions is to provide a layer of abstraction around the
  ;;; conversion to/from Quil<->lisp and to act as a single source of truth for such conversions.
  ;;;
  ;;; Here is a glossary of the terms used in the following function names:
  ;;;
  ;;; lisp-symbol:
  ;;;     a SYMBOL which is fbound to a lisp function appropriate for evaluating the corresponding
  ;;;     Quil function or arithmetic operator.
  ;;;
  ;;; quil-function:
  ;;;     a STRING that denotes a Quil arithmetic function. For example "SIN", "COS", "EXP", etc.
  ;;;     See the table +QUIL<->LISP-FUNCTIONS+ for the list of valid functions.
  ;;;
  ;;; quil-prefix-operator:
  ;;;     a STRING that denotes a Quil prefix (unary) arithmetic operator. For example, the "-" in
  ;;;     the expression "-pi/2". See the table +QUIL<->LISP-PREFIX-ARITHMETIC-OPERATORS+ for the
  ;;;     list of valid prefix operators.
  ;;;
  ;;; quil-infix-operator:
  ;;;     a STRING that denotes a Quil infix (binary) arithmetic operator. For example, the "-" in
  ;;;     the expression "COS(%x) - i * SIN(%x)". See +QUIL<->LISP-INFIX-ARITHMETIC-OPERATORS+ for
  ;;;     the list of valid infix operators.

  (defun lisp-symbol->quil-prefix-operator (symbol)
    (%lisp->quil symbol +quil<->lisp-prefix-arithmetic-operators+))

  (defun quil-prefix-operator->lisp-symbol (quil-prefix-operator)
    (%quil->lisp quil-prefix-operator +quil<->lisp-prefix-arithmetic-operators+))

  (defun lisp-symbol->quil-infix-operator (symbol)
    (%lisp->quil symbol +quil<->lisp-infix-arithmetic-operators+))

  (defun quil-infix-operator->lisp-symbol (quil-infix-operator)
    (%quil->lisp quil-infix-operator +quil<->lisp-infix-arithmetic-operators+))

  (defun lisp-symbol->quil-function (symbol)
    (%lisp->quil symbol +quil<->lisp-functions+))

  (defun quil-function->lisp-symbol (quil-function)
    (%quil->lisp quil-function +quil<->lisp-functions+))

  (defun lisp-symbol->quil-function-or-prefix-operator (symbol)
    (or (lisp-symbol->quil-function symbol)
        (lisp-symbol->quil-prefix-operator symbol)))

  (defun valid-quil-function-or-operator-p (symbol)
    (not (null (or (lisp-symbol->quil-function symbol)
                   (lisp-symbol->quil-prefix-operator symbol)
                   (lisp-symbol->quil-infix-operator symbol)))))

  (defun binary (head)
    (lambda (a i0 b)
      (declare (ignore i0))
      (list head a b)))

  (defun validate-function (func-name)
    "Return the lisp symbol that corresponds to the Quil function named FUNC-NAME, or signal a QUIL-PARSE-ERROR if FUNC-NAME is invalid."
    (or (quil-function->lisp-symbol func-name)
        (quil-parse-error "Invalid function name: ~A." func-name)))

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
        (error "Reference to unknown memory region ~A." region-name))
      (setf *segment-encountered* t)
      (mref region-name region-position))))

(yacc:define-parser *arithmetic-grammar*
  (:start-symbol expr)
  (:terminals (:LEFT-PAREN :RIGHT-PAREN
               :NAME :PARAMETER :INTEGER :COMPLEX
               :PLUS :MINUS :TIMES :DIVIDE :EXPT :AREF))
  (:precedence ((:right :EXPT) (:left :TIMES :DIVIDE) (:left :PLUS :MINUS)))

  (expr
   ;; If you add a new arithmetic operator here, you must also add it to
   ;; +quil<->lisp-infix-arithmetic-operators+, above.
   (expr :PLUS expr (binary (quil-infix-operator->lisp-symbol "+")))
   (expr :MINUS expr (binary (quil-infix-operator->lisp-symbol "-")))
   (expr :TIMES expr (binary (quil-infix-operator->lisp-symbol "*")))
   (expr :DIVIDE expr (binary (quil-infix-operator->lisp-symbol "/")))
   (expr :EXPT expr (binary (quil-infix-operator->lisp-symbol "^")))
   term)

  (term
   (:MINUS expr
           (lambda (i0 x)
             (declare (ignore i0))
             (list (quil-prefix-operator->lisp-symbol "-") x)))
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
