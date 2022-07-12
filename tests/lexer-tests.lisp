;;;; tests/lexer-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil-tests)

(defparameter *lexer-tests*
  '(;; numbers
    ("1" :integer)
    ("0.1" :complex)
    ("1e2" :complex)
    ("1e-2" :complex)
    ("1E+2" :complex)
    ("1.0+2.0i" :complex :plus :complex)
    ("-1.0+2.0i" :minus :complex :plus :complex)
    ("-1.0e+20-.7E-10i" :minus :complex :minus :complex)
    ;; labels and parameters
    ("@FOO" :label-name)
    ("%bar-baz" :parameter)
    ;; keywords
    ("INCLUDE" :include)
    ("PRAGMA" :pragma)
    ("DEFCIRCUIT" :defcircuit)
    ("DEFGATE" :defgate)
    ("MEASURE" :measure)
    ("LABEL" :label)
    ("WAIT" :wait)
    ("NOP" :nop)
    ("HALT" :halt)
    ("RESET" :reset)
    ("NOT AND IOR XOR" :not :and :ior :xor)
    ("NEG ADD SUB MUL DIV" :neg :add :sub :mul :div)
    ("LOAD STORE CONVERT" :load :store :convert)
    ("DECLARE SHARING OFFSET" :declare :sharing :offset)
    ("EQ GT GE LT LE" :EQ :GT :GE :LT :LE)
    ("JUMP-WHEN" :jump-when)
    ("JUMP-UNLESS" :jump-unless)
    ("JUMP" :jump)
    ;; memory references
    ("a[1]" :aref)
    ("abcd[123]" :aref)
    ;; constants
    ("pi" :complex)
    ("i" :complex)
    ("pi-rotation" :name)
    ("ii" :name)
    ;; test for CORESW-375 in which identifiers beginning with
    ;; keywords were falsely tokenized
    ("ORACLE-2" :name)
    ("ANDARE-2" :name)
    ("AND-2" :name)
    ("NOPE" :name)
    ;; Modifiers
    ("CONTROLLED" :controlled)
    ("DAGGER" :dagger)

    ;; newline tests
    (#.(format nil "~%") :newline)
    ;; quilc bug
    (#.(format nil "X 0 ~% Y 0~%Z 0  ~%   ")
     :name :integer :newline
     :name :integer :newline
     :name :integer :newline)

    ;; Test single character names
    ("X" :name)
    ;; indentation
    ("    " :indentation)
    ("        " :indentation)
    (#.(string #\Tab) :indentation)
    (#.(format nil "~C    ~C    " #\Tab #\Tab) :indentation)))

(defun token-type-or-newline (tok)
  (if (eq tok ':newline)
      ':newline
      (cl-quil::token-type tok)))

(deftest test-lexer ()
  "Test different combinations of lexical analysis."
  (declare (optimize (space 0) debug))
  (loop :for (test-string . expected-types) :in *lexer-tests*
        :for lexed := (cl-quil/frontend::tokenize-line #'cl-quil/frontend::line-lexer test-string)
        :do (is (and (= (length expected-types) (length lexed))
                     (equal expected-types (mapcar #'token-type-or-newline lexed)))
                "Failed lex for ~S => ~A"
                test-string
                lexed)))
