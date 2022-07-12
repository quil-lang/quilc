;;;; define-pragma.lisp
;;;;
;;;; Author: Zach Beane

(in-package #:cl-quil/frontend)

(defvar *pragma-definitions* (make-hash-table :test 'equal)
  "A table mapping a pragma Quil name to a specialized pragma class.")

(defgeneric pdef-class-name (pdef))
(defgeneric pdef-class-documentation (pdef))
(defgeneric pdef-name (pdef))
(defgeneric pdef-slot-names (pdef))
(defgeneric pdef-slot-types (pdef))
(defgeneric pdef-word-lambda-list (pdef))
(defgeneric pdef-word-names (pdef))
(defgeneric pdef-word-types (pdef))
(defgeneric pdef-freeform-string-p (pdef))
(defgeneric pdef-freeform-string-name (pdef))
(defgeneric pdef-initialization-code (pdef))
(defgeneric pdef-display-string-code (pdef))
(defgeneric pdef-global-p (pdef))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pdef-keywordize (slot-name)
  (intern (symbol-name slot-name) :keyword))

(defun pdef-accessor-name (slot-name)
  (a:format-symbol (symbol-package slot-name) "PRAGMA-~A" slot-name))

(defun format-complex-list (list)
  (format nil "(~{~A~^ ~})"
          (mapcar (lambda (n) (format nil "~/cl-quil:complex-fmt/" n))
                  list)))

;; Dedicated element list lexer contributed by Zach Beane
(alexa:define-string-lexer element-list-lexer
  ((:number "(?:(?=\\d*[.eE])(?=\\.?\\d)\\d*\\.?\\d*(?:[eE][+-]?\\d+)?|\\d+)"))
  ("(?<REAL>[+-]?{{NUMBER}})(?<IMAG>[+-]{{NUMBER}})i|(?<JUSTIMAG>[+-]?{{NUMBER}})i|(?<JUSTREAL>[+-]?{{NUMBER}})"
   (return
     (cond
       ((and $REAL $IMAG)
        (parse-complex $REAL $IMAG))
       ($JUSTREAL
        (parse-complex $JUSTREAL nil))
       ($JUSTIMAG
        (parse-complex nil $JUSTIMAG)))))
  ("\\s+" nil)
  ("[()]" nil)
  ("-pi"  (return (- pi)))
  ("pi"   (return pi))
  ("i"    (return #C(0.0d0 1.0d0)))
  ("-i"   (return #C(-0.0d0 -1.0d0))))

(defun parse-element-list (operator-def num-elements
		       &optional (elt-type '(complex double-float)))
  "Parse OPERATOR-DEF, the freeform-string of an ADD-KRAUS or
READOUT-POVM pragma statement, and return a list of elements. Ensure
that the length of the returned list is NUM-ELEMENTS. Coerce all
elements to ELT-TYPE.
"
  (when (zerop (length operator-def))
    (quil-parse-error "Malformed element list: ~
                       Received empty string."))
  (unless (char= (char operator-def 0) #\()
    (quil-parse-error "Malformed element list: missing open parenthesis."))
  (unless (char= (char operator-def (1- (length operator-def))) #\))
    (quil-parse-error "Malformed element list: missing close parenthesis."))
  (handler-case
      (let ((raw-elements (tokenize-line #'element-list-lexer operator-def)))
        (unless (= (length raw-elements) num-elements)
          (quil-parse-error "Malformed element list: ~
			     expected ~D matrix element~:P, got ~D."
                            num-elements
                            (length raw-elements)))
        (mapcar (lambda (raw) (coerce raw elt-type)) raw-elements))
    (alexa:lexer-match-error (c)
      (quil-parse-error "Lexer failure: ~A" c))))


;;;;;;;;;;;;;;;;;;;; Macroexpansion-time checking ;;;;;;;;;;;;;;;;;;;;

(defparameter *defpragma-option-keywords*
  '(:documentation
    :slots
    :words
    :freeform-string
    :initialization
    :display-string
    :global))

(defun check-defpragma-options-list (options-list)
  "Check that a OPTIONS-LIST, of the form ((:option1 value1) (:option2
value2) ...), contains only valid defpragma option keywords, and
contains no duplicates."
  (let* ((provided-options (mapcar #'first options-list))
         (unknown-options (set-difference provided-options
                                          *defpragma-option-keywords*)))
    (when unknown-options
      (error "Unknown defpragma option~P:~{ ~S~}."
             (length unknown-options)
             unknown-options))
    (maplist (lambda (s)
               (when (member (first s) (rest s))
                 (error "Duplicate defpragma option: ~S." (first s))))
             provided-options)
    t))

(defvar *pdef-forbidden-lambda-list-keywords*
  (remove '&rest lambda-list-keywords))

(defun check-defpragma-lambda-list (lambda-list)
  "Signal an error if LAMBDA-LIST is unsuitable for pragma argument handling."
  (let ((forbidden (intersection
                    *pdef-forbidden-lambda-list-keywords*
                    lambda-list))
        (rest (position '&rest lambda-list)))
    (when forbidden
      (error "Forbidden lambda list keyword~P in pragma lambda list: ~{~S~^ ~}."
             (length forbidden) forbidden))
    (when rest
      (unless (= rest (- (length lambda-list) 2))
        (error "&REST in bad position in ~S" lambda-list)))))

(defun check-defpragma-names-compatibility (slot-names
                                            word-names
                                            freeform-string-name)
  (let ((bad-names (intersection slot-names  (cons freeform-string-name
                                                   word-names))))
    (when bad-names
      (error "Cannot share names between slots and words/freeform-string -- ~S."
             bad-names))))


;;;;;;;;;;;;;;;;;;;;;;;;;; Code generators ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pdef-check-pragma-arguments-form (pdef)
  (let* ((lambda-list (pdef-word-lambda-list pdef))
         (rest (position '&rest lambda-list))
         (operator (if rest '<= '=))
         (value (or rest (length lambda-list)))
         (name (pdef-name pdef))
         (pragma-class (pdef-class-name pdef )))
    (flet ((error-form (message)
             `(error ,(format nil "Malformed pragma ~A -- ~A."
                              name message))))
      (a:with-gensyms (class words freeform-string)
        `(defmethod check-pragma-arguments ((,class (eql ',pragma-class))
                                           ,words ,freeform-string)
           (unless (,operator ,value (length ,words))
             ;; TODO: Improve this error message with expected/actual info?
             ,(error-form "argument count mismatch"))
           ,(if (pdef-freeform-string-p pdef)
                `(unless ,freeform-string
                   ,(error-form "freeform string missing"))
                `(when ,freeform-string
                           ,(error-form "freeform string provided"))))))))

(defun pdef-pragma-specalized-initargs-form (pdef)
  (a:with-gensyms (class words freeform-string)
    (let ((pragma-class (pdef-class-name pdef))
          (code (pdef-initialization-code pdef))
          (lambda-list (pdef-word-lambda-list pdef))
          (slot-names (pdef-slot-names pdef))
          (slot-types (pdef-slot-types pdef))
          (freeform-string-var
           (pdef-freeform-string-name pdef)))
      `(defmethod pragma-specialized-initargs ((,class (eql ',pragma-class))
                                               ,words
                                               ,freeform-string)
         (let (,@slot-names
               ,@ (when freeform-string-var
                    (list `(,freeform-string-var ,freeform-string))))

           (destructuring-bind ,lambda-list
               ,words
             (progn
               ,@(mapcan (lambda (word-name type)
                           (unless (eql type t)
                             (list `(check-type ,word-name ,type))))
                         (pdef-word-names pdef)
                         (pdef-word-types pdef)))
             (progn ,@code))
           ,@ (mapcan (lambda (name type)
                        (unless (eql type t)
                          (list `(check-type ,name ,type))))
                      slot-names
                      slot-types)
              (list :name ,(pdef-name pdef )
                    ,@(loop :for slot-name :in slot-names
                        :collect (pdef-keywordize slot-name)
                        :collect slot-name)))))))

(defun pdef-pragma-display-string-form (pdef)
  (let ((class-name (pdef-class-name pdef))
        (code (pdef-display-string-code pdef)))
    (when code
      (a:with-gensyms (pragma)
        `(defmethod pragma-display-string ((,pragma ,class-name))
           (let ,(mapcar (lambda (slot)
                           `(,slot (,(pdef-accessor-name slot) ,pragma)))
                         (pdef-slot-names pdef))
             (format nil "~A ~A"
                     (pragma-name ,pragma)
                     (progn ,@code))))))))

(defun pdef-defclass-form (pdef)
  (let ((class-name (pdef-class-name pdef))
        (documentation (pdef-class-documentation pdef))
        (name (pdef-name pdef)))
    `(defclass ,class-name
         (specialized-pragma)
       (,@ (mapcar (lambda (slot)
                     `(,slot :initarg ,(pdef-keywordize slot)
                             :accessor ,(pdef-accessor-name slot)))
                   (pdef-slot-names pdef)))
       ,@(when documentation
           (list `(:documentation
                   ,documentation)))
       (:default-initargs
        :name ,name))))

(defgeneric global-pragma-instruction-p (obj)
  (:documentation "Is OBJ a PRAGMA instruction that affects the program globally?")
  (:method ((obj t))
    nil))

(defun pdef-record-global-pragma-form (pdef)
  ;; We generate a DEFMETHOD regardless so re-evaluation works.
  `(defmethod global-pragma-instruction-p ((obj ,(pdef-class-name pdef)))
     (declare (ignore obj))
     ',(pdef-global-p pdef)))


;;;;;;;;;;;;;;;;;;;;;;;; Class implementation ;;;;;;;;;;;;;;;;;;;;;;;;

(defclass pdef ()
  ((class-name :initarg :class-name
               :reader pdef-class-name)
   (class-documentation :initarg :class-documentation
                        :reader pdef-class-documentation)
   (name :initarg :name
         :reader pdef-name)
   (slot-names :initarg :slot-names
               :reader pdef-slot-names)
   (slot-types :initarg :slot-types
               :reader pdef-slot-types)
   (word-lambda-list :initarg :word-lambda-list
                     :reader pdef-word-lambda-list)
   (word-names :initarg :word-names
               :reader pdef-word-names)
   (word-types :initarg :word-types
               :reader pdef-word-types)
   (freeform-string-name :initarg :freeform-string-name
                         :reader
                         pdef-freeform-string-name)
   (initialization-code :initarg :initialization-code
                        :reader pdef-initialization-code)
   (display-string-code :initarg :display-string-code
                        :reader pdef-display-string-code)
   (global :initarg :global
           :reader pdef-global-p)))

(defmethod pdef-freeform-string-p (pdef)
  (not (null (pdef-freeform-string-name pdef))))


;;;;;;;;;;;;;;;;;;;;;;;;;; Macro definition ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-pragma (name class-name &body pragma-options)
  "Define a specialized pragma instruction for NAME to map to
specialized pragma class CLASS-NAME. Each PRAGMA-OPTION takes the
form (:keyword value). Accepted options are:

  (:documentation <string>) -- If provided, assigns <string> as
  documentation for the specialized class.

  (:slots <slot-name|(slot-name slot-type)>+) -- Define slots in the
  specialized pragma class. If a slot-type is provided for a slot, it
  is checked before converting a generic pragma instruction to the
  specialized class.

  (:words <word-lambda-list>) -- Define bindings in the scope of any
  initialization code to the pragma words of the generic pragma
  instruction. Syntax is similar to an ordinary lambda list, except
  each variable may be specified as either a symbol or a list
  of (symbol type-name). If a type-name is provided, the word's type
  is checked before converting a generic pragma instruction to the
  specialized class. &rest is allowed to collect remaining arguments,
  and it may also have an optional type specification with the same
  specialization constraints.

  (:freefrom-string <name>) -- Bind the symbol NAME in the scope of
  any initialization code to the freeform string of the generic pragma
  instruction.

  (:initialization <code>) -- During specialization, evaluate <code>
  to initialize the slots of the specialized instance. Bindings for
  slots (as variables with the same name as the slot name), words (per
  the :words lambda list), and freeform string are in scope. Code
  should assign a value each slot variable. Multiple forms are
  allowed.

  (:global <boolean>) -- Specifies whether the pragma affects the
  entire program, and must be persisted through different
  transformations. (Default: NIL)

  (:display <code>) -- When printing a specialized instruction, <code>
  should evaluate to a string that displays the data of the
  specialized pragma. Slot variables are in scope. Multiple forms are
  allowed.
"
  (flet ((thing-type (thing)
           (if (consp thing)
               (second thing)
               t))
         (thing-name (thing)
           (if (consp thing)
               (first thing)
               thing))
         (option-value (key)
           (rest (assoc key pragma-options))))
    (check-defpragma-options-list pragma-options)
    (let* ((words (option-value :words))
           (slots (option-value :slots))
           (global (first (option-value :global)))
           (freeform-string-name (first (option-value :freeform-string)))
           (raw-words (remove '&rest words))
           (word-lambda-list (mapcar #'thing-name words))
           (word-names (mapcar #'thing-name raw-words))
           (word-types (mapcar #'thing-type raw-words))
           (slot-names (mapcar #'thing-name slots))
           (pdef
            (make-instance 'pdef
                           :class-name class-name
                           :class-documentation
                           (first (option-value :documentation))
                           :name name
                           :slot-names slot-names
                           :slot-types (mapcar #'thing-type slots)
                           :word-lambda-list word-lambda-list
                           :word-names word-names
                           :word-types word-types
                           :freeform-string-name freeform-string-name
                           :initialization-code
                           (option-value :initialization)
                           :display-string-code
                           (option-value :display-string)
                           :global global)))
      (check-defpragma-lambda-list word-lambda-list)
      (check-defpragma-names-compatibility slot-names
                                           word-names
                                           (option-value :freeform-string))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (find-pdef-specialized-class ,name) ',class-name)
         ,(pdef-defclass-form pdef)
         ,(pdef-check-pragma-arguments-form pdef)
         ,(pdef-pragma-specalized-initargs-form pdef)
         ,(pdef-pragma-display-string-form pdef)
         ,(pdef-record-global-pragma-form pdef)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Driver ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass specialized-pragma (pragma)
  ((name :initarg :name
         :reader pragma-name)))

(defun find-pdef-specialized-class (pragma-name)
  (gethash pragma-name *pragma-definitions*))

(defun (setf find-pdef-specialized-class) (new-value pragma-name)
  (check-type new-value symbol)
  (setf (gethash pragma-name *pragma-definitions*) new-value))

(defgeneric check-pragma-arguments (class words freeform-string)
  (:documentation "Called before pragma specialization. Signals an error if there is an improper number of arguments or an incorrect argument type."))

(defgeneric pragma-specialized-initargs (class words freeform-string)
  (:documentation "Returns a list of initargs to pass to CHANGE-CLASS
  when specializing a generic pragma instruction to one of type
  CLASS."))

(defgeneric pragma-display-string (pragma)
  (:documentation "Returns a string suitable for incorporating into
the display of a specialized pragma instruction.")
  (:method ((pragma specialized-pragma))
    (pragma-name pragma)))

(defmethod print-instruction-generic ((pragma specialized-pragma) (stream stream))
  (format stream "PRAGMA ~A" (pragma-display-string pragma)))

(defgeneric specialize-pragma (instr)
  (:documentation "If INSTR is a pragma instruction with a known
  specialization on its first pragma word, return a specialized pragma
  instruction based on the pragma words and (optional) freeform string
  of the pragma.")
  (:method ((instr pragma))
    (let* ((words (pragma-words instr))
           (name (pop words))
           (freeform-string (pragma-freeform-string instr))
           (new-class (find-pdef-specialized-class name)))
      (cond (new-class
             (check-pragma-arguments new-class
                                     words
                                     freeform-string)
             (let ((initargs (pragma-specialized-initargs new-class
                                                          words
                                                          freeform-string)))
               (apply #'change-class instr new-class initargs)))
            (t
             (format-noise "SPECIALIZE-PRAGMA: Unknown PRAGMA type ~A." name)
             instr)))))
