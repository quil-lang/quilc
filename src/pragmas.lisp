;;;; pragmas.lisp

(in-package #:cl-quil.frontend)

(define-pragma "PRESERVE_BLOCK" pragma-preserve-block
  (:documentation "PRAGMA denoting the start of a basic-block immune to the optimizing compiler.

Expected syntax: PRAGMA PRESERVE_BLOCK"))

(define-pragma "END_PRESERVE_BLOCK" pragma-end-preserve-block
  (:documentation "PRAGMA denoting the end of a basic-block immune to the optimizing compiler.

Expected syntax: PRAGMA END_PRESERVE_BLOCK"))

(define-pragma "COMMUTING_BLOCKS" pragma-commuting-blocks
  (:documentation "PRAGMA denoting the start of a COMMUTING_BLOCKS region, which contains several sub-BLOCK regions.  Used to inform the formation of logical schedules about extra commutativity relations.

Expected syntax: PRAGMA COMMUTING_BLOCKS"))

(define-pragma "END_COMMUTING_BLOCKS" pragma-end-commuting-blocks
  (:documentation "PRAGMA denoting the end of a COMMUTING_BLOCKS region."))

(define-pragma "BLOCK" pragma-block
  (:documentation "PRAGMA denoting the start of a BLOCK region, a generic child of a parent BLOCKS region (e.g., COMMUTING_BLOCKS).

Expected syntax: PRAGMA BLOCK"))

(define-pragma "END_BLOCK" pragma-end-block
  (:documentation "PRAGMA denoting the end of a BLOCK region.

Expected syntax: PRAGMA END_BLOCK"))

(define-pragma "ADD-KRAUS" pragma-add-kraus
  (:documentation
   "PRAGMA that introduces a Kraus noise channel on a fixed instruction.

Expected syntax: PRAGMA ADD-KRAUS operator qubit-list \"(matrix entries)\"")
  (:global t)
  (:slots operator-name qubit-arguments matrix-entries)
  (:words (operator string) &rest (args integer-list))
  (:freeform-string freeform-string)
  (:initialization
   (setf operator-name operator)
   (setf qubit-arguments args)
   (setf matrix-entries (parse-element-list freeform-string
                                            (expt 4 (length args)))))
  (:display-string
   (format nil "~A~{ ~A~} \"~A\""
           operator-name
           qubit-arguments
           (format-complex-list matrix-entries))))

(define-pragma "READOUT-POVM" pragma-readout-povm
  (:documentation "PRAGMA that introduces measurement noise on a particular qubit.

Expected syntax: PRAGMA READOUT-POVM qubit \"(matrix entries)\"")
  (:global t)
  (:slots qubit-index matrix-entries)
  (:words (index integer))
  (:freeform-string freeform-string)
  (:initialization
   (setf qubit-index index)
   (setf matrix-entries
         (parse-element-list freeform-string 4 'double-float)))
  (:display-string
   (format nil "~D \"~A\""
           qubit-index
           (format-complex-list matrix-entries))))

(define-pragma "EXPECTED_REWIRING" pragma-expected-rewiring
  (:documentation "PRAGMA denoting a desired logical-to-physical remapping state, typically found at the start of a control block. This has no effect on Quil code and is included only for human readability, where the human is expected to have as reference logically-specified Quil code for comparison.

Expected syntax: PRAGMA EXPECTED_REWIRING \"#(qubit permutation vector)\"")
  (:slots (rewiring rewiring))
  (:freeform-string rewiring-string)
  (:initialization
   (setf rewiring (let ((*read-eval* nil))
                    (make-rewiring-from-l2p (read-from-string rewiring-string)))))
  (:display-string
   (let ((*print-pretty* nil))
     (prin1-to-string (prin1-to-string (rewiring-l2p rewiring))))))

(define-pragma "CURRENT_REWIRING" pragma-current-rewiring
  (:documentation "PRAGMA denoting the current state of a logical-to-physical remapping, typically found at the end of a control block. This has no effect on Quil code and is included only for human readability, where the human is expected to have as reference logically-specified Quil code for comparison.

Expected syntax: PRAGMA CURRENT_REWIRING \"#(qubit permutation vector)\"")
  (:slots (rewiring rewiring))
  (:freeform-string rewiring-string)
  (:initialization
   (setf rewiring (let ((*read-eval* nil))
                    (make-rewiring-from-l2p (read-from-string rewiring-string)))))
  (:display-string
   (let ((*print-pretty* nil))
     (prin1-to-string (prin1-to-string (rewiring-l2p rewiring))))))

(define-pragma "INITIAL_REWIRING" pragma-initial-rewiring
  (:documentation "PRAGMA denoting what style of initial rewiring the compiler
  should use. This can only appear at the start of a program.

Expected syntax: PRAGMA INITIAL_REWIRING [NAIVE|PARTIAL|GREEDY|RANDOM]")
  (:global t)
  (:slots (rewiring-type (member :naive :random :partial :greedy)))
  (:freeform-string rewiring-type-string)
  (:initialization
   (setf rewiring-type
         (cond
           ((string= rewiring-type-string "NAIVE")
            ':naive)
           ((string= rewiring-type-string "RANDOM")
            ':random)
           ((string= rewiring-type-string "PARTIAL")
            ':partial)
           ((string= rewiring-type-string "GREEDY")
            ':greedy)
           (t
            (error "Invalid PRAGMA INITIAL_REWIRING: ~A" rewiring-type-string)))))
  (:display-string
   (prin1-to-string (symbol-name rewiring-type))))


(define-pragma "REWIRING_SEARCH" pragma-rewiring-search
  (:documentation "PRAGMA denoting the search strategy to be used for selecting the
SWAPs that bring a logical-to-physical rewiring to a target rewiring.

Compilation resource requirements may vary according to the rewiring search type used. 

Expected syntax: PRAGMA REWIRING_SEARCH [\"A*\"|\"GREEDY-QUBIT\"|\"GREEDY-PATH\"]")
  (:global t)
  (:slots (swap-search-type cl-quil::addresser-search-type))
  (:freeform-string rewiring-swap-search-type-string)
  (:initialization
   (setf swap-search-type
         (cond ((string= rewiring-swap-search-type-string "A*") ':a*)
               ((string= rewiring-swap-search-type-string "GREEDY_QUBIT") ':greedy-qubit)
               ((string= rewiring-swap-search-type-string "GREEDY_PATH") ':greedy-path)
               (t
                (error "Invalid PRAGMA REWIRING_SEARCH: ~A" rewiring-swap-search-type-string)))))
  (:display-string
   (prin1-to-string (symbol-name swap-search-type))))


(define-pragma "NON_VOLATILE" pragma-non-volatile
  (:documentation "PRAGMA indicating that declared memory can be assumed to be non-volatile - it will not be modified outside of the Quil program in which it is declared.

Expected syntax: PRAGMA NON_VOLATILE identifier")
  (:global t)
  (:slots (memory-name cl-quil::memory-name))
  (:words (name string))
  (:initialization
   (setf memory-name (cl-quil::memory-name name)))
  (:display-string
   (princ-to-string (cl-quil::memory-name-region-name memory-name))))


(defun tokenize-extern-signature (input)
  (let ((pos 0)) 
    (labels ((peek () (if (< pos (length input)) (elt input pos) nil))
             (next () (prog1 (peek) (incf pos)))
             (skip-whitespace ()
               (loop :for c := (peek)
                     :while (and c (eq #\Space c))
                     :do (incf pos)))
             (next-token ()
               (skip-whitespace)
               (a:if-let (next-char (next))
                 (case next-char
                   (#\( ':LEFT-PAREN)
                   (#\) ':RIGHT-PAREN)
                   (#\: ':COLON)
                   (#\, ':COMMA)
                   (#\[ ':LEFT-BRACKET)
                   (#\] ':RIGHT-BRACKET)
                   (otherwise
                    (let ((token 
                            (with-output-to-string (token)
                              (princ next-char token)
                              (loop :for c := (next)
                                    :while c
                                    :until (find c "[]():, ")
                                    :do (write-char c token)
                                    :finally (when c (decf pos))))))
                      (cond ((equal "mut" token)
                             ':MUT)
                            ((every
                              (load-time-value
                               (a:conjoin (complement #'alpha-char-p) #'alphanumericp)
                               t)
                              token)
                             (values ':INT (parse-integer token)))
                            (t
                             (values ':WORD token))))))
                 nil)))
      #'next-token)))

(yacc:define-parser *extern-signature-grammar*
  (:start-symbol signature)
  (:terminals (:LEFT-PAREN :RIGHT-PAREN
               :LEFT-BRACKET :RIGHT-BRACKET
               :COMMA :COLON :MUT :INT :WORD))

  (signature
   (:WORD :LEFT-PAREN paramlist :RIGHT-PAREN
          (lambda (&rest args) (list :value-type (first args) :param-types (third args))))
   (:LEFT-PAREN paramlist :RIGHT-PAREN
                (lambda (&rest args) (list :param-types (second args)))))

  (paramlist
   (param :COMMA paramlist
          (lambda (p i0 ps) (declare (ignore i0))
            (cons p ps)))
   (param (lambda (p) (list p))))
  
  (param
   (:WORD :COLON :MUT type
          (lambda (var i0 mut type) (declare (ignore i0 mut)) (list* var ':mut type)))
   (:WORD :COLON type
          (lambda (var i0 type) (declare (ignore i0)) (cons var type))))

  (type
   (:WORD)
   (:WORD :LEFT-BRACKET :RIGHT-BRACKET
          (lambda (word rb lb)
            (declare (ignore rb lb))
            (list (string-to-quil-type word) '*)))
   (:WORD :LEFT-BRACKET :INT :RIGHT-BRACKET
          (lambda (word rb size lb)
            (declare (ignore rb lb))
            (list (string-to-quil-type word) size)))))

(define-pragma "EXTERN" pragma-extern-signature
  (:documentation "PRAGMA declaring the function signature of an extern.

Expected syntax: PRAGMA EXTERN extern-name \"TYPE? \( (var : mut? TYPE)+ \)")
  (:global t)
  (:slots extern-name value-type param-types)
  (:words name)
  (:freeform-string function-signature-string)
  (:initialization
   (handler-case (let ((parsed (yacc:parse-with-lexer
                                (tokenize-extern-signature function-signature-string)
                                *extern-signature-grammar*)))
                   (setf extern-name name)
                   (setf value-type (getf parsed :value-type))
                   (setf param-types (getf parsed :param-types)))
     (yacc:yacc-parse-error (err)
       (warn "Syntax error while parsing PRAGMA EXTERN: ~s~%    Error: ~a"
             function-signature-string
             (princ-to-string err)))
     (error (err)
       (warn "Error while parsing PRAGMA EXTERN: ~s~%   Error: ~a"
             function-signature-string
             (princ-to-string err))))))

(defun parsed-program-has-pragma-p (parsed-program &optional (pragma-type 'pragma))
  "Return T if PARSED-PROGRAM's executable code contains any pragma. Optionally use PRAGMA-TYPE to restrict to a particular pragma type."
  (some (a:rcurry #'typep pragma-type)
        (parsed-program-executable-code parsed-program)))

(defun parsed-program-has-preserve-blocks-p (parsed-program)
  "Return T if PARSED-PROGRAM's executable code contains a pragma of type PRAGMA-PRESERVE-BLOCK."
  (parsed-program-has-pragma-p parsed-program 'pragma-preserve-block))
