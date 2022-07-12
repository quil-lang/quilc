;;;; src/cl-quil.lisp
;;;;
;;;; Authors: Robert Smith
;;;           Erik Davis

(in-package #:cl-quil/frontend)

(defvar *standard-post-parsing-transforms*
  '(validate-defgate-loops expand-circuits type-check)
  "The standard transforms that are applied by PARSE-QUIL after parsing. (See also: *STANDARD-PRE-COMPILATION-TRANSFORMS*)")

(defvar *default-ambiguous-definition-handler*
  'warn-on-ambiguous-gate-or-circuit-definition)

(define-condition ambiguous-definition-condition ()
  ((instruction :initarg :instruction
                :reader ambiguous-definition-instruction
                :type instruction
                :documentation "A definition which has has a common signature with some other definition.")
   (file :initarg :file
         :reader ambiguous-definition-file
         :type (or null string)
         :documentation "The file in which the conflicting definition originated.")
   (conflicts :initarg :conflicts
              :reader ambiguous-definition-conflicts
              :type list
              :documentation "A list of (filename . definition) pairs which conflict with the current definition."))
  (:documentation "A condition indicating the presence of multiple conflicting definitions. Here a 'conflict' denotes that the definitions have matching signatures."))

(defun error-on-ambiguous-memory-declaration (condition)
  "Handler which signals an error in the presence of a CONDITION indicating an ambiguous memory declaration."
  (check-type condition ambiguous-definition-condition)
  (let ((instr (ambiguous-definition-instruction condition)))
    (when (typep instr 'memory-descriptor)
      (let* ((name (memory-descriptor-name instr)))
        (quil-parse-error "Memory region ~A~@[ (in ~A)~] has already been DECLAREd~@[ (in ~A)~]."
                          name
                          (ambiguous-definition-file condition)
                          (cdr (first (ambiguous-definition-conflicts condition))))))))

;;; As part of parsing, we need to handle the case when two definitions seem to
;;; conflict. The approach taken here is to map definitions to signatures, with
;;; two signatures EQUALP if there is a context in which both definitions would
;;; be applicable. This ranges from quite simple in the case of memory
;;; definitions (where the basic check is whether the region names are equal),
;;; to a bit more involved for Quilt calibrations (where applicability is
;;; generally determined by a pattern matching system, and so we have to
;;; consider the arity and values of parameters and arguments).

(defun warn-on-ambiguous-gate-or-circuit-definition (condition)
  "Handler which prints a warning in the presence of a CONDITION indicating an ambiguous gate or circuit declaration."
  (check-type condition ambiguous-definition-condition)
  (let ((instr (ambiguous-definition-instruction condition)))
    (typecase instr
      (gate-definition (alexandria:simple-style-warning "Gate ~A has multiple definitions, this leads to ambiguous behavior." (gate-definition-name instr)))
      (circuit-definition (alexandria:simple-style-warning "Circuit ~A has multiple definitions, this leads to ambiguous behavior." (circuit-definition-name instr)))
      (t (alexandria:simple-style-warning "Object ~A (of type ~A) represents a duplicate definition." instr (type-of instr))))
    (continue)))

(defgeneric definition-signature (instr)
  (:documentation "Computes a signature for a Quil definition such that if two definitions are equivalent for the purposes of name resolution, then their signatures are EQUALP.")
  (:method ((instr gate-definition))
    (list 'gate-or-circuit-definition
          (intern (gate-definition-name instr) :keyword)
          (if (typep instr 'parameterized-gate-definition)
              (length (gate-definition-parameters instr))
              0)
          (gate-definition-qubits-needed instr)))

  (:method ((instr circuit-definition))
    (list 'gate-or-circuit-definition
          (intern (circuit-definition-name instr) :keyword)
          (length (circuit-definition-parameters instr))
          (length (circuit-definition-arguments instr))))

  (:method ((instr memory-descriptor))
    (list 'memory-descriptor
          (intern (memory-descriptor-name instr) :keyword)))

  (:method ((instr t))
    nil))

(defun raw-quil-to-unresolved-program (code)
  "This constructs a PARSED-PROGRAM object from the given list of Quil instructions CODE, without any resolution of applications.

This also signals ambiguous definitions, which may be handled as needed."
  ;; Note: the processing below preserves the order of definitions.
  (let ((gate-defs '())
        (circ-defs '())
        (memory-defs '())
        (exec-code '())
        ;; The following maps definition signatures to a list of (filename . defn) pairs
        (all-seen-defns (make-hash-table :test 'equalp)))
    (flet ((bin (instr)
             (a:when-let ((signature (definition-signature instr)))
               (let ((originating-file (typecase (lexical-context instr)
                                         (token
                                          (token-pathname (lexical-context instr)))
                                         (t
                                          nil))))
                 ;; check for conflicts
                 (a:when-let ((entries (gethash signature all-seen-defns)))
                   (cerror "Continue with ambiguous definition."
                           (make-instance 'ambiguous-definition-condition
                                          :instruction instr
                                          :file originating-file
                                          :conflicts entries)))
                 (push (cons instr originating-file)
                       (gethash signature all-seen-defns))))
             (typecase instr
               (gate-definition (push instr gate-defs))
               (circuit-definition (push instr circ-defs))
               (memory-descriptor (push instr memory-defs))
               (t (push instr exec-code)))))
      (mapc #'bin code)
      (make-instance 'parsed-program
                     :gate-definitions (nreverse gate-defs)
                     :circuit-definitions (nreverse circ-defs)
                     :memory-definitions (nreverse memory-defs)
                     :executable-code (coerce (nreverse exec-code)
                                              'simple-vector)))))

(defun %parse-quil (string build-parsed-program &key originating-file
                                                  transforms
                                                  (ambiguous-definition-handler *default-ambiguous-definition-handler*)
                                                  (lexer-extensions '())
                                                  (parser-extensions '()))
  "The actual parsing code. Arguments are as in PARSE-QUIL, except we now have three new ones:

1. BUILD-PARSED-PROGRAM is a function which translates a list of raw ast objects to a PARSED-PROGRAM object,
2. PARSER-EXTENSIONS is a list of parser functions which PARSE-PROGRAM-LINES may dispatch to.
3. LEXER-EXTENSIONS is a list of lexer functions which LINE-LEXER may dispatch to."
  (handler-bind
      ;; We disallow multiple declarations of the same memory region
      ;; (even if equivalent). Otherwise, for gate or circuit
      ;; definitions, the default choice is to "accept the mystery."
      ((ambiguous-definition-condition (a:disjoin
                                        #'error-on-ambiguous-memory-declaration
                                        ambiguous-definition-handler)))
    (let ((*current-file* originating-file)
          (*parser-extensions* parser-extensions)
          (*lexer-extensions* lexer-extensions))
      (let* ((raw-quil (parse-quil-into-raw-program string))
             (pp (resolve-objects
                  (funcall build-parsed-program
                           (process-includes raw-quil originating-file)))))
        (dolist (xform transforms pp)
          (setf pp (transform xform pp)))))))

;; A valid OpenQASM program requires the line "OPENQASM 2.0" possibly
;; preceded by comments (//).
(defun %check-for-qasm-header (string)
  (check-type string string)
  (labels ((line-begins-with (prefix line)
             (when (<= (length prefix) (length line))
               (let* ((start2 (or (position #\Space line :test-not #'eql) 0))
                      (end2 (+ start2 (length prefix))))
                 (string= prefix line :start2 start2 :end2 end2))))
           (commented-line-p (line) (line-begins-with "//" line))
           (qasm-line-p (line) (line-begins-with "OPENQASM 2.0" line)))
    (with-input-from-string (*standard-input* string)
      (loop :for line := (read-line *standard-input* nil) :do
        (cond ((null line)
               ;; EOF encountered before any characters read.
               (return nil))
              ((qasm-line-p line)
               ;; Certainly an OpenQASM program.
               (return t))
              ((or (every (lambda (c) (or (char= #\Space c) (char= #\Tab c)))
                          line)
                   (commented-line-p line))
               ;; Empty or comment line. Continue searching.
               nil)
              (t
               ;; Neither OPENQASM 2.0, comment, or empty line, so
               ;; cannot be legal OpenQASM.
               (return nil)))))))

(defun parse (string &key originating-file
                       (transforms *standard-post-parsing-transforms*)
                       (ambiguous-definition-handler *default-ambiguous-definition-handler*))
  "Parse the input STRING which can be either Quil or OpenQASM code."
  (if (%check-for-qasm-header string)
      (cl-quil/qasm:parse-qasm string)
      (parse-quil string
                  :originating-file originating-file
                  :transforms transforms
                  :ambiguous-definition-handler ambiguous-definition-handler)))

(defun parse-quil (string &key originating-file
                            (transforms *standard-post-parsing-transforms*)
                            (ambiguous-definition-handler *default-ambiguous-definition-handler*))
  "Parse and process the Quil string STRING, which originated from the file ORIGINATING-FILE. Transforms in TRANSFORMS are applied in-order to the processed Quil string.

In the presence of multiple definitions with a common signature, a signal is raised, with the default handler specified by AMBIGUOUS-DEFINITION-HANDLER.
"
  (%parse-quil string
               #'raw-quil-to-unresolved-program
               :originating-file originating-file
               :transforms transforms
               :ambiguous-definition-handler ambiguous-definition-handler))

(defun read-quil-file (filespec)
  "Read the Quil file designated by FILESPEC, and parse it as if by PARSE-QUIL."
  (check-type filespec (or string pathname))
  (assert (and (uiop:file-pathname-p filespec)
               (uiop:file-exists-p filespec))
          (filespec)
          "FILESPEC must be a path to a file that exists")
  (parse-quil (a:read-file-into-string filespec)
              :originating-file (first (directory filespec))))


(defun parse-quil-into-raw-program (string)
  "Parse a string STRING into a list of raw Quil syntax objects."
  (check-type string string)
  (let* ((*memory-region-names* nil)
         (tok-lines (tokenize string)))
    (loop :with parsed-program := nil
          :until (endp tok-lines) :do
            (multiple-value-bind (program-entity rest-toks)
                (parse-program-lines tok-lines)
              (push program-entity parsed-program)
              (setf tok-lines rest-toks))
          :finally (return (nreverse parsed-program)))))

(defvar *safe-include-directory* nil)

(define-condition resolve-filename-safely-condition (quil-parse-error)
  ((filename :initarg :filename :reader resolve-safely-filename)
   (message  :initarg :message  :reader resolve-safely-message))
  (:report (lambda (condition stream)
             (format stream "Cannot safely resolve ~A: ~A."
                     (resolve-safely-filename condition)
                     (resolve-safely-message  condition)))))

(defun resolve-safely (filename)
  "If FILENAME is safe, return it, merged with *SAFE-INCLUDE-DIRECTORY* if set. A safe filename has no :UP or :BACK components in its directory and has filename components present."
  (flet ((contains-up (filename)
           (member-if (lambda (obj)
                        (or (eql ':UP obj)
                            (eql ':BACK obj)))
                      (pathname-directory filename))))
    (cond
      ((uiop:absolute-pathname-p filename)
       (error 'resolve-filename-safely-condition
              :filename filename
              :message "absolute paths disallowed"))

      ((uiop:relative-pathname-p filename)
       ;; Only files allowed.
       (unless (uiop:file-pathname-p filename)
         (error 'resolve-filename-safely-condition
                :filename filename
                :message "provided path does not resolve to a file"))
       (when (contains-up filename)
         (error 'resolve-filename-safely-condition
                :filename filename
                :message "cannot include files from a parent path"))
       (if (null *safe-include-directory*)
           filename
           (merge-pathnames filename *safe-include-directory*)))

      (t
       (error 'resolve-filename-safely-condition
              :filename filename
              :message "invalid pathname")))))

(defun safely-read-quil (filename)
  "Safely read the Quil file designated by FILENAME."
  (flet ((read-it (file)
           (let ((*allow-unresolved-applications* t))
             (read-quil-file file))))
    (if (null *safe-include-directory*)
        (read-it filename)
        (let ((*resolve-include-pathname* #'resolve-safely))
          (read-it filename)))))

(defun safely-parse-quil (string &key originating-file
                                   (transforms *standard-post-parsing-transforms*)
                                   (ambiguous-definition-handler #'continue))
  "Safely parse a Quil string STRING."
  (flet ((parse-it (string)
           (let ((*allow-unresolved-applications* t))
             (parse string
                    :originating-file originating-file
                    :transforms transforms
                    :ambiguous-definition-handler ambiguous-definition-handler))))
    (if (null *safe-include-directory*)
        (parse-it string)
        (let ((*resolve-include-pathname* #'resolve-safely))
          (parse-it string)))))
