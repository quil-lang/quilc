;;;; src/cl-quil.lisp
;;;;
;;;; Authors: Robert Smith
;;;           Erik Davis

(in-package #:cl-quil)

(defvar *standard-post-process-transforms*
  '(expand-circuits type-check simplify-arithmetic)
  "The standard transforms that are applied by PARSE-QUIL.")

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
                                                  (ambiguous-definition-handler #'continue)
                                                  (lexer-extensions '())
                                                  (parser-extensions '()))
  "The actual parsing code. Arguments are as in PARSE-QUIL, except we now have three new ones:

1. BUILD-PARSED-PROGRAM is a function which translates a list of raw ast objects to a PARSED-PROGRAM object,
2. PARSER-EXTENSIONS is a list of parser functions which PARSE-PROGRAM-LINES may dispatch to.
3. LEXER-EXTENSIONS is a list of lexer functions which LINE-LEXER may dispatch to."
  (handler-bind
      ;; We disallow multiple declarations of the same memory region (even if equivalent).
      ;; Otherwise, for gate or circuit definitions, the default choice is to "accept the mystery."
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
  (labels ((commented-line-p (line)
             (string= "//" (subseq (string-left-trim '(#\Space) line) 0 2)))
           (qasm-line-p (line)
             (let ((pos (search "OPENQASM 2.0" (string-left-trim '(#\Space) line))))
               (and pos (= 0 pos)))))
    (with-input-from-string (*standard-input* string)
      (loop :for line := (read-line *standard-input* nil) :do
        (cond ((qasm-line-p line)      (return t))
              ((commented-line-p line) nil)
              (t                       (return nil)))))))

(defun parse-quil (string &key originating-file
                            (transforms *standard-post-process-transforms*)
                            (ambiguous-definition-handler #'continue))
  "Parse and process the Quil string STRING, which originated from the file ORIGINATING-FILE. Transforms in TRANSFORMS are applied in-order to the processed Quil string.

In the presence of multiple definitions with a common signature, a signal is raised, with the default handler specified by AMBIGUOUS-DEFINITION-HANDLER.
"
  (if (%check-for-qasm-header string)
      (quil.qasm:parse-qasm string)
      (%parse-quil string
                   #'raw-quil-to-unresolved-program
                   :originating-file originating-file
                   :transforms transforms
                   :ambiguous-definition-handler ambiguous-definition-handler)))

(defun read-quil-file (filespec)
  "Read the Quil file designated by FILESPEC, and parse it as if by PARSE-QUIL."
  (check-type filespec (or string pathname))
  (assert (and (uiop:file-pathname-p filespec)
               (uiop:file-exists-p filespec))
          (filespec)
          "FILESPEC must be a path to a file that exists")
  (parse-quil (a:read-file-into-string filespec)
              :originating-file (first (directory filespec))))
