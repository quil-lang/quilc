;;;; src/cl-quil.lisp
;;;;
;;;; Authors: Robert Smith
;;;           Erik Davis

(in-package #:cl-quil)

(defvar *standard-post-process-transforms*
  '(expand-circuits type-check)
  "The standard transforms that are applied by PARSE-QUIL.")

(defvar *standard-quilt-transforms*
  '(expand-circuits expand-calibrations type-check)
  "The standard transforms for using PARSE-QUIL with quilt code.")

;;; As part of parsing, we need to handle the case when two definitions seem to
;;; conflict. The approach taken here is to map definitions to signatures, with
;;; two signatures EQUALP if there is a context in which both definitions would
;;; be applicable. This ranges from quite simple in the case of memory
;;; definitions (where the basic check is whether the region names are equal),
;;; to a bit more involved for quilt calibrations (where applicability is
;;; generally determined by a pattern matching system, and so we have to
;;; consider the arity and values of parameters and arguments).

(defun definition-signature (instr)
  "Computes a signature for a quil definition such that if two definitions are equivalent for the purposes of name resolution, then their signatures are EQUALP."
  (flet ((canonicalize-params-args (obj)
           (cond ((is-param obj)
                  'param)
                 ((is-formal obj)
                  'formal)
                 (t obj))))
    (typecase instr
      (gate-definition
       (list 'gate-or-circuit-definition
             (intern (gate-definition-name instr))
             (if (typep instr 'parameterized-gate-definition)
                 (length (gate-definition-parameters instr))
                 0)
             (gate-definition-qubits-needed instr)))
      (circuit-definition
       (list 'gate-or-circuit-definition
             (intern (circuit-definition-name instr))
             (length (circuit-definition-parameters instr))
             (length (circuit-definition-arguments instr))))
      (waveform-definition
       (cons 'waveform-definition
             (intern (waveform-definition-name instr))))
      (gate-calibration-definition
       (list 'gate-calibration-definition
             (intern (operator-description-string
                      (calibration-definition-operator instr)))
             (mapcar #'canonicalize-params-args (calibration-definition-arguments instr))
             (mapcar #'canonicalize-params-args (calibration-definition-parameters instr))))
      (measurement-calibration-definition
       (list 'measurement-calibration-definition
             (measurement-calibration-qubit instr)
             (typep instr 'measure-calibration-definition)))
      (frame-definition (list 'frame-definition
                              (frame-hash (frame-definition-frame instr))))
      (memory-descriptor (list 'memory-descriptor
                               (intern (memory-descriptor-name instr)))))))

(defun error-on-ambiguous-memory-declaration  (condition)
  "Handler which signals an error in the presence of an AMBIGUOUS-MEMORY-DEFINITION."
  (when (typep condition 'ambiguous-memory-declaration)
    (destructuring-bind ((mem . file) (other-mem . other-file) &rest cs)
        (ambiguous-definition-conflicts condition)
      (declare (ignore other-mem cs))
      (let* ((name (memory-descriptor-name mem)))
        (quil-parse-error "Memory region ~A~@[ (in ~A)~] has already been DECLAREd~@[ (in ~A)~]."
                          name file other-file)))))

(defun ambiguous-definition-condition (instr file conflicts)
  "Signal a condition indicating that the instruction INSTR parsed from FILE has a list of conflicts CONFLICTS."
  (let ((combined (acons instr file conflicts)))
    (etypecase instr
      (gate-definition (make-condition 'ambiguous-gate-or-circuit-definition :conflicts combined))
      (circuit-definition (make-condition 'ambiguous-gate-or-circuit-definition :conflicts combined))
      (waveform-definition (make-condition 'ambiguous-waveform-definition :conflicts combined))
      (calibration-definition (make-condition 'ambiguous-calibration-definition :conflicts combined))
      (frame-definition (make-condition 'ambiguous-frame-definition :conflicts combined))
      (memory-descriptor (make-condition 'ambiguous-memory-declaration :conflicts combined)))))

(defun raw-quil-to-unresolved-program (code)
  "This constructs a PARSED-PROGRAM object from the given quil CODE, without any resolution of applications.

This also signals ambiguous definitions, which may be handled as needed."
  ;; Note: the processing below preserves the order of definitions.
  (let ((gate-defs nil)
        (circ-defs nil)
        (memory-defs nil)
        (wf-defs nil)
        (calib-defs nil)
        (frame-defs nil)
        (exec-code nil)
        ;; The following maps definition signatures to a list of (filename . defn) pairs
        (all-seen-defns (make-hash-table :test 'equalp)))
    (flet ((bin (instr)
             (a:when-let ((signature (definition-signature instr)))
               (let ((originating-file (typecase (lexical-context instr)
                                         (token
                                          (token-pathname (lexical-context instr)))
                                         (t
                                          (quil-parse-error "Unable to resolve definition context ~A" instr)))))
                 ;; check for conflicts
                 (a:when-let ((entries (gethash signature all-seen-defns)))
                   (cerror "Continue with ambiguous definition."
                           (ambiguous-definition-condition instr originating-file entries)))
                 (push (cons instr originating-file)
                       (gethash signature all-seen-defns))))
             (typecase instr
               (gate-definition (push instr gate-defs))
               (circuit-definition (push instr circ-defs))
               (waveform-definition (push instr wf-defs))
               (calibration-definition (push instr calib-defs))
               (frame-definition (push instr frame-defs))
               (memory-descriptor (push instr memory-defs))
               (t (push instr exec-code)))))
      (mapc #'bin code)
      (make-instance 'parsed-program
                     :gate-definitions (nreverse gate-defs)
                     :circuit-definitions (nreverse circ-defs)
                     :waveform-definitions (nreverse wf-defs)
                     :calibration-definitions (nreverse calib-defs)
                     :frame-definitions (nreverse frame-defs)
                     :memory-definitions (nreverse memory-defs)
                     :executable-code (coerce (nreverse exec-code)
                                              'simple-vector)))))

(defun parse-quil (string &key originating-file
                            (transforms *standard-post-process-transforms*)
                            (ambiguous-definition-handler #'continue))
  "Parse and process the Quil string STRING, which originated from the file ORIGINATING-FILE. Transforms in TRANSFORMS are applied in-order to the processed Quil string.

In the presence of multiple definitions with a common signature, a signal is raised, with the default handler specified by AMBIGUOUS-DEFINITION-HANDLER.
"
  (handler-bind
      (;; We disallow multiple declarations of the same memory region (even if equivalent).
       (ambiguous-memory-declaration #'error-on-ambiguous-memory-declaration)
       ;; For gate or circuit definitions, the default choice is to "accept the mystery."
       (ambiguous-gate-or-circuit-definition ambiguous-definition-handler)
       (ambiguous-calibration-definition ambiguous-definition-handler)
       (ambiguous-frame-definition ambiguous-definition-handler)
       (ambiguous-waveform-definition ambiguous-definition-handler))
      (let* ((*current-file* originating-file)
             (raw-quil (parse-quil-into-raw-program string))
             (pp (resolve-objects
                  (raw-quil-to-unresolved-program
                   (process-includes raw-quil originating-file)))))
        (dolist (xform transforms pp)
          (setf pp (transform xform pp))))))

(defun read-quil-file (filespec)
  "Read the Quil file designated by FILESPEC, and parse it as if by PARSE-QUIL."
  (check-type filespec (or string pathname))
  (assert (and (uiop:file-pathname-p filespec)
               (uiop:file-exists-p filespec))
          (filespec)
          "FILESPEC must be a path to a file that exists")
  (parse-quil (a:read-file-into-string filespec)
              :originating-file (first (directory filespec))))
