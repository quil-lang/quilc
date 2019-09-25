;;;; src/cl-quil.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

;;; TODO: should quilt calibration expansion be standard?
(defvar *standard-post-process-transforms*
  '(expand-circuits type-check)
  "The standard transforms that are applied by PARSE-QUIL.")

(defvar *standard-quilt-transforms*
  '(expand-circuits expand-calibrations type-check)
  "The standard transforms for using PARSE-QUIL with quilt code.")

(defun error-on-ambiguous-memory-declaration  (condition)
  "Handler which signals an error in the presence of an AMBIGUOUS-MEMORY-DEFINITION."
  (when (typep condition 'ambiguous-memory-declaration)
    (destructuring-bind ((mem . file) (other-mem . other-file) &rest cs)
        (ambiguous-definition-conflicts condition)
      (declare (ignore other-mem cs))
      (let* ((name (memory-descriptor-name mem)))
        (quil-parse-error "Memory region ~A~@[ (in ~A)~] has already been DECLAREd~@[ (in ~A)~]."
                          name file other-file)))))

(defun definition-signature (instr)
  "If INSTR is a definition or memory declaration, returns a signature used for determining ambiguity. Otherwise, return NIL."
  (flet ((gate-or-circuit-signature (name params args)
           ;; TODO in principle the signature should include the number of params and args
           ;; since e.g. we can tell the difference between FOO 1 and FOO 1 2. however,
           ;; for now we follow the convention of not discriminating between these.
           (declare (ignore params args))
           (list 'gate-or-circuit-definition
                 name)))
    (typecase instr
      (gate-definition (gate-or-circuit-signature (gate-definition-name instr)
                                                  (if (typep instr 'parameterized-gate-definition)
                                                      (gate-definition-parameters instr)
                                                      nil)
                                                  (gate-definition-qubits-needed instr)) )
      (circuit-definition (gate-or-circuit-signature (circuit-definition-name instr)
                                                     (circuit-definition-parameters instr)
                                                     (circuit-definition-arguments instr)))
      (waveform-definition (cons 'waveform-definition
                                 (waveform-definition-name instr)))
      ;; TODO actually think through what these signatures should be like
      (gate-calibration-definition (list 'gate-calibration-definition
                                         (calibration-definition-operator instr)))
      (measurement-calibration-definition (list 'measurement-calibration-definition))
      (frame-definition (list 'frame-definition
                              (frame-hash (frame-definition-frame instr))))
      (memory-descriptor (cons 'memory-descriptor
                               (memory-descriptor-name instr))))))

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
  "This constructes a PARSED-PROGRAM object from the given quil CODE, without any resolution of applications.

This also signals ambiguous definitions, which may be handled as needed."
  ;; Note: this preserves the order of definitions.
  (let ((gate-defs nil)
        (circ-defs nil)
        (memory-defs nil)
        (wf-defs nil)
        (calib-defs nil)
        (frame-defs nil)
        (exec-code nil)
        ;; The following maps definition signatures to a list of (filename . defn) pairs
        (all-seen-defns (make-hash-table :test 'equal))) ; TODO equality here
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
  "Parse and process the Quil string STRING, which originated from the file ORIGINATING-FILE. Transforms in TRANSFORMS are applied in-order to the processed Quil string. In the presence of multiple definitions with a common signature, a signal is raised, with the default handler specified by AMBIGUOUS-DEFINITION-HANDLER.
"
  (handler-bind
      (;; We disallow multiple declarations of the same memory region (even if equivalent).
       (ambiguous-memory-declaration #'error-on-ambiguous-memory-declaration)
       ;; For gate or circuit definitions, the default choice is to "accept the mystery."
       (ambiguous-gate-or-circuit-definition ambiguous-definition-handler)
       ;; TODO change these
       (ambiguous-calibration-definition #'continue)
       (ambiguous-frame-definition #'continue)
       (ambiguous-waveform-definition #'continue))
      (let* ((*current-file* originating-file)
             (raw-quil (parse-quil-into-raw-program string))
             (pp (resolve-objects
                  (process-includes raw-quil originating-file))))
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
