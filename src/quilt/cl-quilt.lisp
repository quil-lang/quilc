;;;; src/quilt/cl-quilt.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:cl-quil/quilt)

(defmethod definition-signature ((instr waveform-definition))
  (cons 'waveform-definition
        (intern (waveform-definition-name instr) :keyword)))

;;; When computing signatures for calibrations, we must account for the fact
;;; that parameter and argument names don't matter (e.g. `DEFCAL MEASURE q` is
;;; the same as `DEFCAL MEASURE v`). Our approach here is just to replace all
;;; parameters by the symbol 'PARAM, and all arguments by the symbol 'FORMAL.
(defun canonicalize-params-args (obj)
  "For a parameter or formal argument OBJ, return the canonical form to be used for signatures."
  (cond ((is-param obj)
         'param)
        ((is-formal obj)
         'formal)
        (t obj)))

(defmethod definition-signature ((instr gate-calibration-definition))
  (list 'gate-calibration-definition
        (intern (operator-description-string
                 (calibration-definition-operator instr))
                :keyword)
        (mapcar #'canonicalize-params-args (calibration-definition-arguments instr))
        (mapcar #'canonicalize-params-args (calibration-definition-parameters instr))))

(defmethod definition-signature ((instr measurement-calibration-definition))
  (list 'measurement-calibration-definition
        (measurement-calibration-qubit instr)
        (typep instr 'measure-calibration-definition)))

(defmethod definition-signature ((instr frame-definition))
  (list 'frame-definition
        (frame-hash (frame-definition-frame instr))))

(defun raw-quilt-to-unresolved-program (code)
  "This constructs a PARSED-PROGRAM object from the given list of Quilt instructions CODE, without any resolution of applications.

This also signals ambiguous definitions, which may be handled as needed."
  ;; Note: the processing below preserves the order of definitions.
  (let ((gate-defs '())
        (circ-defs '())
        (memory-defs '())
        (wf-defs '())
        (calib-defs '())
        (frame-defs '())
        (exec-code '())
        ;; The following maps definition signatures to a list of (filename . defn) pairs
        (all-seen-defns (make-hash-table :test 'equalp)))
    (flet ((bin (instr)
             (a:when-let ((signature (definition-signature instr)))
               (let ((originating-file (typecase (lexical-context instr)
                                         (token
                                          (token-pathname (lexical-context instr)))
                                         (t
                                          (quil-parse-error "Unable to resolve definition context ~/cl-quil:instruction-fmt/" instr)))))
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
               (waveform-definition (push instr wf-defs))
               (calibration-definition (push instr calib-defs))
               (frame-definition (push instr frame-defs))
               (memory-descriptor (push instr memory-defs))
               (t (push instr exec-code)))))
      (mapc #'bin code)
      (make-instance 'parsed-quilt-program
                     :gate-definitions (nreverse gate-defs)
                     :circuit-definitions (nreverse circ-defs)
                     :waveform-definitions (nreverse wf-defs)
                     :calibration-definitions (nreverse calib-defs)
                     :frame-definitions (nreverse frame-defs)
                     :memory-definitions (nreverse memory-defs)
                     :executable-code (coerce (nreverse exec-code)
                                              'simple-vector)))))

(defvar *standard-quilt-transforms*
  '(expand-circuits expand-calibrations type-check)
  "The standard transforms for using PARSE-QUIL with Quilt code.")


(defun parse-quilt (string &key originating-file
                                (transforms *standard-quilt-transforms*)
                                (ambiguous-definition-handler #'continue))
  "Parse and process the Quilt string STRING, which originated from the file ORIGINATING-FILE. Transforms in TRANSFORMS are applied in-order to the processed Quil string.

In the presence of multiple definitions with a common signature, a signal is raised, with the default handler specified by AMBIGUOUS-DEFINITION-HANDLER.
"
  (%parse-quil string
               #'raw-quilt-to-unresolved-program
               :originating-file originating-file
               :transforms transforms
               :ambiguous-definition-handler ambiguous-definition-handler
               :parser-extensions (list #'parse-quilt-program-lines)
               :lexer-extensions (list #'quilt-keyword-lexer)))

(defun read-quilt-file (filespec)
  "Read the Quilt file designated by FILESPEC, and parse it as if by PARSE-QUIL."
  (check-type filespec (or string pathname))
  (assert (and (uiop:file-pathname-p filespec)
               (uiop:file-exists-p filespec))
          (filespec)
          "FILESPEC must be a path to a file that exists")
  (parse-quilt (a:read-file-into-string filespec)
               :originating-file (first (directory filespec))))
