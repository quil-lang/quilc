;;;; process-includes.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

(defun definition-signature (instr)
  "If INSTR is a definition or memory declaration, returns a signature used for
determining ambiguity. Otherwise, return NIL."
  (typecase instr
    (gate-definition (cons 'gate-definition (gate-definition-name instr)))
    (circuit-definition (cons 'circuit-definition (circuit-definition-name instr)))
    (memory-descriptor (cons 'memory-descriptor (memory-descriptor-name instr)))))

(defun signal-ambiguous-definition (instr file conflicts)
  "Signal a condition indicating that the instruction INSTR parsed from FILE has
a list of conflicts CONFLICTS."
  (etypecase instr
    (gate-definition (signal 'ambiguous-gate-definition :conflicts (acons instr file conflicts)))
    (circuit-definition (signal 'ambiguous-circuit-definition :conflicts (acons instr file conflicts)))
    (memory-descriptor (signal 'ambiguous-memory-declaration :conflicts (acons instr file conflicts)))))

(defun process-includes (raw-quil &optional originating-file)
  "Recursively process all INCLUDE instructions in the list RAW-QUIL. The
result is a new sequence with the included quil instructions spliced in."
  (let* (;; We track the expanded list of instructions, and build it from its tail out
         (expanded (cons nil nil))
         (expanded-tail expanded)
         ;; We track an active path of files from the "root" to whatever we are processing
         ;; at the moment, in order to check for cycles.
         (seen-files (when originating-file
                       (list (namestring originating-file))))
         ;; The following maps definition signatures to a list of (filename . defn) pairs
         (seen-definitions (make-hash-table :test 'equalp)))
    (labels ((handle-include (incl originating-file)
               ;; load the file to be included, and recursively
               ;; expand includes within it
               (let ((file (funcall *resolve-include-pathname* (include-pathname incl)))
                     (originating-dir (if (null originating-file)
                                          (pathname (uiop:getcwd))
                                          (uiop:pathname-directory-pathname
                                           originating-file))))
                 (when (uiop:relative-pathname-p file)
                   (setf file (merge-pathnames file originating-dir)))
                 (unless (uiop:file-exists-p file)
                   (quil-parse-error "Could not include ~S because it does not exist." file))
                 (when (member (namestring file) seen-files :test #'string=)
                   (quil-parse-error "Cyclic INCLUDE detected: ~S is being INCLUDEd but has already been processed." file))
                 (let* ((*current-file* file)
                        (body (parse-quil-into-raw-program (a:read-file-into-string file))))
                   (push (namestring file) seen-files)
                   (unwind-protect (expand-all-includes body file)
                     (pop seen-files)))))
             (expand-all-includes (instrs originating-file)
               ;; consume an instruction. if it's an include, we handoff to
               ;; HANDLE-INCLUDE before continuing
               (cond ((endp instrs)
                      (cdr expanded))
                     ((typep (car instrs) 'include)
                      (handle-include (car instrs) originating-file)
                      (expand-all-includes (cdr instrs) originating-file))
                     (t
                      (let ((instr (car instrs)))
                        (a:when-let ((signature (definition-signature instr)))
                          ;; check for conflicts
                          (with-simple-restart (continue "Continue with ambiguous definition.")
                            (a:when-let ((entries (gethash signature seen-definitions)))
                              (signal-ambiguous-definition instr originating-file entries)))
                          ;; update definition table
                          (push (cons instr originating-file)
                                (gethash signature seen-definitions))))
                      (setf (cdr expanded-tail) instrs)
                      (setf expanded-tail (cdr expanded-tail))
                      (expand-all-includes (cdr instrs) originating-file)))))
      (expand-all-includes raw-quil originating-file))))
