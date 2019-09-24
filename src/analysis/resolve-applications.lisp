;;;; resolve-applications.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

;;; Used solely for application resolution to detect if we are resolving things
;;; inside of a circuit.
(defvar *in-circuit-body* nil)

(defun definition-signature (instr)
  "If INSTR is a definition or memory declaration, returns a signature used for
determining ambiguity. Otherwise, return NIL."
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
      ;; TODO (frame-definition (list 'frame-definition (frame-definition-frame instr)))
      (memory-descriptor (cons 'memory-descriptor
                               (memory-descriptor-name instr))))))

(defun ambiguous-definition-condition (instr file conflicts)
  "Signal a condition indicating that the instruction INSTR parsed from FILE has
a list of conflicts CONFLICTS."
  (let ((combined (acons instr file conflicts)))
    (etypecase instr
      (gate-definition (make-condition 'ambiguous-gate-or-circuit-definition :conflicts combined))
      (circuit-definition (make-condition 'ambiguous-gate-or-circuit-definition :conflicts combined))
      (waveform-definition (make-condition 'ambiguous-waveform-definition :conflicts combined))
      (calibration-definition (make-condition 'ambiguous-calibration-definition :conflicts combined))
      ;; TODO frame definition
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
        (all-seen-defns (make-hash-table :test 'equal)))
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

;;; TODO: Factor out this gate arity computation to something nicer.
(defgeneric resolve-instruction (instr parsed-program)
  (:method ((instr unresolved-application) parsed-program)
    (macrolet
        ((assert-and-print-instruction (test-form &optional places datum &rest arguments)
           `(assert ,test-form
                    ,places
                    (format nil "Error in resolving ~/quil:instruction-fmt/: ~a"
                            instr
                            ,datum)
                    ,@arguments)))
      (let* ((operator (application-operator instr))
             (addl-qubits (operator-description-additional-qubits operator))
             (name (operator-description-root-name operator))
             (found-gate-defn (or (find name (parsed-program-gate-definitions parsed-program)
                                        :test #'string=
                                        :key #'gate-definition-name)
                                  (lookup-standard-gate name)))
             (found-circ-defn (find name (parsed-program-circuit-definitions parsed-program)
                                    :test #'string=
                                    :key #'circuit-definition-name)))
        (cond
          ;; Gate application
          (found-gate-defn
           ;; Verify correct arguments
           (let ((args (application-arguments instr)))
             ;; Check that all arguments are qubits
             (assert-and-print-instruction (every (a:disjoin #'qubit-p
                                                             (if *in-circuit-body*
                                                                 #'is-formal
                                                                 (constantly nil)))
                                                  args)
                                           ()
                                           "All arguments must be qubits. Check type of args: ~S." args)
             (let* ((num-qubits (length args))
                    (distinct-args (remove-duplicates args :test 'equalp))
                    (expected-qubits
                      (+ addl-qubits
                         (gate-definition-qubits-needed found-gate-defn))))
               ;; Check that all arguments are distinct
               (assert-and-print-instruction (= (length args) (length distinct-args))
                                             ()
                                             "All arguments must be distinct. Check arguments for duplicates.")
               ;; Check that number of arguments matches gate matrix dimension
               (assert-and-print-instruction (= expected-qubits num-qubits)
                                             ()
                                             "Expected ~D qubit~:P, but ~D ~:*~[were~;was~:;were~] provided."
                                             expected-qubits num-qubits)))
           ;; Verification finished. Transform the application.
           (change-class instr 'gate-application :name-resolution found-gate-defn))

          ;; Circuit application
          (found-circ-defn
           (change-class instr 'circuit-application :circuit-definition found-circ-defn))

          ;; None of the above.
          (t
           (unless *allow-unresolved-applications*
             (cerror "Continue with application remaining unresolved."
                     "Unable to resolve operator ~S"
                     name))
           instr)))))

  ;; Anything with a frame
  ;; Anything with a waveform

  ;; Everything else, even non-applications, just resolve to
  ;; themselves.
  (:method ((instr t) parsed-program)
    (declare (ignore parsed-program))
    instr))

(defun resolve-objects (raw-quil)
  "Perform all object resolution within the list RAW-QUIL, returning a PARSED-PROGRAM."
  ;; For straight quil, we need to resolve UNRESOLVED-APPLICATIONS. For quilt,
  ;; we need to also resolve waveform and frame references.
  (check-type raw-quil list)
  (let ((unresolved-program (raw-quil-to-unresolved-program raw-quil)))
    (flet ((resolve-instruction-sequence (seq)
             (map nil (lambda (thing)
                        (resolve-instruction thing unresolved-program))
                  seq)))
      (resolve-instruction-sequence (parsed-program-executable-code unresolved-program))
      ;; resolve circuit definitions
      (map nil (lambda (cd)
                 (let ((*in-circuit-body* t))
                   (resolve-instruction-sequence
                    (circuit-definition-body cd))))
           (parsed-program-circuit-definitions unresolved-program))
      ;; resolve calibration definitions
      (map nil (lambda (cd)
                 (let ((*in-circuit-body* t)) ; TODO rename
                   (resolve-instruction-sequence
                    (calibration-definition-body cd))))
           (parsed-program-calibration-definitions unresolved-program)))
    unresolved-program))
