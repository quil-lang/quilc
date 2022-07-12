;;;; resolve-objects.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil/frontend)

;;; Used solely for object resolution to detect if we are resolving things
;;; inside of a circuit or calibration.
(defvar *in-definition-body* nil)

(defgeneric resolve-instruction (instr parsed-program)
  (:method ((instr unresolved-application) parsed-program)
    (macrolet
        ((assert-and-print-instruction (test-form &optional places datum &rest arguments)
           `(assert ,test-form
                    ,places
                    "Error in resolving ~/cl-quil:instruction-fmt/: ~@?"
                    instr
                    ,datum
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
                                                             (if *in-definition-body*
                                                                 #'is-formal
                                                                 (constantly nil)))
                                                  args)
                                           ()
                                           "All arguments must be qubits. Check type of args: ~S." args)
             (let ((num-qubits (length args))
                   ;; args can be either qubit or formal arguments
                   (distinct-args (remove-duplicates args :test #'argument=))
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

  (:method ((instr t) parsed-program)
    (declare (ignore parsed-program))
    instr))

(defgeneric resolve-objects (unresolved-program)
  (:documentation "Perform all object resolution within UNRESOLVED-PROGRAM, returning a PARSED-PROGRAM.")
  (:method ((unresolved-program parsed-program))
    ;; For straight Quil, we need to resolve UNRESOLVED-APPLICATIONS. For Quilt,
    ;; we need to also resolve waveform and frame references.

    ;; NOTE: Some frames within calibration bodies cannot be resolved here (e.g.
    ;; where one of the qubits is a formal argument). We adopt the convention that
    ;; all frames in calibration bodies are resolved at expansion time.
    (flet ((resolve-instruction-sequence (seq)
             (map nil (lambda (thing)
                        (resolve-instruction thing unresolved-program))
                  seq)))
      (resolve-instruction-sequence (parsed-program-executable-code unresolved-program))
      ;; resolve circuit definitions
      (map nil (lambda (cd)
                 (let ((*in-definition-body* t))
                   (resolve-instruction-sequence
                    (circuit-definition-body cd))))
           (parsed-program-circuit-definitions unresolved-program))
      ;; resolve defgate as sequence definitions
      (map nil (lambda (sd)
                 (when (typep sd 'sequence-gate-definition)
                   (let ((*in-definition-body* t))
                     (resolve-instruction-sequence
                      (sequence-gate-definition-sequence sd)))
                   (validate-resolved-seq-gate-definition sd)))
           (parsed-program-gate-definitions unresolved-program))
      unresolved-program)))

