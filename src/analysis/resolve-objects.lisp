;;;; resolve-applications.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

;;; Used solely for application resolution to detect if we are resolving things
;;; inside of a circuit.
(defvar *in-circuit-body* nil)

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
