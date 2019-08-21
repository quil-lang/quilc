;;;; resolve-applications.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

;;; Used solely for application resolution to detect if we are resolving things inside of a circuit.
(defvar *in-circuit-body* nil)

;;; TODO: Factor out this gate arity computation to something nicer.
(defgeneric resolve-application (app &key gate-definitions circuit-definitions)
  (:method ((app unresolved-application) &key gate-definitions circuit-definitions)
    (macrolet
        ((assert-and-print-instruction (test-form &optional places datum &rest arguments)
           `(assert ,test-form
                    ,places
                    (format nil "Error in resolving ~s: ~a"
                            (print-instruction app nil)
                            ,datum)
                    ,@arguments)))
      (let* ((operator (application-operator app))
             (addl-qubits (operator-description-additional-qubits operator))
             (name (operator-description-root-name operator))
             (found-gate-defn (or (find name gate-definitions
                                        :test #'string=
                                        :key #'gate-definition-name)
                                  (lookup-standard-gate name)))
             (found-circ-defn (find name circuit-definitions
                                    :test #'string=
                                    :key #'circuit-definition-name)))
        (cond
          ;; Gate application
          (found-gate-defn
           ;; Verify correct arguments
           (let ((args (application-arguments app)))
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
           (change-class app 'gate-application :name-resolution found-gate-defn))

          ;; Circuit application
          (found-circ-defn
           (change-class app 'circuit-application :circuit-definition found-circ-defn))

          ;; None of the above.
          (t
           (unless *allow-unresolved-applications*
             (cerror "Continue with application remaining unresolved."
                     "Unable to resolve operator ~S"
                     name))
           app)))))

  ;; Everything else, even non-applications, just resolve to
  ;; themselves.
  (:method ((app t) &key gate-definitions circuit-definitions)
    (declare (ignore gate-definitions circuit-definitions))
    app))

(defun resolve-applications (raw-quil)
  "Resolve all UNRESOLVED-APPLICATIONs within the list RAW-QUIL, returning a PARSED-PROGRAM."
  (check-type raw-quil list)
  (multiple-value-bind (gate-defs circ-defs memory-defs exec-code)
      (extract-code-sections raw-quil)
    (flet ((resolve-instruction-sequence (seq)
             (map nil (lambda (thing)
                        (resolve-application thing
                                             :gate-definitions gate-defs
                                             :circuit-definitions circ-defs))
                  seq)))
      (resolve-instruction-sequence exec-code)
      (map nil (lambda (cd)
                 (let ((*in-circuit-body* t))
                   (resolve-instruction-sequence
                    (circuit-definition-body cd))))
           circ-defs)
      (make-instance 'parsed-program
                     :gate-definitions gate-defs
                     :circuit-definitions circ-defs
                     :memory-definitions memory-defs
                     :executable-code (coerce exec-code 'simple-vector)))))
