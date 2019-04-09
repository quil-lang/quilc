;;;; resolve-applications.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

(define-transform resolve-applications (resolve-applications)
  "A transform which converts a parsed program with unresolved applications to one where the applications have been resolved into gate or circuit applications.")

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
             (found-gate-defn (find name gate-definitions
                                    :test #'string=
                                    :key #'gate-definition-name))
             (found-circ-defn (find name circuit-definitions
                                    :test #'string=
                                    :key #'circuit-definition-name))
             (in-default-gateset (lookup-standard-gate name)))
        (cond
          ;; Gate application
          (found-gate-defn
           ;; Verify correct arguments
           (let ((args (application-arguments app)))
             ;; Check that all arguments are qubits
             (assert-and-print-instruction (every #'qubit-p args)
                                           ()
                                           "All arguments must be qubits.")
             (let* ((qubit-indices (map 'list #'qubit-index args))
                    (num-qubits (length qubit-indices))
                    (distinct-indices (remove-duplicates qubit-indices))
                    (expected-qubits
                      (+ addl-qubits
                         (1- (integer-length
                              (isqrt
                               (length (gate-definition-entries found-gate-defn))))))))
               ;; Check that all arguments are distinct
               (assert-and-print-instruction (= (length qubit-indices) (length distinct-indices))
                                             ()
                                             "All arguments must have distinct indices. Check indices for duplicates.")
               ;; Check that number of arguments matches gate matrix dimension
               (assert-and-print-instruction (= expected-qubits num-qubits)
                                             ()
                                             "Expected ~D qubit~:P, but ~D were provided."
                                             expected-qubits num-qubits)))
           ;; Verification finished. Transform the application.
           (change-class app 'gate-application :name-resolution found-gate-defn))

          ;; Circuit application
          (found-circ-defn
           (change-class app 'circuit-application :circuit-definition found-circ-defn))

          ;; Resolved in default gateset. In this case we won't have a
          ;; gate definition object, but a gate object directly.
          (in-default-gateset
           (prog1 (change-class app 'gate-application
                                ;; Make sure we have the correct gate
                                ;; object according to the operator
                                ;; description.
                                :gate (funcall
                                       (operator-description-gate-lifter
                                        (application-operator app))
                                       in-default-gateset))
             (let ((args (application-arguments app))
                   (params (application-parameters app)))
               ;; Check that all arguments are qubits
               (assert-and-print-instruction (every (alexandria:disjoin #'qubit-p #'is-formal) args)
                                             ()
                                             "All arguments must be qubits. Check type of args: ~S." args)
               (let* ((num-qubits (length args))
                      (distinct-indices (remove-duplicates args :test 'equalp))
                      (default-gate-dimension (gate-dimension in-default-gateset))
                      (expected-qubits (+ addl-qubits
                                          (1- (integer-length default-gate-dimension))))
                      (default-gate-arity (dynamic-gate-arity in-default-gateset)))

                 ;; Check that the parameters are the correct number
                 (assert-and-print-instruction (= (length params) default-gate-arity)
                                               ()
                                               "The number of gate parameters must match.")

                 ;; Check that all arguments are distinct
                 (assert-and-print-instruction (= (length args) (length distinct-indices))
                                               ()
                                               "All arguments must have distinct indices. Check indices for duplicates.")
                 ;; Check that number of arguments matches gate matrix dimension
                 (assert-and-print-instruction (= expected-qubits num-qubits)
                                               ()
                                               "Expected ~D qubit~:P, but ~D were provided."
                                               expected-qubits num-qubits)))))

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

(defun resolve-applications (parsed-prog)
  "Resolve all UNRESOLVE-APPLICATIONs within the executable code of PARSED-PROG."
  (let ((gate-defs (parsed-program-gate-definitions parsed-prog))
        (circ-defs (parsed-program-circuit-definitions parsed-prog)))
    (flet ((resolve-instruction-sequence (seq)
             (map nil (lambda (thing)
                        (resolve-application thing
                                             :gate-definitions gate-defs
                                             :circuit-definitions circ-defs))
                  seq)))
      (resolve-instruction-sequence (parsed-program-executable-code parsed-prog))
      (map nil (lambda (cd)
                 (resolve-instruction-sequence
                  (circuit-definition-body cd)))
           circ-defs)))
  parsed-prog)
