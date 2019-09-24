;;;; resolve-applications.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

;;; Used solely for application resolution to detect if we are resolving things
;;; inside of a circuit.
(defvar *in-circuit-body* nil)

;;; TODO frames have a fixed sample-rate

;;; TODO check i) that values are constant, and ii) of the appropriate type
(defun validate-waveform-parameters (waveform-ref expected-parameters)
  "Determines whether the waveform reference WAVEFORM-REF has parameter names
conforming to the list of EXPECTED-PARAMETERS."
  (let ((actual (mapcar (a:compose #'param-name #'first) (waveform-ref-args waveform-ref))))
    (a:when-let ((missing (set-difference expected-parameters actual :test #'equalp)))
      (quil-parse-error "Expected parameter ~A in waveform ~A."
                        (first missing)
                        (waveform-ref-name waveform-ref)))
    (a:when-let ((unexpected (set-difference actual expected-parameters :test #'equalp)))
      (quil-parse-error "Unexpected parameter ~A in waveform ~A. ~@
                        Expected parameters are: ~{~A~^, ~}."
                        (first unexpected)
                        (waveform-ref-name waveform-ref)
                        expected-parameters))
    t))

(defun resolve-standard-waveform (waveform-ref waveform-class)
  (let ((param-map (quilt-waveform-parameter-alist waveform-class)))
    (validate-waveform-parameters waveform-ref
                                  (mapcar #'first param-map))
    (let ((obj (make-instance waveform-class)))
      (loop :for (param  val) :in (waveform-ref-args waveform-ref)
            :for slot-name := (second (assoc (param-name param) param-map :test #'string=))
            :do (setf (slot-value obj slot-name) val))
      obj)))

(defun resolve-custom-waveform (waveform-ref waveform-defn)
  (let ((expected-names (etypecase waveform-defn
                          (static-waveform-definition
                           nil)
                          (parameterized-waveform-definition
                           (mapcar #'symbol-name (waveform-definition-parameters waveform-defn))))))
    (validate-waveform-parameters waveform-ref expected-names)
    waveform-defn))

(defun resolve-waveform-reference (waveform-ref waveform-defns &key (use-defaults t))
  "Destructively updates the waveform reference's resolution to an appropriate
waveform or waveform definition."
  (assert (null (waveform-ref-name-resolution waveform-ref)))
  (let* ((name (waveform-ref-name waveform-ref))
         (resolution
           (a:if-let ((default-binding (and use-defaults
                                            (gethash name *quilt-to-waveform-class*))))
             (resolve-standard-waveform waveform-ref default-binding)
             (a:if-let ((defwaveform
                            (find name waveform-defns
                                  :key #'waveform-definition-name
                                  :test #'string=)))
               (resolve-custom-waveform waveform-ref defwaveform)
               (quil-parse-error "Waveform reference ~A does not match ~
                                any standard or user defined waveforms." name)))))
    (setf (waveform-ref-name-resolution waveform-ref) resolution)
    waveform-ref))

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

  (:method ((instr pulse) parsed-program)
    (resolve-waveform-reference (pulse-waveform instr)
                                (parsed-program-waveform-definitions parsed-program)))

  (:method ((instr capture) parsed-program)
    (resolve-waveform-reference (capture-waveform instr)
                                (parsed-program-waveform-definitions parsed-program)))

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
