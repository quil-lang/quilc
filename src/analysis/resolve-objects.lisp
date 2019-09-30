;;;; resolve-objects.lisp
;;;;
;;;; Authors: Robert Smith
;;;;          Erik Davis

(in-package #:cl-quil)

;;; Used solely for object resolution to detect if we are resolving things
;;; inside of a circuit or calibration.
(defvar *in-definition-body* nil)

;;; TODO frames have a fixed sample-rate

;;; TODO check i) that values are constant, and ii) of the appropriate type
(defun validate-waveform-parameters (waveform-ref expected-parameters)
  "Determines whether the waveform reference WAVEFORM-REF has parameter names conforming to the list of EXPECTED-PARAMETERS."
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
  "Destructively update WAVEFORM-REF's name resolution to an appropriate waveform or waveform definition."
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

(defun resolve-frame (frame frame-definitions)
  "Destructively update FRAME's name resolution to an appropriate frame definition."
  ;; We do not resolve frames in calibration bodies, /even/ if we could (e.g. because
  ;; they do not involve formal arguments). The motivation for this is twofold:
  ;; i) it's sometimes convenient to parse calibrations separately from frame definitions
  ;; ii) we prefer to handle this resolution uniformly at expansion time
  (unless *in-definition-body*
    (a:if-let ((formal-qubit (find-if #'is-formal (frame-qubits frame))))
      ;; time to get rowdy...
      (quil-parse-error "Unable to resolve formal ~A outside of definition body." formal-qubit)
      (a:if-let ((defn (find frame frame-definitions
                             :key #'frame-definition-frame
                             :test #'frame=)))
        (setf (frame-name-resolution frame) defn)
        (quil-parse-error "No frame definition found for referenced frame ~A."
                          (print-instruction-to-string frame)))))
  frame)

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
                                                             (if *in-definition-body*
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

  (:method ((instr simple-frame-mutation) parsed-program)
    (resolve-frame (frame-mutation-target-frame instr)
                   (parsed-program-frame-definitions parsed-program))
    instr)

  (:method ((instr swap-phase) parsed-program)
    (unless *in-definition-body*
      (let ((defns (parsed-program-frame-definitions parsed-program)))
        (resolve-frame (swap-phase-left-frame instr) defns)
        (resolve-frame (swap-phase-right-frame instr) defns)))
    instr)

  (:method ((instr delay) parsed-program)
    ;; DELAY allows users to specify frames explicitly (by giving their names),
    ;; or implicitly (by giving no names).
    (declare (ignore parsed-program))
    instr)

  (:method ((instr pulse) parsed-program)
    (unless *in-definition-body*
      (resolve-frame (pulse-frame instr)
                     (parsed-program-frame-definitions parsed-program)))
    (resolve-waveform-reference (pulse-waveform instr)
                                (parsed-program-waveform-definitions parsed-program))
    instr)

  (:method ((instr capture) parsed-program)
    (unless *in-definition-body*
      (resolve-frame (capture-frame instr)
                     (parsed-program-frame-definitions parsed-program)))
    (resolve-waveform-reference (capture-waveform instr)
                                (parsed-program-waveform-definitions parsed-program))
    instr)

  (:method ((instr raw-capture) parsed-program)
    (unless *in-definition-body*
      (resolve-frame (raw-capture-frame instr)
                     (parsed-program-frame-definitions parsed-program)))
    instr)

  (:method ((instr t) parsed-program)
    (declare (ignore parsed-program))
    instr))

(defun resolve-objects (raw-quil)
  "Perform all object resolution within the list RAW-QUIL, returning a PARSED-PROGRAM."
  ;; For straight quil, we need to resolve UNRESOLVED-APPLICATIONS. For quilt,
  ;; we need to also resolve waveform and frame references.

  ;; NOTE: Some frames within calibration bodies cannot be resolved here (e.g.
  ;; where one of the qubits is a formal argument). We adopt the convention that
  ;; all frames in calibration bodies are resolved at expansion time.
  (check-type raw-quil list)
  (let ((unresolved-program (raw-quil-to-unresolved-program raw-quil)))
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
      ;; resolve calibration definitions
      (map nil (lambda (cd)
                 (let ((*in-definition-body* t))
                   (resolve-instruction-sequence
                    (calibration-definition-body cd))))
           (parsed-program-calibration-definitions unresolved-program)))
    unresolved-program))
