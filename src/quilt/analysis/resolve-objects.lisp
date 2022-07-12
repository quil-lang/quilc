;;;; resolve-objects.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:cl-quil/quilt)

(defun validate-waveform-parameters (waveform-ref expected-parameters)
  "Determines whether the waveform reference WAVEFORM-REF has parameter names conforming to the list of EXPECTED-PARAMETERS."
  (let ((actual (mapcar (a:compose #'param-name #'car)
                        (waveform-ref-parameter-alist waveform-ref))))
    (a:when-let ((missing (set-difference expected-parameters actual :test #'string=)))
      (quil-parse-error "Expected parameters ~{~A~^, ~} in waveform ~A."
                        missing
                        (waveform-ref-name waveform-ref)))
    (a:when-let ((unexpected (set-difference actual expected-parameters :test #'string=)))
      (quil-parse-error "Unexpected parameters ~{A~^, ~} in waveform ~A. ~@
                        Expected parameters are: ~{~A~^, ~}."
                        unexpected
                        (waveform-ref-name waveform-ref)
                        expected-parameters))
    t))

(defun resolve-standard-waveform (waveform-ref waveform-class)
  ;; pull the alist mapping param objects to their class slot names
  (let ((param-map (quilt-waveform-parameter-alist waveform-class)))
    (validate-waveform-parameters waveform-ref
                                  (mapcar #'car param-map))
    (let ((obj (make-instance waveform-class)))
      (loop :for (param . val) :in (waveform-ref-parameter-alist waveform-ref)
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
  (let ((resolution
          (a:if-let ((default-binding (and use-defaults
                                           (default-waveform-class waveform-ref))))
            (resolve-standard-waveform waveform-ref default-binding)
            (a:if-let ((defwaveform
                           (find (waveform-ref-name waveform-ref)
                                 waveform-defns
                                 :key #'waveform-definition-name
                                 :test #'string=)))
              (resolve-custom-waveform waveform-ref defwaveform)
              (quil-parse-error "Waveform reference ~A does not match ~
                                 any standard or user defined waveforms."
                                (waveform-ref-name waveform-ref))))))
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
        (quil-parse-error "No frame definition found for referenced frame ~/cl-quil:instruction-fmt/."
                          frame))))
  frame)


(defmethod resolve-instruction ((instr simple-frame-mutation) parsed-program)
  (resolve-frame (frame-mutation-target-frame instr)
                 (parsed-program-frame-definitions parsed-program))
  instr)

(defmethod resolve-instruction ((instr swap-phase) parsed-program)
  (unless *in-definition-body*
    (let ((defns (parsed-program-frame-definitions parsed-program)))
      (resolve-frame (swap-phase-left-frame instr) defns)
      (resolve-frame (swap-phase-right-frame instr) defns)))
  instr)

(defmethod resolve-instruction ((instr delay-on-qubits) parsed-program)
  (declare (ignore parsed-program))
  instr)

(defmethod resolve-instruction ((instr delay-on-frames) parsed-program)
  (unless *in-definition-body*
    (dolist (frame (delay-frames instr))
      (resolve-frame frame (parsed-program-frame-definitions parsed-program))))
  instr)

(defmethod resolve-instruction ((instr pulse) parsed-program)
  (unless *in-definition-body*
    (resolve-frame (pulse-frame instr)
                   (parsed-program-frame-definitions parsed-program)))
  (resolve-waveform-reference (pulse-waveform instr)
                              (parsed-program-waveform-definitions parsed-program))
  instr)

(defmethod resolve-instruction ((instr capture) parsed-program)
  (unless *in-definition-body*
    (resolve-frame (capture-frame instr)
                   (parsed-program-frame-definitions parsed-program)))
  (resolve-waveform-reference (capture-waveform instr)
                              (parsed-program-waveform-definitions parsed-program))
  instr)

(defmethod resolve-instruction ((instr raw-capture) parsed-program)
  (unless *in-definition-body*
    (resolve-frame (raw-capture-frame instr)
                   (parsed-program-frame-definitions parsed-program)))
  instr)

(defmethod resolve-objects :after ((unresolved-program parsed-quilt-program))
  ;; For straight quil, we need to resolve UNRESOLVED-APPLICATIONS. For Quilt,
  ;; we need to also resolve waveform and frame references.

  ;; NOTE: Some frames within calibration bodies cannot be resolved here (e.g.
  ;; where one of the qubits is a formal argument). We adopt the convention that
  ;; all frames in calibration bodies are resolved at expansion time.
  (flet ((resolve-instruction-sequence (seq)
           (map nil (lambda (thing)
                      (resolve-instruction thing unresolved-program))
                seq)))
    ;; resolve calibration definitions
    (map nil (lambda (cd)
               (let ((*in-definition-body* t))
                 (resolve-instruction-sequence
                  (calibration-definition-body cd))))
         (parsed-program-calibration-definitions unresolved-program))
    unresolved-program))
