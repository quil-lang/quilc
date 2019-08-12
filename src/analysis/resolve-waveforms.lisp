(in-package #:cl-quil)

(define-transform resolve-waveform-references (resolve-waveform-references)
  "This transform resolves all waveform references to their corresponding
waveforms or waveform definitions."
  expand-calibrations)

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

(defun resolve-waveform-references (parsed-program)
  "For every waveform reference in PARSED-PROGRAM, mutate the resolution field
to point to either a waveform object (if built-in) or a waveform definition (if the
reference corresponds to a suitable DEFWAVEFORM form)."
  (let ((defs (parsed-program-waveform-definitions parsed-program)))
    (loop :for instr :across (parsed-program-executable-code parsed-program)
          ;; There are only two instruction types that matter for this
          :do (typecase instr
                (pulse
                 (resolve-waveform-reference (pulse-waveform instr) defs))
                (capture
                 (resolve-waveform-reference (capture-waveform instr) defs))))
    parsed-program))
