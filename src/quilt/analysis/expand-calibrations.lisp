;;;; expand-calibrations.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:cl-quil/quilt)

;;; Calibration Expansion
;;;
;;; This looks a lot like circuit expansion, and under the hood a lot of the
;;; mechanics are the same. A few differences are worth pointing out:
;;;
;;;   - The task of finding a calibration that matches a measurement or gate application
;;;     is a bit more complicated than what is done for circuit expansion (namely,
;;;     multiple definitions are allowed, with match priority given to later definitions).
;;;   - If *REQUIRE-APPLICABLE-CALIBRATION* is set, then the expansion must terminate in
;;;     non-(gate application, measure, measure discard) instructions.

(define-transform expand-calibrations (expand-calibrations)
  "This transform applies all available calibrations. The result has no gate applications or measurements for which a calibration is defined."
  expand-circuits)

;;; Calibration Matching

(defparameter *require-applicable-calibration* t
  "If T, an error will be signalled if an instruction fails to match an applicable calibration during calibration expansion.")

(defgeneric calibration-matches-p (defn instr)
  (:documentation "Check whether the calibration definition DEFN applies to the given instruction INSTR.")
  ;; The default is to not match.
  (:method (defn instr)
    nil)

  (:method ((defn gate-calibration-definition) (instr gate-application))
    (flet ((params-match (app-params cal-params)
             (and (= (length app-params)
                     (length cal-params))
                  (every (lambda (app-param cal-param)
                           (if (is-constant cal-param)
                               (constant= app-param cal-param)
                               (is-param cal-param)))
                         app-params
                         cal-params)))
           (args-match (app-args cal-args)
             (and (= (length app-args)
                     (length cal-args))
                  (every (lambda (app-arg cal-arg)
                           (if (qubit-p cal-arg)
                               (qubit= app-arg cal-arg)
                               (is-formal cal-arg)))
                         app-args
                         cal-args))))
      (and (operator-description= (application-operator instr)
                                  (calibration-definition-operator defn))
           (params-match (application-parameters instr)
                         (calibration-definition-parameters defn))
           (args-match (application-arguments instr)
                       (calibration-definition-arguments defn)))))

  (:method ((defn measure-calibration-definition) (instr measure))
    ;; Check that the measurement qubit matches, and the address matches.
    (and (if (qubit-p (measurement-calibration-qubit defn))
             (qubit= (measurement-qubit instr) (measurement-calibration-qubit defn))
             (is-formal (measurement-calibration-qubit defn)))
         (is-formal (measure-calibration-address defn))))

  (:method ((defn measure-discard-calibration-definition) (instr measure-discard))
    ;; Just check that the measurement qubit matches.
    (if (qubit-p (measurement-calibration-qubit defn))
        (qubit= (measurement-qubit instr) (measurement-calibration-qubit defn))
        (is-formal (measurement-calibration-qubit defn)))))

(defun compute-calibration-tables (parsed-program)
  "Extract the calibration definitions from PARSED-PROGRAM. Returns three values:

1. a hash table of gate calibrations (keyed by OPERATOR-DESCRIPTION),

2. a list of measure calibrations,

3. a list of measure discard calibrations."
  (let ((gate-calibrations
          (make-hash-table :test #'operator-description=
                           :hash-function #'operator-description-hash))
        (measure-calibrations nil)
        (measure-discard-calibrations nil))
    (dolist (defn (parsed-program-calibration-definitions parsed-program))
      ;; Assuming that the calibration definitions are in the same order
      ;; as their position in the Quilt program, this will put them in
      ;; reverse order.
      (etypecase defn
        (gate-calibration-definition
         (push defn (gethash (calibration-definition-operator defn)
                             gate-calibrations)))
        (measure-calibration-definition
         (push defn measure-calibrations))
        (measure-discard-calibration-definition
         (push defn measure-discard-calibrations))))
    (values gate-calibrations
            measure-calibrations
            measure-discard-calibrations)))

;;; Instruction Instantiation

(defun ensure-instantiated (obj &key param-value arg-value)
  "Ensure that OBJ is instantiated with respect to the provided parameter or argument values."
  (cond ((is-formal obj)
         (assert arg-value () "Formal argument instantiation requires :ARG-VALUE to be specified.")
         (funcall arg-value obj))
        ((is-param obj)
         (assert param-value () "Parameter instantiation requires :PARAM-VALUE to be specified.")
         (funcall param-value obj))
        ((delayed-expression-p obj)
         (evaluate-delayed-expression
          ;; pass the buck upwards
          (map-de-params (substitute-parameter param-value) obj)))
        (t obj)))

(defun instantiate-frame (frame arg-value)
  "Instantiate FRAME with respect to the argument values represented by ARG-VALUE, constructing a new frame if needed."
  (let* ((remake nil)
         (qubits (mapcar (flag-on-update remake
                                         (lambda (q) (ensure-instantiated q :arg-value arg-value)))
                         (frame-qubits frame))))
    (if remake
        (let ((instantiated (frame qubits (frame-name frame))))
          (setf (frame-name-resolution instantiated)
                (frame-name-resolution frame))
          instantiated)
        frame)))

(defun instantiate-waveform-ref (waveform param-value)
  "Instantiate the waveform reference WAVEFORM with respect to the parameter values represented by PARAM-VALUE, constructing a new waveform if needed."
  (let ((remake nil))
    (flet ((instantiate-parameter (assoc)
             (destructuring-bind (name . value) assoc
               (let ((instantiated-value (ensure-instantiated value :param-value param-value)))
                 (if (eq value instantiated-value)
                     ;; Remember folks, a CONS saved is a CONS earned.
                     assoc
                     (cons name instantiated-value))))))
      (let ((updated-alist (mapcar (flag-on-update remake #'instantiate-parameter)
                                   (waveform-ref-parameter-alist waveform))))
        (if remake
            (let ((new-wf (waveform-ref (waveform-ref-name waveform)
                                        updated-alist)))
              (setf (waveform-ref-name-resolution new-wf)
                    (waveform-ref-name-resolution waveform))
              new-wf)
            waveform)))))

(defmethod instantiate-instruction ((instr simple-frame-mutation) param-value arg-value)
  (let ((frame (instantiate-frame (frame-mutation-target-frame instr)
                                  arg-value))
        (value (ensure-instantiated (frame-mutation-value instr)
                                    :param-value param-value)))
    (if (and (eq frame (frame-mutation-target-frame instr))
             (eq value (frame-mutation-value instr)))
        instr
        (make-instance (class-of instr)
          :frame frame
          :value value))))

(defmethod instantiate-instruction ((instr swap-phase) param-value arg-value)
  (let ((left-frame (instantiate-frame (swap-phase-left-frame instr)
                                       arg-value))
        (right-frame (instantiate-frame (swap-phase-right-frame instr)
                                        arg-value)))
    (if (and (eq left-frame (swap-phase-left-frame instr))
             (eq right-frame (swap-phase-right-frame instr)))
        instr
        (make-instance 'swap-phase
          :left-frame left-frame
          :right-frame right-frame))))

(defmethod instantiate-instruction ((instr pulse) param-value arg-value)
  (let ((frame (instantiate-frame (pulse-frame instr)
                                  arg-value))
        (waveform (instantiate-waveform-ref (pulse-waveform instr)
                                            param-value)))
    (if (and (eq frame (pulse-frame instr))
             (eq waveform (pulse-waveform instr)))
        instr
        (make-instance 'pulse
          :frame frame
          :waveform waveform
          :nonblocking (nonblocking-p instr)))))

(defmethod instantiate-instruction ((instr capture) param-value arg-value)
  (let ((frame (instantiate-frame (capture-frame instr)
                                  arg-value))
        (waveform (instantiate-waveform-ref (capture-waveform instr)
                                            param-value))
        (memory-ref (ensure-instantiated (capture-memory-ref instr)
                                         :arg-value arg-value)))
    (check-mref memory-ref)
    (if (and (eq frame (capture-frame instr))
             (eq waveform (capture-waveform instr))
             (eq memory-ref (capture-memory-ref instr)))
        instr
        (make-instance 'capture
          :frame frame
          :waveform waveform
          :memory-ref memory-ref
          :nonblocking (nonblocking-p instr)))))

(defmethod instantiate-instruction ((instr raw-capture) param-value arg-value)
  (let ((frame (instantiate-frame (raw-capture-frame instr)
                                  arg-value))
        (memory-ref (ensure-instantiated (raw-capture-memory-ref instr)
                                         :arg-value arg-value))
        (duration (ensure-instantiated (raw-capture-duration instr)
                                       :param-value param-value)))
    (check-mref memory-ref)
    (if (and (eq frame (raw-capture-frame instr))
             (eq memory-ref (raw-capture-memory-ref instr))
             (eq duration (raw-capture-duration instr)))
        instr
        (make-instance 'raw-capture
          :frame frame
          :duration duration
          :memory-ref memory-ref
          :nonblocking (nonblocking-p instr)))))

(defmethod instantiate-instruction ((instr delay-on-qubits) param-value arg-value)
  (let ((duration (ensure-instantiated (delay-duration instr)
                                       :param-value param-value)))
    (if (and (eq duration (delay-duration instr))
             (not (some #'is-formal (delay-qubits instr))))
        instr
        (make-instance 'delay-on-qubits
          :duration duration
          :qubits (mapcar (transform-if #'is-formal arg-value)
                          (delay-qubits instr))))))

(defmethod instantiate-instruction ((instr delay-on-frames) param-value arg-value)
  (let* ((remake nil)
         (duration (ensure-instantiated (delay-duration instr)
                                        :param-value param-value))
         (frames (mapcar (flag-on-update remake
                                         (lambda (f) (instantiate-frame f arg-value)))
                         (delay-frames instr))))
    (if (and (eq duration (delay-duration instr))
             (not remake))
        instr
        (make-instance 'delay-on-frames
          :duration duration
          :frames frames))))

(defmethod instantiate-instruction ((instr fence) param-value arg-value)
  (let* ((remake nil)
         (qubits (mapcar (flag-on-update remake
                                         (lambda (q) (ensure-instantiated q :arg-value arg-value)))
                         (fence-qubits instr))))
    (if remake
        (make-instance 'fence :qubits qubits)
        instr)))

;;; Calibrations instantiate to their bodies, with parameters and formals substituted
(defmethod instantiate-instruction ((instr gate-calibration-definition) param-value arg-value)
  (instantiate-definition-body instr
                               (calibration-definition-body instr)
                               (calibration-definition-parameters instr)
                               param-value
                               (calibration-definition-arguments instr)
                               arg-value))

(defmethod instantiate-instruction ((instr measure-calibration-definition) param-value arg-value)
  (instantiate-definition-body instr
                               (calibration-definition-body instr)
                               nil
                               param-value
                               (list (measurement-calibration-qubit instr)
                                     (measure-calibration-address instr))
                               arg-value))

(defmethod instantiate-instruction ((instr measure-discard-calibration-definition) param-value arg-value)
  (instantiate-definition-body instr
                               (calibration-definition-body instr)
                               nil
                               param-value
                               (list (measurement-calibration-qubit instr))
                               arg-value))

(defgeneric instantiate-applicable-calibration (instr gate-cals measure-cals measure-discard-cals)
  (:documentation "If INSTR has an associated calibration, return a list of instructions instantiated from the body of the calibration definition. Otherwise, return NIL.")

  (:method ((instr gate-application) gate-cals measure-cals measure-discard-cals)
    (declare (ignore measure-cals measure-discard-cals))
    (let ((op (application-operator instr)))
      (unless (plain-operator-p op)
        (return-from instantiate-applicable-calibration nil))

      (a:if-let ((defn (find-if (lambda (defn) (calibration-matches-p defn instr))
                                (gethash op gate-cals))))
        (instantiate-instruction defn
                                 (application-parameters instr)
                                 (application-arguments instr))
        nil)))

  (:method ((instr measure) gate-cals measure-cals measure-discard-cals)
    (declare (ignore gate-cals measure-discard-cals))
    (a:if-let ((defn (find-if (lambda (defn) (calibration-matches-p defn instr))
                              measure-cals)))
      (instantiate-instruction defn
                               nil
                               (list (measurement-qubit instr)
                                     (measure-address instr)))
      nil))

  (:method ((instr measure-discard) gate-cals measure-cals measure-discard-cals)
    (declare (ignore gate-cals measure-cals))
    (a:if-let ((defn (find-if (lambda (defn) (calibration-matches-p defn instr))
                              measure-discard-cals)))
      (instantiate-instruction defn
                               nil
                               (list (measurement-qubit instr)))
      nil))

  (:method (instr gate-cals measure-cals measure-discard-cals)
    nil))

(defun recursively-expand-instruction (instr gate-cals measure-cals measure-discard-cals)
  (let ((*expansion-depth* (1+ *expansion-depth*)))
    (unless (<= *expansion-depth* *expansion-limit*)
      (quil-parse-error "Exceeded recursion limit of ~D for calibration expansion. ~
                         Current object being expanded is ~A."
                        *expansion-limit*
                        instr))
    (a:if-let ((expanded (instantiate-applicable-calibration instr gate-cals measure-cals measure-discard-cals)))
      ;; Recursively expand the new instructions
      (a:mappend (lambda (instr)
                   (recursively-expand-instruction instr
                                                   gate-cals
                                                   measure-cals
                                                   measure-discard-cals))
                 expanded)
      ;; Otherwise, handle the base case.
      (if (and *require-applicable-calibration*
               (typep instr '(or gate-application measurement)))
          (quil-expansion-error "Expected a calibration definition associated with ~A, but none was found."
                                instr)
          (list instr)))))

;;; Driver

(defun expand-calibrations (parsed-program)
  "Expand all gate applications and measurements in PARSED-PROGRAM for which there is a corresponding calibration definition."
  (multiple-value-bind (gate-cals measure-cals measure-discard-cals)
      (compute-calibration-tables parsed-program)
    (let ((*expansion-context* ':DEFCAL)
          (*expansion-depth* 0))
      (let ((expanded
              (loop :for instr :across (parsed-program-executable-code parsed-program)
                    :for expanded-instrs := (recursively-expand-instruction instr
                                                                            gate-cals
                                                                            measure-cals
                                                                            measure-discard-cals)
                    ;; we need to resolve the new instructions (e.g. since frame
                    ;; `0 q "cz"` might instantiate to to `0 1 "cz"`)
                    :append (mapcar (lambda (i) (resolve-instruction i parsed-program))
                                    expanded-instrs))))
        (setf (parsed-program-executable-code parsed-program)
              (coerce expanded 'vector)))))
  parsed-program)
