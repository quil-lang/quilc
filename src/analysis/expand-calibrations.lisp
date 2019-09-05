(in-package #:cl-quil)

;;; Calibration Expansion
;;;
;;; This looks a lot like circuit expansion, and under the hood a lot of the
;;; mechanics are the same. A few differences are worth pointing out:
;;;
;;;   - In its current incarnation, calibration expansion is not recursive. The assumption
;;;     is that calibration bodies contain simple quilt instructions and nothing else.
;;;   - The task of finding a calibration that matches a measurement or gate application
;;;     is a bit more complicated than what is done for circuit expansion (namely,
;;;     multiple definitions are allowed, and we match (TODO actually spell this out).

;;; TODO Enforce that calibration bodies contain simple quilt instructions...

(define-transform expand-calibrations (expand-calibrations)
  "This transform applies all available calibrations. The result has no gate applications or
measurements for which a calibration is defined."
  expand-circuits)

(defparameter *gate-calibrations* (make-hash-table :test 'equal))
(defparameter *measure-calibrations* nil)
(defparameter *measure-discard-calibrations* nil)

(defun compute-calibration-tables (parsed-program)
  "Extract the calibration definitions from PARSED-PROGRAM. Returns three values:

1. a hash table of gate calibrations (keyed by name),

2. a list of measure calibrations,

3. a list of measure discard calibrations."
  (let ((gate-calibrations
          (make-hash-table :test 'equal)) ; quil is case sensitive
        measure-calibrations
        measure-discard-calibrations)
    (dolist (defn (parsed-program-calibration-definitions parsed-program))
      ;; Assuming that the calibration definitions are in the same order
      ;; as their position in the quilt program, this will put them in
      ;; reverse order.
      (etypecase defn
        (gate-calibration-definition
         (push defn (gethash (calibration-definition-name defn)
                             gate-calibrations)))
        (measure-calibration-definition
         (push defn measure-calibrations))
        (measure-discard-calibration-definition
         (push defn measure-discard-calibrations))))
    (values gate-calibrations
            measure-calibrations
            measure-discard-calibrations)))

(defgeneric calibration-matches-p (defn instr)
  (:documentation "Check whether the calibration definition DEFN applies to the given instruction INSTR.")
  ;; The default is to not match.
  (:method (defn instr)
    nil)

  (:method ((defn gate-calibration-definition) (instr gate-application))
    (with-slots (operator parameters arguments) instr
      ;; Check INSTR is a simple application, with the right operator name
      (unless (and (plain-operator-p operator)
                   (string= (operator-description-name operator)
                            (calibration-definition-name defn)))
        (return-from calibration-matches-p))

      ;; Check that the parameters match.
      (unless (and (= (length parameters)
                      (length (calibration-definition-parameters defn)))
                   ;; For each calibration parameter, there are two cases:
                   ;; constant values or formal parameters
                   (every (lambda (app-param cal-param)
                            (if (is-constant cal-param)
                                (equalp app-param cal-param) ; TODO is this the right comparison to make?
                                (is-param cal-param)))
                          parameters
                          (calibration-definition-parameters defn)))
        (return-from calibration-matches-p))

      ;; Check that the arguments match.
      (unless (and (= (length arguments)
                      (length (calibration-definition-arguments defn)))
                   (every (lambda (app-arg cal-arg)
                            (if (qubit-p cal-arg)
                                (equalp app-arg cal-arg) ; TODO is this the right comparison to make?
                                (is-formal cal-arg)))
                          arguments
                          (calibration-definition-arguments defn)))
        (return-from calibration-matches-p))
      t))

  (:method ((defn measure-calibration-definition) (instr measure))
    ;; Check that the measurement qubit matches, and the address matches.
    (and (if (qubit-p (measurement-calibration-qubit defn))
             (equalp (measurement-qubit instr) (measurement-calibration-qubit defn))
             (is-formal (measurement-calibration-qubit defn)))
         (is-formal (measure-calibration-address defn))))

  (:method ((defn measure-discard-calibration-definition) (instr measure-discard))
    ;; Just check that the measurement qubit matches.
    (if (qubit-p (measurement-calibration-qubit defn))
        (equalp (measurement-qubit instr) (measurement-calibration-qubit defn))
        (is-formal (measurement-calibration-qubit defn)))))

(defgeneric apply-calibration (instr)
  (:documentation "If INSTR has an associated calibration, return a list of instructions instantiated from the body of the calibration definition. Otherwise, return NIL.")

  (:method ((instr gate-application))
    (let ((op (application-operator instr)))
      (unless (plain-operator-p op)
        (return-from apply-calibration))

      (a:if-let ((defn (find-if (lambda (defn) (calibration-matches-p defn instr))
                                (gethash (operator-description-name op) *gate-calibrations*))))
        (instantiate-definition defn
                                (application-parameters instr)
                                (application-arguments instr))
        nil)))

  (:method ((instr measure))
    (a:if-let ((defn (find-if (lambda (defn) (calibration-matches-p defn instr))
                              *measure-calibrations*)))
      (instantiate-definition defn
                              nil
                              (list (measurement-qubit instr)
                                    (measure-address instr)))
      nil))

  (:method ((instr measure-discard))
    (a:if-let ((defn (find-if (lambda (defn) (calibration-matches-p defn instr))
                              *measure-discard-calibrations*)))
      (instantiate-definition defn
                              nil
                              (list (measurement-qubit instr)))
      nil))

  (:method ((instr t))
    nil))

(defparameter *require-applicable-calibration* nil
  "If T, an error will be signalled if an instruction fails to match an
  applicable calibration during calibration expansion.")

(defun recursively-expand-instruction (instr)
  (let ((expanded (apply-calibration instr)))
    (format t "Expanding ~A to ~A~%" instr expanded)
    (when (and *require-applicable-calibration*
               (null expanded)
               (typep instr '(or gate-application measure measure-discard)))
      (quil-parse-error "Expected a calibration definition associated with ~A, but none was found."
                        instr))
    (if expanded
        ;; Recursively expand the new instructions
        (a:mappend #'recursively-expand-instruction expanded)
        (list instr))))

(defun expand-calibrations (parsed-program &key strict)
  "Expand all gate applications and measurements in PARSED-PROGRAM for which there is a corresponding calibration definition. If STRICT is T, then it is an error for a gate application or measurement to not have a matching calibration."
  (multiple-value-bind (*gate-calibrations*
                        *measure-calibrations*
                        *measure-discard-calibrations*)
      (compute-calibration-tables parsed-program)
    (let* ((*require-applicable-calibration* strict)
           (expanded
            (loop :for instr :across (parsed-program-executable-code parsed-program)
                  :append (recursively-expand-instruction instr))))
      (setf (parsed-program-executable-code parsed-program)
            (coerce expanded 'vector))
      parsed-program)))
