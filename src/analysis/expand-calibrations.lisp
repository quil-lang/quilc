(in-package #:cl-quil)

(define-transform expand-calibrations (expand-calibrations)
  "This transform applies all available calibrations. The result has no gate applications or
measurements for which a calibration is defined."
  process-includes
  resolve-applications
  expand-circuits)

(defparameter *gate-calibrations* (make-hash-table))

(defparameter *measure-calibrations* nil)

(defparameter *measure-discard-calibrations* nil)

(defun compute-calibration-tables (parsed-program)
  (let ((gate-calibrations (make-hash-table))
        measure-calibrations
        measure-discard-calibrations)
    (dolist (defn (parsed-program-calibration-definitions parsed-program))
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
    nil))

(defmethod calibration-matches-p ((defn gate-calibration-definition) (instr gate-application))
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

(defmethod calibration-matches-p ((defn measure-calibration-definition) (instr measure))
  ;; Check that the measurement qubit matches, and (depending on whether we discard results),
  ;; the address matches.
  (and (if (qubit-p (measurement-calibration-qubit defn))
           (equalp (measurement-qubit instr) (measurement-calibration-qubit defn))
           (is-formal (measurement-calibration-qubit defn)))
       (is-formal (measure-calibration-address defn))))

(defmethod calibration-matches-p ((defn measure-discard-calibration-definition) (instr measure-discard))
  (if (qubit-p (measurement-calibration-qubit defn))
      (equalp (measurement-qubit instr) (measurement-calibration-qubit defn))
      (is-formal (measurement-calibration-qubit defn))))

(defgeneric apply-calibration (instr)
  (:method ((instr gate-application))
    (let ((op (application-operator instr)))
      (unless (plain-operator-p op)
        (return-from apply-calibration instr))

      (a:if-let ((defn (find-if (lambda (defn) (calibration-matches-p defn instr))
                                (gethash (operator-description-name op) *gate-calibrations*))))
        (instantiate-definition defn
                                (application-parameters instr)
                                (application-arguments instr))
        instr)))

  (:method ((instr measure))
    (a:if-let ((defn (find-if (lambda (defn) (calibration-matches-p defn instr))
                              *measure-calibrations*)))
      (instantiate-definition defn
                              nil
                              (list (measurement-qubit instr)
                                    (measure-address instr)))
      instr))

  (:method ((instr measure-discard))
    (a:if-let ((defn (find-if (lambda (defn) (calibration-matches-p defn instr))
                              *measure-discard-calibrations*)))
      (instantiate-definition defn
                              nil
                              (list (measurement-qubit instr)))
      instr))
  (:method ((instr t))
    instr))

(defun expand-calibrations (parsed-program)
  ;; build up calibration tables
  (multiple-value-bind (*gate-calibrations*
                        *measure-calibrations*
                        *measure-discard-calibrations*)
      (compute-calibration-tables parsed-program)
    (let ((fully-expanded
            (loop :for instr :across (parsed-program-executable-code parsed-program)
                  :for expanded := (apply-calibration instr)
                  :if (listp expanded)
                    :append expanded
                  :else
                    :collect expanded)))
      (setf (parsed-program-executable-code parsed-program)
            (coerce fully-expanded 'vector))
      parsed-program)))
