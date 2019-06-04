;;;; environment.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

(define-condition gate-not-found (error)
  ((name :initarg :name
         :reader gate-not-found-name))
  (:documentation "An error that is signaled in the event a gate was require but not found.")
  (:report (lambda (condition stream)
             (format stream "A gate named ~S was required but the definition is not known."
                     (gate-not-found-name condition)))))

(defmacro tolerate-unknown-gate (&body body)
  "Evaluate BODY. If a gate wasn't found, just reurn NIL."
  `(a:ignore-some-conditions (gate-not-found) ,@body))

(defgeneric lookup-gate-in-environment (gate environs)
  (:documentation "Looks up a definition of the instruction GATE inside of the environment ENVIRONS. Results in an object compatible with GATE-MATRIX if a definition is found. Signal the error GATE-NOT-FOUND if it wasn't found."))

;; TODO: in general, a local environment might have a parent field so that it
;;       can recursively call lookup-gate-in-environment in case a local def'n
;;       is not found. by default, we can always use the system of generic fns
;;       to reference the basic lookup-gate-in-environment call that thumbs
;;       through the global tables.

(defmethod lookup-gate-in-environment ((gate operator-description) environs)
  (funcall (operator-description-gate-lifter gate)
           (lookup-gate-in-environment (operator-description-root-name gate) environs)))

(defmethod lookup-gate-in-environment ((gate string) (environs null))
  (declare (ignore environs))
  (or (lookup-standard-gate gate)
      (error 'gate-not-found :name gate)))

(defmethod lookup-gate-in-environment ((gate string) (environs parsed-program))
  (a:if-let ((gate-defn (find gate
                              (parsed-program-gate-definitions environs)
                              :test #'string=
                              :key #'gate-definition-name)))
    gate-defn
    (lookup-gate-in-environment gate nil)))

(defmethod lookup-gate-in-environment ((gate gate-application) environs)
  (if (anonymous-gate-application-p gate)
      (gate-application-gate gate)
      (lookup-gate-in-environment (application-operator gate) environs)))

