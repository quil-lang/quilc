;;;; transformable-mixin.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil/frontend)

;;; This file doesn't itself have any transforms in it, but rather
;;; manages the documentation and recording of transforms that have
;;; occurred on an object.

(defvar *transforms* (make-hash-table :test 'eq)
  "A table of defined transforms.")

(defun find-transform (xform)
  "Find the transform named XFORM, return its description, or NIL if not found."
  (nth-value 0 (gethash xform *transforms*)))

(defstruct transform-description
  "A description of a transform."
  name
  documentation
  predecessors
  function)

(defmacro define-transform (name (function-name &rest options)
                            &body docs-and-predecessors)
  "Define a transform pass named NAME for the function named FUNCTION-NAME, with optional documentation and predecessor transforms. A predecessor transform is one that has to be executed for this transforms to be valid."
  (check-type name (and symbol (not null)))
  (assert (null options) (options)
          "No options are supported at this time.")
  (multiple-value-bind (predecessors decls doc)
      (a:parse-body (append docs-and-predecessors '(nil)) :documentation t)
    (assert (null decls) (docs-and-predecessors)
            "DEFINE-TRANSFORM can't have any declarations. Found ~S." decls)
    (assert (every #'symbolp predecessors) (docs-and-predecessors)
            "DEFINE-TRANSFORM takes a list of transforms as symbols. Got ~S."
            predecessors)
    `(progn
       (setf (gethash ',name *transforms*)
             (make-transform-description
              :name ',name
              :documentation ,doc
              :predecessors ',(remove-if #'null predecessors)
              :function ',function-name))
       ',name)))

(defclass transformable ()
  ((transforms
    :accessor transforms-performed
    :initform nil
    :documentation "All of the transforms that have been performed on the instance."))
  (:documentation "A mixin for classes of objects which can be transformed."))

(defgeneric transformedp (instance transform)
  (:documentation "Has the TRANSFORMABLE object INSTANCE been transformd with TRANSFORM?")
  (:method ((instance transformable) (xform symbol))
    (and (member xform (transforms-performed instance) :test #'eq)
         t)))

(defgeneric record-transform (xform instance)
  (:documentation "Record in the TRANSFORMABLE object INSTANCE that the transform XFORM has been performed.")
  (:method ((xform symbol) (instance transformable))
    (pushnew xform (transforms-performed instance) :test #'eq)
    xform))

(defun validate-instance-for-transform-predecessors (xform instance)
  "Validate that the predecessors of XFORM have been performed on INSTANCE. Return the values (VALUES T NIL) if everything has been performed.

If any of its predecessors have not been performed, return NIL and the first one found that hasn't been performed."
  (let ((descr (find-transform xform)))
    (assert (not (null descr)) (xform) "Unknown transform ~S." xform)
    (dolist (pred (transform-description-predecessors descr)
             ;; Return T on success
             (values t nil))
      (multiple-value-bind (validated? missing-xform)
          (validate-instance-for-transform pred instance)
        (unless validated?
          (return (values nil missing-xform)))))))

(defun validate-instance-for-transform (xform instance)
  "Validate that XFORM and its predecessors have been performed on INSTANCE. Return the values (VALUES T NIL) if everything has been performed.

If any of XFORM or its predecessors have not been performed, return NIL and the first one found that hasn't been performed."
  ;; Check this xform.
  (unless (member xform (transforms-performed instance))
    (return-from validate-instance-for-transform (values nil xform)))

  ;; Check all of its predecessors.
  (validate-instance-for-transform-predecessors xform instance))

(define-condition unsatisfied-transform-dependency (error)
  ((attempted-transform :reader unsatisfied-transform-dependency-attempted-transform
                        :initarg :attempted-transform)
   (needed-transform :reader unsatisfied-transform-dependency-needed-transform
                     :initarg :needed-transform)
   (object :reader unsatisfied-transform-dependency-object
           :initarg :object))
  (:documentation "An error that is signalled when a prerequisite transform has not been done.")
  (:report (lambda (condition stream)
             (format stream "Cannot transform ~A by ~A because ~A ~
                             is a prerequisite transform."
                     (unsatisfied-transform-dependency-object condition)
                     (unsatisfied-transform-dependency-attempted-transform condition)
                     (unsatisfied-transform-dependency-needed-transform condition)))))

(defvar *max-transform-attempts* 5
  "The number of times to retry transforming when a dependency isn't satisfied.")

(defgeneric transform (xform instance &rest args)
  (:documentation "Perform and record the transform XFORM on the instance INSTANCE. Pass INSTANCE and ARGS to the respective function representing this transform.

Signals UNSATISFIED-TRANSFORM-DEPENDENCY if the transform cannot be applied because a transform is needed."))

(defmethod transform :before ((xform symbol) instance &rest args)
  (declare (ignore args))
  (assert (find-transform xform) (xform)
          "The transform ~S is unknown to me." xform)
  (prog ((attempts-left *max-transform-attempts*))
   :CHECK-AGAIN
     (multiple-value-bind (validated? xform-to-do)
         (validate-instance-for-transform-predecessors xform instance)
       (unless validated?
         (cond
           ((zerop attempts-left)
            (error "Exceeded the number of attempts to transform ~
                    an object with missing transform dependencies."))
           (t
            (cerror "Perform requisite transform."
                    'unsatisfied-transform-dependency
                    :attempted-transform xform
                    :needed-transform xform-to-do
                    :object instance)
            (transform xform-to-do instance)
            (decf attempts-left)
            (go :CHECK-AGAIN)))))))

(defmethod transform :around ((xform symbol) (instance transformable) &rest args)
  (declare (ignore args))
  (let ((xformed-instance (if (transformedp instance xform)
                              instance  ; Do transform only once.
                              (call-next-method))))
    (record-transform xform instance)
    xformed-instance))

(defmethod transform ((xform symbol) (instance transformable) &rest args)
  (apply (transform-description-function (find-transform xform))
         instance
         args))

