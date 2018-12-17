;;;; src/scheduling.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil)

;;; This file contains basic routines for working with events and
;;; schedules abstractly.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass event ()
  ()
  (:metaclass abstract-class)
  (:documentation "Abstract representation of an event."))

(defgeneric left (event)
  (:documentation "The left endpoint of an event. The condition

    (< (LEFT EVENT) (RIGHT EVENT))

must be satisfied."))

(defgeneric right (event)
  (:documentation "The right endpoint of an event. The condition

    (< (LEFT EVENT) (RIGHT EVENT))

must be satisfied."))

(defclass explicit-event (event)
  ((left :initarg :left
         :accessor left)
   (right :initarg :right
          :accessor right))
  (:metaclass abstract-class)
  (:documentation "Abstract representation of an event with explicitly specified endpoints."))

(defmethod initialize-instance :after ((instance event) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((left (left instance))
        (right (right instance)))
    (check-type left (real 0))
    (check-type right (real (0)))
    (assert (< left right))))

(defgeneric shift-event (event shift)
  (:documentation "Shift the event EVENT in time by SHIFT units of time.")
  (:method ((event explicit-event) shift)
    (let ((e (copy-instance event)))
      (incf (left e) shift)
      (incf (right e) shift)
      e)))

(defun sigma (shift)
  "Create an event transformer that shifts it relatively by SHIFT units of time."
  (lambda (event)
    (shift-event event shift)))

(defun coalescable (a b)
  "Are events A and B coalescable?"
  (flet ((overlap (a b)
           (and (<= (left a)
                    (left b))
                (< (left b)
                   (right a))))
         (adjacent (a b)
           (= (right a) (left b))))
    (or (overlap a b) (adjacent a b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Schedules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant ∞
  #+sbcl sb-ext:double-float-positive-infinity
  #+ccl CCL::DOUBLE-FLOAT-POSITIVE-INFINITY
  #-(or sbcl ccl) (* 2 most-positive-double-float))

(defclass schedule ()
  ((resource-schedules :initarg :resource-schedules
                       :reader resource-schedules))
  (:documentation "Representation of a \"schedule\", a collection of resource schedules."))

(deftype resource-schedule ()
  '(array * (*)))

(defun make-resource-schedule ()
  (make-array 10 :adjustable t
                 :fill-pointer 0
                 :initial-element nil))

(defun schedule-event (S e)
  "Schedule the event e in the resource schedule S."
  (assert (or (zerop (length S))
              (not (< (left e)
                      (right (aref S (1- (length S))))))))
  (vector-push-extend e S))

(defun make-schedule (n)
  "Make a schedule of N resource schedules."
  (make-instance 'schedule
                 :resource-schedules
                 (make-array n
                             :initial-contents
                             (loop :repeat n
                                   :collect (make-resource-schedule)))))

(defun schedule-close (Σ i &optional (otherwise 0))
  "At what time does the latest event of resource schedule Σᵢ finish? If Σᵢ has no events, then return OTHERWISE."
  (let* ((S (aref (resource-schedules Σ) i))
         (l (length S)))
    (if (zerop l)
        otherwise
        (right (aref S (1- l))))))

(defun left-contour (Σ)
  "Compute the left contour of the schedule Σ. The \"left contour\" is a sequence of the earliest event starting points for each resource schedule (or 0). "
  (let* ((l (length (resource-schedules Σ)))
         (contour (make-array l)))
    (dotimes (i l contour)
      (let ((S (aref (resource-schedules Σ) i)))
        (setf (aref contour i)
              (if (plusp (length S))
                  (left (aref S 0))
                  0))))))

(defun right-contour (Σ)
  "Compute the right contour of the schedule Σ. The \"right contour\" is a sequence of the latest times of each resource schedule (or infinity)."
  (let* ((l (length (resource-schedules Σ)))
         (contour (make-array l)))
    (dotimes (i l contour)
      (setf (aref contour i) (schedule-close Σ i ∞)))))

(defun minimum-gap (Σ Γ)
  "Compute the minimum gap length between the two schedules Σ and Γ."
  (loop :for l :across (right-contour Σ)
        :for r :across (left-contour Γ)
        :minimize (- l r)))

(defun white-box-transform (Γ)
  "Create an accretive transformation that optimally schedules the subschedule Γ into an arbitrary schedule."
  (lambda (Σ)
    (let* ((δ (minimum-gap Σ Γ))
           (σ (sigma (- δ))))
      (loop :for S :across (resource-schedules Σ)
            :for G :across (resource-schedules Γ)
            :for σG := (map 'list σ G) :do
              (mapc (lambda (e) (schedule-event S e)) σG)))
    Σ))
