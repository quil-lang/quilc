;;;; with-inst.lisp
;;;;
;;;; Authors: Eric Peterson, Zach Beane
;;;;
;;;; This file contains a macro WITH-INST for constructing lists of
;;;; Quil instructions. Originally this was written to support
;;;; DEFINE-COMPILER, but is useful more generally.

(in-package #:cl-quil/frontend)

;;; Within the dynamic extent of a DEFINE-COMPILER, we publish
;;; instructions to output using a DEFINE-VOP-like INST macro, and we
;;; quit a compiler early using FINISH-COMPILER.

;;; Special variable private to WITH-INST. Within the context of
;;; WITH-INST, it will be bound to a cons (LIST . TAIL).
(declaim (type cons *inst-collection*))
(defvar *inst-collection*)

(declaim (inline %new-inst-collection %accumulate-inst %collected-insts))
(defun %new-inst-collection ()
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (let* ((list (cons nil nil))
         (tail list))
    (cons list tail)))

(defun %accumulate-inst (x)
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (let* ((tail (cdr *inst-collection*)))
    (declare (type cons tail))
    (rplacd tail (cons x nil))
    (rplacd *inst-collection* (cdr tail))
    (values)))

(defun %collected-insts ()
  (cdar *inst-collection*))

(defconstant +inst-tag+ '%%inst-tag%%)  ; Tag used to throw values from INST.

(defmacro with-inst (() &body body)
  "Allow INST, INST*, and FINISH-COMPILER handlers extending over BODY.

This macro is used for DEFINE-COMPILER."
  `(let ((*inst-collection* (%new-inst-collection)))
     (catch ',+inst-tag+
       ,@body
       (%collected-insts))))            ; FINISH-COMPILER would also be OK.

(defun finish-compiler (&optional (retval nil retval-p))
  "Finish compiling, by default returning a list of all instructed emitted with INST.

Optionally, if an argument is supplied, it will be returned instead.

FINISH-COMPILER can only be called within the dynamic extent of a compiler body."
  (unless (boundp '*inst-collection*)
    (error "FINISH-COMPILER called outside of a WITH-INST context."))
  (throw +inst-tag+ (if retval-p retval (%collected-insts))))

(defun inst (&rest xs)
  "Emit the instructions XS in the current compiler context.

INST can only be called within the dynamic extent of a compiler body."
  (declare (dynamic-extent xs))
  (unless (boundp '*inst-collection*)
    (error "INST called outside of a WITH-INST context."))
  ;; INST is a no-op if nothing is provided.
  (when (null xs)
    (return-from inst))
  ;; Otherwise we have to parse it out.
  (let ((xs-len (length xs))
        (x nil))
    (cond
      ;; check for a raw gate object
      ((and (= 1 xs-len)
            (typep (first xs) 'gate-application))
       (setf x (first xs)))
      ;; check for an anon-gate signature
      ((and (<= 3 xs-len)
            (or (typep (second xs) 'magicl:matrix)
                (typep (first xs) 'gate)))
       (setf x (apply #'anon-gate xs)))
      ;; check for a build-gate signature
      ((and (<= 3 xs-len)
            (listp (second xs)))
       (setf x (apply #'build-gate xs)))
      (t
       (error "INST argument pattern not recognized: ~A" xs)))
    (%accumulate-inst x)))

(defun inst* (&rest xs)
    "Emit the instructions XS in the current compiler context. Treat the last argument of XS as a list of arguments, as if using APPLY #'BUILD-GATE.

INST* can only be called within the dynamic extent of a compiler body."
  (apply #'inst (apply #'list* xs)))
