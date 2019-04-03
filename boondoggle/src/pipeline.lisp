;;;; pipeline.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:boondoggle)


(defmacro pipeline (vars &rest pipes)
  ;; first, deal with all the definition bindings
  (let* ((vars (nreverse vars))
         (pipes (nreverse pipes))
         (result-form `,(first (first pipes))))
    (dolist (pipe pipes)
      (destructuring-bind (name lambda-list instruction) pipe
        (let ((lambda-names-only (mapcar #'first lambda-list))
              (lambda-list-reverse (reverse lambda-list)))
          (dolist (lambda-form lambda-list-reverse)
            (destructuring-bind (lambda-name lambda-bound) lambda-form
              (setf instruction
                    `(loop :for ,lambda-name :below (length ,lambda-bound) :collect ,instruction))))
          (setf result-form
                `(let ((,name ,instruction))
                   (flet ((,name ,lambda-names-only
                            ,(let ((recursive-ref name))
                               (dolist (lambda-name lambda-list recursive-ref)
                                 (setf recursive-ref
                                       `(nth ,(first lambda-name) ,recursive-ref))))))
                     ,result-form))))))
    (dolist (var vars)
      (destructuring-bind (name lambda-list data) var
        (cond
          ((null lambda-list)
           (setf result-form
                 `(let ((,name ,data))
                    (flet ((,name () ,name))
                      ,result-form))))
          (t
           (setf result-form
                 `(let ((,name ,data))
                    (flet ((,name ,lambda-list (nth ,@lambda-list ,name)))
                      ,result-form)))))))
    result-form))
