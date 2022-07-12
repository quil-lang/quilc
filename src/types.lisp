;;;; src/types.lisp
;;;;
;;;; Author: Appleby
;;;;
;;;; This file contains general-purpose types and type-predicates. Types that are more specialized
;;;; do not belong here and should live in the file or package where they are most used,
;;;; e.g. OPTIMAL-2Q-TARGET-ATOM in compilers/approx.lisp.

(in-package #:cl-quil/frontend)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet
      ((def (type-name predicate-name container-type element-type)
         (check-type type-name symbol)
         (check-type predicate-name symbol)
         (check-type container-type symbol)
         (let* ((name (symbol-name predicate-name))
                (article (if (position (schar name 0) "AEIOU") "an" "a"))
                (predicate-docstring
                 (format nil "Is OBJECT ~A ~A where every element satisfies (TYPEP element '~A)."
                         article container-type element-type))
                (deftype-docstring
                  (format nil "Type of an object that (SATISFIES ~A)." predicate-name)))
           `(progn
              (defun ,predicate-name (object)
                ,predicate-docstring
                (and (typep object ',container-type)
                     (every (lambda (element)
                              (typep element ',element-type))
                            object)))
              (deftype ,type-name ()
                ,deftype-docstring
                '(satisfies ,predicate-name))))))

    ;; Normally, we'd want ALEXANDRIA:PROPER-LIST in place of LIST in the definitions
    ;; below. However, because the predicate we define calls EVERY on the OBJECT (which will error
    ;; if given an improper list), we're OK to use LISTs here.
    (def integer-list      integer-list-p      list     integer)
    (def number-list       number-list-p       list     number)
    (def symbol-list       symbol-list-p       list     symbol)
    (def string-sequence   string-sequence-p   sequence string)
    (def integeropt-vector integeropt-vector-p vector   (or null integer))
    (def integer-vector    integer-vector-p    vector   integer)))

(deftype unsigned-fixnum ()
  `(and fixnum unsigned-byte))
