;;;; src/quilec/package.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(defpackage #:cl-quil.quilec
  (:nicknames #:qec)
  (:use #:common-lisp
        #:alexandria
        #:cl-quil
        #:cl-quil.clifford)
  (:import-from #:alexandria #:non-negative-fixnum #:if-let #:when-let)
  (:import-from #:cl-quil.clifford #:pauli-components)
  (:export #:code
           #:make-code
           #:cleve-gottesman))
