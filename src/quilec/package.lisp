;;;; src/quilec/package.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(defpackage #:cl-quil/quilec
  (:use #:common-lisp
        #:alexandria
        #:cl-quil
        #:cl-quil/clifford)
  (:import-from #:alexandria #:non-negative-fixnum #:if-let #:when-let)
  (:import-from #:cl-quil/clifford #:pauli-components #:print-pauli)
  (:export
   #:cleve-gottesman                    ; FUNCTION
   #:codeword-circuit                   ; FUNCTION
   #:group                              ; CLASS
   #:make-group                         ; FUNCTION
   #:make-stabilizer-group              ; FUNCTION
   #:stabilizer-group                   ; CLASS
   ))
