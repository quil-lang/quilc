(defpackage #:quil/ast/memory
  (:use #:coalton)
  (:export
   #:QuilType
   #:QuilBit
   #:QuilOctet
   #:QuilInteger
   #:QuilReal
   #:Ref
   #:Offset
   #:offset-type
   #:offset-amount
   #:Descriptor
   #:ref-to
   #:ref-to-at
   #:single
   ))
  
(in-package #:quil/ast/memory)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  
  (repr :enum)
  (define-type QuilType
    "A valid data type for Quil memory."
    QuilBit QuilOctet QuilInteger QuilReal)

  (define-type Ref
    "A Memory Reference"
    (Ref String                         ; name
         Ufix))                         ; position

  (define-type Offset
    "Used in declaring offsets into shared memory declarations"
    (Offset QuilType Ufix))

  (define (offset-type (Offset type _)) type)
  (define (offset-amount (Offset _ amount)) amount)

  (define-type Descriptor
    (Descriptor
     String
     QuilType
     Ufix
     Boolean
     (List Offset)))

  ;;; conveniences

  (define (ref-to (Descriptor name _ _ _ _))
    (Ref name 0))

  (define (ref-to-at (Descriptor name _ _ _ _) i)
    (Ref name i))

  (define (single name type)
    (Descriptor name type 1 False Nil))



)
