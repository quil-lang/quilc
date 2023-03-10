;;;; src/discrete/package.lisp
;;;;
;;;; Author: Robert Smith, A.J. Nyquist

(defpackage #:cl-quil.discrete/numeric
  (:documentation "Numeric systems")
  (:use #:coalton
        #:coalton-prelude
        #:coalton-library/math
        #:cl-quil.clifford)
  #+sbcl (:import-from #:coalton-library/big-float #:Big-Float)
  (:shadow #:coalton-library/list #:singleton)
  (:local-nicknames
   (#:hash #:coalton-library/hash))
  ;; utilities.lisp
  (:export
   #:RealFrac
   #:toRational
   #:fraction->reciprocable
   #:ToFloat
   #:toSingle
   #:toDouble
   #:toBig
   #:same-type
   #:psuedo-random
   #:makeComplex
   #:complexReal
   #:complexImag
   #:Group
   #:Invert
   #:smallest-dividing-exponent
   #:euclid-gcd
   #:euclid-norm
   #:expt-mod
   )
  ;; linear-algebra.lisp
  (:export
   #:.*
   #:*.
   #:Inner
   #:<.>
   #:square-norm
   #:Normed
   #:norm
   #:Composable
   #:apply
   #:Identity
   #:Inverse
   #:@
   #:@^
   #:@^^
   #:Degreed
   #:degree
   #:Action
   #:act
   #:constructor-act
   #:safe-inverse
   #:foldable->action
   #:Dagger
   #:Matrix
   #:rows
   #:cols
   #:entry
   #:map-over-indices
   #:element-wise
   #:fixed-element-wise
   #:square-transpose
   #:Det
   #:Eigen
   #:eigenvalues
   #:Tr
   #:projective-equivalence
   #:global-phase-invariant-distance
   #:spectral-radius
   #:spectral-distance
   )
  ;; naturals.lisp
  (:export
   #:Nat
   #:fromNat
   #:nat-type
   #:single
   #:with-singleton
   #:has-singleton
   )
  ;; modulo.lisp
  (:export
   #:modulo
   #:the-modulo
   #:modulo->integer
   )
  ;; interval.lisp
  (:export
   #:Interval
   #:lower
   #:upper
   #:interval-distance
   #:interval-elem
   )
  ;; root2plex.lisp
  (:export
   #:Root2plex
   #:Root2
   #:root2-real-part
   #:root2-root-part
   #:root2-conjugate
   )
  ;; cyclotomic8.lisp
  (:export
   #:Cyclotomic8
   #:cyclotomic8->complex
   #:cyclotomic8-square-modulus
   )
  ;; dyadic.lisp
  (:export
   #:dyadic
   #:OneHalf
   )
  ;; circle.lisp
  (:export
   #:Circle
   #:circleComplex
   #:RootUnity8
   #:RootUnity4
   #:rootunity4-plus-1
   #:rootunity4-minus-1
   #:rootunity4-plus-i
   #:rootunity4-minus-i
   #:rootunity4->integer
   ))

(defpackage #:cl-quil.discrete/operators
  (:documentation "Linear operators")
  (:use #:coalton
        #:coalton-prelude
        #:coalton-library/math
        #:cl-quil.discrete/numeric
        )
  ;; mat2.lisp
  (:export #:Mat2)
  ;; sunitary2.lisp
  (:export #:SUnitary2)
  ;; unitary2.lisp
  (:export #:Unitary2)
  (:export
   #:HadamardT
   #:HT-H
   #:HT-T^^
   #:omega-unitary->ht-sequence
   )
  ;; rz.lisp
  (:export #:Rz)
  ;; c2root2.lisp
  (:export #:times-root2^^)
  ;; gates1.lisp
  (:export
   #:Universal1
   #:OutputGate1
   #:OutputGate1/Discrete-H
   #:OutputGate1/Discrete-S
   #:OutputGate1/Discrete-T
   )
  ;; maform.lisp
  (:export #:MAForm))

(defpackage #:cl-quil.discrete/rz-approx
  (:documentation "Approximator for Rz gates")
  (:use #:coalton
        #:coalton-prelude
        #:coalton-library/math
        #:cl-quil.discrete/numeric
        #:cl-quil.discrete/operators
        )
  ;; generate-solution.lisp
  (:export
   #:generate-maform-output-with-double ; FUNCTION
   #:output-T^                          ; FUNCTION
   )
  (:import-from #:coalton-library/big-float #:Big-Float)
  (:local-nicknames
   (#:operators #:cl-quil.discrete/operators)))

(defpackage #:cl-quil.discrete
  (:use #:cl
        #:cl-quil.discrete/operators
        #:cl-quil.discrete/rz-approx
        )
  (:import-from #:cl-quil.discrete/numeric
                #:RootUnity8
                #:Root2plex
                #:Dyadic
                )

  (:local-nicknames (#:q #:cl-quil)
                    (#:chips #:cl-quil.chip-library))
  ;; discrete-chip.lisp
  (:export
   #:+discrete-gate-names+              ; CONSTANT
   #:build-discrete-qubit               ; FUNCTION
   #:install-discrete-link-onto-chip    ; FUNCTION
   #:build-discrete-linear-chip         ; FUNCTION
   ))

(flet ((inherit-local-nicknames
           (package parent)
         (mapcar
          (lambda (x) (add-package-local-nickname
                       (car x) (cdr x) package))
          (package-local-nicknames parent))
         nil))

  (inherit-local-nicknames "CL-QUIL.DISCRETE/NUMERIC" "COALTON-USER")
  (inherit-local-nicknames "CL-QUIL.DISCRETE/OPERATORS" "COALTON-USER")
  (inherit-local-nicknames "CL-QUIL.DISCRETE/RZ-APPROX" "COALTON-USER"))
