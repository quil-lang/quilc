;;;; foust.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust
  (:documentation
   "This package defines the complete set `foust` cycles, including a `releasing-foust`, a `preserving-foust`

and a `foust` which takes `preserve-state` as a parameter.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:import-from
   #:coalton-library/math/integral
   #:divmod)
  (:use
   #:cl-quil.foust/pauli
   #:cl-quil.foust/frame
   #:cl-quil.foust/assignments
   #:cl-quil.foust/node
   #:cl-quil.foust/gate
   #:cl-quil.foust/circuit
   #:cl-quil.foust/graph
   #:cl-quil.foust/optimize
   #:cl-quil.foust/compile)
  (:export
   #:PauliOperator
   #:I
   #:X
   #:Y
   #:Z
   #:Sign
   #:Plus
   #:Minus
   #:Pauli
   #:Pauli
   #:Angle
   #:Angle
   #:angle->radians
   #:Frame
   #:Frame
   #:frame->
   #:frame<-
   #:frame-inverse
   #:frame-from-pauli-gate
   #:frame-from-s
   #:frame-from-h
   #:frame-from-permute
   #:frame-from-tqe
   #:frame-from-controlled
   #:frame-from-swap
   #:frame-from-npi2-rotation
   #:Assignments
   #:get-assignments-instructions
   #:get-classical-expression-variables
   #:get-classical-expression-bit
   #:Gate
   #:PauliGate
   #:S
   #:SDag
   #:H
   #:Permute
   #:TQE
   #:iSwap
   #:Swap
   #:Controlled
   #:R
   #:RR
   #:RMult
   #:R2
   #:R2Mult
   #:Prep
   #:PrepMult
   #:Meas
   #:MeasMult
   #:make-tqe
   #:gate->frame
   #:Circuit
   #:get-circuit-gates
   #:set-circuit-gates
   #:map-circuit-gates
   #:get-circuit-assignments
   #:set-circuit-assignments
   #:map-circuit-assignments
   #:make-circuit
   #:releasing-foust
   #:preserving-foust
   #:foust))

(in-package #:cl-quil.foust)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare releasing-foust ((Gate -> (Tuple (List Gate) Frame))
                            -> (Gate -> IFix)
                            -> (UFix -> UFix -> Boolean)
                            -> Circuit
                            -> Circuit
                            ))
  (define (releasing-foust corrections clifford-costs then-swap?? c)
    "Foust a `Circuit, releasing the quantum state for additional optimizations."
    (pipe c
          circuit->graph
          (optimize-graph False clifford-costs)
          (graph->circuit False corrections clifford-costs then-swap??)))

  (declare preserving-foust ((Gate -> (Tuple (List Gate) Frame))
                             -> (Gate -> IFix)
                             -> (UFix -> UFix -> Boolean)
                             -> Circuit
                             -> Circuit
                             ))
  (define (preserving-foust corrections clifford-costs then-swap?? c)
    "Foust a `Circuit`, preserving the quantum state for future compution."
    (pipe c
          circuit->graph
          (optimize-graph True clifford-costs)
          (graph->circuit True corrections clifford-costs then-swap??)))


  (declare foust (Boolean
                  -> (Gate -> (Tuple (List Gate) Frame))
                  -> (Gate -> IFix)
                  -> (UFix -> UFix -> Boolean)
                  -> Circuit
                  -> Circuit))
  (define (foust preserve-state? corrections clifford-costs then-swap?? c)
    "Foust a `Circuit`.

`preserve-state?`
  : Should `foust` make sure its output is equivalent to its input as a unitary matrix,
    i.e., should the quantum states which are the results of applying each successive gate
    in the input and output circuits, respectively, be preserved? If not, more aggressive
    optimizations are applied. However, this is only suitable for the case that measurements
    are included in the circuit.

`corrections`
  : This is a user-defined function for nativizing the Clifford gates emitted by `foust`. It
    operates by replacing a Clifford `Gate` with a `List` of `Gate`s followed by a `Frame`.
    The `Frame` will be \"pushed\" back into `Foust` for further optimizations. For example,
    if Prep(+Z) is desired and Prep(-X) is encountered, then `corrections` should return
    `(Tuple (singleton (Prep Plus Z X qubit)) (frame-from-s True Y qubit))`.

`clifford-costs`
  : This user-defined function operates on TQE gates whenever they are used for reduction.
    TQE costs should return an IFIX that reflects both architecture of the quantum processor
    as well as the  native gate set. For example, if the native gate set only contains CX, then
    TQE Z X should return a lower cost than TQE Z Y which should also return a lower cost than
    TQE Y Y. Similarly, if the processor has a linear, nearest-neighbor architecture, then TQE
    P Q 0 3 should return a higher cost than TQE P Q 0 2.

`then-swap??`
  : This user-defined function takes two qubit indices and returns `True` if the physical
    interaction among them is conjugable by single-qubit Cliffords to an `iSwap` and False
    otherwise, i.e., if it is conjugable to a `CZ`. If two qubits are not at all connected,
    then it does not matter which option is chosen. Instead, use `clifford-costs` to indicate
    how expensive it would be to implement that gate natively. For homogeneous architecures,
    use `(fn (_ _) True)` or `(fn (_ _) False)`."
    ((if preserve-state? preserving-foust releasing-foust)
     corrections clifford-costs then-swap?? c)))
