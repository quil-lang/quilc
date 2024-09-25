;;;; foust-quil.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust-quil
  (:documentation
   "This package defines an interface between Foust and the Quil compiler via the Coalton function

`foust-parsed-program` and the equivalent Common Lisp function `cl-foust-parsed-program`, which

each Foust a `cl-quil:parsed-program`, parametrized by a variable `preserve-state?` or

`preserve-state-p`, respectively.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:use
   #:coalton-quil)
  (:local-nicknames
   (#:foust #:cl-quil.foust)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:map #:coalton-library/ord-map)
   (#:tree #:coalton-library/ord-tree))
  (:export
   #:foust-parsed-program
   #:cl-foust-parsed-program))

(in-package #:cl-quil.foust-quil)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type FoustAtlas
    "A `FoustAtlas` will keep track of the qubits used in the original ciruit, as well as the original

`Measurement`s mapped to from the classical variables used within foust. Moreover, it keeps track of

the gates in a circuit prior to conversion into `foust:Circuit`."
    (FoustAtlas
     ;; A reverse List of Gates.
     (List foust:Gate)
     ;; A Tree of qubit indices.
     (tree:Tree UFix)
     ;; Map from Foust classical variable IDs
     ;; to `cl-quil` memory addresses.
     (map:Map UFix QuilMemoryRef)
     ;; The next available classical variable ID.
     UFix))

  (define-instance (Default FoustAtlas)
    (define (default) (FoustAtlas Nil tree:Empty map:Empty 0)))

  (declare get-foust-atlas-ro-map (FoustAtlas -> (map:Map UFix QuilMemoryRef)))
  (define (get-foust-atlas-ro-map (FoustAtlas _ _ ro-map _)) ro-map)

  (declare add-gate-to-atlas (FoustAtlas -> foust:Gate -> FoustAtlas))
  (define (add-gate-to-atlas (FoustAtlas gs qubits ro-map next-fresh-index) g)
    "Add a `foust:Gate` to a `FoustAtlas`"
    (FoustAtlas (Cons g gs) (fold tree:insert-or-replace qubits (foust:get-gate-qubits g)) ro-map next-fresh-index))

  (declare add-measurement-to-atlas (FoustAtlas -> UFix -> (Optional QuilMemoryRef) -> FoustAtlas))
  (define (add-measurement-to-atlas (FoustAtlas gs qubits ro-map next-fresh-index) qubit wrapped-memory-ref)
    "Add a `Measurement` of `qubit-q` to a `FoustAtlas`. Record the `Measurement` if an address is supplied."
    (FoustAtlas (Cons (foust:Meas foust:Plus foust:Z qubit next-fresh-index) gs)
                (tree:insert-or-replace qubits qubit)
                (match wrapped-memory-ref
                  ((Some memory-ref) (map:insert-or-replace ro-map next-fresh-index memory-ref))
                  ((None) ro-map))
                (1+ next-fresh-index)))

  (declare circuit-from-atlas (Boolean -> FoustAtlas -> foust:Circuit))
  (define (circuit-from-atlas add-preparations? (FoustAtlas gs qubits  _ _))
    "Make a `foust:Circuit` from a `FoustAtlas`, preparing all qubits in |0⟩ if `add-preparations?`."
    (foust:make-circuit (iter:fold! (flip Cons)
                                    (reverse gs)
                                    (if add-preparations?
                                        (map (foust:Prep foust:Plus foust:Z foust:X)
                                             (tree:decreasing-order qubits))
                                        iter:Empty)))))

(coalton-toplevel

  (declare add-quil-instruction-to-foust-atlas (FoustAtlas -> QuilInstruction -> FoustAtlas))
  (define (add-quil-instruction-to-foust-atlas foust-atlas instruction-i)
    "Compile a `cl-quil:instruction` to a `foust:Gate`, and add it to `foust-atlas`."
    (match instruction-i
      ((QuilPragma _) foust-atlas)
      ((QuilHalt _) foust-atlas)
      ((QuilMeasurement measurement-m)
       (add-measurement-to-atlas foust-atlas
                                 (get-quil-measurement-qubit measurement-m)
                                 (get-quil-measurement-address measurement-m)))
      ((QuilGateApplication gate-application-g)
       (add-gate-to-atlas
        foust-atlas
        (match (get-quil-operator-description gate-application-g)
          ((QuilNamedOperator named-operator-o)
           (let ((name (get-quil-named-operator-name named-operator-o)))
             (cond
               ((== name "X")
                (foust:PauliGate foust:X (list:nth 0 (get-quil-gate-application-qubits gate-application-g))))
               ((== name "Y")
                (foust:PauliGate foust:Y (list:nth 0 (get-quil-gate-application-qubits gate-application-g))))
               ((== name "Z")
                (foust:PauliGate foust:Z (list:nth 0 (get-quil-gate-application-qubits gate-application-g))))
               ((== name "H")
                (foust:H foust:Plus foust:Y (list:nth 0 (get-quil-gate-application-qubits gate-application-g))))
               ((== name "S")
                (foust:S foust:Z (list:nth 0 (get-quil-gate-application-qubits gate-application-g))))
               ((== name "CNOT")
                (foust:Controlled foust:X
                                  (list:nth 0 (get-quil-gate-application-qubits gate-application-g))
                                  (list:nth 1 (get-quil-gate-application-qubits gate-application-g))))
               ((== name "CZ")
                (foust:Controlled foust:Z
                                  (list:nth 0 (get-quil-gate-application-qubits gate-application-g))
                                  (list:nth 1 (get-quil-gate-application-qubits gate-application-g))))
               ((== name "ISWAP")
                (foust:iSwap (list:nth 0 (get-quil-gate-application-qubits gate-application-g))
                             (list:nth 1 (get-quil-gate-application-qubits gate-application-g))))
               ((== name "SWAP")
                (foust:Swap (list:nth 0 (get-quil-gate-application-qubits gate-application-g))
                            (list:nth 1 (get-quil-gate-application-qubits gate-application-g))))
               ((== name "RX")
                (foust:R foust:X
                         (foust:Angle (get-quil-gate-application-angle gate-application-g))
                         (list:nth 0 (get-quil-gate-application-qubits gate-application-g))))
               ((== name "RY")
                (foust:R foust:Y
                         (foust:Angle (get-quil-gate-application-angle gate-application-g))
                         (list:nth 0 (get-quil-gate-application-qubits gate-application-g))))
               ((== name "RZ")
                (foust:R foust:Z
                         (foust:Angle (get-quil-gate-application-angle gate-application-g))
                         (list:nth 0 (get-quil-gate-application-qubits gate-application-g))))
               ((== name "T")
                (foust:R foust:Z
                         (foust:Angle 1/8)
                         (list:nth 0 (get-quil-gate-application-qubits gate-application-g))))
               (True (error (mconcat (make-list "Gate " name " not supported.")))))))
          ((QuilDaggerOperator dagger-operator-o)
           (match (get-quil-dagger-operator-operator dagger-operator-o)
             ((QuilNamedOperator named-operator-o)
              (let ((name (get-quil-named-operator-name named-operator-o)))
                (cond
                  ((== name "S")
                   (foust:SDag foust:Z (list:nth 0 (get-quil-gate-application-qubits gate-application-g))))
                  ((== name "T")
                   (foust:R foust:Z (foust:Angle -1/8) (list:nth 0 (get-quil-gate-application-qubits gate-application-g))))
                  (True (error (mconcat (make-list "Gate DAGGER " name " not supported.")))))))
             (_ (error "Operand of `dagger-operator` must be `named-operator`.")))))))
      (_ (error "Instruction not supported.")))))

(coalton-toplevel

  (declare quil-parsed-program->foust-atlas (QuilParsedProgram -> FoustAtlas))
  (define (quil-parsed-program->foust-atlas parsed-program-p)
    "Represent a `parsed-program` as a `FoustAtlas`, by adding all compatible instructions."
    (fold add-quil-instruction-to-foust-atlas
          (default)
          (match (get-parsed-program-executable-code parsed-program-p)
            ((QuilExecutableCode instructions-is) instructions-is)))))

(coalton-toplevel

  (declare handle-quil-memory-from-simplified-assignments
           (foust:Assignments -> (Optional (Tuple QuilMemoryDescriptor (map:Map UFix QuilMemoryRef)))))
  (define (handle-quil-memory-from-simplified-assignments assignments-mu)
    "If classical variables are assigned, construct a classical register for them, and

a mapping from variable IDs to dense memory addresses."
    (let ((classical-variables (remove-duplicates
                                (concatmap (compose foust:get-classical-expression-variables snd)
                                           (foust:get-assignments-instructions assignments-mu))))
          (number-of-classical-variables (length classical-variables)))
      (if (== 0 number-of-classical-variables)
          None
          (let ((foustro (make-quil-memory-descriptor "foustro" number-of-classical-variables)))
            (Some (Tuple foustro
                         (pipe (zip classical-variables (range 0 (1- number-of-classical-variables)))
                               (map (map-snd (make-quil-memory-ref foustro)))
                               iter:into-iter
                               map:collect!))))))))

(coalton-toplevel

  (declare foust-gate->quil-instruction ((map:Map UFix QuilMemoryRef) -> foust:Gate -> QuilInstruction))
  (define (foust-gate->quil-instruction memory-map gate-g)
    "Compile a `foust:Gate` to a `cl-quil:instruction`."
    (match gate-g
      ((foust:Meas sign-s pauli-operator-p qubit cvar-c)
       (if (and (== sign-s foust:Plus)
                (== pauli-operator-p foust:Z))
           (QuilMeasurement (make-quil-measurement (map:lookup memory-map cvar-c) qubit))
           (error "Encountered non-standard Meas gate.")))
      ((foust:PauliGate pauli-operator-p qubit)
       (cond
         ((== pauli-operator-p foust:X)
          (QuilGateApplication (make-quil-gate-application False "X" Nil (singleton qubit))))
         ((== pauli-operator-p foust:Y)
          (QuilGateApplication (make-quil-gate-application False "Y" Nil (singleton qubit))))
         ((== pauli-operator-p foust:Z)
          (QuilGateApplication (make-quil-gate-application False "Z" Nil (singleton qubit))))
         (True (error "Unexpected PauliOperator in PauliGate gate."))))
      ((foust:H sign-s pauli-operator-p qubit)
       (if (and (== sign-s foust:Plus)
                (== pauli-operator-p foust:Y))
           (QuilGateApplication (make-quil-gate-application False "H" Nil (singleton qubit)))
           (error "Cannot handle non-standard Hadamard gate.")))
      ((foust:S pauli-operator-p qubit)
       (if (== pauli-operator-p foust:Z)
           (QuilGateApplication (make-quil-gate-application False "S" Nil (singleton qubit)))
           (error "Cannot handle non-standard S gate.")))
      ((foust:SDag pauli-operator-p qubit)
       (if (== pauli-operator-p foust:Z)
           (QuilGateApplication (make-quil-gate-application True "S" Nil (singleton qubit)))
           (error "Cannot handle non-standard SDag gate.")))
      ((foust:Controlled pauli-operator-p control-index target-index)
       (cond
         ((== pauli-operator-p foust:X)
          (QuilGateApplication (make-quil-gate-application False "CNOT" Nil (make-list control-index target-index))))
         ((== pauli-operator-p foust:Z)
          (QuilGateApplication (make-quil-gate-application False "CZ" NIL (make-list control-index target-index))))
         (True
          (error "Can only handle CZ and CNOT, not CY."))))
      ((foust:iSwap qubit-one qubit-two)
       (QuilGateApplication (make-quil-gate-application False "ISWAP" Nil (make-list qubit-one qubit-two))))
      ((foust:Swap qubit-one qubit-two)
       (QuilGateApplication (make-quil-gate-application False "SWAP" Nil (make-list qubit-one qubit-two))))
      ((foust:R pauli-operator-p theta qubit)
       (QuilGateApplication
        (cond
          ((== pauli-operator-p foust:X)
           (make-quil-gate-application False "RX" (singleton (foust:angle->radians theta)) (singleton qubit)))
          ((== pauli-operator-p foust:Y)
           (make-quil-gate-application False "RY" (singleton (foust:angle->radians theta)) (singleton qubit)))
          ((== pauli-operator-p foust:Z)
           (cond
             ((== theta (foust:Angle 1/4))
              (make-quil-gate-application False "S" Nil (singleton qubit)))
             ((== theta (foust:Angle 3/4))
              (make-quil-gate-application True "S" Nil (singleton qubit)))
             ((== theta (foust:Angle 1/8))
              (make-quil-gate-application False "T" Nil (singleton qubit)))
             ((== theta (foust:Angle 7/8))
              (make-quil-gate-application True "T" Nil (singleton qubit)))
             (True
              (make-quil-gate-application False "RZ" (singleton (foust:angle->radians theta)) (singleton qubit)))))
          (True (error "Unexpected `PauliOperator` in `R` gate.")))))
      (_ (error (mconcat (make-list "Foust `Gate` " (into gate-g) " cannot be converted to a QuilInstruction.")))))))

(coalton-toplevel

  ;; The following three functions define a basic, or default, chip
  ;; architecture to be used with QuilC and Foust. It supports CZ and
  ;; CNOT, single-qubit rotations RX, RY, and RZ, and single-qubit
  ;; measurements of +Z, as well as H and S. It assumed all-to-all
  ;; connectivity.

  (declare basic-quil-corrections (foust:Gate -> (Tuple (List foust:Gate) foust:Frame)))
  (define (basic-quil-corrections gate-g)
    "Map a Foust `Gate` to a list of Quil-compatible Foust `Gate`s followed by a correcting `Frame` s.t G = G1;G2;...;GN;GF."
    (match gate-g
      ((foust:Prep sign-s p _ qubit)
       (Tuple (singleton (foust:Prep foust:Plus foust:Z foust:X qubit))
              (cond
                ((== p foust:X)
                 (foust:frame-from-s (== sign-s foust:Plus) foust:Y qubit))
                ((== p foust:Y)
                 (foust:frame-from-s (== sign-s foust:Minus) foust:X qubit))
                (True (default)))))
      ((foust:Meas sign-s p qubit classical-variable-c)
       (if (and (== sign-s foust:Plus)
                (== p foust:Z))
           (Tuple (singleton gate-g) (default))
           (let ((pre-correction (cond
                                   ((== p foust:X)
                                    (foust:R foust:Y
                                             (foust:Angle (if (== sign-s foust:Plus) -1/4 1/4))
                                             qubit))
                                   ((== p foust:Y)
                                    (foust:R foust:X
                                             (foust:Angle (if (== sign-s foust:Plus) 1/4 -1/4))
                                             qubit))
                                   (True (foust:PauliGate foust:X qubit)))))
             (Tuple (make-list pre-correction
                               (foust:Meas foust:Plus foust:Z qubit classical-variable-c))
                    (foust:frame-inverse (foust:gate->frame pre-correction))))))
      (_ (Tuple (singleton gate-g) (default)))))

  (declare dag-distance-helper ((map:Map UFix (tree:Tree UFix)) -> UFix -> UFix -> (List UFix) -> UFix))
  (define (dag-distance-helper link-map from to visited)
    (match (map:lookup link-map from)
      ((Some tre) (match (tree:lookup tre to)
                    ((Some _) 1)
                    ((None)
                     (match (iter:min!
                             (map (fn (new-from)
                                    (dag-distance-helper link-map new-from to (Cons from visited)))
                                  (iter:filter! (complement (flip list:member visited)) (iter:into-iter tre))))
                       ((Some x) (1+ x))
                       ((None) (coalton-library/bits:not 0))))))
      ((None) (error "Encountered unconnected qubit!"))))

  (declare dag-distance ((map:Map UFix (tree:Tree UFix)) -> UFix -> UFix -> UFix))
  (define (dag-distance link-map from to)
    (dag-distance-helper link-map from to Nil))

  (declare basic-quil-costs ((Optional (map:Map UFix (tree:Tree UFix))) -> foust:Gate -> IFix))
  (define (basic-quil-costs wrapped-link-map gate-g)
    "Map a `TQE` gate to an `IFIX` reflecting the cost to implement the gate."
    (match gate-g
      ((foust:TQE pauli-operator-one pauli-operator-two index-one index-two _)
       (pipe
        (cond
          ((== pauli-operator-one foust:Z)
           (if (== pauli-operator-two foust:Y) 2 0))
          ((== pauli-operator-one foust:X)
           (if (== pauli-operator-two foust:Z) 0 2))
          (True
           (if (== pauli-operator-two foust:Y) 4 2)))
        (match wrapped-link-map
          ((Some link-map) (+ (into (* 3 (1- (dag-distance link-map index-one index-two))))))
          ((None) id))))
      (_ (error "Can only compute cost for TQE gates."))))

  (declare basic-quil-then-swap?? (UFix -> UFix -> Boolean))
  (define (basic-quil-then-swap?? _ _)
    "Indicate the kind of interaction between each pair of indexed qubits:

True if the interaction can be conjugated to an ISWAP with single-qubit Cliffords.

False if the interaction can be conjugated to a CZ with single-qubit Cliffords."
    False)

  (declare basic-quil-replacements (foust:Gate -> (List foust:Gate)))
  (define (basic-quil-replacements gate-g)
    "A mapping from Foust single-qubit Cliffords and TQE gates to an equivalent list of QUIL-compatible gates."
    (match gate-g
      ((foust:TQE pauli-operator-one pauli-operator-two index-one index-two then-swap?)
       (if then-swap?
           (error "Expected only non-swapping TQEs for basic QUIL Foust.")
           (cond
             ((== pauli-operator-one foust:Y)
              (concat (make-list (singleton (foust:R foust:X (foust:Angle 1/4) index-one))
                                 (basic-quil-replacements (foust:TQE foust:Z
                                                                     pauli-operator-two
                                                                     index-one
                                                                     index-two
                                                                     then-swap?))
                                 (singleton (foust:R foust:X (foust:Angle -1/4) index-one)))))
             ((== pauli-operator-two foust:Y)
              (concat (make-list (singleton (foust:R foust:X (foust:Angle 1/4) index-two))
                                 (basic-quil-replacements (foust:TQE pauli-operator-one
                                                                     foust:Z
                                                                     index-one
                                                                     index-two
                                                                     then-swap?))
                                 (singleton (foust:R foust:X (foust:Angle -1/4) index-two)))))
             ((== pauli-operator-one foust:Z)
              (singleton (foust:Controlled pauli-operator-two index-one index-two)))
             ((== pauli-operator-two foust:X)
              (make-list (foust:H foust:Plus foust:Y index-one)
                         (foust:Controlled foust:X index-one index-two)
                         (foust:H foust:Plus foust:Y index-one)))
             (True
              (singleton (foust:Controlled foust:X index-two index-one))))))
      ((foust:S p index-q)
       (if (== p foust:Z)
           (singleton gate-g)
           (singleton (foust:R p (foust:Angle 1/4) index-q))))
      ((foust:SDag p index-q)
       (if (== p foust:Z)
           (singleton gate-g)
           (singleton (foust:R p (foust:Angle -1/4) index-q))))
      ((foust:H sign-s p index-q)
       (if (and (== sign-s foust:Plus)
                (== p foust:Y))
           (singleton gate-g)
           (make-list (foust:PauliGate (foust:next-pauli-operator p) index-q)
                      (foust:R p (foust:Angle (if (== sign-s foust:Plus) 1/4 -1/4)) index-q))))
      ((foust:Permute sign-x sign-y sign-z index-q)
       (pipe (make-list (foust:R foust:X (foust:Angle (if (== sign-x foust:Plus) -1/4 1/4)) index-q)
                        (foust:R foust:Y (foust:Angle (if (== sign-y foust:Plus) -1/4 1/4)) index-q))
             (if (== foust:Plus (msum (make-list sign-x sign-y sign-z))) id reverse)))
      (_ (singleton gate-g)))))

(coalton-toplevel

  (declare is-basic-preparation? (foust:Gate -> Boolean))
  (define (is-basic-preparation? gate-g)
    "Is `gate-g` a basic preparation (Prep+Z)?"
    (match gate-g
      ((foust:Prep sign-s p _ _)
       (and (== sign-s foust:Plus) (== p foust:Z)))
      (_ False)))

  (declare remove-entry-preparations (foust:Circuit -> foust:Circuit))
  (define (remove-entry-preparations (foust:Circuit gates-gs assignments-mu))
    "While the first gate in a Circuit is a basic preparation Prep+Z, remove

the first gate of the Circuit; repeat."
    (match (head gates-gs)
      ((Some gate-g)
       (if (is-basic-preparation? gate-g)
           (remove-entry-preparations (foust:Circuit (unwrap (tail gates-gs)) assignments-mu))
           (foust:Circuit gates-gs assignments-mu)))
      ((None) (foust:Circuit gates-gs assignments-mu)))))

(coalton-toplevel

  (declare process-foust-assignment ((map:Map UFix QuilMemoryRef)
                                     -> (map:Map UFix QuilMemoryRef)
                                     -> (Tuple UFix foust:ClassicalExpression)
                                     -> (List QuilInstruction)))
  (define (process-foust-assignment ro-map foustro-map (Tuple cvar-c cexpr-e))
    "Given maps from classical variables to original and new classical addresses,

construct a list of classical instructions equivalent to a classical assignment."
    (let ((ro-address (unwrap (map:lookup ro-map cvar-c))))
      (map QuilBinaryClassicalInstruction
           (Cons (QuilClassicalMove
                  (make-quil-classical-move ro-address (if (foust:get-classical-expression-bit cexpr-e) 1 0)))
                 (map (compose
                       (fn (foustro-address)
                         (QuilClassicalExclusiveOr
                          (make-quil-classical-exclusive-or ro-address foustro-address)))
                       (compose unwrap (map:lookup foustro-map)))
                      (foust:get-classical-expression-variables cexpr-e)))))))

(coalton-toplevel

  (declare build-executable-code (foust:Circuit -> (map:Map UFix QuilMemoryRef) -> FoustAtlas -> QuilExecutableCode))
  (define (build-executable-code final-circuit foustro-map foust-atlas)
    "Construct the executable code of a parsed program by compiling all `foust:Gate`s and

then compiling all classical assignments into a list of classical instructions to be appended."
    (pipe (let ((ro-map (get-foust-atlas-ro-map foust-atlas))
                (gates (foust:get-circuit-gates final-circuit))
                (assignments (foust:get-assignments-instructions
                              (foust:get-circuit-assignments final-circuit))))
            (append (map (foust-gate->quil-instruction foustro-map) gates)
                    (concatmap (process-foust-assignment ro-map foustro-map)
                               (filter (fn (assignment)
                                         (iter:any! (== (fst assignment)) (map:keys ro-map)))
                                       assignments))))
          QuilExecutableCode)))

(coalton-toplevel

  (declare foust-quil ((Optional QuilChipSpecification) -> Boolean -> foust:Circuit -> foust:Circuit))
  (define (foust-quil chip-specification preserve-state?)
    "Perform a `foust`, preserving the quantum state if `preserve-state?`, and applying

the basic corrections, costs, and TQE spec functions."
    (let ((link-map (map get-chip-specification-links chip-specification)))
      (foust:foust preserve-state? basic-quil-corrections (basic-quil-costs link-map) basic-quil-then-swap??)))

  (declare foust-parsed-program (QuilParsedProgram -> (Optional QuilChipSpecification) -> Boolean -> Boolean -> QuilParsedProgram))
  (define (foust-parsed-program parsed-program chip-specification preserve-state? add-preparations?)
    "Foust a parsed program by compiling it to a `foust:Circuit, adding initial preparations if

`add-preparations` then fousting the `foust:Circuit` according to `preserve-state?` and then

compiling the `foust:Circuit` back to a `cl-quil:parsed-program`. Lastly, print all four stages

if `print-progress?`."
    (let ((parsed-program-prime (copy-parsed-program parsed-program))
          (foust-atlas (quil-parsed-program->foust-atlas parsed-program-prime))
          (initial-circuit (circuit-from-atlas add-preparations? foust-atlas))
          (final-circuit (pipe initial-circuit
                               (foust-quil chip-specification preserve-state?)
                               remove-entry-preparations
                               (foust:map-circuit-gates (concatmap basic-quil-replacements)))))
      (progn (match (handle-quil-memory-from-simplified-assignments
                     (foust:get-circuit-assignments final-circuit))
               ((None) (pipe parsed-program-prime
                             (set-parsed-program-executable-code!
                              (build-executable-code final-circuit map:empty foust-atlas))))
               ((Some (Tuple memory-descriptor foustro-map))
                (pipe parsed-program-prime
                      (map-parsed-program-memory-definitions! (Cons memory-descriptor))
                      (set-parsed-program-executable-code!
                       (build-executable-code final-circuit foustro-map foust-atlas)))))
             parsed-program-prime))))

(cl:defun cl-foust-parsed-program (parsed-program cl:&key chip-specification (preserve-state-p cl:t) (add-preparations-p cl:nil))
  (coalton (foust-parsed-program (lisp QuilParsedProgram () parsed-program)
                                 (lisp (Optional QuilChipSpecification) ()
                                   (cl:if chip-specification (Some chip-specification) None))
                                 (lisp Boolean () preserve-state-p)
                                 (lisp Boolean () add-preparations-p))))
