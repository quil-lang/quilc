;;;; chip-specification.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

;;; some data structures that encode hardware structure
;;;
;;; Use MAKE-ADJUSTABLE-VECTOR and VNTH for these objects.

(defconstant +near-perfect-fidelity+ 0.99999d0
  "Even perfect operations are typically limited in their physical realization by, say, the granularity of control electronics. (For instance, waveform IQ values might be stored as complex fixnums of some specified depth.) This constant is a mnemonic for \"supposedly perfect\" and captures some of the loss incurred by these imperfections.")

;; TODO: actually put a small value here.
(defconstant +totally-awful-fidelity+ 0.99997d0
  "In the event that the Forest database indicates that a gate is too error-prone even to accurately estimate the error, we use this value as a generic lowball for how awful the gate must be.")

(defconstant +forest-error-sentinel+ -1
  "The Forest database sometimes stores a fidelity of -1 to indicate that the fidelity estimation routine failed.")

(defstruct chip-specification
  "Houses information about hardware components on a QPU.

OBJECTS is an array. In its nth position, there is a vector of order n hardware objects on the chip.

GENERIC-COMPILERS is a vector of functions used as fallback compilation methods, applied when a hardware object does not exist or when a hardware object's compilation methods all fail.  The order of this array is important: compilers earlier in the array are preferentially applied over compilers later in the array.

GENERIC-REWRITING-RULES is a similar vector of REWRITING-RULE structures that the compressor loop can use to generate shorter gate strings when rules specialized to local hardware objects have been exhausted.  Again, the array is sorted by descending preference.

LOOKUP-CACHE is a hash table mapping lists of qubit indices to hardware objects.  It gets auto-populated by `WARM-CHIP-SPEC-LOOKUP-CACHE'. This should not be accessed directly; use `LOOKUP-HARDWARE-ADDRESS-BY-QUBITS'."
  (objects (make-array 2 :initial-contents (list (make-adjustable-vector)
                                                 (make-adjustable-vector)))
   :type vector)
  (generic-compilers (make-adjustable-vector) :type vector)
  (generic-rewriting-rules (make-adjustable-vector) :type vector)
  (lookup-cache nil :type (or null hash-table)))

(defmethod print-object ((cs chip-specification) stream)
  (print-unreadable-object (cs stream :type t :identity nil)
    (format stream "of ~{~D~^:~} objects" (map 'list #'length (chip-specification-objects cs)))))

(defstruct permutation-record
  "Houses information about a permutation gate, for ease of lookup by the greedy scheduler.

OPERATOR is a string that names the permutation gate.

ARGUMENTS is a list of positions into the hardware object's cxns list's 0th level, naming the argument order for the gate application.

PERMUTATION is a list of positions into the hardware object's cxns list's 0th level, indicating the permutation that this gate application has on the qubit data.

DURATION is the time duration in nanoseconds of this gate application."
  (operator "SWAP")
  (arguments (list 0 1))
  (permutation (list 1 0))
  (duration 600))

(defstruct (gate-record (:copier nil))
  "Houses information about a hardware instantiation of a gate.

FIDELITY stores the measured gate fidelity.

DURATION stores the measured gate duration (in nanoseconds)."
  (fidelity +near-perfect-fidelity+ :type (or null real))
  (duration 1/100 :type (or null real)))

(defun copy-gate-record (record &key fidelity duration)
  (make-gate-record :fidelity (or fidelity (gate-record-fidelity record))
                    :duration (or duration (gate-record-duration record))))


;;; The HARDWARE object structure stores a lot of information. It
;;; serves many purposes, principally to solve some of the following
;;; problems:
;;;
;;;     1. To address operations on qubits, qubit pairs, qubit
;;;     triplets, etc. This requires determining a notion of
;;;     _occupancy_ of resources.
;;;
;;;     2. To determine what options are available for compiling
;;;     operations on that resource.
;;;
;;; The first point is particularly important. You should *not* use
;;; CXNS to determine directed connectivity. (By "directed", we mean
;;; anything to do with what operations are allowed.) CXNS just
;;; encodes some physical relationships on physical hardware;
;;; collections of resources on which a variety of operations can
;;; possibly act.
;;;
;;; To determine available instructions, their validity, etc., the
;;; information in NATIVE-INSTRUCTIONS and MISC-DATA (namely the
;;; duration table) are helpful.
;;;
;;; Now for some miscellaneous comments about the design.

;;; some design decisions about CXNS:
;;; + easy to iterate over the objects in positions above ORDER and know when to terminate
;;; + easy to iterate over the objects in positions below ORDER and know when to terminate
;;; + easy to tell when there are no objects in any given position
;;;
;;; using an array to do this sometimes means some obnoxious array index safety,
;;; which we wouldn't experience if we were using e.g. a hash-table, but in trade
;;; we would have a harder time asking for the half of the keys that were below
;;; our order. maybe there is a more intelligent data structure to use. something
;;; to think about in the future.
;;;
;;; We NOTINLINE here because a previous file "compilation-methods.lisp" uses this
;;; structure, but the dependency is circular, so moving this doesn't make much sense.
(declaim (notinline hardware-object-order
                    hardware-object-native-instruction-p
                    hardware-object-compilation-methods
                    hardware-object-permutation-gates
                    hardware-object-rewriting-rules
                    hardware-object-cxns
                    hardware-object-misc-data))
(defstruct hardware-object
  "Houses information about a particular hardware object on a QPU.

ORDER is a non-negative integer that counts the number of qubit subsidiaries of this hardware object. Equals (1- (length (vnth 0 (hardware-object-cxns this)))). (If you drew a schematic of a chip, this is also the dimension of the graphical representation of the hardware object: 0 for qubits, 1 for links, ... .)

GATE-INFORMATION is a hash table mapping gate bindings to GATE-RECORD objects.

COMPILATION-METHODS is a vector of compilers that can be employed to convert non-native instructions to native ones, sorted in descending order of precedence. An individual method receives an instruction and an environment (typically a PARSED-PROGRAM). The same method returns a list of instructions if successful and NIL if unsuccessful.  This slot is typically populated by WARM-HARDWARE-OBJECTS.

PERMUTATION-GATES is a vector of permutation gates that this device can natively compile, stored as PERMUTATION-RECORD structs. This data is used by the event scheduler.

REWRITING-RULES is a vector of compilers that the compressor loop can use to generate shorter gate strings.  This slot is typically populated by WARM-HARDWARE-OBJECTS.

CXNS is an array. In its nth position, there is a vector of the order n hardware objects on the chip that are connected to this one. Among other things, this is used to determine shared resource blocking.

MISC-DATA is a hash-table of miscellaneous data associated to this hardware object: scratch data, scheduling hints (e.g., qubit coherence time), ... ."
  (order 0 :type unsigned-byte :read-only t)
  (gate-information (make-hash-table :test #'equalp))
  (compilation-methods (make-adjustable-vector))
  (permutation-gates (make-adjustable-vector))
  (rewriting-rules (make-adjustable-vector))
  (cxns (make-array 2 :initial-contents (list (make-adjustable-vector)
                                              (make-adjustable-vector))))
  (misc-data (make-hash-table :test #'equal) :type hash-table))

(defun hardware-object-native-instruction-p (obj instr)
  "Emits the physical duration in nanoseconds if this instruction translates to a physical pulse (i.e., if it is a native gate, \"instruction native\"), and emits NIL if this instruction does not admit direct translation to a physical pulse.

Used to be an anonymous function associated to HARDWARE-OBJECT; now computed from its GATE-INFORMATION table."
  (a:when-let ((gate-record
                (loop :for key :being :the :hash-keys :of (hardware-object-gate-information obj)
                        :using (hash-value value)
                      :when (binding-subsumes-p key (binding-from-instr instr))
                        :do (return value))))
    (gate-record-duration gate-record)))

(defmethod print-object ((obj hardware-object) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "of order ~D" (hardware-object-order obj))))

;;; Adding a HARDWARE-OBJECT to a CHIP-SPECIFICATION

(defun adjoin-hardware-object (obj chip-spec)
  "Add the hardware object OBJ to the chip specification SPEC."
  ;; Note: argument order matches CL:ADJOIN.
  (check-type obj hardware-object)
  (check-type chip-spec chip-specification)
  (let ((order (hardware-object-order obj)))
    (vector-push-extend obj (vnth order (chip-specification-objects chip-spec))))
  nil)

;;; Some readers are very common, so we provide abbreviations
;;;
;;; These are tailored for devices with order 0 and 1 objects.

(defun chip-spec-qubits (chip-spec)
  "Get the qubits (as hardware-objects) for CHIP-SPEC."
  (vnth 0 (chip-specification-objects chip-spec)))
(defun chip-spec-links (chip-spec)
  "Get the links (as hardware-objects) for CHIP-SPEC."
  (vnth 1 (chip-specification-objects chip-spec)))

(defun chip-spec-n-qubits (chip-spec)
  "Get the number of qubits on CHIP-SPEC. Equivalent to the largest qubit index
used to specify CHIP-SPEC."
  (length (chip-spec-qubits chip-spec)))
(defun chip-spec-n-links (chip-spec)
  "Get the number of links on CHIP-SPEC."
  (length (chip-spec-links chip-spec)))

(defun chip-spec-nth-qubit (chip-spec n)
  "Get the Nth qubit on chip-spec (as a hardware-object)."
  (vnth n (chip-spec-qubits chip-spec)))
(defun chip-spec-nth-link (chip-spec n)
  "Get the Nth link on chip-spec (as a hardware-object)."
  (vnth n (chip-spec-links chip-spec)))

(defun chip-spec-links-on-qubit (chip-spec qubit-index)
  "Get the links associated with QUBIT-INDEX in CHIP-SPEC."
  (vnth 1 (hardware-object-cxns (chip-spec-nth-qubit chip-spec qubit-index))))
(defun chip-spec-qubits-on-link (chip-spec link-index)
  "Get the qubits associated with LINK-INDEX in CHIP-SPEC."
  (vnth 0 (hardware-object-cxns (chip-spec-nth-link chip-spec link-index))))
(defun chip-spec-hw-object (chip-spec order address)
  "Get the hardware-object with matching ORDER and ADDRESS."
  (vnth address (vnth order (chip-specification-objects chip-spec))))
(defun chip-spec-adj-qubits (chip-spec qubit-index)
  "Get the qubits adjacent (connected by a link) to QUBIT-INDEX in CHIP-SPEC."
  (loop
    :for link-index :across (chip-spec-links-on-qubit chip-spec qubit-index)
    :append (remove qubit-index (coerce (chip-spec-qubits-on-link chip-spec link-index) 'list))))
(defun chip-spec-adj-links (chip-spec link-index)
  "Get the links adjacent (connected by a qubit) to LINK-INDEX in CHIP-SPEC."
  (loop
    :for qubit-index :across (chip-spec-qubits-on-link chip-spec link-index)
    :append (remove link-index (coerce (chip-spec-links-on-qubit chip-spec qubit-index) 'list))))

(defun hardware-object-dead-p (hw-obj)
  (gethash "dead" (hardware-object-misc-data hw-obj)))
(defun chip-spec-qubit-dead? (chip-spec qubit-index)
  "Is QUBIT-INDEX a dead qubit in CHIP-SPEC."
  (hardware-object-dead-p (chip-spec-nth-qubit chip-spec qubit-index)))
(defun chip-spec-dead-qubits (chip-spec)
  "Get all dead qubit indices in CHIP-SPEC."
  (check-type chip-spec chip-specification)
  (loop :for qi :below (chip-spec-n-qubits chip-spec)
        :when (chip-spec-qubit-dead? chip-spec qi)
          :collect qi))
(defun chip-spec-live-qubits (chip-spec)
  "Get all live qubit indices in CHIP-SPEC."
  (check-type chip-spec chip-specification)
  (loop :with dead-qubits := (chip-spec-dead-qubits chip-spec)
        :for qi :below (chip-spec-n-qubits chip-spec)
        :unless (find qi dead-qubits)
          :collect qi))

(defun chip-spec-link-dead? (chip-spec link-index)
  "Is QUBIT-INDEX a dead qubit in CHIP-SPEC."
  (hardware-object-dead-p (chip-spec-nth-link chip-spec link-index)))

(defun lookup-hardware-address-by-qubits (chip-spec args)
  (unless (chip-specification-lookup-cache chip-spec)
    (warm-chip-spec-lookup-cache chip-spec))
  (a:when-let ((hash-value (gethash args (chip-specification-lookup-cache chip-spec))))
    (destructuring-bind (index obj) hash-value
      (values (1- (length args)) index obj))))

(defun lookup-hardware-address (chip-spec instr)
  "Finds a hardware object OBJ in CHIP-SPEC whose qubit resources match those used by INSTR. Returns the values object (ORDER ADDRESS OBJ), so that OBJ equals (vnth ADDRESS (vnth ORDER (chip-specification-objects CHIP-SPEC)))."
  ;; Only APPLICATIONs and MEASUREs use qubits in contexts where you'd
  ;; actually want to call this function.
  ;;
  ;; XXX: What about RESET q?
  (etypecase instr
    (application
     (lookup-hardware-address-by-qubits chip-spec (mapcar #'qubit-index (application-arguments instr))))
    (measurement
     (lookup-hardware-address-by-qubits chip-spec (list (qubit-index (measurement-qubit instr)))))))

(defun lookup-hardware-object-by-qubits (chip-spec args)
  (nth-value 2 (lookup-hardware-address-by-qubits chip-spec args)))

(defun lookup-hardware-object (chip-spec instr)
  (nth-value 2 (lookup-hardware-address chip-spec instr)))


;;; constructors for hardware object building blocks

(defun build-link (qubit0 qubit1 &key type gate-information)
  "Constructs a template link. The native gates for this link can be specified by one of two mutually exclusive means:

 * The TYPE keyword can consist of (lists of) :CZ, :CPHASE, :ISWAP, :PISWAP, :CNOT.  This routine constructs a table of native gates based on 'templates' associated to each of these atoms, e.g., :CZ indicates that `CZ _ _` is native for this link.

 * The GATE-INFORMATION keyword can be used to directly supply a hash table to be installed in the GATE-INFORMATION slot on the hardware object, allowing completely custom gateset control."
  (check-type qubit0 unsigned-byte)
  (check-type qubit1 unsigned-byte)
  (check-type gate-information (or null hash-table))
  (assert (/= qubit0 qubit1))
  (assert (a:xor (null gate-information) (null type)))
  (setf type (a:ensure-list type))
  (let* ((obj (make-hardware-object
               :order 1
               :cxns (vector (vector qubit0 qubit1) #()))))
    ;; set up the SWAP record
    (vector-push-extend (make-permutation-record
                         :operator #.(named-operator "SWAP")
                         :arguments (list 0 1)
                         :permutation (list 1 0)
                         :duration (cond
                                     ((optimal-2q-target-meets-requirements type (list ':iswap ':cz))
                                      400)
                                     (t
                                      600)))
                        (hardware-object-permutation-gates obj))
    
    ;; this is the new model for setting up gate data
    (when gate-information
      (setf (hardware-object-gate-information obj) gate-information))
    
    ;; this is the legacy model for setting up gate data
    (flet ((stash-gate-record (atom gate-name parameters arguments fidelity)
             (when (member atom type) #+ignore (optimal-2q-target-meets-requirements type atom)
                   (setf (gethash (make-gate-binding :operator (named-operator gate-name)
                                                     :parameters parameters
                                                     :arguments arguments)
                                  (hardware-object-gate-information obj))
                         (make-gate-record :duration 150
                                           :fidelity fidelity)))))
      (dolist (data `((:cz     "CZ"     ()  (_ _) 0.89d0)
                      (:iswap  "ISWAP"  ()  (_ _) 0.91d0)
                      (:cphase "CPHASE" (_) (_ _) 0.80d0)
                      (:piswap "PISWAP" (_) (_ _) 0.80d0)
                      (:cnot   "CNOT"   ()  (,qubit0 ,qubit1) 0.90d0)))
        (destructuring-bind (atom gate-name parameters arguments fidelity) data
          (stash-gate-record atom gate-name parameters arguments fidelity))))
    (when (member ':cnot type)
      (vector-push-extend #'CNOT-to-flipped-CNOT
                          (hardware-object-compilation-methods obj)))
    ;; return the link
    obj))


(defun build-qubit (q &key type gate-information)
  "Constructs a template qubit. The native gates for this qubit can be specified by one of two mutually exclusive means:

 * The TYPE keyword can consist of (lists of) ':RZ, ':X/2, ':MEASURE.  This routine constructs a table of native gates based on 'templates' associated to each of these atoms, e.g., :CZ indicates that `CZ _ _` is native for this link.

 * The GATE-INFORMATION keyword can be used to directly supply a hash table to be installed in the GATE-INFORMATION slot on the hardware object, allowing completely custom gateset control."
  (check-type gate-information (or null hash-table))
  (assert (a:xor (null gate-information) (null type)))
  (let ((obj (make-hardware-object :order 0)))
    ;; new style of initialization
    (when gate-information
      (setf (hardware-object-gate-information obj) gate-information))
    ;; old style of initialization
    (flet ((stash-gate-record (gate-name parameters arguments duration fidelity)
             (setf (gethash (make-gate-binding :operator (named-operator gate-name)
                                               :parameters parameters
                                               :arguments arguments)
                            (hardware-object-gate-information obj))
                   (make-gate-record :duration duration
                                     :fidelity fidelity))))
      (when (member ':MEASURE type)
        (setf (gethash (make-measure-binding :qubit q
                                             :target '_)
                       (hardware-object-gate-information obj))
              (make-gate-record :duration 2000 :fidelity 0.90d0))
        (setf (gethash (make-measure-binding :qubit '_)
                       (hardware-object-gate-information obj))
              (make-gate-record :duration 2000 :fidelity 0.90d0)))
      (when (member ':RZ type)
        (stash-gate-record "RZ" '(_) (list q) 1/100 +near-perfect-fidelity+))
      (when (member ':X/2 type)
        (stash-gate-record "RX" '(#.pi/2)  `(,q) 9 0.98d0)
        (stash-gate-record "RX" '(#.-pi/2) `(,q) 9 0.98d0)
        (stash-gate-record "RX" '(#.pi)    `(,q) 9 0.98d0)
        (stash-gate-record "RX" '(#.-pi)   `(,q) 9 0.98d0)
        (stash-gate-record "RX" '(0d0)     `(,q) 9 +near-perfect-fidelity+)))
    ;; return the qubit
    obj))

(defun architecture-has-ucr-compiler-p (architecture)
  (or (optimal-2q-target-meets-requirements architecture ':cz)
      (optimal-2q-target-meets-requirements architecture ':iswap)
      (find ':cnot (a:ensure-list architecture))))

;;; routines for populating the fields of a CHIP-SPECIFICATION object (and
;;; maintaining the appropriate interrelations).
(defvar *global-compilers*
  (list (constantly 'swap-to-native-swaps)
        (constantly 'cz-to-native-czs)
        (lambda (chip-spec arch)
          (declare (ignore chip-spec))
          (when (optimal-2q-target-meets-requirements arch ':cz)
            'cnot-to-native-cnots))
        (lambda (chip-spec arch)
          (declare (ignore chip-spec))
          (when (optimal-2q-target-meets-requirements arch ':iswap)
            'ISWAP-to-native-ISWAPs))
        (lambda (chip-spec arch)
          (declare (ignore chip-spec))
          (when (optimal-2q-target-meets-requirements arch ':cphase)
            'CPHASE-to-native-CPHASEs))
        (lambda (chip-spec arch)
          (declare (ignore chip-spec))
          (when (optimal-2q-target-meets-requirements arch ':piswap)
            'PISWAP-to-native-PISWAPs))
        ;; We make this unconditional. We could later conditionalize it if
        ;; we happen to have better CCNOT translations for specific target
        ;; gate sets.
        (constantly 'ccnot-to-cnot)
        (constantly 'phase-to-rz)
        (lambda (chip-spec arch)
          (declare (ignore chip-spec))
          (when (optimal-2q-target-meets-requirements arch ':cz)
            'ucr-compiler-to-cz))
        (lambda (chip-spec arch)
          (declare (ignore chip-spec))
          (when (optimal-2q-target-meets-requirements arch ':iswap)
            'ucr-compiler-to-iswap))
        (constantly 'undagger-rotation)
        (constantly 'uncontrol-rotation)
        (constantly 'state-prep-1q-compiler)
        (constantly 'state-prep-2q-compiler)
        (constantly 'state-prep-4q-compiler)
        (constantly 'state-prep-trampolining-compiler)
        (constantly 'parametric-diagonal-compiler)
        (constantly 'parametric-pauli-compiler)
        (constantly 'recognize-ucr)
        (constantly 'nearest-circuit-of-depth-0)
        (lambda (chip-spec arch)
          (declare (ignore chip-spec))
          (when (optimal-2q-target-meets-requirements arch ':iswap)
            'nearest-iswap-circuit-of-depth-1))
        (lambda (chip-spec arch)
          (declare (ignore chip-spec))
          (when (optimal-2q-target-meets-requirements arch ':iswap)
            'nearest-iswap-circuit-of-depth-2))
        (lambda (chip-spec arch)
          (declare (ignore chip-spec))
          (when (optimal-2q-target-meets-requirements arch ':iswap)
            'nearest-iswap-circuit-of-depth-3))
        (constantly 'nearest-cz-circuit-of-depth-1)
        (constantly 'nearest-cz-circuit-of-depth-2)
        (constantly 'nearest-cz-circuit-of-depth-3)
        (constantly 'canonical-decomposition)
        (constantly 'qs-compiler))
  "List of functions taking a CHIP-SPECIFICATION and an architecture specification, and returns an instruction compiler if applicable to the given specs or otherwise returns nil.

Compilers are listed in descending precedence.")

(defun install-generic-compilers (chip-spec architecture)
  (assert (architecture-has-ucr-compiler-p architecture))

  (let ((ret (make-adjustable-vector)))
    (setf (chip-specification-generic-compilers chip-spec) ret)
    (dolist (get-compiler *global-compilers*)
      (a:when-let ((compiler (funcall get-compiler chip-spec architecture)))
        (vector-push-extend compiler ret)))))


(defun install-link-onto-chip (chip-specification q0 q1 &key (architecture (list ':cz)))
  "Adds a link, built using BUILD-LINK, between qubits Q0 and Q1 on the chip described by CHIP-SPECIFICATION.  Returns the HARDWARE-OBJECT instance corresponding to the new link."
  (let ((link (build-link q0 q1 :type architecture))
        (link-index (chip-spec-n-links chip-specification)))
    (adjoin-hardware-object link chip-specification)
    (vector-push-extend link-index (vnth 1 (hardware-object-cxns (chip-spec-nth-qubit chip-specification q0))))
    (vector-push-extend link-index (vnth 1 (hardware-object-cxns (chip-spec-nth-qubit chip-specification q1))))
    ;; Return the link.
    link))

(defun warm-chip-spec-lookup-cache (chip-spec)
  "Warm the lookup cache of the CHIP-SPEC. This sets the table of the chip specification as a side effect. See the documentation of the `CHIP-SPECIFICATION' structure."
  (let ((hash (make-hash-table :test 'equalp)))
    (loop :for q :across (chip-spec-qubits chip-spec)
          :for index :from 0
          :do (setf (gethash (list index) hash)
                    (list index q)))
    (loop :for l :across (chip-spec-links chip-spec)
          :for index :from 0
          :for pair := (coerce (vnth 0 (hardware-object-cxns l)) 'list)
          :for other-pair := (reverse pair)
          :do (setf (gethash pair       hash) (list index l))
              (setf (gethash other-pair hash) (list index l)))
    (setf (chip-specification-lookup-cache chip-spec) hash)
    nil))

(defun warm-hardware-objects (chip-specification)
  "Initializes the compiler feature sets of HARDWARE-OBJECT instances installed on a CHIP-SPECIFICATION.  Preserves whatever feature sets might be there already; don't call this repeatedly."
  (dotimes (order (length (chip-specification-objects chip-specification)) chip-specification)
    (loop :for obj :across (vnth order (chip-specification-objects chip-specification))
          ;; :unless (hardware-object-dead-p obj)
          :do (handler-case
                  (setf (hardware-object-compilation-methods obj)
                        (concatenate 'vector
                                     (hardware-object-compilation-methods obj)
                                     (compute-applicable-compilers (hardware-object-gate-information obj)
                                                                   (1+ order))))
                (no-compiler-path ()
                  (setf (gethash "dead" (hardware-object-misc-data obj)) t)))
              ;; TODO: incorporate child object gatesets too
              (let ((gate-information (a:copy-hash-table (hardware-object-gate-information obj)
                                                         :test #'equalp)))
                (dotimes (suborder order)
                  (loop :for subobject-address :across (vnth suborder (hardware-object-cxns obj))
                        :for subobject := (chip-spec-hw-object chip-specification suborder subobject-address)
                        :do (dohash ((key val) (hardware-object-gate-information subobject))
                              (setf (gethash key gate-information) val))))
                (setf (hardware-object-rewriting-rules obj)
                      (concatenate 'vector
                                   (hardware-object-rewriting-rules obj)
                                   (compute-applicable-reducers gate-information)))))))



;;; now constructors for whole chips.

;; here we provide a function that generates an example chip. in general, chip
;; specifications should be generated based on read-in ISA data, and not hard-
;; wired into the code.
;;
;; Example chip topology and naming scheme:
;; 0 --0-- 1 --1-- 2
;;
;; |               |
;; |               |
;; 7               2
;; |               |
;; |               |
;;
;; 7               3
;;
;; |               |
;; |               |
;; 6               3
;; |               |
;; |               |
;;
;; 6 --5-- 5 --4-- 4
;; all qubits Xhalves-type, all links CZ-type
(defun build-8Q-chip (&key (architecture ':cz))
  "Return an example 8Q square chip."
  (let ((chip-spec (make-chip-specification
                    :generic-rewriting-rules (coerce (global-rewriting-rules) 'simple-vector))))
    (install-generic-compilers chip-spec architecture)
    (loop :for q :below 8
          :do (adjoin-hardware-object (build-qubit q :type '(:RZ :X/2 :MEASURE)) chip-spec))
    (dotimes (i 8)
      (install-link-onto-chip chip-spec i (mod (1+ i) 8)
                              :architecture architecture))
    (warm-hardware-objects chip-spec)))

;; here's another example chip with a linear topology:
;; 0 --0-- 1 --1-- 2 --2-- ... --(n-2)-- (n-1)
(defun build-nQ-linear-chip (n &key (architecture ':cz))
  (let ((chip-spec (make-chip-specification
                    :generic-rewriting-rules (coerce (global-rewriting-rules) 'vector))))
    (install-generic-compilers chip-spec architecture)
    ;; prep the qubits
    (loop :for q :below n
          :do (adjoin-hardware-object (build-qubit q :type '(:RZ :X/2 :MEASURE)) chip-spec))
    ;; prep the links
    (dotimes (i (1- n))
      (install-link-onto-chip chip-spec i (1+ i) :architecture architecture))
    (warm-hardware-objects chip-spec)))

(defun build-nQ-fully-connected-chip (n &key (architecture ':cz))
  (let ((chip-spec (make-chip-specification
                    :generic-rewriting-rules (coerce (global-rewriting-rules) 'vector))))
    (install-generic-compilers chip-spec architecture)
    ;; prep the qubits
    (loop :for q :below n
          :do (adjoin-hardware-object (build-qubit q :type '(:RZ :X/2 :MEASURE)) chip-spec))
    ;; prep the links
    (dotimes (i n)
      (dotimes (j i)
        (install-link-onto-chip chip-spec j i :architecture architecture)))
    (warm-hardware-objects chip-spec)))

(defun build-16QMUX-chip ()
  (qpu-hash-table-to-chip-specification
   (a:plist-hash-table
    (list "isa" (a:plist-hash-table
                 (list "1Q" (a:plist-hash-table
                             (list "0" (make-hash-table)
                                   "1" (make-hash-table)
                                   "2" (make-hash-table)
                                   "3" (make-hash-table)
                                   "4" (make-hash-table)
                                   "5" (make-hash-table)
                                   "6" (make-hash-table)
                                   "7" (make-hash-table)
                                   "10" (make-hash-table)
                                   "11" (make-hash-table)
                                   "12" (make-hash-table)
                                   "13" (make-hash-table)
                                   "14" (make-hash-table)
                                   "15" (make-hash-table)
                                   "16" (make-hash-table)
                                   "17" (make-hash-table))
                             :test #'equal)
                       "2Q" (a:plist-hash-table
                             (list "0-1" (make-hash-table)
                                   "1-2" (make-hash-table)
                                   "2-3" (make-hash-table)
                                   "3-4" (make-hash-table)
                                   "4-5" (make-hash-table)
                                   "5-6" (make-hash-table)
                                   "6-7" (make-hash-table)
                                   "0-7" (make-hash-table)
                                   "10-11" (make-hash-table)
                                   "11-12" (make-hash-table)
                                   "12-13" (make-hash-table)
                                   "13-14" (make-hash-table)
                                   "14-15" (make-hash-table)
                                   "15-16" (make-hash-table)
                                   "16-17" (make-hash-table)
                                   "10-17" (make-hash-table)
                                   "1-16" (make-hash-table)
                                   "2-15" (make-hash-table))
                             :test #'equal))
                 :test #'equal))
    :test #'equal)))

;; here's the scalable lattice that Rigetti is currently considering for use as
;; an n-Q chip template (2017-10-16):
;;
;; **  **  **
;; **  **  **
;;   **  **   ...
;;   **  **   ...
;; **  **  **
;; **  **  **
;;    ...
;;
;; with the squares internally connected along the square sides and externally
;; interconnected at corners.
;;
;; XXX / NOTE: The routine below does **not** follow the qubit numbering
;;             conventions we've adopted internally, even for Aspen / Lutsen.
(defun build-nQ-trivalent-chip (top left height width &optional (architecture ':cz))
  (let ((qubit-ref-hash (make-hash-table :test 'equal))
        (chip-spec (make-chip-specification
                    :generic-rewriting-rules (coerce (global-rewriting-rules) 'vector))))
    (install-generic-compilers chip-spec architecture)
    (loop :for j :from left :below (+ left width) :do
      (loop :for i :from top :below (+ top height) :do
        ;; the basic grid element looks like
        ;;
        ;;  0 1 2 3
        ;;
        ;; \   /
        ;;  *-*      0
        ;;  | |
        ;;  *-*      1
        ;; /   \   /
        ;;      *-*  2
        ;;      | |
        ;;      *-*  3,
        ;;     /   \
        ;;
        ;; which is shaded when the two indices share a 2^1-bit. these are
        ;; the locations that need a new qubit to get generated.
        (unless (logbitp 1 (logxor i j))
          (let* ((fresh-qubit-index (chip-spec-n-qubits chip-spec))
                 (fresh-qubit (build-qubit fresh-qubit-index :type '(:RZ :X/2 :MEASURE))))
            ;; poke this qubit ID into the hashtable for later lookup
            (setf (gethash (list i j) qubit-ref-hash) fresh-qubit-index)
            ;; poke this qubit object into the hardware array
            (adjoin-hardware-object fresh-qubit chip-spec)
            ;; now we want to add the links. we're only going to reference
            ;; qubits that we've already constructed, so we need only look in
            ;; the directions NW, N, NE, and W. additionally, if we're on the
            ;; second line of a dense square (i.e., if the 2^0-bit of i is set)
            ;; then we don't bother with the diagonals (lest the square turn
            ;; into a tetrahedron)
            (let* ((nearby-qubit-coordinates
                     (list (list (1- i) j)
                           (list i (1- j))
                           (unless (logbitp 0 j)
                             (list (1- i) (1- j)))
                           (unless (logbitp 0 j)
                             (list (1+ i) (1- j)))))
                   (nearby-qubit-indices
                     (remove nil (mapcar (lambda (k) (gethash k qubit-ref-hash)) nearby-qubit-coordinates))))
              (dolist (other-qubit-index nearby-qubit-indices)
                (install-link-onto-chip chip-spec fresh-qubit-index other-qubit-index
                                        :architecture architecture)))))))
    (warm-hardware-objects chip-spec)))

;; here's another scalable lattice that Rigetti is using for the 20Q chip to
;; appear in the Forest v1.2 release (2017-12-14), usually presented as:
;;
;;   *   *   *   *   *
;;  / \ / \ / \ / \ /
;; *   *   *   *   *
;;  \   \   \   \   \  ...
;;   *   *   *   *   *
;;  / \ / \ / \ / \ /
;; *   *   *   *   *
;;         ...
;;
;; shearing the picture, this is equivalent to
;; * * * * *
;; |\|\|\|\|
;; * * * * *
;; | | | | | ...
;; * * * * *
;; |\|\|\|\|
;; * * * * *
;;    ...
;; which is a little easier to programmatically generate.
(defun build-skew-rectangular-chip (top height width &optional (architecture ':cz))
  (let ((chip-spec (make-chip-specification
                    :generic-rewriting-rules (coerce (global-rewriting-rules) 'vector))))
    (install-generic-compilers chip-spec architecture)
    ;; set up the qubits
    (loop :for q :below (* height width)
          :do (adjoin-hardware-object (build-qubit q :type '(:RZ :X/2 :MEASURE)) chip-spec))
    ;; now add the links, row-by-row
    (dotimes (i (1- height))
      (dotimes (j width)
        (let* ((qubit-index (+ j (* width i)))
               (below-qubit-index (+ qubit-index width))
               (slant-qubit-index (+ below-qubit-index 1)))
          ;; add the straight-down link
          (install-link-onto-chip chip-spec qubit-index below-qubit-index :architecture architecture)
          ;; add the diagonal link if appropriate
          (when (and (not (= j (1- width)))
                     (zerop (mod (+ i top) 2)))
            (install-link-onto-chip chip-spec qubit-index slant-qubit-index :architecture architecture)))))
    (warm-hardware-objects chip-spec)))

;;; Google Bristlecone
;;
;; The best docs I have on Bristlecone come from:
;;  * http://sos22.ahsc-nm.org/uploads/4/9/7/0/49704495/2018-kissell.pdf
;;  * https://ai.googleblog.com/2018/03/a-preview-of-bristlecone-googles-new.html
;;  * https://cirq.readthedocs.io/en/latest/gates.html#xmon-gates
;; none of which is authoritative/academic. Ideally would have the
;; paper that was presented at March Meeting 2018.
;;
;; The above show Bristlecone as a square grid, rotated, with qubits
;; coupled to nearest-neighbours. I assume this means that qubit-qubit
;; edges have CZ.
;;
;; 0   1   2
;;  \ / \ / \
;;   3   4   5
;;  / \ / \ /
;; 6   7   8
;;
;; 0 1 2
;; |/|/|
;; 3 4 5
;; |\|\|
;; 6 7 8
;; |/|/|

(defun build-bristlecone-chip-pattern (height width)
  "Create Bristlecone CHIP-SPECIFICATION with HEIGHT x WIDTH qubits."
  (let ((chip-spec (make-chip-specification
                    :generic-rewriting-rules (coerce (global-rewriting-rules) 'vector))))
    (install-generic-compilers chip-spec ':cz)
    ;; set up the qubits
    (dotimes (j (* height width))
      (adjoin-hardware-object (build-qubit j :type '(:RZ :X/2 :MEASURE)) chip-spec))
    ;; now add the links, row-by-row
    (dotimes (i (1- height))
      (dotimes (j width)
        (let* ((qubit-index (+ j (* width i)))
               (below-index (+ qubit-index width)))
          (install-link-onto-chip chip-spec qubit-index below-index)
          (cond ((and (evenp i) (plusp (mod qubit-index width)))
                 (install-link-onto-chip chip-spec qubit-index (1- below-index)))
                ((and (oddp i) (plusp (mod (1+ qubit-index) width)))
                 (install-link-onto-chip chip-spec qubit-index (1+ below-index)))))))
    (warm-hardware-objects chip-spec)))

(defun build-bristlecone-chip ()
  "Create a full 72-qubit Bristlecone CHIP-SPECIFICATION."
  (build-bristlecone-chip-pattern 12 6))

(defun build-chip-from-digraph (digraph &key (architecture ':cz))
  "Build a CHIP-SPECIFICATION from the directed graph DIGRAPH. DIGRAPH is a list of pairs (qa, qb) which implies there is a (directed) two-qubit gate from qubit qa to qubit qb."
  (let ((nqubits (1+ (reduce #'max (a:flatten digraph))))
        (chip-spec (make-chip-specification
                    :generic-rewriting-rules (coerce (global-rewriting-rules) 'vector))))
    (install-generic-compilers chip-spec architecture)
    (loop :for q :below nqubits :do
      (adjoin-hardware-object (build-qubit q :type '(:RZ :X/2 :MEASURE)) chip-spec))
    (loop :for (control target) :in digraph :do
      (install-link-onto-chip chip-spec control target :architecture architecture))
    (warm-hardware-objects chip-spec)))

(defun build-chip-from-graph (graph &key (architecture ':cz))
  "Build a CHIP-SPECIFICATION from the directed graph DIGRAPH. DIGRAPH is a list of pairs (qa, qb) which implies there is a (directed) two-qubit gate from qubit qa to qubit qb."
  (let ((nqubits (1+ (reduce #'max (a:flatten graph))))
        (chip-spec (make-chip-specification
                    :generic-rewriting-rules (coerce (global-rewriting-rules) 'vector))))
    (install-generic-compilers chip-spec architecture)
    (loop :for q :below nqubits :do
      (adjoin-hardware-object (build-qubit q :type '(:RZ :X/2 :MEASURE)) chip-spec))
    ;; Mark dead qubits
    (loop :for deadq :in (set-difference (loop :for i :below nqubits :collect i)
                                         (remove-duplicates (a:flatten graph)))
          :do (setf (gethash "dead" (hardware-object-misc-data
                                     (chip-spec-hw-object chip-spec 0 deadq)))
                    t))
    (loop :for (control target) :in graph :do
      (install-link-onto-chip chip-spec control target :architecture architecture)
      (install-link-onto-chip chip-spec target control :architecture architecture))
    (warm-hardware-objects chip-spec)))

(defun build-ibm-qx5 ()
  "Create a CHIP-SPECIFICATION matching IBM's qx5 chip."
  ;; From "16-qubit IBM universal quantum computer can be fully entangled" by Wang, Li, Yin, & Zeng.
  ;;
  ;; https://www.nature.com/articles/s41534-018-0095-x
  ;;
  ;; Also known as IBM Q 16 Rüschlikon:
  ;;
  ;; https://www.research.ibm.com/ibm-q/technology/devices/
  ;;
  ;; accessed 11 Jan 2019.
  (let ((digraph '((1 2) (2 3) (3 4) (5 4) (6 5) (6 7) (8 7)
                   (15 0) (15 14) (13 14) (12 13) (12 11) (11 10) (9 10)
                   (1 0) (15 2) (3 14) (13 4) (12 5) (6 11) (7 10) (9 8))))
    (build-chip-from-digraph digraph :architecture ':cnot)))
