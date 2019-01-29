;;;; chip-specification.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

;;; some data structures that encode hardware structure
;;;
;;; Use MAKE-ADJUSTABLE-VECTOR and VNTH for these objects.

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
                    hardware-object-native-instructions
                    hardware-object-compilation-methods
                    hardware-object-permutation-gates
                    hardware-object-rewriting-rules
                    hardware-object-cxns
                    hardware-object-misc-data))
(defstruct hardware-object
  "Houses information about a particular hardware object on a QPU.

ORDER is a non-negative integer that counts the number of qubit subsidiaries of this hardware object. Equals (1- (length (vnth 0 (hardware-object-cxns this)))). (If you drew a schematic of a chip, this is also the dimension of the graphical representation of the hardware object: 0 for qubits, 1 for links, ... .)

NATIVE-INSTRUCTIONS is a function that takes an APPLICATION as an argument. It emits the physical duration in nanoseconds if this instruction translates to a physical pulse (i.e., if it is a native gate, \"instruction native\"), and it emits NIL if this instruction does not admit direct translation to a physical pulse. A second value is returned, which is T if the instruction would be native were the qubits permuted in some fashion (\"gate native\").

COMPILATION-METHODS is a vector of functions that this device can employ to convert non-native instructions to native ones, sorted in descending order of precedence. An individual method receives an instruction and an environment (typically a PARSED-PROGRAM). The same method returns a list of instructions if successful and NIL if unsuccessful.

PERMUTATION-GATES is a vector of permutation gates that this device can natively compile, stored as PERMUTATION-RECORD structs. This data is used by the event scheduler.

REWRITING-RULES is a vector of REWRITING-RULE structures that the compressor loop can use to generate shorter gate strings.

CXNS is an array. In its nth position, there is a vector of the order n hardware objects on the chip that are connected to this one. Among other things, this is used to determine shared resource blocking.

MISC-DATA is a hash-table of miscellaneous data associated to this hardware object: scratch data, scheduling hints (e.g., qubit coherence time), ... ."
  (order 0 :type unsigned-byte :read-only t)
  (native-instructions (lambda (instr)
                         (declare (ignore instr))
                         (values nil nil))
   :type (function (t) (values t t)))
  (compilation-methods (make-adjustable-vector))
  (permutation-gates (make-adjustable-vector))
  (rewriting-rules (make-adjustable-vector))
  (cxns (make-array 2 :initial-contents (list (make-adjustable-vector)
                                              (make-adjustable-vector))))
  (misc-data (make-hash-table :test #'equal) :type hash-table))

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
  (vnth 0 (chip-specification-objects chip-spec)))
(defun chip-spec-links (chip-spec)
  (vnth 1 (chip-specification-objects chip-spec)))

(defun chip-spec-n-qubits (chip-spec)
  (length (chip-spec-qubits chip-spec)))
(defun chip-spec-n-links (chip-spec)
  (length (chip-spec-links chip-spec)))

(defun chip-spec-nth-qubit (chip-spec n)
  (vnth n (chip-spec-qubits chip-spec)))
(defun chip-spec-nth-link (chip-spec n)
  (vnth n (chip-spec-links chip-spec)))

(defun chip-spec-links-on-qubit (chip-spec qubit-index)
  (vnth 1 (hardware-object-cxns (chip-spec-nth-qubit chip-spec qubit-index))))
(defun chip-spec-qubits-on-link (chip-spec link-index)
  (vnth 0 (hardware-object-cxns (chip-spec-nth-link chip-spec link-index))))
(defun chip-spec-hw-object (chip-spec order address)
  (vnth address (vnth order (chip-specification-objects chip-spec))))
(defun chip-spec-adj-qubits (chip-spec qubit-index)
  (loop
    :for link-index :across (chip-spec-links-on-qubit chip-spec qubit-index)
    :append (remove qubit-index (coerce (chip-spec-qubits-on-link chip-spec link-index) 'list))))
(defun chip-spec-adj-links (chip-spec link-index)
  (loop
    :for qubit-index :across (chip-spec-qubits-on-link chip-spec link-index)
    :append (remove link-index (coerce (chip-spec-links-on-qubit chip-spec qubit-index) 'list))))
(defun chip-spec-qubit-dead? (chip-spec qubit-index)
  (gethash "dead" (hardware-object-misc-data (chip-spec-nth-qubit chip-spec qubit-index))))

(defun lookup-hardware-address-by-qubits (chip-spec args)
  (unless (chip-specification-lookup-cache chip-spec)
    (warm-chip-spec-lookup-cache chip-spec))
  (alexandria:when-let ((hash-value (gethash args (chip-specification-lookup-cache chip-spec))))
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


;;; constructors for hardware object building blocks

(defun build-link (qubit0 qubit1 &optional (type (list ':CZ)))
  "Constructs a template link. Legal types: (lists of) :CZ, :CPHASE, :ISWAP, :PISWAP, :CNOT."
  (check-type qubit0 unsigned-byte)
  (check-type qubit1 unsigned-byte)
  (assert (/= qubit0 qubit1))
  (setf type (alexandria:ensure-list type))
  (let* ((misc-data (make-hash-table :test #'equal))
         (obj (make-hardware-object
               :order 1
               :native-instructions
               (lambda (instr)
                 (if (not (or (typep instr 'gate-application)
                              (typep instr 'unresolved-application)))
                     (values nil nil)
                     (let* ((duration-alist (gethash "duration-alist" misc-data))
                            (duration (cdr (assoc instr duration-alist :test #'operator-match-p))))
                       (values
                        ;; Is the instruction precisely native? (both gate + qubit indexes)
                        duration
                        ;; Is the gate native? (just the gate)
                        (and (or duration
                                 (and (plain-operator-p (application-operator instr))
                                      (assoc (application-operator-name instr)
                                             duration-alist
                                             :test (lambda (name pattern)
                                                     (string= name (first pattern))))))
                             t)))))
               :cxns (vector (vector qubit0 qubit1) #())
               :misc-data misc-data)))
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
    ;; set up the other records
    (loop :with lower-precedence := type
          :with higher-precedence := nil
          :for current-type :in type
          :do (setf lower-precedence (rest lower-precedence))
              ;; set up duration-alist
          :nconc (case current-type
                   ;; In usual interactions, these are all capable of
                   ;; acting bidirectionally.
                   (:CZ     (list (cons `("CZ"     ()  ,qubit0 ,qubit1) 150)
                                  (cons `("CZ"     ()  ,qubit1 ,qubit0) 150)))
                   (:CPHASE (list (cons `("CPHASE" (_) ,qubit0 ,qubit1) 150)
                                  (cons `("CPHASE" (_) ,qubit1 ,qubit0) 150)))
                   (:ISWAP  (list (cons `("ISWAP"  ()  ,qubit0 ,qubit1) 150)
                                  (cons `("ISWAP"  ()  ,qubit1 ,qubit0) 150)))
                   (:PISWAP (list (cons `("PISWAP" (_) ,qubit0 ,qubit1) 150)
                                  (cons `("PISWAP" (_) ,qubit1 ,qubit0) 150)))
                   ;; CNOT typically only acts in one direction
                   ;; natively. One can swap the direction of CNOT 0 1
                   ;; by sandwiching it between Hadamard gates. (The
                   ;; compiler will do this for you elsewhere.)
                   ;;
                   ;;     +----------+
                   ;;     | H 0      |
                   ;;     | H 1      |    +----------+
                   ;;     | CNOT 0 1 | == | CNOT 1 0 |
                   ;;     | H 1      |    +----------+
                   ;;     | H 0      |
                   ;;     +----------+
                   ;;
                   (:CNOT   (list (cons `("CNOT"   ()  ,qubit0 ,qubit1) 150)))
                   ;; We need some qubit type to be specified...
                   (otherwise (error "Unknown qubit type.")))
            :into duration-alist
          ;; set up single-type rewriting rules
          :nconc (case current-type
                   (:CZ     (rewriting-rules-for-link-of-CZ-type))
                   (:CNOT   (rewriting-rules-for-link-of-CNOT-type))
                   (:CPHASE (rewriting-rules-for-link-of-CPHASE-type))
                   (:ISWAP  (rewriting-rules-for-link-of-ISWAP-type))
                   (:PISWAP (rewriting-rules-for-link-of-PISWAP-type)))
            :into rewriting-rules
          ;; set up double-type rewriting rules
          :nconc (cond
                   ((and (eql  ':PISWAP current-type)
                         (find ':ISWAP  lower-precedence))
                    (rewriting-rules-preferring-PISWAP-to-ISWAP))
                   ((and (eql  ':ISWAP  current-type)
                         (find ':PISWAP lower-precedence))
                    (rewriting-rules-preferring-ISWAP-to-PISWAP))
                   ((and (eql  ':CPHASE current-type)
                         (find ':CZ     lower-precedence))
                    (rewriting-rules-preferring-CPHASE-to-CZ))
                   ((and (eql  ':CZ     current-type)
                         (find ':CPHASE lower-precedence))
                    (rewriting-rules-preferring-CZ-to-CPHASE)))
            :into rewriting-rules
          ;; set up compilation methods
          :nconc (cond
                   ((and (eql ':CNOT current-type)
                         (not (find ':CPHASE higher-precedence))
                         (not (find ':CZ higher-precedence)))
                    (list (build-CZ-to-CNOT-translator qubit0 qubit1)))
                   ((and (eql ':CZ current-type)
                         (not (find ':CPHASE higher-precedence))
                         (not (find ':CZ     higher-precedence)))
                    (list* #'CNOT-to-CZ
                           (unless (member ':CPHASE type)
                             (list #'CPHASE-to-CNOT))))
                   ((and (eql ':CPHASE current-type)
                         (not (find ':CPHASE higher-precedence))
                         (not (find ':CZ     higher-precedence)))
                    (list #'CNOT-to-CZ #'CZ-to-CPHASE))
                   ((and (eql ':PISWAP current-type)
                         (not (find ':ISWAP  higher-precedence)))
                    (list #'ISWAP-to-PISWAP)))
            :into compilation-methods
          :finally
             (setf (gethash "duration-alist" misc-data)
                   (nconc duration-alist (gethash "duration-alist" misc-data)))
             (setf (hardware-object-rewriting-rules obj)
                   (make-array (length rewriting-rules)
                               :adjustable t
                               :initial-contents rewriting-rules))
             (loop :for method :in compilation-methods
                   :do (vector-push-extend method
                                           (hardware-object-compilation-methods obj))))
    ;; set up the basic optimal 2Q compiler
    (vector-push-extend (optimal-2q-compiler-for type)
                        (hardware-object-compilation-methods obj))
    ;; return the qubit
    obj))


(defun build-qubit (&optional (type (list ':RZ ':X/2)))
  "Constructs a template qubit.  Legal types: (lists of) ':RZ, ':Z/2, ':RX, ':X/2."
  (let* ((misc-data (make-hash-table :test #'equal))
         (obj (make-hardware-object :order 0
                                    :native-instructions (lambda (instr)
                                                           (cdr (assoc instr
                                                                       (gethash "duration-alist" misc-data)
                                                                       :test #'operator-match-p)))
                                    :misc-data misc-data)))
    (setf (gethash "duration-alist" misc-data)
          (list (cons '("MEASURE" _ _) 2000)
                (cons '("MEASURE" _) 2000)))
    (loop :with lower-precedence := type
          :with higher-precedence := nil
          :for current-type :in type
          :do (setf lower-precedence (rest lower-precedence))
              ;; set up duration-alist
          :nconc (case current-type
                   ;; NOTE: supposedly RZ's take 0ns, but this causes too much
                   ;; of a headache in addresser/outgoing-schedule.lisp, so we
                   ;; instead use a small value.
                   (:RZ   (list (cons `("RZ" (_)          _) 1/100)))
                   (:X/2  (list (cons `("RX" (,(/ pi 2))  _) 9)
                                (cons `("RX" (,(/ pi -2)) _) 9)
                                (cons `("RX" (,pi)        _) 9)
                                (cons `("RX" (,(- pi))    _) 9)))
                   (otherwise (error "Unknown qubit type.")))
            :into duration-alist
          ;; set up single-type rewriting rules
          :nconc (cond
                   ((and (find current-type '(:RX :X/2))
                         (not (find ':RX higher-precedence))
                         (not (find ':X/2 higher-precedence)))
                    (rewriting-rules-for-roll-RX))
                   ((and (find current-type '(:RZ :Z/2))
                         (not (find ':RZ  higher-precedence))
                         (not (find ':Z/2 higher-precedence)))
                    (rewriting-rules-for-roll-RZ)))
            :into rewriting-rules
          ;; set up double-type rewriting rules
          :nconc (cond
                   ((and (find current-type '(:RX :X/2))
                         (or (find  ':RZ  lower-precedence)
                             (find  ':Z/2 lower-precedence))
                         (not (find ':RX  higher-precedence))
                         (not (find ':X/2 higher-precedence)))
                    (rewriting-rules-preferring-RX-to-RZ))
                   ((and (find current-type '(:RZ :Z/2))
                         (or (find  ':RX  lower-precedence)
                             (find  ':X/2 lower-precedence))
                         (not (find ':RZ  higher-precedence))
                         (not (find ':Z/2 higher-precedence)))
                    (rewriting-rules-preferring-RZ-to-RX)))
            :into rewriting-rules
          ;; set up compilation methods
          :nconc (cond
                   ((and (find current-type '(:RZ :Z/2))
                         (or (find  ':RX  lower-precedence)
                             (find  ':X/2 lower-precedence))
                         (not (find ':RZ  higher-precedence))
                         (not (find ':Z/2 higher-precedence)))
                    (list #'RY-to-XZX
                          #'RX-to-ZXZXZ
                          #'euler-compiler)))
            :into compilation-methods
          :finally
             (setf (gethash "duration-alist" misc-data)
                   (nconc duration-alist (gethash "duration-alist" misc-data)))
             (setf (hardware-object-rewriting-rules obj)
                   (make-array (length rewriting-rules)
                               :adjustable t
                               :initial-contents rewriting-rules))
             (setf (hardware-object-compilation-methods obj)
                   (make-array (length compilation-methods)
                               :adjustable t
                               :initial-contents compilation-methods)))
    ;; return the qubit
    obj))


;;; routines for populating the fields of a CHIP-SPECIFICATION object (and
;;; maintaining the appropriate interrelations).

(defun install-generic-compilers (chip-spec architecture)
  (let ((ret (make-adjustable-vector)))
    (vector-push-extend (lambda (instr)
                          (SWAP-to-native-SWAPs chip-spec instr))
                        ret)
    (when (optimal-2q-target-meets-requirements architecture ':cz)
      (vector-push-extend (lambda (instr)
                            (CNOT-to-native-CNOTs chip-spec instr))
                          ret)
      (vector-push-extend (lambda (instr)
                            (CZ-to-native-CZs chip-spec instr))
                          ret))
    (when (optimal-2q-target-meets-requirements architecture ':iswap)
      (vector-push-extend (lambda (instr)
                            (ISWAP-to-native-ISWAPs chip-spec instr))
                          ret))
    (when (optimal-2q-target-meets-requirements architecture ':cphase)
      (vector-push-extend (lambda (instr)
                            (CPHASE-to-native-CPHASEs chip-spec instr))
                          ret))
    (when (optimal-2q-target-meets-requirements architecture ':piswap)
      (vector-push-extend (lambda (instr)
                            (PISWAP-to-native-PISWAPs chip-spec instr))
                          ret))
    (when (find ':cnot (alexandria:ensure-list architecture))
      (vector-push-extend (lambda (instr)
                            (CNOT-to-native-CNOTs chip-spec instr))
                          ret))
    (cond
      ((optimal-2q-target-meets-requirements architecture ':cz)
       (vector-push-extend #'ucr-compiler ret))
      ((optimal-2q-target-meets-requirements architecture ':iswap)
       (vector-push-extend (lambda (instr)
                             (ucr-compiler instr :target ':iswap))
                           ret))
      ((find ':cnot (alexandria:ensure-list architecture))
       (vector-push-extend (lambda (instr)
                             (ucr-compiler instr :target ':cnot))
                           ret))
      (t
       (error "Can't find a general UCR compiler for this target type.")))
    (vector-push-extend #'state-prep-compiler ret)
    (vector-push-extend #'recognize-ucr ret)
    (when (typep architecture 'optimal-2q-target)
      (vector-push-extend (lambda (instr)
                            (optimal-2q-compiler instr :target architecture))
                          ret))
    (vector-push-extend #'qs-compiler ret)
    (setf (chip-specification-generic-compilers chip-spec) ret)))


(defun install-link-onto-chip (chip-specification q0 q1 &key (architecture (list ':cz)))
  "Adds a link, built using BUILD-LINK, between qubits Q0 and Q1 on the chip described by CHIP-SPECIFICATION.  Returns the HARDWARE-OBJECT instance corresponding to the new link."
  (let ((link (build-link q0 q1 architecture))
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



;;; now constructors for whole chips.

;; here we provide a function that generates an example chip. in general, chip
;; specifications should be generated based on read-in ISA data, and not hard-
;; wired into the code. (certain aspects, though, like which compilation methods
;; to use or what it means to be a link "of CZ type" are things that will
;; probably need to be hard-wired.)
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
  (let ((chip-spec (make-chip-specification
                    :generic-rewriting-rules (coerce (global-rewriting-rules) 'simple-vector))))
    (install-generic-compilers chip-spec architecture)
    (loop :repeat 8 :do
      (adjoin-hardware-object (build-qubit) chip-spec))
    (dotimes (i 8)
      (install-link-onto-chip chip-spec i (mod (1+ i) 8)
                              :architecture architecture))
    chip-spec))

;; here's another example chip with a linear topology:
;; 0 --0-- 1 --1-- 2 --2-- ... --(n-2)-- (n-1)
(defun build-nQ-linear-chip (n &key (architecture ':cz))
  (let ((chip-spec (make-chip-specification
                    :generic-rewriting-rules (coerce (global-rewriting-rules) 'vector))))
    (install-generic-compilers chip-spec architecture)
    ;; prep the qubits
    (loop :repeat n :do
      (adjoin-hardware-object (build-qubit) chip-spec))
    ;; prep the links
    (dotimes (i (1- n))
      (install-link-onto-chip chip-spec i (1+ i) :architecture architecture))
    chip-spec))

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
          (let* ((fresh-qubit (build-qubit))
                 (fresh-qubit-index (chip-spec-n-qubits chip-spec)))
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
                (install-link-onto-chip chip-spec fresh-qubit-index other-qubit-index :architecture architecture)))))))
    chip-spec))

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
    (loop :repeat (* height width) :do
      (adjoin-hardware-object (build-qubit) chip-spec))
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
    chip-spec))

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
      (adjoin-hardware-object (build-qubit) chip-spec))
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
    chip-spec))

(defun build-bristlecone-chip ()
  "Create a full 72-qubit Bristlecone CHIP-SPECIFICATION."
  (build-bristlecone-chip-pattern 12 6))

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
  (let ((chip-spec (make-chip-specification
                    :generic-rewriting-rules (coerce (global-rewriting-rules) 'vector)))
        (nqubits 16)
        (links '((1 2)  (2 3)   (3 4)   (5 4)   (6 5)   (6 7)   (8 7)
                 (15 0) (15 14) (13 14) (12 13) (12 11) (11 10) (9 10)
                 (1 0) (15 2) (3 14) (13 4) (12 5) (6 11) (7 10) (9 8))))
    (install-generic-compilers chip-spec ':cnot)
    (loop :repeat nqubits :do
      (adjoin-hardware-object (build-qubit) chip-spec))
    (loop :for (control target) :in links :do
      (install-link-onto-chip chip-spec control target :architecture '(:CNOT)))
    chip-spec))
