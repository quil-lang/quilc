;;;; chip-reader.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; This file contains routines that read in "ISA" specifications, which are
;;;; JSON dictionaries that specify QPU information.  The top level entries of
;;;; the dictionary are "layers", which each contain some semantically cohesive
;;;; information about the QPU.  The only irregular entry is keyed on "id",
;;;; whose value is a dictionary of the following form:
;;;;
;;;; { name: optional string,
;;;;   version: optional string,
;;;;   ... any other identification info (e.g., a timestamp) }
;;;;
;;;; All other toplevel keys are dictionaries, structured the same way. The
;;;; dictionary is keyed on strings of the form "nQ", n a positive integer, which
;;;; holds the information related to n-qubit operations. Each value is again
;;;; a dictionary, keyed on strings of the form "m1-...-mn", where each mj is a
;;;; positive integer and mj < m(j+1) is sorted ascending.  The value for this
;;;; object contains the information related to the specific n-qubit complex
;;;; (m1, ..., mn).
;;;;
;;;; In the case of the layer "isa", this complex data is a dictionary which
;;;; tracks information about legal operations on the QPU components:
;;;; { type: optional string or optional list of strings, in the case of 1Q
;;;;         drawn from the set { "X/2" } and in the case of 2Q drawn from
;;;;          the set { "CZ", "ISWAP", "CPHASE", "PISWAP" }. these indicate which
;;;;          operations are legal on this component of the QPU. in the case of a
;;;;          list, the operations are sorted into "preference order": the
;;;;          compiler will make an effort to prefer instructions earlier in the
;;;;          sequence to those later in the sequence. for 1Q the default is
;;;;          [ "RZ", "X/2" ], and for 2Q the default is "CZ".
;;;;   gates: a (unordered) list of dictionaries recording the native operations for
;;;;          this object, each given via one of the two following forms:
;;;;          { operator: the string "MEASURE".
;;;;            qubit: an integer indicating the qubit to be MEASUREd
;;;;                   OR the string "_" to indicate any qubit.
;;;;            target: an optional field indicating the memory region to be written to
;;;;                   OR the string "_" to indicate any region.
;;;;            duration: time in ns that it takes to perform a measurement.
;;;;            fidelity: readout fidelity for this measurement.
;;;;          } OR {
;;;;            operator: the name of the gate application as a string.
;;;;            parameters: list of parameters supplied to this native instruction,
;;;;                        which can be numeric literals or "_".
;;;;            arguments: list of qubit arguments supplied to this native instruction,
;;;;                       which can be numeric literals or "_".
;;;;            duration: time in ns that it takes to execute this instruction.
;;;;            fidelity: gate fidelity for this instruction.
;;;;          }
;;;;   dead:  optional boolean. when set to T, the compiler will treat all
;;;;          instructions on this component as illegal. }
;;;;
;;;; In the case of the layer "specs", this complex data is again a dictionary,
;;;; which tracks information about fidelity properties of operations. it is
;;;; keyed on strings irregularly related to the names of the operations, as in:
;;;; { "fCZ": optional float for CZ fidelity,
;;;;   "fCZ_std_err": standard error in quoted fCZ measurement,
;;;;   "fCPHASE": optional float for CPHASE fidelity,    ; NB: it is unspecified what angle value this pertains to.
;;;;   "f1QRB": optional float for X/2 fidelity,
;;;;   "fRO": optional float for readout fidelity,
;;;;   "fActiveReset": optional float for active reset success probability,
;;;;   "fBellState": optional float for Bell state preparation success,
;;;;   "T1": optional float measuring the relaxation lifetime of a qubit,
;;;;   "T2": optional float measuring the decoherence lifetime of a qubit,
;;;;   ... }

(in-package #:cl-quil)

(define-condition missing-isa-layer-error (error)
  ()
  (:documentation "This condition can be signaled when the chip reader fails to find an ISA layer.")
  (:report "Invalid QPU description file: missing required ISA layer or sub-layer."))

(defun expand-key-to-integer-list (key)
  "Expands a string of the form \"n1-...-nm\" to the list of integers (list nj1 ... njm), sorted ascending."
  (etypecase key
    (string
     (let ((unsorted-list (mapcar #'parse-integer (split-sequence:split-sequence #\- key))))
       (cond
         ((every #'<= unsorted-list (rest unsorted-list))
          unsorted-list)
         (t
          (warn "Encountered an unsorted ISA key: ~A" unsorted-list)
          (sort unsorted-list #'<)))))
    (integer-list
     key)))

(defun dead-qubit-hash-table ()
  (yason:parse "{\"dead\": true}"))

(defun ensure-1q-layer-is-complete (isa-hash)
  "Ensure the 1Q field has complete qubit information. If it doesn't, fill it in with dead qubits. Return the (possibly) modified ISA-HASH."
  (let ((1q-layer (gethash "1Q" isa-hash)))
    (when (null 1q-layer)
      (error 'missing-isa-layer-error))
    ;; Get the maximally indexed qubit, so we can bound the indexes of
    ;; the qubits we need to add.
    (let ((max-qubit-index
            (loop :for (k) :being :the :hash-keys :of 1q-layer
                  :maximize k)))
      ;; Now, go through the hash table, filling in anything
      ;; missing. We can go :BELOW and not :TO with DOTIMES since we
      ;; know the last index exists.
      (dotimes (q max-qubit-index isa-hash)
        (multiple-value-bind (qubit-hash exists?)
            (gethash (list q) 1q-layer)
          (declare (ignore qubit-hash))
          (unless exists?
            (setf (gethash (list q) 1q-layer) (dead-qubit-hash-table))))))))

(defun parse-binding (gate-datum)
  "Parses an entry in the \"gates\" field of an ISA object descriptor."
  (flet ((intern-if-string (x)
           (typecase x
             (string '_)
             (otherwise x))))
    (cond
      ((string= "MEASURE" (gethash "operator" gate-datum))
       (make-measure-binding :target (intern-if-string (gethash "target" gate-datum))
                             :qubit (intern-if-string (gethash "qubit" gate-datum))))
      ((string= "_" (gethash "operator" gate-datum))
       (make-wildcard-binding))
      (t
       (multiple-value-bind (operator op-p) (gethash "operator" gate-datum)
         (unless op-p (error 'missing-gates-default-value
                             "A \"gates\" field was provided in its ISA, but no \"operator\" key was present in its dictionary."))
         (multiple-value-bind (params params-p) (gethash "parameters" gate-datum)
           (unless params-p (error "A \"gates\" field was provided in its ISA, but no \"parameters\" key was present in its dictionary."))
           (multiple-value-bind (args args-p) (gethash "arguments" gate-datum)
             (unless args-p (error "A \"gates\" field was provided in its ISA, but no \"arguments\" key was present in its dictionary."))
             (make-gate-binding :operator (named-operator operator)
                                :parameters (map 'list #'intern-if-string params)
                                :arguments (map 'list #'intern-if-string args)))))))))

(defun deforestify-fidelity (fidelity)
  "The fidelity values recorded in Forest DB are subject to various quirks. This \"parser\" un-quirks them."
  (cond
    ((double= fidelity +forest-error-sentinel+)
     (warn "Chip specification contains a fidelity field populated by the Forest error sentinel. Replacing it with the default ~A." +totally-awful-fidelity+)
     +totally-awful-fidelity+)
    ((> fidelity 1.0)
     (warn "Chip specification contained fidelity ~A > 1.0:*. Truncating to ~A." fidelity +near-perfect-fidelity+)
     +near-perfect-fidelity+)
    ((> fidelity +near-perfect-fidelity+)
     ;; Silent truncation.
     +near-perfect-fidelity+)
    ((minusp fidelity)
     (error "Chip specification contained negative fidelity ~A. I don't know what to do with this value." fidelity))
    ((zerop fidelity)
     double-float-epsilon)
    (t
     fidelity)))

(defun parse-gate-information (gate-datum)
  (let (args)
    (a:when-let ((fidelity (gethash "fidelity" gate-datum)))
      (setf fidelity (deforestify-fidelity fidelity)
            args (list* ':fidelity fidelity args)))
    (a:when-let ((duration (gethash "duration" gate-datum)))
      (setf args (list* ':duration duration args)))
    (apply #'make-gate-record args)))

(defun parse-gates-field (gates)
  (let ((gate-information (make-hash-table :test #'equalp)))
    (map nil (lambda (gate-datum)
               (setf (gethash (parse-binding gate-datum) gate-information)
                     (parse-gate-information gate-datum)))
         gates)
    gate-information))

(defun load-isa-layer (chip-spec isa-hash)
  "Loads the \"isa\" layer into a new chip-specification object."
  (unless (and isa-hash
               (gethash "1Q" isa-hash))
    (error 'missing-isa-layer-error))
  ;; Make sure some dummy didn't forget to specify a qubit. Declare it
  ;; as dead if they weren't listed.
  (ensure-1q-layer-is-complete isa-hash)
  ;; initialize an empty qubit list with entries drawn from [0, n)
  (let ((qubit-count (hash-table-count (gethash "1Q" isa-hash))))
    (setf (vnth 0 (chip-specification-objects chip-spec))
          (make-array qubit-count :initial-element nil))
    ;; for each logical qubit descriptor...
    (dohash ((key qubit-hash) (gethash "1Q" isa-hash))
      (destructuring-bind (i) key
        (assert (< i qubit-count)
                nil
                "ISA contains a 1Q descriptor of index ~A, but there are only ~A qubit(s) altogether."
                i (1- qubit-count))
        ;; form a qubit
        (let (qubit)
          (cond
            ;; there's a "gates" field
            ((gethash "gates" qubit-hash)
             (setf qubit (build-qubit i :gate-information (parse-gates-field (gethash "gates" qubit-hash)))))
            ;; there's no "gates" field, but there is a "type" field, and it's supported
            ((and (string= "Xhalves" (gethash "type" qubit-hash)))
             (warn "The ISA \"type\" field is deprecated and will be removed in favor of the \"gates\" field.")
             (setf qubit (build-qubit i :type '(:RZ :X/2 :MEASURE))))
            ;; there's no "gates" field, but there is a "type" field, but it isn't supported
            ((gethash "type" qubit-hash)
             (error "On qubit ~A, unknown qubit type field in QPU descriptor: ~A."
                    i (gethash "type" qubit-hash)))
            ;; there's neither a "gates" nor a "type" field. install a default.
            (t
             (setf qubit (build-qubit i :type '(:RZ :X/2 :MEASURE)))))
          ;; store the descriptor in the qubit hardware-object for later reference
          (setf (hardware-object-misc-data qubit) qubit-hash)
          ;; and store the hardware-object into the chip specification
          (setf (vnth i (vnth 0 (chip-specification-objects chip-spec)))
                qubit))))
    ;; for each logical link descriptor...
    (when (gethash "2Q" isa-hash)
      (dohash ((key link-hash) (gethash "2Q" isa-hash))
        (destructuring-bind (q0 q1) key
          ;; check that the link is ordered
          (assert (< q0 q1)
                  nil
                  "Link descriptor found with edge label \"~{~A~^-~}\". Edge labels are required to be sorted ascending; use \"~{~A~^-~}\" instead."
                  (list q0 q1) (list q1 q0))
          ;; check that the link lies on reasonable qubits
          (assert (< q0 q1 qubit-count)
                  nil
                  "ISA contains a 2Q hardware descriptor attached to qubits ~A, but there are only ~A qubit(s) altogether."
                  (list q0 q1) qubit-count)
          (let (link
                (link-index (chip-spec-n-links chip-spec)))
            (cond
              ;; NOTE: By skipping the dead links, we're shifting the internal link
              ;;       indices from what a user (or debugger) might expect.  Beware!
              ((or (gethash "dead" link-hash)
                   (hardware-object-dead-p (chip-spec-nth-qubit chip-spec q0))
                   (hardware-object-dead-p (chip-spec-nth-qubit chip-spec q1)))
               t)
              ;; there's a "gates" field
              ((gethash "gates" link-hash)
               (setf link (build-link q0 q1 :gate-information (parse-gates-field (gethash "gates" link-hash)))))
              ;; there's no "gates" field, but there is a "type" field
              ((gethash "type" link-hash)
               (warn "The ISA \"type\" field is deprecated and will be removed in favor of the \"gates\" field.")
               (labels ((individual-target-parser (string)
                          (cond
                            ((null string) nil)
                            ((string= "CZ" string) ':cz)
                            ((string= "ISWAP" string) ':iswap)
                            ((string= "SWAP" string) ':swap)
                            ((string= "CPHASE" string) ':cphase)
                            ((string= "PISWAP" string) ':piswap)
                            ((string= "XY" string) ':xy)
                            (t (error "Unknown link type in QPU descriptor on link ~A: ~A."
                                      (list q0 q1) string)))))
                 (setf link (build-link q0 q1
                                        :type (mapcar #'individual-target-parser
                                                      (a:ensure-list (gethash "type" link-hash)))))))
              ;; there's no "gates" or "type" field, so install a default
              (t
               (setf link (build-link q0 q1 :type '(:CZ)))))
            (when link
              ;; notify the qubits that they're attached.
              (dolist (qubit-index (list q0 q1))
                (vector-push-extend link-index (chip-spec-links-on-qubit chip-spec qubit-index)))
              ;; store the descriptor in the link hardware-object for later reference
              (setf (hardware-object-misc-data link) link-hash)
              ;; and store the hardware-object into the chip specification
              (vector-push-extend link (chip-spec-links chip-spec)))))))))

(defun load-specs-layer (chip-spec specs-hash)
  "Loads the \"specs\" layer into a chip-specification object."
  (a:when-let ((1q-specs (gethash "1Q" specs-hash)))
    (loop :for i :from 0
          :for qubit :across (chip-spec-qubits chip-spec)
          :for gate-info := (hardware-object-gate-information qubit)
          :do ;; load the qubit specs info
              (a:when-let ((spec (gethash (list i) 1q-specs)))
                (setf (gethash "specs" (hardware-object-misc-data qubit))
                      spec)
                (a:when-let ((fidelity (gethash "fRO" spec)))
                  (dohash ((binding record) gate-info)
                    (when (measure-binding-p binding)
                      (setf (gethash binding gate-info)
                            (copy-gate-record record :fidelity fidelity)))))
                (a:when-let* ((fidelity (gethash "f1QRB" spec))
                              (fidelity (deforestify-fidelity fidelity)))
                  (dohash ((binding record) gate-info)
                    (when (and (gate-binding-p binding)
                               (equalp (named-operator "RX") (gate-binding-operator binding))
                               ;; Allow for free parameters (e.g. "theta") RXs in ISAs
                               (numberp (first (gate-binding-parameters binding)))
                               (not (double= 0d0 (first (gate-binding-parameters binding)))))
                      (unless (double= 0d0 (mod (first (gate-binding-parameters binding)) pi/2))
                        (warn "Qubit ~A: applying f1QRB spec to unusual native gate RX(~A)" i (first (gate-binding-parameters binding))))
                      (setf (gethash binding gate-info)
                            (copy-gate-record record :fidelity fidelity))))))))
  (a:when-let ((2q-specs (gethash "2Q" specs-hash)))
    (loop :for key :being :the :hash-keys :of 2q-specs
            :using (hash-value spec)
          :for (q0 q1) := key
          :for link := (lookup-hardware-object-by-qubits chip-spec (list q0 q1))
          :for gate-info := (and link (hardware-object-gate-information link))
          :when gate-info
            :do (flet ((stash-fidelity (gate-name parameters)
                         (a:when-let* ((fidelity (gethash (format nil "f~A" gate-name) spec))
                                       (binding (make-gate-binding :operator (named-operator gate-name)
                                                                   :parameters parameters
                                                                   :arguments '(_ _)))
                                       (record (gethash binding gate-info)))
                           (setf fidelity (deforestify-fidelity fidelity))
                           (setf (gethash binding gate-info)
                                 (copy-gate-record record :fidelity fidelity)))))
                  (setf (gethash "specs" (hardware-object-misc-data link))
                        spec)
                  (dolist (args '(("CZ" ())
                                  ("ISWAP" ())
                                  ("CPHASE" (_))
                                  ("PISWAP" (_))))
                    (destructuring-bind (name params) args
                      (stash-fidelity name params)))))))

;; NOTE: This function (and its call sites) are an unfortunate anachronism of
;; CL-QUIL / quilc development. QPU specifications were originally sent over
;; the wire (or read in from disk) as JSON dictionaries, which have the
;; restriction that they can only be keyed on strings. Internally, it would have
;; been more convenient to have them keyed on lists of numbers, and so we decided
;; on the convention that the string "n1" (with n1 a number) deserialize to the
;; list (n1), the string "n1-n2" deserialize to the list (n1 n2), and so on. we
;; managed this right at the deserialization layer by providing an :object-key-fn
;; key to yason:parse.
;;
;; later, we migrated to RPCQ, which does support sending dictionaries keyed on
;; non-strings over the wire, but we didn't initially make use of it: the client
;; still serialized the QPU specification through JSON, then sent the JSON string
;; along to be deserialized by the above means. later, someone half-assed the
;; RPCQ message |TargetDevice|, which broke the "isa" and "specs" top-level keys
;; from a QPU specification into RPCQ slots, but then marked their contents as
;; :any -> :any dictionaries and stored the non-JSON-serialized dictionaries
;; there, which were keyed on single-qubit unwrapped integers and qubit-qubit
;; pairs of integers (breaking consistently with the deser layer above).
;;
;; this type mismatch caused bugs, which we awkwardly patched over for a while,
;; until we ultimately found ourselves in a position without a backwards
;; compatible solution. the situation is improving: quilc's HTTP endpoint is
;; sunsetting, and with it the JSON payloads are likely to disappear too (though
;; one must still consider their CLI usage), and so we need only consider the
;; client's awkward behavior with the RPCQ payloads. for now, we are going to
;; isolate this awkward behavior into this single function, which converts all
;; of the possible options above into integer lists, and adhere rigidly to the
;; use of integer vectors within the CL-QUIL codebase.
;;
;;        -ecp
(defun sanitize-qpu-hash-layer (layer)
  (flet ((walk-sublayer (sublayer)
           (let ((new-sublayer (make-hash-table :test #'equalp)))
             (dohash ((key val) sublayer new-sublayer)
               (etypecase key
                 (string
                  (setf key (sort (expand-key-to-integer-list key) #'<)))
                 (integer
                  (setf key (list key)))
                 (integer-vector
                  (setf key (sort (coerce key 'list) #'<)))
                 (integer-list
                  (setf key (sort (copy-list key) #'<))))
               (setf (gethash key new-sublayer) val)))))
    (let ((new-layer (make-hash-table :test #'equalp)))
      (a:when-let ((sublayer (gethash "1Q" layer)))
        (setf (gethash "1Q" new-layer) (walk-sublayer sublayer)))
      (a:when-let ((sublayer (gethash "2Q" layer)))
        (setf (gethash "2Q" new-layer) (walk-sublayer sublayer)))
      new-layer)))

(defun qpu-hash-table-to-chip-specification (hash-table)
  "Converts a QPU specification HASH-TABLE, to a chip-specification object.  Requires an \"isa\" layer."
  (check-type hash-table hash-table)
  (let* ((isa-hash (sanitize-qpu-hash-layer
                    (or (gethash "isa" hash-table)
                        (error 'missing-isa-layer-error))))
         (chip-spec (make-chip-specification
                     :objects (make-array 2 :initial-contents (list (make-adjustable-vector)
                                                                    (make-adjustable-vector)))
                     :generic-rewriting-rules (coerce (global-rewriting-rules) 'vector))))
    ;; set up the self-referential compilers
    (install-generic-compilers chip-spec (list ':cz))
    ;; now load the layers out of the .qpu hash
    (load-isa-layer chip-spec isa-hash)
    (a:when-let ((specs-hash (gethash "specs" hash-table)))
      (load-specs-layer chip-spec (sanitize-qpu-hash-layer specs-hash)))
    ;; and return the chip-specification that we've built
    (warm-hardware-objects chip-spec)))

(defun chip-specification-to-qpu-hash-table (chip-spec)
  "Convert a chip-specification CHIP-SPEC to a QPU hash table. Useful when you want to pull an ISA out of a pre-built chip."
  (check-type chip-spec chip-specification)
  (let* ((nq (chip-spec-n-qubits chip-spec))
         (nl (chip-spec-n-links chip-spec))
         (table (make-hash-table :test 'equalp)))
    (let* ((isa (setf (gethash "isa" table) (make-hash-table :test #'equalp)))
           (oneq (setf (gethash "1Q" isa) (make-hash-table :test #'equalp)))
           (twoq (setf (gethash "2Q" isa) (make-hash-table :test #'equalp))))
      (loop :for qi :below nq
            :unless (chip-spec-qubit-dead? chip-spec qi) :do
              (setf (gethash (format nil "~S" qi) oneq)
                    (hardware-object-misc-data (chip-spec-nth-qubit chip-spec qi))))
      (loop :for li :below nl
            :for qs := (chip-spec-qubits-on-link chip-spec li)
            :unless (zerop (length qs)) :do
              (setf (gethash (format nil "~D-~D" (elt qs 0) (elt qs 1)) twoq)
                    (hardware-object-misc-data (chip-spec-nth-link chip-spec li)))))
    table))

(defun read-chip-spec-file (file)
  "Read a QPU specification from a file."
  (qpu-hash-table-to-chip-specification
   (with-open-file (s file)
     (let ((*read-default-float-format* 'double-float))
       (yason:parse s)))))

(define-condition illegal-qubits-used-error (error)
  ((illegal-qubits :initarg :illegal-qubits :reader illegal-qubits-used-error-illegal-qubits)
   (instruction :initarg :instruction :reader illegal-qubits-used-error-instruction))
  (:documentation "Error representing the use of an illegal qubit.")
  (:report (lambda (c s)
             (format s "Instruction '~/cl-quil:instruction-fmt/' uses the illegal qubits ~{~A~^, ~}."
                     (illegal-qubits-used-error-instruction c)
                     (illegal-qubits-used-error-illegal-qubits c)))))

(define-condition illegal-qubits-used-in-preserved-block-error (illegal-qubits-used-error)
  ()
  (:documentation "Error representing the use of an illegal qubit in a preserved block.")
  (:report (lambda (c s)
             (format s "Within a preserved block, instruction '~/cl-quil:instruction-fmt/' uses the illegal qubits ~{~A~^, ~}."
                     (illegal-qubits-used-error-instruction c)
                     (illegal-qubits-used-error-illegal-qubits c)))))

(defun check-instructions-skip-dead-qubits (instrs dead-qubits &key (condition-class 'illegal-qubits-used-error))
  (map nil
       (lambda (instr)
         (a:when-let ((qubits-used (qubits-used instr)))
           (let ((instr-dead-qubits (intersection qubits-used dead-qubits)))
             (unless (endp instr-dead-qubits)
               (error (make-condition condition-class :illegal-qubits instr-dead-qubits :instruction instr))))))
       instrs))

(defun check-program-skips-dead-qubits (parsed-program chip-specification)
  "Errors if the PARSED-PROGRAM uses any qubits that are dead in the CHIP-SPECIFICATION."
  (check-instructions-skip-dead-qubits (parsed-program-executable-code parsed-program)
                                       (chip-spec-dead-qubits chip-specification)))

(defun check-preserved-blocks-skip-dead-qubits (cfg chip-specification)
  "Errors if any preserved block in the CFG uses any qubits that are dead in the CHIP-SPECIFICATION."
  (let ((dead-qubits (chip-spec-dead-qubits chip-specification)))
    (dolist (blk (cfg-blocks cfg))
      (when (typep blk 'preserved-block)
        (check-instructions-skip-dead-qubits (basic-block-code blk) dead-qubits
                                             :condition-class 'illegal-qubits-used-in-preserved-block-error)))))
