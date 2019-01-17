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
;;;;         drawn from the set { "RZ", "X/2" } and in the case of 2Q drawn from
;;;;         the set { "CZ", "ISWAP", "CPHASE", "ISWAP" }. these indicate which
;;;;         operations are legal on this component of the QPU. in the case of a
;;;;         list, the operations are sorted into "preference order": the
;;;;         compiler will make an effort to prefer instructions earlier in the
;;;;         sequence to those later in the sequence. for 1Q the default is
;;;;         [ "RZ", "X/2" ], and for 2Q the default is "CZ".
;;;;   dead: optional boolean. when set to T, the compiler will treat all
;;;;         instructions on this component as illegal. }
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

(defun integer-list-p (list)
  (every #'integerp list))

(defun expand-key-to-integer-list (key)
  "Expands a string of the form \"n1-...-nm\" to the list of integers (list n1 ... nm)."
  (etypecase key
    (string
     (mapcar #'parse-integer (split-sequence:split-sequence #\- key)))
    ((and list (satisfies integer-list-p))
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
            (loop :for k :being :the :hash-keys :of 1q-layer
                  :maximize (parse-integer k :radix 10 :junk-allowed nil))))
      ;; Now, go through the hash table, filling in anything
      ;; missing. We can go :BELOW and not :TO with DOTIMES since we
      ;; know the last index exists.
      (dotimes (q max-qubit-index isa-hash)
        (let ((qkey (prin1-to-string q)))
          (multiple-value-bind (qubit-hash exists?)
              (gethash qkey 1q-layer)
            (declare (ignore qubit-hash))
            (unless exists?
              (setf (gethash qkey 1q-layer) (dead-qubit-hash-table)))))))))

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
      (destructuring-bind (i) (expand-key-to-integer-list key)
        (assert (< i qubit-count)
                nil
                "ISA contains a 1Q descriptor of index ~a, but there are only ~a qubit(s) altogether."
                i (1- qubit-count))
        ;; form a qubit
        (let* ((qubit-type (gethash "type" qubit-hash))
               (qubit (cond
                        ((or (null qubit-type)
                             (string= "Xhalves" qubit-type))
                         (build-qubit))
                        (t
                         (error "On qubit ~a, unknown qubit type field in QPU descriptor: ~a."
                                i qubit-type)))))
          ;; store the descriptor in the qubit hardware-object for later reference
          (setf (hardware-object-misc-data qubit) qubit-hash)
          ;; and store the hardware-object into the chip specification
          (setf (vnth i (vnth 0 (chip-specification-objects chip-spec)))
                qubit))))
    ;; for each logical link descriptor...
    (when (gethash "2Q" isa-hash)
      (dohash ((key link-hash) (gethash "2Q" isa-hash))
        (destructuring-bind (q0 q1) (expand-key-to-integer-list key)
          ;; check that the link is ordered
          (assert (< q0 q1)
                  nil
                  "Link descriptor found with edge label \"~{~a~^-~}\". Edge labels are required to be sorted ascending; use \"~{~a~^-~}\" instead."
                  (list q0 q1) (list q1 q0))
          ;; check that the link lies on reasonable qubits
          (assert (< q0 q1 qubit-count)
                  nil
                  "ISA contains a 2Q hardware descriptor attached to qubits ~a, but there are only ~a qubit(s) altogether."
                  (list q0 q1 )qubit-count)
          ;; check that the link isn't dead
          ;; NOTE: By skipping the dead links, we're shifting the internal link
          ;;       indices from what a user (or debugger) might expect.  Beware!
          (when (and
                 ;; the link itself is alive
                 (null (gethash "dead" link-hash))
                 ;; the link is not attached to dead qubits
                 (null (gethash "dead" (hardware-object-misc-data
                                        (chip-spec-nth-qubit chip-spec
                                                             q0))))
                 (null (gethash "dead" (hardware-object-misc-data
                                        (chip-spec-nth-qubit chip-spec
                                                             q1)))))
            ;; form a link attached to the two qubit indices
            (labels ((individual-target-parser (string)
                       (cond
                         ((null string) nil)
                         ((string= "CZ" string) ':cz)
                         ((string= "ISWAP" string) ':iswap)
                         ((string= "CPHASE" string) ':cphase)
                         ((string= "PISWAP" string) ':piswap)
                         (t (error "Unknown link type in QPU descriptor on link ~a: ~a."
                                   (list q0 q1) string))))
                     (target-parser (hash-value)
                       (mapcar #'individual-target-parser (alexandria:ensure-list hash-value))))
              (let ((link (build-link q0 q1
                                      (if (gethash "type" link-hash)
                                          (target-parser (gethash "type" link-hash))
                                          (list ':CZ))))
                    (link-index (length (vnth 1 (chip-specification-objects chip-spec)))))
                ;; notify the qubits that they're attached to this link
                (dolist (qubit-index (list q0 q1))
                  (vector-push-extend link-index
                                      (vnth 1 (hardware-object-cxns
                                               (vnth qubit-index
                                                     (vnth 0 (chip-specification-objects chip-spec)))))))
                ;; store the descriptor in the link hardware-object for later reference
                (setf (hardware-object-misc-data link) link-hash)
                ;; and store the hardware-object into the chip specification
                (vector-push-extend link (vnth 1 (chip-specification-objects chip-spec)))))))))))

(defun load-specs-layer (chip-spec specs-hash)
  "Loads the \"specs\" layer into a chip-specification object."
  (when (gethash "1Q" specs-hash)
    (loop :for i :from 0
          :for qubit :across (vnth 0 (chip-specification-objects chip-spec))
          :do ;; load the qubit specs info
              (alexandria:when-let ((spec (gethash (list i) (gethash "1Q" specs-hash))))
                (setf (gethash "specs" (hardware-object-misc-data qubit))
                      spec))))
  (when (gethash "2Q" specs-hash)
    (loop :for link :across (vnth 1 (chip-specification-objects chip-spec))
          :for spec := (gethash (sort (coerce (vnth 0 (hardware-object-cxns link)) 'list) #'<)
                                (gethash "2Q" specs-hash))
          :do (setf (gethash "specs" (hardware-object-misc-data link))
                    spec))))

(defun qpu-hash-table-to-chip-specification (hash-table)
  "Converts a QPU specification HASH-TABLE, to a chip-specification object.  Requires an \"isa\" layer."
  (check-type hash-table hash-table)
  (let ((isa-hash (or (gethash "isa" hash-table)
                      (error 'missing-isa-layer-error)))
        (chip-spec (make-chip-specification
                    :objects (make-array 2 :initial-contents (list (make-adjustable-vector)
                                                                   (make-adjustable-vector)))
                    :generic-rewriting-rules (coerce (global-rewriting-rules) 'vector))))
    ;; set up the self-referential compilers
    (install-generic-compilers chip-spec (list ':cz))
    ;; now load the layers out of the .qpu hash
    (load-isa-layer chip-spec isa-hash)
    (alexandria:when-let ((specs-hash (gethash "specs" hash-table)))
      (load-specs-layer chip-spec specs-hash))
    ;; and return the chip-specification that we've built
    chip-spec))

(defun read-chip-spec-file (file)
  "Read a QPU specification from a file."
  (qpu-hash-table-to-chip-specification
   (with-open-file (s file)
     (yason:parse s))))

(defun check-program-skips-dead-qubits (parsed-program chip-specification)
  "Throws an assert when PARSED-PROGRAM's instructions, assumed flat,
touch any qubits marked as dead in CHIP-SPECIFICATION."
  (let ((dead-qubits (loop :for qubit-index :below (length
                                                    (vnth 0 (chip-specification-objects chip-specification)))
                           :nconc (when (gethash "dead" (hardware-object-misc-data
                                                         (chip-spec-nth-qubit chip-specification qubit-index)))
                                    (list qubit-index)))))
    (loop :for instr :across (parsed-program-executable-code parsed-program)
          :do (when (typep instr 'application)
                (let ((instr-dead-qubits (intersection (mapcar #'qubit-index (application-arguments instr))
                                                       dead-qubits)))
                  (assert (endp instr-dead-qubits)
                          nil
                          "Program instruction '~A' attempts to use illegal qubits: ~{~A~^, ~}. Illegal qubits on this QPU: ~{~A~^, ~}."
                          (print-instruction instr nil) instr-dead-qubits dead-qubits))))))
