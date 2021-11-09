(in-package #:cl-quil)

(defvar *default-constraint-scheme* ':tb-olsq
  "The default scheme to use in the constraint based addresser.")

(defvar *smt-debug-stream* nil
  "When non-NIL, this indicates the stream at which debug lines get printed.")

(defvar *constraint-solver-command* '("z3" "-in" "-smt2")
  "The default command for invoking the constraint solver.")

(defun smt-debug-line (ctxt fmt-msg &rest fmt-args)
  (when *smt-debug-stream*
    (apply #'format *smt-debug-stream* (format nil "~A: ~A~%" ctxt fmt-msg) fmt-args)))

(define-condition addressing-failed (simple-error)
  ()
  (:documentation "A condition indicating a failure of addressing, which may or may not be recoverable."))

(defun addressing-failed (format-control &rest format-args)
  (error 'addressing-failed :format-control format-control
                            :format-arguments format-args))

(defun initiate-smt-solver (command)
  "Get a CL-SMT-LIB:PROCESS-TWO-WAY-STREAM by invoking COMMAND."
  (smt-debug-line 'initiate-smt-solver "Starting solver:~{ ~A~}" command)
  (let ((smt (apply #'cl-smt-lib:make-smt command)))
    (unless (uiop:process-alive-p (cl-smt-lib::process smt))
      (addressing-failed "Tried~{ ~A~}, but solver process is dead on arrival." command))
    smt))

(defclass constraint-program ()
  ((declarations :initarg :declarations
                 :initform nil
                 :accessor constraint-program-declarations)
   (assertions :initarg :assertions
               :initform nil
               :accessor constraint-program-assertions))
  (:documentation "A representation of a simple SMT program, with declarations and assertions."))

(defun write-constraint-program (cp &optional (stream *standard-output*))
  "Write the constraint program CP to the STREAM "
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'null (lambda (s o)
                                 (declare (ignore o))
                                 (format s "()")))
    (let* ((full-program (append (constraint-program-declarations cp)
                                 (constraint-program-assertions cp)
                                 `((|check-sat|) (|get-model|)))))
      (smt-debug-line 'write-constraint-program "~%~{    ~A~%~}" full-program)
      (cl-smt-lib:write-to-smt stream full-program))))

;;; Encodings

(defclass encoding ()
  ((gates :initarg :gates
          :reader encoding-gates
          :documentation "The gates associated with this encoding.")
   (chip :initarg :chip
         :reader encoding-chip-spec
         :documentation "The chip specification associated with this encoding."))
  (:metaclass abstract-class:abstract-class)
  (:documentation "A base class for encodings which represent the problem of addressing GATES to CHIP."))

(defun encoding-num-qubits (encoding)
  (chip-spec-n-qubits (encoding-chip-spec encoding)))

(defun encoding-num-links (encoding)
  (chip-spec-n-links (encoding-chip-spec encoding)))

(defun encoding-num-gates (encoding)
  (length (encoding-gates encoding)))

(defgeneric encode-constraint-program (scheme instrs chip-spec &rest args &key initial-l2p final-l2p &allow-other-keys)
  (:documentation "Represent the problem of addressing INSTRS to CHIP-SPEC according to the provided SCHEME.

Keyword Arguments
- INITIAL-L2P is an optional rewiring vector, indicating the initial logical to physical mapping.
- FINAL-L2P is an optional rewiring vector, indicating the final logical to physical mapping.

Returns two values: a CONSTRAINT-PROGRAM and ENCODING.")
  (:method (scheme instrs chip-spec &key initial-l2p final-l2p)
    (declare (ignore instrs chip-spec initial-l2p final-l2p))
    (addressing-failed "Unsupported addressing scheme ~A" scheme)))

(defgeneric attempt-to-recover-model (encoding smt)
  (:documentation "Attempt a model from the given ENCODING and the smt stream SMT.

Returns a hash table mapping variable names to values, or NIL on failure."))

(defgeneric unpack-model (encoding model)
  (:documentation "Unpack the given MODEL, defined with respect to ENCODING.

Returns a triple (INSTRS, INITIAL-L2P, FINAL-L2P)."))

(defun do-constraint-based-addressing (instrs chip-spec &rest args
                                       &key
                                         initial-rewiring
                                         final-rewiring
                                         (scheme *default-constraint-scheme*)
                                         (solver-command *constraint-solver-command*)
                                       &allow-other-keys)
  "Address INSTRUCTONS to be compatible with CHIP-SPEC, using the encoding indicated by SCHEME.

Returns three values: (ADDRESSED-INSTRUCTIONS, INITIAL-REWIRING, FINAL-REWIRING)."
  ;; check whether instructions are addressable by these means
  (multiple-value-bind (cp encoding)
      (apply #'encode-constraint-program
             scheme instrs chip-spec
             :initial-l2p initial-rewiring
             :final-l2p final-rewiring
             args)
    (let ((smt (initiate-smt-solver solver-command)))
      ;; TODO: set options
      (write-constraint-program cp smt)
      (let ((model (attempt-to-recover-model encoding smt)))
        (unless model
          (addressing-failed "Unable to recover model; likely your problem is overconstrained."))
        (unpack-model encoding model)))))
