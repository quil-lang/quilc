;;; constraint-addresser.lisp
;;;
;;; Author: Erik Davis
;;;
;;; The basic API of a "constraint-based addresser" is here. The main
;;; entry point is DO-CONSTRAINT-BASED-ADDRESSING, which dispatches
;;; according to the specified scheme.
;;;
;;; Generally speaking, a constraint-based addressing scheme specifies
;;; how to translate an addressing problem (input program, chip spec,
;;; options) to a constraint program along with metadata about the
;;; encoding. A SMT solver can run this program, and if the
;;; constraints are satisfiable then a `model` (an assignment of
;;; values to constraint variables) may be recovered. From this model
;;; and the encoding, the details of addressing can be unpacked.
;;;
;;; In general, to add a new scheme, one needs to define appropriate
;;; methods on the following generics
;;; - ENCODE-CONSTRAINT-PROGRAM
;;; - ATTEMPT-TO-RECOVER-MODEL
;;; - UNPACK-MODEL
;;;
;;; Note that in general, DO-CONSTRAINT-BASED-ADDRESSING does not
;;; nativize instructions. In other words, it is purely concerned with
;;; questions of resource mapping/layout. In general, the joint
;;; problem of addressing with nativization is sufficiently
;;; challenging that we shouldn't rely on SMT or constraint formalisms
;;; to accomplish it all at once.

(in-package #:cl-quil.smt)

;;; This file contains the basic API for constraint-based addressing.

(defvar *default-constraint-scheme* ':tb-olsq
  "The default scheme to use in the constraint based addresser.")


(define-condition addressing-failed (simple-error)
  ()
  (:documentation "A condition indicating a failure of addressing, which may or may not be recoverable."))

(defun addressing-failed (format-control &rest format-args)
  (error 'addressing-failed :format-control format-control
                            :format-arguments format-args))

(defclass constraint-program ()
  ((declarations :initarg :declarations
                 :initform nil
                 :accessor constraint-program-declarations
		 :documentation "A list of SMT-LIB forms, indicating variable declarations.")
   (assertions :initarg :assertions
               :initform nil
               :accessor constraint-program-assertions
	       :documentation "A list of SMT-LIB forms, indicating constraints or assertions.")
   (optimize :initarg :optimize
	     :initform nil
	     :accessor constraint-program-optimize
	     :documentation "An optional list of optimization declarations."))
  (:documentation "A representation of a simple SMT program, with declarations and assertions."))

(defun write-constraint-program (cp &optional (stream *standard-output*))
  "Write the constraint program CP to the STREAM "
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'null (lambda (s o)
                                 (declare (ignore o))
                                 (format s "()")))
    (let* ((full-program (append (constraint-program-declarations cp)
                                 (constraint-program-assertions cp)
				 (constraint-program-optimize cp)
                                 `((|check-sat|) (|get-model|)))))
      (smt-debug-line 'write-constraint-program "~%~{    ~A~%~}" full-program)
      (write-smt-forms full-program stream))))

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
  (cl-quil::chip-spec-n-qubits (encoding-chip-spec encoding)))

(defun encoding-num-links (encoding)
  (cl-quil::chip-spec-n-links (encoding-chip-spec encoding)))

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
					 (timeout-ms nil)
                                       &allow-other-keys)
  "Address INSTRUCTONS to be compatible with CHIP-SPEC, using the encoding indicated by SCHEME.

Encoding-specific keyword arguments are handed off to ENCODE-CONSTRAINT-PROGRAM.

Returns three values: (ADDRESSED-INSTRUCTIONS, INITIAL-REWIRING, FINAL-REWIRING)."
  (multiple-value-bind (cp encoding)
      (apply #'encode-constraint-program
             scheme instrs chip-spec
             :initial-l2p initial-rewiring
             :final-l2p final-rewiring
             args)
    (let ((smt (initiate-smt-solver solver-command)))
      ;; if we wanted to set solver options, now would be the time to do it...
      (when timeout-ms
	(write-smt-forms `((|set-option| :|timeout| ,(ceiling timeout-ms))) smt))
      (write-constraint-program cp smt)
      (let ((model (attempt-to-recover-model encoding smt)))
        (unless model
          (addressing-failed "Unable to recover model; likely your problem is overconstrained."))
        (unpack-model encoding model)))))

;; Note: The above API is not "turnkey" in the sense that DO-CONSTRAINT-BASED-ADDRESSING
;; represents a single round of constraint generation & solving, whereas in practice we may
;; need to do several rounds. For example, in the TB-OLSQ encoding, we need to make a
;; choice for :NUM-BLOCKS. This might involve picking a small initial guess,
;; then exponentially growing this until we have a large enough value that addressing works,
;; followed by a costlier run with :MINIMIZE-SWAPS set to T. Proper tuning of such a search
;; does take a bit of work.
