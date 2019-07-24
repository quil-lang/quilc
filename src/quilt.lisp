(in-package #:cl-quil)

;;; Atoms

;;; TODO think through this
;;; does a frame have a fixed arity?
;;; how do pairs of frames interact
;;; should qubits be added to this?
(defstruct (frame (:constructor frame (name)))
  "A rotating frame, relative to which control or readout waveforms may be defined."
  (name nil :type string))


(defstruct  (waveform-ref (:constructor %waveform-ref (name args)))
  "A reference to a (possibly parametric) waveform."
  (name nil :read-only t :type string)
  (args nil :read-only t :type list))

(defun waveform-ref (name &rest args)
  (%waveform-ref name args))

;;; Frame Mutations


;;; frame is <list of qubits> followed by <frame name>
;;; Real is either a literal or an expression.
;;; TODO check on whether this makes sense

(defclass frame-mutation (instruction)
  ((qubits :initarg :qubits
           :accessor target-qubits)
   (frame :initarg :frame
          :accessor target-frame)
   (value :initarg :value
          :accessor mutation-value))
  (:documentation "An instruction representing the mutation of a frame attribute.")
  (:metaclass abstract-class))

(defmethod arguments ((instr frame-mutation))
  (with-slots (qubits frame value)
      instr
    (coerce (append qubits (list frame value)) 'vector)))

(defmacro define-frame-mutation (name mnemonic &body body)
  (check-type mnemonic string)
  (check-type body list)
  (assert (= 1 (length body)))
  (expand-frame-mutation-definition name mnemonic (first body)))

(defun expand-frame-mutation-definition (name mnemonic docstring)
  (check-type name symbol)
  (check-type mnemonic string)
  `(progn
     (defclass ,name (frame-mutation)
       ()
       (:documentation ,docstring))

     (defmethod mnemonic ((inst ,name)) (values ',mnemonic ',name))))

(define-frame-mutation set-frequency "SET-FREQUENCY"
  "An instruction setting the frequency of a frame.")

(define-frame-mutation set-phase "SET-PHASE"
  "An instruction setting the phase of a frame.")

(define-frame-mutation shift-phase "SHIFT-PHASE"
  "An instruction performing an additive shift of the phase of a frame.")

(define-frame-mutation set-scale "SET-SCALE"
  "An instruction setting the scale of a frame.")

;; ;;; TODO Swap frame phases (not sure why)




;;; TODO arguments and mnemonic
;;; what is the motivation behind this? what is the "protocol" which is specified? how do we handle pulses?
;;; - it seems like they are used for printing


;;; Pulse

(defclass pulse (instruction)
  ((gates :initarg :gates
          :accessor pulse-gates)
   (frame :initarg :frame
          :accessor pulse-frame)
   (waveform :initarg :waveform
             :accessor pulse-waveform))
  (:documentation "A pulse instruction."))

;;; Capture
;;; TODO: do we need RAW-CAPTURE? couldn't we just have a raw(duration) waveform?
;;; the only tricky business here is that the duration needs to be a rational number
(defclass capture (instruction)
  ((qubit :initarg :qubit
          :accessor capture-qubit)
   (frame :initarg :frame
          :accessor capture-frame)
   (waveform :initarg :waveform
             :accessor capture-waveform)
   (memory-ref :initarg :memory-ref
               :accessor capture-memory-ref))
  (:documentation "An instruction expressing the readout and integration of raw
  IQ values, to be stored in a region of classical memory."))

;;; Timing control
;;; TODO Why delay only one qubit?
(defclass delay (instruction)
  ((qubit :initarg :qubit
          :accessor delay-qubit)
   (duration :initarg :duration
             :accessor delay-duration))
  (:documentation "A delay of a specific time on a specific qubit."))

(defclass fence (instruction)
  ((qubits :initarg :qubits
           :accessor fence-qubits))
  (:documentation "A synchronization barrier on a set of qubits, demarcating
  preceding and succeeding instructions."))


;;; Print Methods
;;; TODO move these to the "total" defgeneric

(defmethod print-instruction-generic ((thing frame) (stream stream))
  (format stream "~S" (frame-name thing)))

(defmethod print-instruction-generic ((thing waveform-ref) (stream stream))
  (format stream "~A~@[(~{~A~^, ~})~]"
          (waveform-ref-name thing)
          (waveform-ref-args thing)))

(defmethod print-instruction-generic ((instr pulse) (stream stream))
  (format stream "PULSE ~{~A ~} ~A ~A"
          (pulse-gates instr)
          (print-instruction-generic (pulse-frame instr) nil)
          (print-instruction-generic (pulse-waveform instr) nil)))

(defmethod print-instruction-generic ((instr delay) (stream stream))
  (format stream "DELAY ~A ~A" (delay-qubit instr) (delay-duration instr)))

(defmethod print-instruction-generic ((instr fence) (stream stream))
  (format stream "FENCE ~{~A ~}" (fence-qubits instr)))

(defmethod print-instruction-generic ((instr capture) (stream stream))
  (format stream "CAPTURE ~A ~A ~A ~A"
          (print-instruction-generic (capture-qubit instr) nil)
          (print-instruction-generic (capture-frame instr) nil)
          (print-instruction-generic (capture-waveform instr) nil)
          (print-instruction-generic (capture-memory-ref instr) nil)))

(defmethod print-instruction-generic ((instr frame-mutation) (stream stream))
  (format stream "~A~{ ~A~}"
          (mnemonic instr)
          (map 'list #'print-instruction-to-string (arguments instr))))

;;; General steps:
;;;  - add code to the parser for this (using the match-line macro)
;;;  - add DEFWAVEFORM and DEFCAL

