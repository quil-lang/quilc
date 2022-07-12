;;;; src/quilt/ast.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:cl-quil/quilt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Objects ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (frame (:constructor frame (qubits name)))
  "A reference to a Quilt rotating frame, relative to which control or readout waveforms may be defined."
  (name nil :read-only t :type string)
  (qubits nil :read-only t :type list)
  ;; Will later be resolved
  (name-resolution nil :type (or null frame-definition)))

(defun frame= (a b)
  (check-type a frame)
  (check-type b frame)
  (and (string= (frame-name a) (frame-name b))
       (list= (frame-qubits a) (frame-qubits b) :test #'qubit=)))

(defun frame-hash (f)
  "Return a hash for frame F. This is to frame= as sxhash is to equal, i.e., (frame= a b) implies (= (frame-hash a) (frame-hash b))."
  (check-type f frame)
  (let* ((name (frame-name f))
         (qubits (frame-qubits f))
         (hash (sxhash name)))
    (dolist (qubit qubits hash)
      (setq hash
            (#+sbcl sb-int:mix #-sbcl logxor
             hash
             (qubit-index qubit))))))

(defmethod print-instruction-generic ((thing frame) (stream stream))
  (format stream "~{~/cl-quil:instruction-fmt/ ~}\"~A\""
          (frame-qubits thing)
          (frame-name thing)))

(defstruct (waveform-ref (:constructor waveform-ref (name parameter-alist)))
  "An reference to a (possibly parametric) Quilt waveform."
  (name nil :read-only t :type string)
  ;; An alist of parameters and their values.
  (parameter-alist nil :read-only t :type list)
  ;; Will later be resolved
  (name-resolution nil :type (or null
                                 standard-waveform
                                 waveform-definition)))

;;; This exists mainly to keep PRINT-INSTRUCTION-GENERIC simple
(defun waveform-parameter-association-fmt (stream alist-entry &optional colon-modifier at-modifier)
  "Format function for parameter-value associations, compatible with format strings using ~/.../ directive."
  (declare (ignore colon-modifier at-modifier))
  (let ((param (car alist-entry))
        (value (cdr alist-entry)))
    (format stream "~A: ~/cl-quil:instruction-fmt/" (param-name param) value)))

(defmethod print-instruction-generic ((thing waveform-ref) (stream stream))
  (format stream "~A~@[(~{~/quilt::waveform-parameter-association-fmt/~^, ~})~]"
          (waveform-ref-name thing)
          (waveform-ref-parameter-alist thing)))

(defclass standard-waveform ()
  ((duration :initarg :duration
             :reader waveform-duration
             :type (or float constant param)
             :documentation "Duration of the waveform, in seconds."))
  (:documentation "Base class for built-in waveforms.")
  (:metaclass abstract-class))

;;; NOTE: Standard waveform definitions may be found in waveform.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Instructions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Frame Mutations

(defclass simple-frame-mutation (instruction)
  ((frame :initarg :frame
          :type frame
          :accessor frame-mutation-target-frame)
   (value :initarg :value
          :type (or constant param)
          :accessor frame-mutation-value))
  (:documentation "An instruction representing the mutation of a frame attribute.")
  (:metaclass abstract-class))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-simple-frame-mutation-definition (name mnemonic docstring)
    (check-type name symbol)
    (check-type mnemonic string)
    `(progn
       (defclass ,name (simple-frame-mutation)
         ()
         (:documentation ,docstring))

       (defmethod mnemonic ((inst ,name)) (values ,mnemonic ',name)))))

(defmethod arguments ((instr simple-frame-mutation))
  (vector (frame-mutation-target-frame instr)
          (frame-mutation-value instr)))

(defmacro define-simple-frame-mutation (name mnemonic &body body)
  (assert (= 1 (length body)))
  (expand-simple-frame-mutation-definition name mnemonic (first body)))

(define-simple-frame-mutation set-frequency "SET-FREQUENCY"
  "An instruction setting the frequency of a frame.")

(define-simple-frame-mutation shift-frequency "SHIFT-FREQUENCY"
  "An instruction shifting the frequency of a frame.")

(define-simple-frame-mutation set-phase "SET-PHASE"
  "An instruction setting the phase of a frame.")

(define-simple-frame-mutation shift-phase "SHIFT-PHASE"
  "An instruction performing an additive shift of the phase of a frame.")

(define-simple-frame-mutation set-scale "SET-SCALE"
  "An instruction setting the scale of a frame.")

(defmethod print-instruction-generic ((instr simple-frame-mutation) (stream stream))
  (format stream "~A" (mnemonic instr))
  (loop :for arg :across (arguments instr)
        :do (format stream " ~/cl-quil:instruction-fmt/" arg)))

(defclass swap-phase (instruction)
  ((left-frame :initarg :left-frame
               :type frame
               :accessor swap-phase-left-frame)
   (right-frame :initarg :right-frame
                :type frame
                :accessor swap-phase-right-frame))
  (:documentation "An instruction representing a phase swap between two frames."))

(defmethod print-instruction-generic ((instr swap-phase) (stream stream))
  (format stream "SWAP-PHASE ~/cl-quil:instruction-fmt/ ~/cl-quil:instruction-fmt/"
          (swap-phase-left-frame instr)
          (swap-phase-right-frame instr)))

;;; Pulse Operations

(defclass pulse (instruction)
  ((frame :initarg :frame
          :accessor pulse-frame
          :type frame
          :documentation "The frame on which the pulse will be applied.")
   (waveform :initarg :waveform
             :accessor pulse-waveform
             :type waveform-ref
             :documentation "The waveform to be applied.")
   (nonblocking :initarg :nonblocking
                :initform nil
                :accessor nonblocking-p
                :type boolean
                :documentation "A flag indicating whether the pulse blocks frames sharing a qubit with the PULSE-FRAME."))
  (:documentation "A pulse instruction."))

(defmethod print-instruction-generic ((instr pulse) (stream stream))
  (format stream "~:[~;NONBLOCKING ~]PULSE ~/cl-quil:instruction-fmt/ ~/cl-quil:instruction-fmt/"
          (nonblocking-p instr)
          (pulse-frame instr)
          (pulse-waveform instr)))

(defclass capture (instruction)
  ((frame :initarg :frame
          :accessor capture-frame
          :type frame
          :documentation "The frame from which a value is to be captured.")
   (waveform :initarg :waveform
             :accessor capture-waveform
             :type waveform-ref
             :documentation "A waveform, used as an integration kernel for the capture operation.")
   (memory-ref :initarg :memory-ref
               :accessor capture-memory-ref
               :type (or memory-ref formal)
               :documentation "The location in memory to store the captured IQ value.")
   (nonblocking :initarg :nonblocking
                :initform nil
                :accessor nonblocking-p
                :type boolean
                :documentation "A flag indicating whether the capture blocks frames sharing a qubit with the CAPTURE-FRAME."))
  (:documentation "An instruction expressing the readout and integration of raw IQ values, to be stored in a region of classical memory."))

(defmethod print-instruction-generic ((instr capture) (stream stream))
  (format stream "~:[~;NONBLOCKING ~]CAPTURE ~/cl-quil:instruction-fmt/ ~/cl-quil:instruction-fmt/ ~/cl-quil:instruction-fmt/"
          (nonblocking-p instr)
          (capture-frame instr)
          (capture-waveform instr)
          (capture-memory-ref instr)))

(defclass raw-capture (instruction)
  ((frame :initarg :frame
          :accessor raw-capture-frame
          :type frame
          :documentation "The frame from which a value is to be captured.")
   (duration :initarg :duration
             :accessor raw-capture-duration
             :type (or constant param)
             :documentation "The duration for which IQ values will be recorded.")
   (memory-ref :initarg :memory-ref
               :accessor raw-capture-memory-ref
               :type (or memory-ref formal)
               :documentation "The location in memory to store captured IQ values.")
   (nonblocking :initarg :nonblocking
                :initform nil
                :accessor nonblocking-p
                :type boolean
                :documentation "A flag indicating whether the raw capture blocks frames sharing a qubit with the RAW-CAPTURE-FRAME."))
  (:documentation "An instruction expressing the readout of raw IQ values, to be stored in a region of classical memory."))

(defmethod print-instruction-generic ((instr raw-capture) (stream stream))
  (format stream "~:[~;NONBLOCKING ~]RAW-CAPTURE ~/cl-quil:instruction-fmt/ ~/cl-quil:instruction-fmt/ ~/cl-quil:instruction-fmt/"
          (nonblocking-p instr)
          (raw-capture-frame instr)
          (raw-capture-duration instr)
          (raw-capture-memory-ref instr)))

;;; Timing Control and Synchronization

(defclass delay (instruction)
  ((duration :initarg :duration
             :accessor delay-duration
             :type (or constant param)
             :documentation "The duration (in seconds) of the DELAY instruction."))
  (:metaclass abstract-class)
  (:documentation "A delay of a specific time on a specific qubit."))

(defclass delay-on-frames (delay)
  ((delayed-frames :initarg :frames
                   :accessor delay-frames
                   :type list
                   :documentation "A list of frames which should be delayed.")))

(defmethod print-instruction-generic ((instr delay-on-frames) (stream stream))
  (let* ((frames (delay-frames instr))
         (qubits (frame-qubits (first frames))))
    ;; This is just a sanity check -- all frames have the same qubits.
    (assert (every (lambda (frame)
                     (equalp qubits (frame-qubits frame)))
                   frames))
    (format stream "DELAY~{ ~/cl-quil:instruction-fmt/~}~{ ~S~} ~/cl-quil:instruction-fmt/"
            qubits
            (mapcar #'frame-name (delay-frames instr))
            (delay-duration instr))))

(defclass delay-on-qubits (delay)
  ((qubits :initarg :qubits
           :accessor delay-qubits
           :type list
           :documentation "A list of qubits. Any frame on these qubits will be delayed.")))

(defmethod print-instruction-generic ((instr delay-on-qubits) (stream stream))
  (format stream "DELAY~{ ~/cl-quil:instruction-fmt/~} ~/cl-quil:instruction-fmt/"
          (delay-qubits instr)
          (delay-duration instr)))

(defclass fence (instruction)
  ((qubits :initarg :qubits
           :accessor fence-qubits
           :type list
           :documentation "A list of qubits. Any frame intersecting these qubits will be synchronized to a common time."))
  (:documentation "A synchronization barrier on a set of qubits, demarcating preceding and succeeding instructions."))

(defmethod print-instruction-generic ((instr fence) (stream stream))
  (format stream "FENCE~{ ~/cl-quil:instruction-fmt/~}"
          (fence-qubits instr)))

(defclass fence-all (instruction)
  ()
  (:documentation "A synchronization barrier on all qubits.")
  #+#:appleby-sufficiently-classy
  (:metaclass singleton-class))

(defmethod print-instruction-generic ((instr fence-all) (stream stream))
  (format stream "FENCE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype frame-direction ()
  `(member :TX :RX))

(defclass frame-definition ()
  ((frame :initarg :frame
          :reader frame-definition-frame
          :type frame
          :documentation "The frame being defined.")
   (sample-rate :initarg :sample-rate
                :initform nil
                :reader frame-definition-sample-rate
                :type (or null constant)
                :documentation "The sample rate associated with the frame. If specified, this should be a positive constant.")
   (initial-frequency :initarg :initial-frequency
                      :initform nil
                      :reader frame-definition-initial-frequency
                      :type (or null constant)
                      :documentation "The initial frequency of the frame. If specified, this should be a positive constant.")
   (direction :initarg :direction
              :initform nil
              :reader frame-definition-direction
              :type (or null frame-direction)
              :documentation "The associated frame direction, indicating usage for transmission or reception.")
   (hardware-object :initarg :hardware-object
                    :initform nil
                    :reader frame-definition-hardware-object
                    :type (or null string)
                    :documentation "The name of the hardware object associated with this frame.")
   (context :initarg :context
            :type lexical-context
            :accessor lexical-context
            :documentation "The lexical context of the frame definition, used for error messages in subsequent analysis.")))

(defmethod print-instruction-generic ((defn frame-definition) (stream stream))
  (with-slots (frame sample-rate initial-frequency direction hardware-object)
      defn
    (format stream "DEFFRAME ~/cl-quil:instruction-fmt/" frame)
    (when (or sample-rate initial-frequency direction hardware-object)
      (format stream ":")
      (when sample-rate
        (format stream "~%    SAMPLE-RATE: ~/cl-quil:instruction-fmt/" sample-rate))
      (when initial-frequency
        (format stream "~%    INITIAL-FREQUENCY: ~/cl-quil:instruction-fmt/" initial-frequency))
      (when direction
        (format stream "~%    DIRECTION: ~S"
                (ecase direction
                  (:TX "tx")
                  (:RX "rx"))))
      (when hardware-object
        (format stream "~%    HARDWARE-OBJECT: ~S" hardware-object))
      (terpri stream))))

(defclass waveform-definition ()
  ((name :initarg :name
         :reader waveform-definition-name
         :type string
         :documentation "The name of the waveform being defined.")
   (entries :initarg :entries
            :reader waveform-definition-entries
            :type list
            :documentation "The raw IQ values of the waveform being defined.")
   (sample-rate :initarg :sample-rate
                :reader waveform-definition-sample-rate
                :type constant
                :documentation "The sample rate for which the waveform is applicable.")
   (context :initarg :context
            :type lexical-context
            :accessor lexical-context
            :documentation "The lexical context of the waveform definition, used for error messages in subsequent analysis."))
  (:metaclass abstract-class)
  (:documentation "A representation of a user-specified Quilt waveform definition."))


(defclass static-waveform-definition (waveform-definition)
  ()
  (:documentation "A waveform definition that has no parameters."))

(defclass parameterized-waveform-definition (waveform-definition)
  ((parameters :initarg :parameters
               :type list
               :reader waveform-definition-parameters
               :documentation "A list of symbol parameter names."))
  (:documentation "A waveform definition that has named parameters."))

(defun make-waveform-definition (name parameters entries sample-rate &key context)
  (check-type name string)
  (check-type parameters symbol-list)
  (if (not (endp parameters))
      (make-instance 'parameterized-waveform-definition
        :name name
        :parameters parameters
        :entries entries
        :sample-rate sample-rate
        :context context)
      (make-instance 'static-waveform-definition
        :name name
        :entries entries
        :sample-rate sample-rate
        :context context)))

(defmethod print-instruction-generic ((defn waveform-definition) (stream stream))
  (format stream "DEFWAVEFORM ~A~@[(~{%~A~^, ~})~] ~/cl-quil:instruction-fmt/:"
          (waveform-definition-name defn)
          (if (typep defn 'static-waveform-definition)
              '()
              (waveform-definition-parameters defn))
          (waveform-definition-sample-rate defn))
  (format stream "~%    ~{~A~^, ~}"
          (mapcar (lambda (z)
                    (with-output-to-string (s)
                      (etypecase z
                        (number
                         (format-complex z s))
                        ((or list symbol)
                         (print-instruction (make-delayed-expression nil nil z) s)))))
                  (waveform-definition-entries defn)))
  (terpri stream))

(defclass calibration-definition ()
  ((body :initarg :body
         :reader calibration-definition-body
         :type list
         :documentation "A list of Quilt instructions in the body of the calibration definition.")
   (context :initarg :context
            :type lexical-context
            :accessor lexical-context
            :documentation "The lexical context of the calibration definition, used for error messages in subsequent analysis."))
  (:metaclass abstract-class)
  (:documentation "A representation of a user-specified calibration."))

(defclass gate-calibration-definition (calibration-definition)
  ((operator :initarg :operator
             :reader calibration-definition-operator
             :type operator-description
             :documentation "The operator for which the defined calibration is applicable.")
   (parameters :initarg :parameters
               :reader calibration-definition-parameters
               :type list
               :documentation "The parameters of the gate calibration.")
   (arguments :initarg :arguments
              :reader calibration-definition-arguments
              :type list
              :documentation "The arguments of the gate calibration."))
  (:documentation "A representation of a user-specified gate calibration."))

(defmethod print-instruction-generic ((defn gate-calibration-definition) (stream stream))
  (format stream "DEFCAL ")
  (print-operator-description (calibration-definition-operator defn) stream)
  (unless (endp (calibration-definition-parameters defn))
    (format stream "(~{~/cl-quil:instruction-fmt/~^, ~})"
            (calibration-definition-parameters defn)))
  (unless (endp (calibration-definition-arguments defn))
    (format stream "~{ ~/cl-quil:instruction-fmt/~}"
            (calibration-definition-arguments defn)))
  (format stream ":~%")
  (print-instruction-sequence (calibration-definition-body defn)
                              :stream stream
                              :prefix "    "))

(defclass measurement-calibration-definition (calibration-definition)
  ((qubit :initarg :qubit
          :reader measurement-calibration-qubit
          :type (or qubit formal)
          :documentation "The qubit being measured."))
  (:metaclass abstract-class)
  (:documentation "Superclass to measurement calibration definitions."))

(defclass measure-calibration-definition (measurement-calibration-definition)
  ((address :initarg :address
            :reader measure-calibration-address
            :type formal
            :documentation "The classical memory destination for measured values."))
  (:documentation "A representation of a user-specified MEASURE calibration."))

(defclass measure-discard-calibration-definition (measurement-calibration-definition)
  ()
  (:documentation "A representation of a user-specifieed MEASURE (discard) calibration."))

(defmethod print-instruction-generic ((defn measurement-calibration-definition) (stream stream))
  (format stream "DEFCAL MEASURE ~/cl-quil:instruction-fmt/~@[ ~/cl-quil:instruction-fmt/~]:~%"
          (measurement-calibration-qubit defn)
          (if (typep defn 'measure-calibration-definition)
              (measure-calibration-address defn)
              nil))
  (print-instruction-sequence (calibration-definition-body defn)
                              :stream stream
                              :prefix "    "))

;;;;;;;;;;;;;;;;;;;;;; Program Representations ;;;;;;;;;;;;;;;;;;;;;;;

(defclass parsed-quilt-program (parsed-program)
  ((waveform-definitions :initarg :waveform-definitions
                         :accessor parsed-program-waveform-definitions
                         :type list
                         :documentation "The waveform definitions introduced by DEFWAVEFORM.")
   (calibration-definitions :initarg :calibration-definitions
                            :accessor parsed-program-calibration-definitions
                            :type list
                            :documentation "The calibration definitions introduced by DEFCAL.")
   (frame-definitions :initarg :frame-definitions
                      :accessor parsed-program-frame-definitions
                      :type list
                      :documentation "The frame definitions introduced by DEFFRAME."))
  (:default-initargs
   :waveform-definitions '()
   :calibration-definitions '()
   :frame-definitions '())
  (:documentation "A representation of a parsed Quilt program, in which instructions have been duly sorted into their various categories (e.g. definitions vs executable code), and internal references have been resolved."))

(defmethod print-parsed-program-generic ((pp parsed-quilt-program) (stream stream))
  (flet ((print-definitions (defns)
           (dolist (defn defns)
             (print-instruction defn stream)
             (terpri stream))))

    (print-definitions (parsed-program-memory-definitions pp))
    (when (endp (parsed-program-memory-definitions pp))
      (terpri stream))
    (print-definitions (parsed-program-frame-definitions pp))
    (print-definitions (parsed-program-waveform-definitions pp))
    (print-definitions (parsed-program-calibration-definitions pp))
    (print-definitions (parsed-program-gate-definitions pp))
    (print-definitions (parsed-program-circuit-definitions pp))

    (print-instruction-sequence (parsed-program-executable-code pp) :stream stream))) 
