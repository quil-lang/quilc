(in-package #:cl-quil)

;;; Atoms

;;; TODO think through this
;;; does a frame have a fixed arity?
;;; how do pairs of frames interact
;;; should qubits be added to this?
(defstruct (frame (:constructor frame (name)))
  "A rotating frame, relative to which control or readout waveforms may be defined."
  (name nil :type string))

(defmethod print-instruction-generic ((thing frame) (stream stream))
  (format stream "~S" (frame-name thing)))

(defstruct  (waveform-ref (:constructor %waveform-ref (name args)))
  "A reference to a (possibly parametric) waveform."
  (name nil :read-only t :type string)
  (args nil :read-only t :type list))

(defun waveform-ref (name &rest args)
  (%waveform-ref name args))

(defmethod print-instruction-generic ((thing waveform-ref) (stream stream))
  (format stream "~A~@[(~{~A~^, ~})~]"
          (waveform-ref-name thing)
          (mapcar (lambda (arg) (print-instruction-generic arg nil))
                  (waveform-ref-args thing))))

;;; Definitions

(defclass waveform-definition ()
  ((name :initarg :name
         :reader waveform-definition-name)
   (entries :initarg :entries
            :reader waveform-definition-entries))
  ;; TODO cache entries?
  (:metaclass abstract-class)
  (:documentation "A representation of a user-specified waveform definition."))

(defclass static-waveform-definition (waveform-definition)
  ()
  (:documentation "A waveform definition that has no parameters."))

(defclass parameterized-waveform-definition (waveform-definition)
  ((parameters :initarg :parameters
               :reader waveform-definition-parameters
               :documentation "A list of symbol parameter names."))
  (:documentation "A waveform definition that has named parameters."))

(defun make-waveform-definition (name parameters entries)
  (check-type name string)
  (check-type parameters symbol-list)
  (if parameters
      (make-instance 'parameterized-waveform-definition
                     :name name
                     :parameters parameters
                     :entries entries)
      (make-instance 'static-waveform-definition
                     :name name
                     :entries entries)))

;;; TODO should this even be PRINT-INSTRUCTION-GENERIC?
;;; could go into PRINT-PARSED-PROGRAM

(defmethod print-instruction-generic ((thing waveform-definition) (stream stream))
  (format stream "DEFWAVEFORM ~a~@[(~{%~a~^, ~})~]:~%"
          (waveform-definition-name thing)
          (if (typep thing 'static-waveform-definition)
              nil
              (waveform-definition-parameters thing)))
  (format stream "    ~{~a~^, ~}~%"
          (mapcar (lambda (z)
                    (with-output-to-string (s)
                      (etypecase z
                        (number
                         (format-complex z s))
                        ((or list symbol)
                         (print-instruction (make-delayed-expression nil nil z) s)))))
                  (waveform-definition-entries thing))))


(defclass calibration-definition ()
  ((body :initarg :body
         :reader calibration-definition-body))
  (:metaclass abstract-class)
  (:documentation "A representation of a user-specified calibration."))

(defclass gate-calibration-definition (calibration-definition)
  ((name :initarg :name
         :reader calibration-definition-name)
   (parameters :initarg :parameters
               :reader calibration-definition-parameters)
   (arguments :initarg :arguments
              :reader calibration-definition-arguments))
  (:documentation "A representation of a user-specified gate calibration."))

(defclass measurement-calibration-definition (calibration-definition)
  ((qubit :initarg :qubit
          :reader measurement-calibration-qubit))
  (:metaclass abstract-class)
  (:documentation "Superclass to measurement calibration definitions."))

(defclass measure-calibration-definition (measurement-calibration-definition)
  ((address :initarg :address
             :reader measure-calibration-address))
  (:documentation "A representation of a user-specified MEASURE calibration."))

(defclass measure-discard-calibration-definition (measurement-calibration-definition)
  ()
  (:documentation "A representation of a user-specifieed MEASURE (discard) calibration."))

(defmethod print-instruction-generic ((defn gate-calibration-definition) (stream stream))
  (format stream "DEFCAL ~a" (calibration-definition-name defn))
  (flet ((print-thing (thing)
           (print-instruction thing nil)))
    (unless (endp (calibration-definition-parameters defn))
      (format stream "(~{~a~^, ~})"
              (mapcar #'print-thing (calibration-definition-parameters defn))))
    (unless (endp (calibration-definition-arguments defn))
      (format stream "~{ ~a~}"
              (mapcar #'print-thing (calibration-definition-arguments defn))))
    (format stream ":~%")
    (print-instruction-sequence (calibration-definition-body defn)
                                :stream stream
                                :prefix "    ")
    (terpri stream)))

(defmethod print-instruction-generic ((defn measure-calibration-definition) (stream stream))
  (format stream "DEFCAL ~a" (calibration-definition-name defn))
  (flet ((print-thing (thing)
           (print-instruction thing nil)))
    (unless (endp (calibration-definition-arguments defn))
      (format stream "~{ ~a~}"
              (mapcar #'print-thing (calibration-definition-arguments defn))))
    (unless (endp (calibration-definition-parameters defn))
      (format stream "~{ ~a~}"
              (mapcar #'print-thing (calibration-definition-parameters defn))))
    (format stream ":~%")
    (print-instruction-sequence (calibration-definition-body defn)
                                :stream stream
                                :prefix "    ")
    (terpri stream)))

(defun make-calibration-definition (name params args body)
  (check-type name string)
  (assert (every #'is-param params))
  (assert (every (lambda (arg)
                   (or (is-formal arg)
                       (qubit-p arg)))
                 args))
  (make-instance (if (string-equal "MEASURE" name)
                     'measure-calibration-definition
                     'gate-calibration-definition)
                 :name name
                 :parameters params
                 :arguments args
                 :body body))

;;; Frame Mutations
;;; frame is <list of qubits> followed by <frame name>
;;; Real is either a literal or an expression.
;;; TODO check on whether this makes sense
;;; TODO why can't we read frame attributes to classical memory?

(defclass frame-mutation (instruction)
  ((qubits :initarg :qubits
           :accessor target-qubits)
   (frame :initarg :frame
          :accessor target-frame)
   (value :initarg :value
          :accessor mutation-value))
  (:documentation "An instruction representing the mutation of a frame attribute.")
  (:metaclass abstract-class))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-frame-mutation-definition (name mnemonic docstring)
    (check-type name symbol)
    (check-type mnemonic string)
    `(progn
       (defclass ,name (frame-mutation)
         ()
         (:documentation ,docstring))

       (defmethod mnemonic ((inst ,name)) (values ',mnemonic ',name)))))

(defmethod arguments ((instr frame-mutation))
  (with-slots (qubits frame value)
      instr
    (coerce (append qubits (list frame value)) 'vector)))

(defmacro define-frame-mutation (name mnemonic &body body)
  (check-type mnemonic string)
  (check-type body list)
  (assert (= 1 (length body)))
  (expand-frame-mutation-definition name mnemonic (first body)))

(defmethod print-instruction-generic ((instr frame-mutation) (stream stream))
  (format stream "~A~{ ~A~}"
          (mnemonic instr)
          (map 'list #'print-instruction-to-string (arguments instr))))

(defmethod instantiate-instruction ((instr frame-mutation) param-value arg-value)
  (let ((qubits (mapcar (transform-if #'is-formal arg-value)
                        (target-qubits instr)))
        (value (funcall (transform-if #'is-formal arg-value)
                        (mutation-value instr))))
    (make-instance (class-of instr)
                   :qubits qubits
                   :frame (target-frame instr)
                   :value value)))


(define-frame-mutation set-frequency "SET-FREQUENCY"
  "An instruction setting the frequency of a frame.")

(define-frame-mutation set-phase "SET-PHASE"
  "An instruction setting the phase of a frame.")

(define-frame-mutation shift-phase "SHIFT-PHASE"
  "An instruction performing an additive shift of the phase of a frame.")

(define-frame-mutation set-scale "SET-SCALE"
  "An instruction setting the scale of a frame.")

;;; TODO Swap frame phases (not sure why)




;;; TODO arguments and mnemonic
;;; what is the motivation behind this? what is the "protocol" which is specified? how do we handle pulses?
;;; - it seems like they are used for printing


;;; Pulse

(defclass pulse (instruction)
  ((qubits :initarg :qubits
          :accessor pulse-qubits)
   (frame :initarg :frame
          :accessor pulse-frame)
   (waveform :initarg :waveform
             :accessor pulse-waveform))
  (:documentation "A pulse instruction."))

(defmethod print-instruction-generic ((instr pulse) (stream stream))
  (format stream "PULSE ~{~A ~}~A ~A"
          (mapcar (lambda (q) (print-instruction-generic q nil))
                  (pulse-qubits instr))
          (print-instruction-generic (pulse-frame instr) nil)
          (print-instruction-generic (pulse-waveform instr) nil)))

;;; TODO can waveform arguments be formal parameters?
(defmethod instantiate-instruction ((instr pulse) param-value arg-value)
  (let ((qubits (mapcar (transform-if #'is-formal arg-value)
                        (pulse-qubits instr))))
    (make-instance 'pulse
                   :qubits qubits
                   :frame (pulse-frame instr)
                   :waveform (pulse-waveform instr))))

;;; Capture
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

(defmethod print-instruction-generic ((instr capture) (stream stream))
  (format stream "CAPTURE ~A ~A ~A ~A"
          (print-instruction-generic (capture-qubit instr) nil)
          (print-instruction-generic (capture-frame instr) nil)
          (print-instruction-generic (capture-waveform instr) nil)
          (print-instruction-generic (capture-memory-ref instr) nil)))

(defmethod instantiate-instruction ((instr capture) param-value arg-value)
  (let ((qubit (funcall (transform-if #'is-formal arg-value)
                        (capture-qubit instr)))
        (memory-ref (funcall (transform-if #'is-formal arg-value)
                             (capture-memory-ref instr))))
    (check-mref memory-ref)
    (make-instance 'capture
                   :qubit qubit
                   :frame (capture-frame instr)
                   :waveform (capture-waveform instr)
                   :memory-ref memory-ref)))

(defclass raw-capture (instruction)
  ((qubit :initarg :qubit
          :accessor raw-capture-qubit)
   (frame :initarg :frame
          :accessor raw-capture-frame)
   (duration :initarg :duration
             :accessor raw-capture-duration)
   (memory-ref :initarg :memory-ref
               :accessor raw-capture-memory-ref))
  (:documentation "An instruction expressing the readout of raw
  IQ values, to be stored in a region of classical memory."))

(defmethod print-instruction-generic ((instr raw-capture) (stream stream))
  (format stream "RAW-CAPTURE ~A ~A ~A ~A"
          (print-instruction-generic (raw-capture-qubit instr) nil)
          (print-instruction-generic (raw-capture-frame instr) nil)
          (print-instruction-generic (raw-capture-duration instr) nil)
          (print-instruction-generic (raw-capture-memory-ref instr) nil)))

(defmethod instantiate-instruction ((instr raw-capture) param-value arg-value)
  (let ((qubit (funcall (transform-if #'is-formal arg-value)
                        (raw-capture-qubit instr)))
        (memory-ref (funcall (transform-if #'is-formal arg-value)
                             (raw-capture-memory-ref instr)))
        (duration (funcall (transform-if #'is-formal arg-value)
                           (raw-capture-duration instr))))
    (check-mref memory-ref)
    (make-instance 'raw-capture
                   :qubit qubit
                   :frame (raw-capture-frame instr)
                   :duration duration
                   :memory-ref memory-ref)))

;;; Timing control
(defclass delay (instruction)
  ((qubit :initarg :qubit
          :accessor delay-qubit)
   (duration :initarg :duration
             :accessor delay-duration))
  (:documentation "A delay of a specific time on a specific qubit."))

(defmethod print-instruction-generic ((instr delay) (stream stream))
  (format stream "DELAY ~A ~A"
          (print-instruction-generic (delay-qubit instr) nil)
          (print-instruction-generic (delay-duration instr) nil)))

(defmethod instantiate-instruction ((instr delay) param-value arg-value)
  (let ((qubit (funcall (transform-if #'is-formal arg-value)
                        (delay-qubit instr)))
        (duration (funcall (transform-if #'is-formal arg-value)
                           (delay-duration instr))))
    (make-instance 'delay
                   :qubit qubit
                   :duration duration)))

(defclass fence (instruction)
  ((qubits :initarg :qubits
           :accessor fence-qubits))
  (:documentation "A synchronization barrier on a set of qubits, demarcating
  preceding and succeeding instructions."))

(defmethod print-instruction-generic ((instr fence) (stream stream))
  (format stream "FENCE ~{~A ~}" (mapcar (lambda (q)
                                           (print-instruction-generic q nil))
                                         (fence-qubits instr))))

(defmethod instantiate-instruction ((instr fence) param-value arg-value)
  (let ((qubits (mapcar (transform-if #'is-formal arg-value)
                        (fence-qubits instr))))
    (make-instance 'fence :qubits qubits)))
