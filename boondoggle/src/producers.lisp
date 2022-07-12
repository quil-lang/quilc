;;;; producers.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:boondoggle)

(defstruct gate-set-record
  "Houses information about a gate in an ISA specification.

OPERATOR is the Quil name for this gate.
ORDER is one less than the number of arguments expected by this gate.
PARAMETER-BOUNDS is a list of maximum random values for the gate parameters."
  (order 0)
  (operator "I")
  (parameter-bounds nil))

(defclass producer ()
  ())

(defgeneric produce-quil-program (producer)
  (:documentation "Builds a Quil program or nil if exhausted."))

(defmethod producer-quil-program ((producer producer))
  (error "This producer has not been implemented."))

(defclass producer-random (producer)
  ((chip-specification :initform (cl-quil::build-8Q-chip)
                       :initarg :chip-specification
                       :reader producer-random-chip-specification)
   (program-depth-limit :initform nil
                        :initarg :program-depth-limit
                        :reader producer-random-program-depth-limit)
   (program-volume-limit :initform 20
                         :initarg :program-volume-limit
                         :reader producer-random-program-volume-limit)
   (gate-set :initform (list (make-gate-set-record :order 0
                                                   :operator "RZ"
                                                   :parameter-bounds (list (* 2 pi)))
                             (make-gate-set-record :order 0
                                                   :operator "RX"
                                                   :parameter-bounds (list (* 2 pi)))
                             (make-gate-set-record :order 1
                                                   :operator "CNOT"))
             :initarg :gate-set
             :reader producer-random-gate-set)
   (defgate-size-limit :initform 2
                       :initarg :size-limit
                       :reader producer-random-defgate-size-limit)
   (random-gate-rate :initform 0.1d0
                     :initarg :gate-rate
                     :reader producer-random-random-gate-rate)
   (respect-topology :initform nil
                     :initarg :respect-topology
                     :reader producer-random-respect-topology)))

(defmethod produce-quil-program ((producer producer-random))
  (random-protoquil (producer-random-chip-specification producer)
                    (producer-random-program-depth-limit producer)
                    (producer-random-program-volume-limit producer)
                    (producer-random-gate-set producer)
                    (producer-random-defgate-size-limit producer)
                    (producer-random-random-gate-rate producer)
                    (producer-random-respect-topology producer)))

(defclass producer-from-directory (producer)
  ((directory-path :initform nil)))

(defun measure-at-close-instrs (chip-specification)
  "Apply MEASURE instructions on all qubits in a chip-specification. Assumes qubits are ordered from 0 to n with no skipped indices."
  (loop :for j :below (cl-quil::chip-spec-n-qubits chip-specification)
        :collect (make-instance 'cl-quil:measure
                                :address (cl-quil:mref "ro" j)
                                :qubit (cl-quil:qubit j))))

(defun random-protoquil (chip-specification   ; chip topology
                         program-depth-limit  ; limit to randomly generated program depth
                         program-volume-limit ; limit to randomly generated program volume
                         gate-set             ; list of gate-set-records, used to draw from
                         defgate-size-limit   ; limit to size of randomly generated defgates
                         random-gate-rate     ; rate (0d0-1d0) of defgate appearance
                         respect-topology     ; place multiqubit gates only on chip-specification links
                         )
  "Generates random Quil strings to the statistics specifications set out by the argument list."
  (let ((parsed-program (make-instance 'cl-quil::parsed-program))
        (instruction-list nil)
        (random-gate-count 0))
    (dotimes (j (random program-volume-limit))
      ;; generate an instruction
      (let* ((random-gate-flag (< (random 1d0) random-gate-rate))
             (gate-record
               (unless random-gate-flag
                 (nth (random (length gate-set)) gate-set)))
             (qubits-on-device (length (aref (cl-quil::chip-specification-objects chip-specification) 0)))
             (gate-name
               (cond
                 (random-gate-flag
                  (format nil "RANDOM-GATE-~D" random-gate-count))
                 (t
                  (gate-set-record-operator gate-record))))
             (gate-order
               (cond
                 (random-gate-flag
                  (random (cond
                            ((and defgate-size-limit respect-topology)
                             (min defgate-size-limit qubits-on-device))
                            (defgate-size-limit defgate-size-limit)
                            (respect-topology qubits-on-device)
                            (t (error "I refuse to generate unbounded random gates.")))))
                 (t
                  (gate-set-record-order gate-record))))
             (qubit-indices
               (cond
                 ((= gate-order 0)
                  (list (random qubits-on-device)))
                 (respect-topology
                  (let* ((available-devices
                           (progn
                             (assert (elt (cl-quil::chip-specification-objects chip-specification) gate-order)
                                     nil
                                     "This gate spec expects higher order hardware objects to exist.")
                             (elt (cl-quil::chip-specification-objects chip-specification) gate-order)))
                         (device-index (random (length available-devices))))
                    (cl-quil::vnth 0 (cl-quil::hardware-object-cxns (cl-quil::vnth device-index available-devices)))))
                 (t
                  (assert (< gate-order qubits-on-device)
                          nil
                          "More random qubits requested than exist on this QPU to instantiate this instruction.")
                  (loop :with ret := nil
                        :until (> (length ret) gate-order)
                        :do (let ((qubit (random qubits-on-device)))
                              (unless (member qubit ret)
                                (push qubit ret)))
                        :finally (return ret)))))
             (gate-definition
               (when random-gate-flag
                 (let ((gate-matrix (cl-quil::random-special-unitary (expt 2 (1+ gate-order)))))
                   (cl-quil::make-gate-definition gate-name
                                               nil
                                               (loop :for i :below (magicl:ncols gate-matrix)
                                                     :nconc (loop :for j :below (magicl:nrows gate-matrix)
                                                                  :collect (magicl:tref gate-matrix j i)))))))
             (gate-parameters
               (unless random-gate-flag
                 (mapcar #'random (gate-set-record-parameter-bounds gate-record))))
             (gate-invocation (make-instance 'cl-quil::gate-application
                                             :operator (cl-quil:named-operator gate-name)
                                             :arguments (map 'list #'cl-quil:qubit qubit-indices)
                                             :parameters (map 'list #'cl-quil:constant gate-parameters))))
        (push gate-invocation instruction-list)
        ;; check to see if we need to bail because of depth
        (when (and
               program-depth-limit
               (< program-depth-limit
                  (let ((lschedule (make-instance 'cl-quil::lscheduler-empty)))
                    (cl-quil::append-instructions-to-lschedule lschedule instruction-list)
                    (cl-quil::lscheduler-calculate-depth lschedule))))
          (pop instruction-list)
          (return))
        ;; if we built a random gate, store its definition
        (when random-gate-flag
          (incf random-gate-count)
          (push gate-definition (cl-quil::parsed-program-gate-definitions parsed-program)))))
    (setf (cl-quil::parsed-program-executable-code parsed-program)
          (make-array (length instruction-list)
                      :initial-contents instruction-list))
    ;; Add readout memory definitions to the program
    (push (cl-quil::make-memory-descriptor
                :name "ro"
                :type cl-quil::quil-bit
                :length (cl-quil::chip-spec-n-qubits chip-specification))
          (cl-quil:parsed-program-memory-definitions parsed-program))
    ;; Add measure instructions to the end of the program
    (setf (cl-quil::parsed-program-executable-code parsed-program)
          (concatenate 'vector
                       (cl-quil::parsed-program-executable-code parsed-program)
                       (measure-at-close-instrs chip-specification)))
    parsed-program))
