(in-package :cl-quil)

;;; TODO rpcq can generate a message spec from this + a bit more metadata

;;; there are three names: lisp (waveform class), quilt, rpcq
;;; each slot has three names: lisp, quilt, rpcq
;;; for now, we focus on the first two

;;; We should be able to
;;; i) produce lisp classes,
;;; ii) produce language documentation
;;; iii) translate to rpcq messages

;;; class has i) the usual slots

;;; we also have the following mappings:


;;; operations: check if a waveform-ref is adequately specified
;;;             construct a new instance from a waveform reference


;;; each waveform type gets a class
;;; we have friendly lisp slots, but also each slot has a quilt-name

(defparameter *standard-template-waveforms* (make-hash-table :test 'equal)
  "The built-in QuilT waveforms.")

(defgeneric standard-waveform-quilt-parameters (class-name))

(defclass gaussian-waveform (standard-waveform)
  ((fwhm :initarg :fwhm
         :type float
         :documentation "Full Width Half Max shape parameter, in seconds.")
   (t0 :initarg :t0
       :type float
       :documentation "Center time coordinate of the shape in seconds. Defaults
       to mid-point of pulse."))
  (:documentation "A Gaussian shaped waveform envelope defined for a specific frame."))

(setf (gethash "gaussian" *standard-template-waveforms*)
      'gaussian-waveform)

(defmethod standard-waveform-quilt-parameters ((class-name (eql 'gaussian-waveform)))
  `(("duration" duration)
    ("fwhm" fwhm)
    ("t0" t0)))

(defclass drag-gaussian-waveform (standard-waveform)
  ((fwhm :initarg :fwhm
         :type float
         :documentation "Full Width Half Max shape parameter, in seconds.")
   (t0 :initarg :t0
       :type float
       :documentation "Center time coordinate of the shape in seconds. Defaults
       to mid-point of pulse.")
   (anh :initarg :anh
        :type float
        :initform -210e6
        :documentation "Anharmonicity of the qubit, f01-f12 in (Hz)")
   (alpha :initarg :alpha
          :type float
          :documentation "Dimensionless DRAG parameter"))
  (:documentation "A DRAG Gaussian shaped waveform envelope defined for a specific frame."))

(setf (gethash "draggaussian" *standard-template-waveforms*)
      'drag-gaussian-waveform)

(defmethod standard-waveform-quilt-parameters ((class-name (eql 'drag-gaussian-waveform)))
  `(("duration" duration)
    ("fwhm" fwhm)
    ("t0" t0)
    ("anh" anh)
    ("alpha" alpha)))

(defclass hermite-gaussian-waveform (standard-waveform)
  ((fwhm :initarg :fwhm
         :type float
         :documentation "Full Width Half Max shape parameter, in seconds.")
   (t0 :initarg :t0
       :type float
       :documentation "Center time coordinate of the shape in seconds. Defaults
       to mid-point of pulse.")
   (anh :initarg :anh
        :type float
        :initform -210e6
        :documentation "Anharmonicity of the qubit, f01-f12 in (Hz)")
   (alpha :initarg :alpha
          :type float
          :documentation "Dimensionless DRAG parameter")
   (second-order-hrm-coeff :initarg :second-order-hrm-coeff
                           :type float
                           :documentation "Second order coefficient (see paper)"))
  (:documentation "Hermite-Gaussian shaped pulse. Reference: Effects of arbitrary laser
or NMR pulse shapes on population inversion and coherence Warren S. Warren.
81, (1984); doi: 10.1063/1.447644"))

(defmethod standard-waveform-quilt-parameters ((class-name (eql 'hermite-gaussian-waveform)))
  `(("duration" duration)
    ("fwhm" fwhm)
    ("t0" t0)
    ("anh" anh)
    ("alpha" alpha)
    ("second_order_hrm_coeff" second-order-hrm-coeff)))

(setf (gethash "hermitegaussian" *standard-template-waveforms*)
      'hermite-gaussian-waveform)

(defclass erf-square-waveform (standard-waveform)
  ((risetime
    :documentation "The width of the rise and fall sections in seconds."
    :type :float)

   (pad-left
    :documentation "Length of zero-amplitude padding before the pulse in seconds."
    :type :float)

   (pad-right
    :documentation "Length of zero-amplitude padding after the pulse in seconds."
    :type :float))
  (:documentation "Pulse with a flat top and rounded shoulders given by error functions"))

(setf (gethash "erfsquare" *standard-template-waveforms*)
      'erf-square-waveform)

(defmethod standard-waveform-quilt-parameters ((class-name (eql 'erf-square-waveform)))
  `(("duration" duration)
    ("risetime" risetime)
    ("pad_left" pad-left)
    ("pad_right" pad-left)))

(defclass flat-waveform (standard-waveform)
  ((iq :initarg :iq
       :type complex
       :documentation "Individual IQ point to hold constant"))
  (:documentation "Flat pulse."))

(setf (gethash "flat" *standard-template-waveforms*)
      'flat-waveform)

(defmethod standard-waveform-quilt-parameters ((class-name (eql 'flat-waveform)))
  `(("duration" duration)
    ("iq" iq)))
