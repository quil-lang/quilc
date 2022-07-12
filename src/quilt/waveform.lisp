;;;; src/quilt/waveform.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:cl-quil/quilt)

;;; Standard Waveform Definitions
;;;
;;; PULSE and CAPTURE instructions require the specification of an appropriate
;;; waveform to use. Waveforms come in two flavors.
;;;   - "Standard" waveforms are baked-in to the language, and supported by downstream
;;;     consumers. These are represented by a subclass of STANDARD-WAVEFORM. Each
;;;     instance has a well-specified duration, which is part of the waveform parameters.
;;;   - Custom waveforms, which are associated with a DEFWAVEFORM definition. These
;;;     are ultimately specifications for a fixed-length sequence of IQ values. They may
;;;     be parametric, but their duration is the sequence length * sample rate.
;;;
;;; This file gives the definitions of the standard waveforms, together with the
;;; required information to construct an appropriate instance from a quilt
;;; waveform reference.


(defvar *quilt-to-waveform-class* (make-hash-table :test 'equal)
  "A mapping from Quilt name (a string) to the corresponding waveform class name.")

(defun default-waveform-class (waveform-ref)
  "Return the built-in waveform class named by the given WAVEFORM-REF."
  (gethash (waveform-ref-name waveform-ref) *quilt-to-waveform-class*))

(defvar *waveform-class-parameter-alists* (make-hash-table :test 'equal)
  "This is keyed by the built-in waveform CLASS-NAMEs, and maintains for each an association list mapping Quilt parameter names to their corresponding slot names.")

(defun quilt-waveform-parameter-alist (class-name)
  "Given the CLASS-NAME of a built-in Quilt waveform, return an association list mapping Quilt waveform parameter names and their corresponding slot names on the waveform class."
  (gethash class-name *waveform-class-parameter-alists*))

(defmacro define-standard-waveform (class-name quilt-name slot-specs &key documentation)
  "Define a standard waveform.

Parameters:
  * CLASS-NAME: The name of the standard waveform type.
  * QUILT-NAME: A string indicating the waveform name as exposed to Quilt.
  * SLOT-SPECS: A list of slot specifications. An entry is a name followed by a plist with the following keys:
    :QUILT-NAME    - (required) A string indicating the corresponding named parameter in Quilt source.
    :RPCQ-TYPE     - (optional) A type specifier indicating the RPCQ type of this parameter.

    Any other entries in the plist are passed on to the slot specification of the waveform class."

  ;; Re: RPCQ-TYPE, note that this is the type which RPCQ expects, but is not
  ;; necessarily the type that CL-QUIL/QUILT will use place in the slot (for the
  ;; latter, generally one of QUIL:CONSTANT or QUIL:PARAM). We do not currently
  ;; make use of this information, but it is informative to readers.
  (check-type quilt-name string)
  (check-type class-name symbol)
  (check-type documentation (or null string))
  (let ((quilt-param-alist
          (loop :for spec :in slot-specs
                :for (slot-name . slot-plist) := spec
                :for param-quilt-name := (getf slot-plist :quilt-name)
                :unless param-quilt-name
                  :do (error "Expected :QUILT-NAME in property list for ~A" slot-name)
                :do (progn
                      (remf (rest spec) :quilt-name)
                      (remf (rest spec) :rpcq-type))
                :collect (list param-quilt-name slot-name))))
    ;; We get this from STANDARD-WAVEFORM
    (push (list "duration" 'duration) quilt-param-alist)

    `(prog1
       (defclass ,class-name (standard-waveform)
         ,slot-specs
         ,@(when documentation
             `((:documentation ,documentation))))

       (setf (gethash ,quilt-name *quilt-to-waveform-class*) ',class-name)
       (setf (gethash ',class-name *waveform-class-parameter-alists*) ',quilt-param-alist))))

;;; These are produced from the similar definitions in RPCQ.

(define-standard-waveform gaussian-waveform "gaussian"
  ((fwhm :quilt-name "fwhm"
         :rpcq-type float
         :documentation "Full Width Half Max shape parameter, in seconds.")
   (t0 :quilt-name "t0"
       :rpcq-type float
       :documentation "Center time coordinate of the shape in seconds. Defaults to mid-point of pulse.")))

(define-standard-waveform drag-gaussian-waveform "drag_gaussian"
  ((fwhm :quilt-name "fwhm"
         :rpcq-type float
         :documentation "Full Width Half Max shape parameter, in seconds.")
   (t0 :quilt-name "t0"
       :rpcq-type float
       :documentation "Center time coordinate of the shape in seconds. Defaults to mid-point of pulse.")
   (anh :quilt-name "anh"
        :rpcq-type float
        :documentation "Anharmonicity of the qubit, f01-f12 in (Hz).")
   (alpha :quilt-name "alpha"
          :rpcq-type float
          :documentation "Dimensionless DRAG parameter."))
  :documentation "A DRAG Gaussian shaped waveform envelope defined for a specific frame.")

(define-standard-waveform hermite-gaussian-waveform "hermite_gaussian"
  ((fwhm :quilt-name "fwhm"
         :rpcq-type float
         :documentation "Full Width Half Max shape parameter, in seconds.")
   (t0 :quilt-name "t0"
       :rpcq-type float
       :documentation "Center time coordinate of the shape in seconds. Defaults to mid-point of pulse.")
   (anh :quilt-name "anh"
        :rpcq-type float
        :documentation "Anharmonicity of the qubit, f01-f12 in (Hz).")
   (alpha :quilt-name "alpha"
          :rpcq-type float
          :documentation "Dimensionless DRAG parameter.")
   (second-order-hrm-coeff :quilt-name "hrm_coeff"
                           :rpcq-type float
                           :documentation "Second order coefficient (see paper)"))
  :documentation "Hermite-Gaussian shaped pulse.

Reference: Effects of arbitrary laser or NMR pulse shapes on population inversion and coherence Warren S. Warren. 81, (1984); doi: 10.1063/1.447644")

(define-standard-waveform erf-square-waveform "erf_square"
  ((risetime :quilt-name "risetime"
             :documentation "The width of the rise and fall sections in seconds."
             :rpcq-type float)

   (pad-left :quilt-name "pad_left"
             :documentation "Length of zero-amplitude padding before the pulse in seconds."
             :rpcq-type float)

   (pad-right :quilt-name "pad_right"
              :documentation "Length of zero-amplitude padding after the pulse in seconds."
              :rpcq-type float))
  :documentation "Pulse with a flat top and rounded shoulders given by error functions.")

(define-standard-waveform flat-waveform "flat"
  ((iq :quilt-name "iq"
       :rpcq-type complex
       :documentation "A complex number representing the in-phase (real) and quadrature (imaginary) values to hold constant."))
  :documentation "Flat pulse.")


;;; TODO: This could probably be deprecated in favor of just having FLAT-WAVEFORMs.
;;; Seemingly the only special thing about BOXCAR-AVERAGER-KERNEL is that it is
;;; normalized (presumably to integrate to 1), rather than having a specified IQ value.
(define-standard-waveform boxcar-averager-kernel "boxcar_kernel"
  ((phase :quilt-name "phase"
          :rpcq-type complex
          :documentation "Phase [units of revolutions] to rotate the kernel by.")
   (detuning :quilt-name "detuning"
             :rpcq-type float
             :documentation "Frequency modulation to apply to the kernel [Hz]."))
  :documentation "A normalized flat or boxcar integration kernel.")
