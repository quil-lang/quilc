(in-package :cl-quil.quilt)

(defun ceil-eps (x)
  ;; From pidgin:
  ;;
  ;; A custom ceil() method that includes a machine-epsilon sized region above each integer in the
  ;; set of values that are mapped to that integer. In other words:
  ;; ceil_eps(x) = n, for all x and integers n s.t. n <= x < (n + n*epsilon)

  ;; To handle accumulated floating point errors in sweeps above typical floating point imprecision
  ;; we make epsilon 10x larger than floating point epsilon.
  (ceiling (- x (* x 10 double-float-epsilon))))

(cffi:defcfun "erf" :double
  (arg :double))

(defun slot-constant-value (obj slot-name)
  "Get the constant value of slot SLOT-NAME on OBJ."
  (let ((val (slot-value obj slot-name)))
    (typecase val
      (constant (constant-value val))
      (otherwise
       (error "Expected ~A to be a Quil constant, but got ~A instead." slot-name val)))))

(defun fwhm->sigma (fwhm)
  "Convert the 'full-width half-max' value FWHM to a corresponding 'standard deviation' or 'sigma' value."
  (* 0.5d0 fwhm (/ (sqrt (* 2.0d0 (log 2.0d0))))))

(defun waveform-iq-values (waveform sample-rate)
  "Construct a complex vector, corresponding to the entries of WAVEFORM sampled at SAMPLE-RATE.

This incorporates and scaling, detuning, and phase shifts."
  (let* ((scale (slot-constant-value waveform 'scale))
         (phase (slot-constant-value waveform 'phase))
         (detuning (slot-constant-value waveform 'detuning))
         (iqs (default-waveform-iq-values waveform sample-rate)))
    (dotimes (j (length iqs) iqs)
      (setf (aref iqs j)
            (* (aref iqs j)
               scale
               (exp (* 2 pi #C(0d0 1d0)
                       phase))
               (exp (* 2 pi #C(0d0 1d0)
                       detuning
                       j
                       (/ sample-rate))))))))

(defgeneric default-waveform-iq-values (waveform sample-rate)
  (:documentation "Construct a complex vector, corresponding to the entries of WAVEFORM sampled at SAMPLE-RATE.

NOTE: This does not apply scaling, detuning, or phase shifts.")
  (:method ((wf quilt::flat-waveform) sample-rate)
    (let ((duration (slot-constant-value wf 'duration))
          (iq (coerce (slot-constant-value wf 'iq)
                      '(complex double-float))))
      (let ((length (ceil-eps (* duration sample-rate))))
        (make-array length :element-type '(complex double-float)
                           :initial-element iq))))

  (:method ((wf quilt::gaussian-waveform) sample-rate)
    (gaussian-samples (slot-constant-value wf 'duration)
                      sample-rate
                      (slot-constant-value wf 'fwhm)
                      (slot-constant-value wf 't0)))

  (:method ((wf quilt::drag-gaussian-waveform) sample-rate)
    (drag-gaussian-samples (slot-constant-value wf 'duration)
                           sample-rate
                           (slot-constant-value wf 'fwhm)
                           (slot-constant-value wf 't0)
                           (slot-constant-value wf 'anh)
                           (slot-constant-value wf 'alpha)))

  (:method ((wf quilt::hermite-gaussian-waveform) sample-rate)
    (hermite-gaussian-samples (slot-constant-value wf 'duration)
                              sample-rate
                              (slot-constant-value wf 'anh)
                              (slot-constant-value wf 'alpha)
                              (slot-constant-value wf 'fwhm)
                              (slot-constant-value wf 't0)
                              (slot-constant-value wf 'second-order-hrm-coeff)))

  (:method ((wf quilt::erf-square-waveform) sample-rate)
    (erf-square-samples (slot-constant-value wf 'duration)
                        sample-rate
                        (slot-constant-value wf 'risetime)
                        (slot-constant-value wf 'pad-left)
                        (slot-constant-value wf 'pad-right))))


(defun gaussian-samples (duration sample-rate fwhm &optional t0)
  "Compute the samples of a Gaussian pulse.

FWHM (seconds) is a shape parameter determining the width of the pulse.
T0 (seconds) is a shape parameter determining the midpoint of the pulse."
  (unless t0
    (setf t0 (/ duration 2.0d0)))
  (let* ((length (ceil-eps (* duration sample-rate)))
         (iqs (make-array length :element-type '(complex double-float))))
    (dotimes (i length iqs)
      (setf (aref iqs i)
            (complex (exp (* -0.5d0
                             (expt (- (/ i sample-rate) t0) 2)
                             (/ (expt (fwhm->sigma fwhm) 2)))))))))

;;; The Derivative Removal by Adiabatic Gate (DRAG) technique involves
;;; augmenting a simple basic pulse (e.g. a Gaussian) with a related
;;; off-quadrature dependent pulse, which serves as a correction factor
;;; for certain kinds of errors.
;;;
;;; The DRAG-GAUSSIAN and HERMITE-GAUSSIAN waveforms fall into this category.
;;; Their constructors are pretty jargon-laden. For now we just take it for
;;; granted that this is meaningful to someone....

(defun drag-gaussian-samples (duration sample-rate fwhm t0 anh alpha)
  "Compute the samples of a simple DRAG gaussian pulse.

FWHM (seconds) is a shape parameter determining the width of the pulse.
T0 (seconds) is a shape parameter determining the midpoint of the pulse.
ANH (Hz) is the anharmonicity (f01-f12) of the qubit.
ALPHA is a dimensionless DRAG parameter."
  (let* ((length (ceil-eps (* duration sample-rate)))
         (iqs (make-array length :element-type '(complex double-float)))
         (sigma2 (expt (fwhm->sigma fwhm) 2)))
    (dotimes (i length iqs)
      (let* ((delta (- (/ i sample-rate) t0))
             (env (exp (* -0.5d0
                          (expt delta 2)
                          (/ sigma2))))
             (denv (* alpha
                      (/ (* 2.0d0 pi anh sigma2))
                      delta
                      env)))
        (setf (aref iqs i)
              (complex env denv))))))

(defun hermite-gaussian-samples (duration sample-rate anh alpha fwhm t0 second-order-hrm-coeff)
  "Compute the samples of a second-order corrected DRAG gaussian.

ANH (Hz) is the anharmonicity (f01-f12) of the qubit.
ALPHA is a dimensionless DRAG parameter.
FWHM (seconds) is a shape parameter determining the width of the pulse.
T0 (seconds) is a shape parameter determining the midpoint of the pulse.
SECOND-ORDER-HERM-COEFF is a dimensionless DRAG parameter. See reference: Effects of arbitrary laser or NMR pulse shapes on population inversion and coherence Warren S. Warren. 81, (1984); doi: 10.1063/1.447644"
  (let* ((length (ceil-eps (* duration sample-rate)))
         (iqs (make-array length :element-type '(complex double-float)))
         (sigma2 (expt (fwhm->sigma fwhm) 2)))
    (dotimes (i length iqs)
      (let* ((delta (- (/ i sample-rate) t0))
             (expt (* 0.5d0
                      (expt delta 2)
                      (/ sigma2)))
             (env (* (- 1.0d0 (* second-order-hrm-coeff expt))
                     (exp (- expt))))
             (deriv-prefactor (- (/ alpha
                                    (* 2.0d0 pi anh))) )
             (denv (* deriv-prefactor
                      delta
                      (/ sigma2)
                      (exp (- expt))
                      (- (* second-order-hrm-coeff (- expt 1.0d0))
                         1.0d0))))
        (setf (aref iqs i)
              (complex env denv))))))

(defun erf-square-samples (duration sample-rate risetime pad-left pad-right &optional (neg-polarity nil))
  "Compute the samples of a pulse with a flat top and edges that are determined by rescaled error functions (erf).

RISETIME (seconds) is the width of the rise and fall sections of the pulse.
PAD-LEFT (seconds) is the amount of zero-padding to add to the start of the pulse.
PAD-RIGHT (seconds) is the amount of zero-padding to add to the end of the pulse.
If NEG-POLARITY is nil, the amplitude if positive. Otherwise, it is negative."
  (let* ((length-left (ceil-eps (* pad-left sample-rate)))
         (length-mid (ceil-eps (* duration sample-rate)))
         (length-right (ceil-eps (* pad-right sample-rate)))
         (iqs (make-array (+ length-left length-mid length-right)
                          :element-type '(complex double-float)))
         (fwhm (* 0.5d0 risetime))
         (t1 fwhm)
         (t2 (- duration fwhm))
         (sigma (fwhm->sigma fwhm)))
    (dotimes (i length-mid iqs)
      (let* ((ts (/ i sample-rate))
             (val (* (if neg-polarity -0.5d0 0.5d0)
                     (- (erf (/ (- ts t1) sigma))
                        (erf (/ (- ts t2) sigma))))))
        (setf (aref iqs (+ i length-left))
              (complex val))))))


(defun expand-and-resolve-template-waveform (instr sample-rate wf-cache)
  "Convert template waveforms to static waveforms in the pulse or capture instruction INSTR.

SAMPLE-RATE is the number of samples per second.
WF-CACHE is an alist associating waveform references to static waveform definitions.

Returns the instructions waveform reference as well as a new static waveform definition, if one is constructed."
  (check-type instr (or pulse capture))
  (flet ((fresh-name (&optional (prefix "waveform"))
           (loop :with name := (symbol-name (gensym prefix))
                 :while (some (lambda (entry)
                                (string= name (waveform-definition-name (cdr entry))))
                              wf-cache)
                 :do (setf name (symbol-name (gensym prefix)))
                 :finally (return-from fresh-name name)))
         (refer-to (defn)
           (let ((ref (waveform-ref (waveform-definition-name defn) nil)))
             (setf (waveform-ref-name-resolution ref) defn)
             ref)))
    (let ((wf-ref (slot-value instr 'waveform)))
      (a:when-let ((cache-entry (assoc wf-ref wf-cache :test #'quilt::waveform-ref=)))
        (setf (slot-value instr 'waveform)
              (refer-to (cdr cache-entry)))
        (return-from expand-and-resolve-template-waveform (values wf-ref nil)))
      (etypecase (waveform-ref-name-resolution wf-ref)
        (template-waveform
         (let* ((iqs (waveform-iq-values (waveform-ref-name-resolution wf-ref)
                                         sample-rate))
                (name (fresh-name))
                (defn (make-instance 'static-waveform-definition :entries iqs :name name))
                (new-ref (refer-to defn)))
           (setf (slot-value instr 'waveform) new-ref)
           (values wf-ref defn)))
        (static-waveform-definition
         (values wf-ref (waveform-ref-name-resolution wf-ref)))
        (parameterized-waveform-definition ; TODO: decide between schizophrenic tension of position vs named arguments
         (error "Error expanding waveform in ~/quil::instruction-fmt/. Parametric waveforms are not currently implemented."
                instr))))))

(defun expand-template-waveforms (parsed-program sample-rate)
  "Transform a Quilt program PARSED-PROGRAM to one with all template waveform references expanded to static waveform references."
  (loop :with wf-cache := nil
        :for instr :across (parsed-program-executable-code parsed-program)
        :when (typep instr '(or pulse capture))
          :do (multiple-value-bind (old-ref defn)
                  (expand-and-resolve-template-waveform instr
                                                        sample-rate
                                                        wf-cache)
                (when defn
                  (setf wf-cache (acons old-ref defn wf-cache))))
        :finally (setf (parsed-program-waveform-definitions parsed-program)
                       (nreverse (mapcar #'cdr wf-cache))))
  parsed-program)
