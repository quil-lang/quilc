(in-package #:cl-quil)

(defparameter *standard-template-waveforms*
  (a:alist-hash-table
   (list (cons "gaussian" (find-class 'gaussian-waveform))
         (cons "draggaussian" (find-class 'drag-gaussian-waveform))
         (cons "flat" (find-class 'flat-waveform)))
   :test 'equal)
  "The built-in QuilT waveforms.")

;;; TODO maybe this should be part of the standard waveform definition
(defun standard-waveform-expected-parameters (standard-waveform-class)
  (cons (param "duration")
        (mapcar (lambda (slot)
                  (param
                   (string-downcase
                    (closer-mop:slot-definition-name slot))))
                (closer-mop:class-direct-slots standard-waveform-class))))

(defun validate-waveform-parameters (waveform-ref expected-parameters)
  (let ((actual (mapcar #'first (waveform-ref-args waveform-ref))))
    (a:when-let ((missing (set-difference expected-parameters actual :test #'equalp)))
      (quil-parse-error "Expected parameter ~A in waveform ~A, but none was found."
                        (param-name (first missing))
                        (waveform-ref-name waveform-ref)))
    (a:when-let ((unexpected (set-difference actual expected-parameters :test #'equalp)))
      (quil-parse-error "Unexpected parameter ~A in waveform ~A. ~@
                        Expected parameters are: ~{~A~^, ~}."
                        (param-name (first unexpected))
                        (waveform-ref-name waveform-ref)
                        (mapcar #'param-name expected-parameters)))
    t))

(defun resolve-standard-waveform (waveform-ref standard-waveform-class)
  (validate-waveform-parameters waveform-ref
                                (standard-waveform-expected-parameters standard-waveform-class))
  (let ((initargs-and-vals
          (loop :for (p . v) :in (waveform-ref-args waveform-ref)
                :for initarg := (intern (string-upcase (param-name p))
                                        :KEYWORD)
                :for initval := v
                :collect initarg
                :collect initval)))
    (apply #'make-instance standard-waveform-class initargs-and-vals)))

(defun custom-waveform-expected-parameters (waveform-definition)
  (etypecase waveform-definition
    (static-waveform-definition
     nil)
    (parameterized-waveform-definition
     (mapcar (lambda (p)
               (param (symbol-name p)))
             (waveform-definition-parameters waveform-definition)))))

(defun resolve-custom-waveform (waveform-ref waveform-definition)
  (validate-waveform-parameters waveform-ref
                                (custom-waveform-expected-parameters waveform-definition))
  waveform-definition)

(defun resolve-waveform (waveform-ref waveform-definitions &key (use-defaults t))
  "Construct an appropriate waveform instance from a waveform reference and a
  list of definitions."
  (assert (null (waveform-ref-name-resolution waveform-ref)))
  (let* ((name (waveform-ref-name waveform-ref))
         (resolution
          (a:if-let ((default-binding (and use-defaults
                                           (gethash name *standard-template-waveforms*))))
            (resolve-standard-waveform waveform-ref default-binding)
            (a:if-let ((defwaveform
                           (or (find name waveform-definitions
                                     :key #'waveform-definition-name
                                     :test #'string=))))
              (resolve-custom-waveform waveform-ref defwaveform)
              (quil-parse-error "Waveform reference ~A does not match ~
                                any standard or user defined waveforms." name)))))
    (setf (waveform-ref-name-resolution waveform-ref) resolution)
    waveform-ref))

