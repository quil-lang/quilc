;;;; type-safety.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:cl-quil.quilt)

(defun raw-capture-num-real-samples (instr)
  (check-type instr raw-capture)
  (let ((frame-defn (frame-name-resolution
                     (raw-capture-frame instr))))
    (if (frame-definition-sample-rate frame-defn)
        (ceiling (* 2                            ; real, imag
                   (constant-value (raw-capture-duration instr))
                   (constant-value (frame-definition-sample-rate frame-defn))))
        nil)))

;; CAPTURE must target a REAL[2]
(defmethod type-check-instr ((instr capture) memory-regions)
  (let* ((mref (capture-memory-ref instr))
         (mdesc (quil::find-descriptor-for-mref mref memory-regions)))
    (quil::enforce-mref-bounds mref mdesc)
    (adt:match quil-type (memory-descriptor-type mdesc)
      (quil-real
       (if (> 2 (quil::memory-segment-length mdesc :offset mref))
           (quil-type-error "CAPTURE instruction target ~/quil:instruction-fmt/ must be a REAL ~
                            vector of length no less than 2."
                            mref)
           t))
      (_
       (quil-type-error "CAPTURE instruction target must be of type ~
                        REAL, but got ~/quil:instruction-fmt/ of type ~A."
                        mref
                        (quil::quil-type-string (memory-descriptor-type mdesc)))))))

;; RAW-CAPTURE must target a REAL[n] where n is 2*(the number of iq values)
(defmethod type-check-instr ((instr raw-capture) memory-regions)
  (let* ((mref (raw-capture-memory-ref instr))
         (mdesc (quil::find-descriptor-for-mref mref memory-regions))
         (frame-defn (frame-name-resolution
                      (raw-capture-frame instr))))
    (quil::enforce-mref-bounds mref mdesc)
    (adt:match quil-type (memory-descriptor-type mdesc)
      (quil-real
       (a:if-let ((samples (raw-capture-num-real-samples instr)))
         (if (> samples (quil::memory-segment-length mdesc :offset mref))
             (quil-type-error "RAW-CAPTURE instruction target ~/quil:instruction-fmt/ must be a REAL ~
                              vector of length no less than ~A."
                              mref
                              samples))
         (warn "RAW-CAPTURE on frame ~/quil:instruction-fmt/ with unknown sample rate."
               (frame-definition-frame frame-defn))))
      (_
       (quil-type-error "RAW-CAPTURE instruction target must be of type ~
                        REAL, but got ~/quil:instruction-fmt/ of type ~A."
                        mref
                        (quil::quil-type-string (memory-descriptor-type mdesc)))))))
