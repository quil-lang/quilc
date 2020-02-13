;;;; src/quilt/package.lisp
;;;;
;;;; Author: Erik Davis

;;; Allegro (and other Lisps) don't support the non-standard "package
;;; local nicknames".
#-(or sbcl ecl ccl)
(rename-package :alexandria :alexandria '(:a))

(defpackage #:cl-quil.quilt
  (:nicknames #:quilt)
  (:use #:cl
        #:cl-quil
        #:abstract-classes
        #:singleton-classes)
  ;; We define a number of methods on generic functions from CL-QUIL. We import these here.
  (:import-from #:cl-quil
                ;; ast.lisp
                #:arguments
                #:mnemonic
                #:print-instruction-generic
                #:print-parsed-program-generic
                ;; cl-quilt.lisp
                #:definition-signature
                ;; analysis/resolve-objects.lisp
                #:resolve-instruction
                #:resolve-objects
                ;; analysis/expand-calibrations.lisp
                #:instantiate-instruction
                ;; analysis/type-safety.lisp
                #:type-check-instr
                )
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))

  (:export

   #:frame                              ; STRUCTURE
   #:frame-name                         ; READER
   #:frame-qubits                       ; READER
   #:frame=                             ; FUNCTION
   #:frame-hash                         ; FUNCTION
   #:frame-intersects-p                 ; FUNCTION
   #:frame-on-p                         ; FUNCTION

   #:waveform-ref                       ; STRUCTURE
   #:waveform-ref-name                  ; READER
   #:waveform-ref-parameter-alist       ; READER

   #:pulse                              ; CLASS
   #:pulse-frame                        ; READER
   #:pulse-waveform                     ; READER

   #:capture                            ; CLASS
   #:capture-frame                      ; READER
   #:capture-waveform                   ; READER
   #:capture-memory-ref                 ; READER

   #:raw-capture                        ; CLASS
   #:raw-capture-frame                  ; READER
   #:raw-capture-duration               ; READER
   #:raw-capture-memory-ref             ; READER

   #:nonblocking-p                      ; READER

   #:delay                              ; CLASS
   #:delay-duration                     ; READER
   #:delay-on-frames                    ; CLASS
   #:delay-frames                       ; READER
   #:delay-on-qubits                    ; READER
   #:delay-qubits                       ; READER

   #:fence                              ; CLASS
   #:fence-qubits                       ; READER

   #:simple-frame-mutation              ; ABSTRACT CLASS
   #:set-frequency                      ; CLASS
   #:set-phase                          ; CLASS
   #:shift-phase                        ; CLASS
   #:set-scale                          ; CLASS
   #:frame-mutation-target-frame        ; READER
   #:frame-mutation-value               ; READER

   #:swap-phase                         ; CLASS
   #:swap-phase-left-frame              ; READER
   #:swap-phase-right-frame             ; READER

   #:gate-definition                    ; CLASS
   #:static-gate-definition             ; CLASS
   #:parameterized-gate-definition      ; CLASS
   #:gate-definition-name               ; READER
   #:gate-definition-entries            ; READER
   #:gate-definition-parameters         ; READER

   #:circuit-definition                 ; CLASS
   #:circuit-definition-name            ; READER
   #:circuit-definition-parameters      ; READER
   #:circuit-definition-arguments       ; READER
   #:circuit-definition-body            ; READER

   #:waveform-definition                ; ABSTRACT CLASS
   #:static-waveform-definition         ; CLASS
   #:parameterized-waveform-definition  ; CLASS
   #:waveform-definition-name           ; READER
   #:waveform-definition-entries        ; READER
   #:waveform-definition-parameters     ; READER
   #:waveform-definition-sample-rate    ; READER

   #:calibration-definition             ; ABSTRACT CLASS
   #:gate-calibration-definition        ; CLASS
   #:measurement-calibration-definition ; ABSTRACT CLASS
   #:measure-calibration-definition     ; CLASS
   #:measure-discard-calibration-definition ; CLASS
   #:calibration-definition-body        ; READER
   #:calibration-definition-operator    ; READER
   #:calibration-definition-parameters  ; READER
   #:calibration-definition-arguments   ; READER
   #:measurement-calibration-qubit      ; READER
   #:measure-calibration-address        ; READER

   #:frame-definition                   ; CLASS
   #:frame-definition-frame             ; READER
   #:frame-definition-sample-rate       ; READER
   #:frame-definition-initial-frequency ; READER

   #:parse-quilt-into-raw-program       ; FUNCTION
   #:parse-quilt                        ; FUNCTION
   #:read-quilt-file                    ; FUNCTION

   #:parsed-quilt-program                   ; CLASS
   #:parsed-program-waveform-definitions    ; READER
   #:parsed-program-calibration-definitions ; READER
   #:parsed-program-frame-definitions       ; READER
   )
  )
