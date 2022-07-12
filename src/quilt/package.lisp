;;;; src/quilt/package.lisp
;;;;
;;;; Author: Erik Davis

(defpackage #:cl-quil/quilt
  (:use #:cl
        #:cl-quil/frontend
        #:abstract-classes
        #:singleton-classes)
  ;; We define a number of methods on generic functions from
  ;; CL-QUIL. We import these here, as well as other internal symbols
  ;; that we want to get our hands on.
  (:import-from #:cl-quil/frontend
                ;; frontend-utilities.lisp
                #:list=
                ;; ast.lisp
                #:arguments
                #:mnemonic
                #:map-de-params
                #:substitute-parameter
                #:check-mref
                #:apply-modifiers-to-operator                
                #:print-instruction-sequence
                #:print-instruction-generic
                #:print-parsed-program-generic
                #:format-complex
                ;; parser.lisp
                #:match-line
                #:take-until
                #:take-while-from-end
                #:gate-modifier-token-p
                #:*arithmetic-parameters*
                #:*segment-encountered*
                #:*definitions-allowed*
                #:*formal-arguments-allowed*
                #:parse-qubit                
                #:parse-parameter-or-expression
                #:parse-memory-or-formal-token
                #:parse-parameters
                #:parse-arithmetic-tokens               
                #:parse-indented-entries
                #:parse-indented-body
                ;; cl-quilt.lisp
                #:definition-signature
                #:%parse-quil
                ;; analysis/resolve-objects.lisp
                #:resolve-instruction
                #:resolve-objects
                #:*in-definition-body*
                ;; analysis/expansion.lisp
                #:flag-on-update
                #:instantiate-definition-body
                ;; analysis/expand-calibrations.lisp
                #:instantiate-instruction
                ;; analysis/type-safety.lisp
                #:type-check-instr
                #:enforce-mref-bounds
                #:find-descriptor-for-mref
                #:memory-segment-length
                )
  (:shadowing-import-from #:cl-quil/frontend
                          #:pi)
  (:local-nicknames (#:a    #:alexandria)
                    (#:quil #:cl-quil/frontend))

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
