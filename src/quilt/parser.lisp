;;;; src/quilt/parser.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:cl-quil/quilt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tokenization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype quilt-keyword ()
  '(member
    :PULSE :CAPTURE :RAW-CAPTURE :DELAY :FENCE
    :NONBLOCKING
    :DEFWAVEFORM :DEFCAL :DEFFRAME
    :SET-FREQUENCY :SHIFT-FREQUENCY :SET-PHASE :SHIFT-PHASE :SET-SCALE :SWAP-PHASE))

(deftype quilt-token-type ()
  '(or
    quilt-keyword
    quil:token-type))

(defun quilt-keyword-lexer (str)
  "Lexer extension for Quilt keywords. If the string STR is a Quilt keyword, a corresponding token will be returned. Otherwise, returns NIL."
  (let ((quilt-token-strings '("PULSE"
                               "CAPTURE"
                               "RAW-CAPTURE"
                               "DELAY"
                               "FENCE"
                               "NONBLOCKING"
                               "DEFWAVEFORM"
                               "DEFCAL"
                               "DEFFRAME"
                               "SET-FREQUENCY"
                               "SHIFT-FREQUENCY"
                               "SET-PHASE"
                               "SHIFT-PHASE"
                               "SET-SCALE"
                               "SWAP-PHASE")))
    (if (member str quilt-token-strings :test #'string=)
        (quil:tok (intern str :keyword))
        nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Objects ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-frame (toks)
  "Parse a frame from the list of tokens TOKS. Returns the frame and the remaining tokens."
  (multiple-value-bind (qubit-toks rest-toks)
      (take-until (lambda (tok) (eq ':STRING (quil:token-type tok)))
                  toks)
    (let ((qubits (mapcar #'parse-qubit qubit-toks)))
      (when (endp rest-toks)
        (quil-parse-error "Expected a frame name in ~A, but none were found. Did you forget quotes?"
                          quil:*parse-context*))
      (values
       (frame qubits (quil:token-payload (first rest-toks)))
       (rest rest-toks)))))

(defun parse-waveform-ref (toks)
  "Parse a waveform reference from the tokens TOKS. Returns the refererence and the remaining tokens."
  (when (endp toks)
    (quil-parse-error "Expected a waveform reference."))
  (let ((name-tok (first toks))
        (rest-toks (rest toks)))
    (unless (eq :NAME (quil:token-type name-tok))
      (quil-parse-error "Expected a NAME when parsing waveform reference~@[ in ~A~]."
                        name-tok))
    (if (endp rest-toks)
        (waveform-ref (quil:token-payload name-tok) '())
        (multiple-value-bind (param-alist rest)
            (parse-waveform-parameter-alist rest-toks)
          (values
           (waveform-ref (quil:token-payload name-tok) param-alist)
           rest)))))

(defun parse-waveform-parameter-and-value (toks)
  "Parse a <param>:<value> pair, consuming TOKS."
  (match-line ((name :NAME) (colon :COLON) &rest value-expr) (list toks)
    (cons
     (param (quil:token-payload name))
     (parse-parameter-or-expression value-expr))))

(defun parse-waveform-parameter-alist (params-args)
  "Parse waveform parameters and their assigned values from the list of tokens PARAMS-ARGS. Returns an association list and the remaining tokens."
  (unless (eq ':LEFT-PAREN (quil:token-type (first params-args)))
    (return-from parse-waveform-parameter-alist (values nil params-args)))

  ;; Remove :LEFT-PAREN
  (pop params-args)

  ;; Parse out the parameters enclosed.
  (multiple-value-bind (found-params rest-line)
      (take-while-from-end (lambda (x) (eq ':RIGHT-PAREN (quil:token-type x)))
                           params-args)
    
    ;; Error if we didn't find a right parenthesis.
    (when (endp rest-line)
      (quil-parse-error "No matching right paren in~@[ ~A~] parameters." quil:*parse-context*))

    ;; Remove right paren and stash away params.
    (pop rest-line)

    ;; Parse out the parameters.
    (let ((entries
            (split-sequence:split-sequence-if
             (lambda (tok)
               (eq ':COMMA (quil:token-type tok)))
             found-params)))
      (values (mapcar #'parse-waveform-parameter-and-value entries)
              rest-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Instructions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-pulse (tok-lines)
  "Parse a PULSE instruction from TOK-LINES. Returns the instruction object and the remaining lines."
  (match-line ((op :PULSE) &rest rest-toks) tok-lines
    (multiple-value-bind (frame rest-toks)
        (parse-frame rest-toks)
      (multiple-value-bind (waveform-ref rest)
          (parse-waveform-ref rest-toks)
        (unless (endp rest)
          (quil-parse-error "Unexpected token ~A at end of PULSE instruction." (first rest)))
        (make-instance 'pulse
                       :frame frame
                       :waveform waveform-ref)))))

(defun parse-delay (tok-lines)
  "Parse a DELAY instruction from TOK-LINES. Returns the instruction object, and the remaining lines."
  (match-line ((op :DELAY) &rest rest-toks) tok-lines
    (let (qubits
          frame-names
          duration)

      (multiple-value-bind (qubit-toks remaining)
          (take-until (lambda (tok)
                        (not (member (quil:token-type tok) '(:NAME :INTEGER))))
                      rest-toks)
        (when (endp qubit-toks)
          (quil-parse-error "Expected one or more qubits specified in DELAY instruction."))
        (setf qubits (mapcar #'parse-qubit qubit-toks))
        (setf rest-toks remaining))

      (multiple-value-bind (frame-name-toks duration-toks)
          (take-until (lambda (tok)
                        (not (eq ':STRING (quil:token-type tok))))
                      rest-toks)
        (when (endp duration-toks)
          (quil-parse-error "Expected a duration in DELAY instruction."))
        (setf frame-names (mapcar #'quil:token-payload frame-name-toks))
        (setf duration (parse-parameter-or-expression duration-toks)))

      (if frame-names
          (make-instance 'delay-on-frames
                         :duration duration
                         :frames (mapcar (lambda (name) (frame qubits name))
                                         frame-names))
          (make-instance 'delay-on-qubits
                         :duration duration
                         :qubits qubits)))))

(defun parse-fence (tok-lines)
  "Parse a FENCE instruction from TOK-LINES. Returns the instruction object, and the remaining lines."
  (match-line ((op :FENCE) &rest qubits) tok-lines
    (if (endp qubits)
        (make-instance 'fence-all)
        (make-instance 'fence
                       :qubits (mapcar #'parse-qubit qubits)))))

(defun parse-capture (tok-lines)
  "Parse a CAPTURE instruction from TOK-LINES. Returns the instruction object, and the remaining lines."
  (match-line ((op :CAPTURE) &rest rest-toks) tok-lines
    (multiple-value-bind (frame rest-toks)
        (parse-frame rest-toks)
      (unless (= 1 (length (frame-qubits frame)))
        (quil-parse-error "CAPTURE instruction is only applicable to single-qubit frames."))
      (when (endp rest-toks)
        (quil-parse-error "CAPTURE instruction is missing waveform reference."))
      (multiple-value-bind (waveform-ref rest-toks)
          (parse-waveform-ref rest-toks)
        (when (endp rest-toks)
          (quil-parse-error "CAPTURE instruction is missing memory reference."))
        (unless (endp (rest rest-toks))
          (quil-parse-error "Unexpected token ~A in CAPTURE" (cadr rest-toks)))
        (let ((address-obj (parse-memory-or-formal-token (first rest-toks)
                                                                 :ensure-valid t)))
          (make-instance 'capture
                         :frame frame
                         :waveform waveform-ref
                         :memory-ref address-obj))))))

(defun parse-raw-capture (tok-lines)
  "Parse a RAW-CAPTURE instruction from TOK-LINES. Returns the instruction object, and the remaining lines."
  (match-line ((op :RAW-CAPTURE) &rest rest-toks) tok-lines
    (multiple-value-bind (frame rest-toks)
        (parse-frame rest-toks)
      (unless (= 1 (length (frame-qubits frame)))
        (quil-parse-error "RAW-CAPTURE instruction is only applicable to single-qubit frames."))
      (unless (= 2 (length rest-toks))
        (quil-parse-error "Unexpected format for RAW-CAPTURE. Expected duration followed by a memory reference."))
      (let ((duration (parse-parameter-or-expression (butlast rest-toks)))
            (addr (parse-memory-or-formal-token (car (last rest-toks)) :ensure-valid t)))
        (unless (or (and (is-constant duration)
                         (realp (constant-value duration)))
                    (is-param duration))
          (quil-parse-error "Expected RAW-CAPTURE duration to be a real number or formal argument."))
        (make-instance 'raw-capture
                       :frame frame
                       :duration duration
                       :memory-ref addr)))))

(defun parse-nonblocking-op (tok-lines)
  "Parse a NONBLOCKING pulse/capture/raw-capture instruction from TOK-LINES. Returns the instruction object, and the remaining lines."
  (let* ((line (first tok-lines))
         (mod (first line))
         (op (second line)))
    (when (null op)
      (quil-parse-error "Unexpected line format. NONBLOCKING should be followed by a pulse or capture operation."))

    (unless (eq ':NONBLOCKING (quil:token-type mod))
      (quil:disappointing-token-error mod "NONBLOCKING"))

    (unless (member (quil:token-type op) '(:PULSE :CAPTURE :RAW-CAPTURE))
      (quil:disappointing-token-error op "a pulse or capture operation"))

    (multiple-value-bind (op-instr rest-lines)
        (quil:parse-program-lines (cons (rest line)
                                        (rest tok-lines)))
      (setf (nonblocking-p op-instr) t)
      (values op-instr
              rest-lines))))

(defun parse-simple-frame-mutation (tok-type tok-lines)
  "Parse a frame mutation instruction, as determined by TOK-TYPE, from TOK-LINES. Returns the instruction object, and the remaining lines."
  (match-line ((op tok-type) &rest rest-toks) tok-lines
    (multiple-value-bind (frame value-toks)
        (parse-frame rest-toks)
      (let ((result (parse-parameter-or-expression value-toks)))
        (make-instance
         (ecase tok-type
           (:SHIFT-FREQUENCY 'shift-frequency)
           (:SET-FREQUENCY 'set-frequency)
           (:SET-PHASE     'set-phase)
           (:SHIFT-PHASE   'shift-phase)
           (:SET-SCALE     'set-scale))
         :frame frame
         :value result)))))

(defun parse-swap-phase (tok-lines)
  "Parse a SWAP-PHASE instruction from TOK-LINES. Returns the instruction object, and the remaining lines."
  (match-line ((op :SWAP-PHASE) &rest rest-toks) tok-lines
    (multiple-value-bind (left-frame rest-toks)
        (parse-frame rest-toks)
      (multiple-value-bind (right-frame rest-toks)
          (parse-frame rest-toks)
        (unless (endp rest-toks)
          (quil-parse-error "Unexpected token ~A in SWAP-PHASE."
                            (quil:token-type (first rest-toks))))
        (make-instance 'swap-phase
                       :left-frame left-frame
                       :right-frame right-frame)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Frame Definitions

(defun parse-frame-definition (tok-lines)
  "Parse a frame definition from TOK-LINES."
  (when (null tok-lines)
    (quil-parse-error "EOF reached when frame definition was expected."))
  (destructuring-bind (defframe-tok . frame-toks) (first tok-lines)
    (unless (eq ':DEFFRAME (quil:token-type defframe-tok))
      (quil-parse-error "DEFFRAME expected. Got ~A."
                        (quil:token-type defframe-tok)))

    (when (null frame-toks)
      (quil-parse-error "Expected a frame after DEFFRAME token."))

    (multiple-value-bind (frame rest-line)
        (parse-frame frame-toks)
      ;; empty frame definition, no body
      (when (endp rest-line)
        (return-from parse-frame-definition
          (values
           (make-instance 'frame-definition
                          :frame frame
                          :context defframe-tok)
           (rest tok-lines))))
      ;; nonempty body, so we pull the parameters
      (symbol-macrolet ((next-line (cadr tok-lines)))
        (unless (and (= 1 (length rest-line))
                     (eq ':COLON (quil:token-type (first rest-line)))
                     ;; next token is indentation
                     (eq ':INDENT (quil:token-type (first next-line))))
          (quil-parse-error "Expected DEFFRAME line to be followed by a colon (:) and an indented body."))
        ;; remove the indent token
        (pop next-line))
      (multiple-value-bind (plist rest-lines)
          (parse-frame-definition-body (rest tok-lines))
        (values
         (apply #'make-instance 'frame-definition
                :frame frame
                :context defframe-tok
                plist)
         rest-lines)))))

(defun parse-sample-rate (toks)
  "Parse a sample rate from TOKS, consuming all tokens and returning a constant."
  (assert quil:*parse-context*)
  (unless toks
    (quil-parse-error "~A requires a sample rate, but none was given."
                      quil:*parse-context*))
  (when (< 1 (length toks))
    (quil-parse-error "~A sample rate may not be a compound arithmetic expression."
                      quil:*parse-context*))
  (let ((rate (parse-arithmetic-tokens toks)))
    (unless (realp rate)
      (quil-parse-error "Expected a real number for ~A sample rate, but got ~A instead."
                        quil:*parse-context*
                        rate))
    (unless (plusp rate)
      (quil-parse-error "Expected sample rate for ~A to be strictly positive, but got ~A instead."
                        quil:*parse-context*
                        rate))
    (constant rate)))

(defun parse-frame-definition-property-value (property-name value-toks)
  "Parse and validate a value associated with PROPERTY-NAME. Consumes VALUE-TOKS, and returns the parsed value."
  (case property-name
    ((:SAMPLE-RATE)
     (parse-sample-rate value-toks))
    ((:HARDWARE-OBJECT)
     (unless (and (= 1 (length value-toks))
                  (eq ':STRING (quil:token-type (first value-toks))))
       (quil-parse-error "Expected HARDWARE-OBJECT to be a string literal, but got ~{ ~A~}." value-toks))
     (quil:token-payload (first value-toks)))
    ((:DIRECTION)
     (unless (and (= 1 (length value-toks))
                  (eq ':STRING (quil:token-type (first value-toks))))
       (quil-parse-error "Expected DIRECTION to be a string literal, but got ~{ ~A~}." value-toks))
     (let ((direction (quil:token-payload (first value-toks))))
       (cond ((string= "tx" direction) ':TX)
             ((string= "rx" direction) ':RX)
             (t
              (quil-parse-error "Expected DIRECTION to be one of \"tx\" or \"rx\", but got \"~S.\"" direction)))))
    ((:INITIAL-FREQUENCY)
     (let ((freq (parse-parameter-or-expression value-toks)))
       (unless (and (is-constant freq)
                    (realp (constant-value freq)))
         (quil-parse-error "Expected INITIAL-FREQUENCY to be a real number, but got ~/quil:instruction-fmt/."
                           freq))
       (unless (plusp (constant-value freq))
         (warn "Expected INITIAL-FREQUENCY to be positive, but got ~/quil:instruction-fmt/."
               freq))
       freq))
    (otherwise
     (quil-parse-error "Unknown property ~A in DEFFRAME. Note: This is case sensitive."
                       property-name))))

(defun parse-frame-definition-body (lines &optional argument-plist)
  "Parse the body of a frame definition from the given LINES. Returns a plist of property-value pairs, along with the remaining lines."
  (when (endp lines)
    (return-from parse-frame-definition-body
      (values argument-plist lines)))
  (let ((line (first lines)))
    ;; we parse until DEDENT
    (when (eq ':DEDENT (quil:token-type (first line)))
      (pop (first lines))
      (return-from parse-frame-definition-body
        (values argument-plist lines)))
    (when (< 3 (length line))
      (quil-parse-error "DEFFRAME body lines should be of the form <property>: <value>."))
    (destructuring-bind (property-tok col-tok &rest value-toks) line
      (unless (eq ':NAME (quil:token-type property-tok))
        (quil:disappointing-token-error property-tok "an identifier"))
      (unless (eq ':COLON (quil:token-type col-tok))
        (quil:disappointing-token-error col-tok "a colon"))
      (let* ((property-name (intern (quil:token-payload property-tok)
                                    ':keyword))
             (property-value (parse-frame-definition-property-value property-name value-toks)))
        (when (getf argument-plist property-name)
          (quil-parse-error "Duplicate property ~A in DEFFRAME body." property-name))
        (parse-frame-definition-body (rest lines)
                                     (cons property-name
                                           (cons property-value
                                                 argument-plist)))))))


;;; Waveform Definitions

(defun parse-waveform-definition (tok-lines)
  "Parse a waveform definition from the token lines TOK-LINES."
  ;; Check that we have tokens left
  (when (null tok-lines)
    (quil-parse-error "EOF reached when waveform definition expected."))

  ;; Get the parameter and body lines
  (let (name
        sample-rate)
    (destructuring-bind (parameter-line &rest body-lines) tok-lines
      (destructuring-bind (op . params-args) parameter-line
        ;; Check that we are dealing with a DEFWAVEFORM.
        (unless (eq ':DEFWAVEFORM (quil:token-type op))
          (quil:disappointing-token-error op "DEFWAVEFORM"))

        ;; Check that something is following the DEFWAVEFORM.
        (when (null params-args)
          (quil-parse-error "Expected more after DEFWAVEFORM token."))

        ;; Check for a name.
        (unless (eq ':NAME (quil:token-type (first params-args)))
          (quil:disappointing-token-error (first params-args) "a name"))

        ;; We have a name. Stash it away.
        (setf name (quil:token-payload (pop params-args)))

        (multiple-value-bind (params rest-line) (parse-parameters params-args)

          (setf sample-rate (parse-sample-rate (butlast rest-line)))

          ;; Check for colon and incise it.
          (let ((maybe-colon (last rest-line)))
            (when (or (null maybe-colon)
                      (not (eq ':COLON (quil:token-type (first maybe-colon)))))
              (quil-parse-error "Expected a colon terminating the first line of DEFWAVEFORM.")))

          (let ((*arithmetic-parameters* nil)
                (*segment-encountered* nil))
            (multiple-value-bind (parsed-entries rest-lines)
                (parse-indented-entries body-lines :require-indent t)
              ;; Check that we only refered to parameters in our param list.
              (loop :for body-p :in (mapcar #'first *arithmetic-parameters*)
                    :unless (find (param-name body-p) params :key #'param-name
                                                             :test #'string=)
                      :do (quil-parse-error
                           "The parameter ~A was found in the body of the waveform definition of ~
                           ~A but wasn't in the declared parameter list."
                           (param-name body-p)
                           name))
              (let ((param-symbols
                      ;; Make sure we have symbols for everything. Collect
                      ;; a list of them in the same order as PARAMS.
                      (loop :for p :in params
                            :for found-p := (assoc (param-name p) *arithmetic-parameters*
                                                   :key #'param-name
                                                   :test #'string=)
                            :if (null found-p)
                              :collect (gensym (concatenate 'string (param-name p) "-UNUSED"))
                            :else
                              :collect (second found-p))))
                (values (make-waveform-definition name param-symbols parsed-entries sample-rate :context op)
                        rest-lines)))))))))

;;; Calibration Definitions

(defun parse-calibration-definition (tok-lines)
  "Parse out a calibration definition from the lines of tokens TOK-LINES."

  ;; Check that we have tokens left to parse.
  (when (null tok-lines)
    (quil-parse-error "EOF reached when calibration definition expected."))

  (destructuring-bind (parameter-line &rest body-lines) tok-lines
    ;; Check that we have a well-formed header line.
    (destructuring-bind (defcal-tok . rest-line) parameter-line
      ;; We must be working with a DEFCAL
      (unless (eq ':DEFCAL (quil:token-type defcal-tok))
        (quil-parse-error "DEFCAL expected. Got ~S"
                          (quil:token-type defcal-tok)))

      ;; Check that there are tokens following DEFCAL.
      (when (null rest-line)
        (quil-parse-error "Expected more after DEFCAL token."))

      (if (eq ':MEASURE (quil:token-type (first rest-line)))
          (parse-measurement-calibration-definition defcal-tok rest-line body-lines)
          (parse-gate-calibration-definition defcal-tok rest-line body-lines)))))

(defun parse-measurement-calibration-definition (defcal-tok header rest-lines)
  "Parse a measurement calibration definition from the HEADER line and the REST-LINES (which includes the body)."
  ;; Pop off the "MEASURE" tok
  (pop header)
  ;; Check for colon and incise it.
  (let ((maybe-colon (last header)))
    (when (or (null maybe-colon)
              (not (eq ':COLON (quil:token-type (first maybe-colon)))))
      (quil-parse-error "Expected a colon in DEFCAL."))
    (setf header (butlast header)))

  (when (= 0 (length header))
    (quil-parse-error "DEFCAL MEASURE requires a target qubit (and optionally an address)."))

  (when (< 2 (length header))
    (quil-parse-error "Too many arguments for DEFCAL MEASURE."))

  (let* ((qubit (parse-qubit (first header)))
         (address-tok (second header))
         (formal-address (if address-tok
                             (case (quil:token-type address-tok)
                               ((:NAME)
                                (formal (quil:token-payload address-tok)))
                               ((:AREF)
                                (disappointing-token-error address-tok
                                                                 "a name (not an actual memory reference)"))
                               (otherwise
                                (disappointing-token-error address-tok
                                                                 "a formal address argument")))
                             nil)))
    (multiple-value-bind (parsed-body remaining-lines)
        (parse-indented-body rest-lines)
      (values
       (if formal-address
           (make-instance 'measure-calibration-definition
                          :qubit qubit
                          :address formal-address
                          :body parsed-body
                          :context defcal-tok)
           (make-instance 'measure-discard-calibration-definition
                          :qubit qubit
                          :body parsed-body
                          :context defcal-tok))
       remaining-lines))))

(defun parse-gate-calibration-definition (defcal-tok header rest-lines)
  "Parse a gate calibration definition from the HEADER line and the REST-LINES (which includes the body)."
  ;; Build an operator description, consuming tokens from HEADER
  (let (operator
        arguments)
    (multiple-value-bind (modifiers remaining)
        (take-until (complement #'gate-modifier-token-p) header)

      (setf header remaining)

      ;; Check for name.
      (unless (eq ':NAME (quil:token-type (first header)))
        (quil-parse-error "Expected a name for the gate calibration."))

      (let ((name (quil:token-payload (pop header))))
        (setf operator (apply-modifiers-to-operator
                        (mapcar (lambda (tok)
                                  (list (quil:token-type tok)))
                                (reverse modifiers))
                        (named-operator name)))))
    (let ((*formal-arguments-allowed* t))
      (multiple-value-bind (params rest-line) (parse-parameters header :allow-expressions t)
        (dolist (param params)
          (unless (or (is-constant param)
                      (is-param param))
            (quil-parse-error "Unexpected parameter type ~A in DEFCAL."
                              (type-of param))))

        ;; Check for colon and incise it.
        (let ((maybe-colon (last rest-line)))
          (when (or (null maybe-colon)
                    (not (eq ':COLON (quil:token-type (first maybe-colon)))))
            (quil-parse-error "Expected a colon in DEFCAL."))
          (setf rest-line (butlast rest-line)))

        ;; Collect arguments and stash them away.
        (setf arguments (mapcar #'parse-qubit rest-line))

        (multiple-value-bind (parsed-body remaining-lines)
            (parse-indented-body rest-lines)
          (values
           (make-instance 'gate-calibration-definition
                          :operator operator
                          :parameters params
                          :arguments arguments
                          :body parsed-body
                          :context defcal-tok)
           remaining-lines))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Quil Parser Extension ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-quilt-program-lines (tok-lines)
  "Quil parser extension, which parses the next Quilt instruction from TOK-LINES."
  (let* ((line (first tok-lines))
         (tok (first line))
         (tok-type (quil:token-type tok)))

    (case tok-type
      ((:SHIFT-FREQUENCY :SET-FREQUENCY :SET-PHASE :SHIFT-PHASE :SET-SCALE)
       (parse-simple-frame-mutation tok-type tok-lines))

      ((:SWAP-PHASE)
       (parse-swap-phase tok-lines))

      ((:PULSE)
       (parse-pulse tok-lines))

      ((:DELAY)
       (parse-delay tok-lines))

      ((:FENCE)
       (parse-fence tok-lines))

      ((:CAPTURE)
       (parse-capture tok-lines))

      ((:RAW-CAPTURE)
       (parse-raw-capture tok-lines))

      ((:NONBLOCKING)
       (parse-nonblocking-op tok-lines))

      ((:DEFWAVEFORM)
       (unless *definitions-allowed*
         (quil-parse-error "Found DEFWAVEFORM where it's not allowed."))

       (let ((*definitions-allowed* nil)
             (*formal-arguments-allowed* t))
         (parse-waveform-definition tok-lines)))

      ((:DEFCAL)
       (unless *definitions-allowed*
         (quil-parse-error "Found DEFCAL where it's not allowed."))

       (let ((*definitions-allowed* nil)
             (*formal-arguments-allowed* t))
         (parse-calibration-definition tok-lines)))

      ((:DEFFRAME)
       (unless *definitions-allowed*
         (quil-parse-error "Found DEFFRAME where it's not allowed."))

       (let ((*definitions-allowed* nil)
             (*formal-arguments-allowed* nil))
         (parse-frame-definition tok-lines)))

      (otherwise nil))))
