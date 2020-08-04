;;;; entry-point.lisp
;;;;
;;;; Binary app interface entry point.
;;;;
;;;; Author: Eric Peterson
;;;;

(in-package #:quilc)

(defun slurp-lines (&optional (stream *standard-input*))
  (flet ((line () (read-line stream nil nil nil)))
    (with-output-to-string (s)
      (loop :for line := (line) :then (line)
            :while line
            :do (write-line line s)))))

(defun reload-foreign-libraries ()
  (locally
      (declare #+sbcl (sb-ext:muffle-conditions style-warning))
    (handler-bind (#+sbcl (style-warning #'muffle-warning))
      (cffi:load-foreign-library 'magicl.foreign-libraries::libblas)
      (cffi:load-foreign-library 'magicl.foreign-libraries::liblapack))))

(defun print-matrix-with-comment-hashes (matrix &optional (stream *standard-output*))
  (format stream "~D"
          (cl-ppcre:regex-replace-all
           (coerce #(#\Newline) 'string)
           (with-output-to-string (s)
             (princ matrix s))
           (coerce #(#\Newline #\#) 'string))))

(defun lookup-isa-descriptor-for-name (isa)
  (a:switch (isa :test #'string=)
    ("8Q" (quil::build-8Q-chip))
    ("20Q" (quil::build-skew-rectangular-chip 0 4 5))
    ("16QMUX" (quil::build-16QMUX-chip))
    ("bristlecone" (quil::build-bristlecone-chip))
    ("ibmqx5" (quil::build-ibm-qx5))
    (t
     (if (probe-file isa)
         (quil::read-chip-spec-file isa)
         (error "ISA descriptor does not name a known template or an extant file.")))))

(defun log-level-string-to-symbol (log-level)
  (a:eswitch (log-level :test #'string=)
    ("debug" :debug)
    ("info" :info)
    ("notice" :notice)
    ("warning" :warning)
    ("err" :err)
    ("crit" :crit)
    ("alert" :alert)
    ("emerg" :emerg)))

(defvar *nick-banner* t)

(defun show-banner ()
  (cond
    (*nick-banner*
     (format t "~
+-----------------+
|  W E L C O M E  |
|   T O   T H E   |
|  R I G E T T I  |
|     Q U I L     |
| C O M P I L E R |
+-----------------+"))
    (t
     (format t "~
****************************************
* Welcome to the Rigetti Quil Compiler *
****************************************")))
  (format t "~%Copyright (c) 2016-2020 Rigetti Computing.~2%")
  #+forest-sdk
  (format t "This is a part of the Forest SDK. By using this program~%~
             you agree to the End User License Agreement (EULA) supplied~%~
             with this program. If you did not receive the EULA, please~%~
             contact <support@rigetti.com>.~2%"))

(defun show-help ()
  (format t "Usage:~%")
  (format t "  ~A [options]~%" *program-name*)
  (format t "Options:~%")
  (command-line-arguments:show-option-help (append *option-spec* *deprecated-option-spec*)
                                           :sort-names t))

(defun show-version ()
  (format t "~A [~A]~%" +QUILC-VERSION+ +GIT-HASH+))

(defun show-backends ()
  (format t "Available backends:~%")
  (format t "~{  ~(~A~)~^~%~}~%" (mapcar #'quil:backend-name (quil:list-available-backends))))

(defun check-libraries ()
  "Check that the foreign libraries are adequate. Exits with status
  0 if so, 1 if not."
  #+sbcl
  (format t "Loaded libraries:~%~{  ~A~%~}~%"
          (mapcar 'sb-alien::shared-object-pathname sb-sys:*shared-objects*))
  (unless (magicl.foreign-libraries:foreign-symbol-available-p "zuncsd_"
                                                               'magicl.foreign-libraries:liblapack)
    (format t "The loaded version of LAPACK is missing functionality. The compiler will still work with your current LAPACK but it is advisable to install a more complete version.~%")
    (uiop:quit 1))
  (format t "Library check passed.~%")
  (uiop:quit 0))

(defun benchmarks ()
  (uiop:quit (if (cl-quil-benchmarking:run-benchmarks :verbose t)
                 0 1)))

(defun command-line-debugger (condition previous-hook)
  (declare (ignore previous-hook))
  (format *error-output* "~&Fatal ~A: ~%  ~A~%"
          (type-of condition)
          condition)
  (force-output *error-output*)
  (uiop:quit 1))

(defun setup-debugger ()
  #+forest-sdk
  (setf *debugger-hook* 'command-line-debugger)
  #-forest-sdk
  (disable-debugger))

(defun entry-point (argv)
  ;; TODO Both figure out how to distribute tweedledum that works
  ;; everywhere (or almost everywhere), and enable some sort of
  ;; "contrib" architecture that can be used to selectively
  ;; enable/disable these not-strictly-mandatory features.

  ;; #-win32
  ;; (handler-bind ((warning #'muffle-warning))
  ;;   (uiop:symbol-call ':cl-quil.tweedledum '#:load-tweedledum))
  (handler-case
      (%entry-point argv)
    (interactive-interrupt (c)
      (declare (ignore c))
      (format *error-output* "~&! ! ! Caught keyboard interrupt. Exiting.~%")
      (uiop:quit 0))
    (error (c)
      (format *error-output* "~&! ! ! Error: ~A~%" c)
      (uiop:quit 1))))

(defun %entry-point (argv)
  (let ((*program-name* (pop argv)))
    (command-line-arguments:handle-command-line
     (append *option-spec* *deprecated-option-spec* *ignored-option-spec*)
     'process-options
     :command-line argv
     :name *program-name*
     :positional-arity 0
     :rest-arity nil)))

(defmacro special-bindings-let* (let-defs &body body)
  "Bind LET-DEFS as in LET, and add those LET-DEFS to bordeaux-threads:*default-special-bindings* in the same LET."
  `(let* (,@(loop :for (name value) :in let-defs
                  :collect `(,name ,value))
          (bordeaux-threads:*default-special-bindings*
            (list* ,@(loop :for (name value) :in let-defs
                           :collect `(cons ',name (list 'quote ,name)))
                   bordeaux-threads:*default-special-bindings*)))
     ,@body))

(defun warn-deprecated-options (args)
  "Print a warning message if the server is started with deprecated options (see *DEPRECATED-OPTION-SPEC*)."
  (loop :for kw :in args :by #'cddr
        :for found := (find kw *deprecated-option-spec* :test #'string-equal :key #'caar)
        :when found :do
          (format *error-output* "The option --~A is deprecated and will be removed. See --help for more information.~%"
                  (caar found))))

(defun warn-ignored-options (args)
  "Print a warning message if the server is started with deprecated options (see *DEPRECATED-OPTION-SPEC*)."
  (loop :for kw :in args :by #'cddr
        :for found := (find kw *ignored-option-spec* :test #'string-equal :key #'caar)
        :when found :do
          (format *error-output* "The option --~A is deprecated and disabled, and will be removed. See --help for more information.~%"
                  (caar found))))

(defun process-options (&rest all-args
                        &key
                          (prefer-gate-ladders nil)
                          (compute-gate-depth nil)
                          (compute-gate-volume nil)
                          (compute-runtime nil)
                          (compute-fidelity nil)
                          (compute-matrix-reps nil)
                          (compute-2Q-gate-depth nil)
                          (compute-unused-qubits nil)
                          (show-topological-overhead nil)
                          (json-serialize nil)
                          (gate-blacklist nil)
                          (gate-whitelist nil)
                          (without-pretty-printing nil)
                          (print-logical-schedule nil)
                          (verbose nil)
                          (isa nil)
                          ;; Backend-related
                          (compile nil)
                          (backend nil)
                          (list-backends nil)
                          (output nil)

                          (enable-state-prep-reductions nil)
                          (protoquil nil)
                          (print-statistics nil)
                          (print-circuit-definitions nil)
                          (version nil)
                          (check-libraries nil)
                          #-forest-sdk
                          (benchmark nil)
                          (server-mode-rpc nil)
                          (host nil)
                          (port nil)
                          time-limit
                          (help nil)
                          (log-level nil)
                          (quiet nil)
                          (check-sdk-version nil)
                          (proxy nil)
                          (safe-include-directory nil)
                          (swank-port nil))
  ;; Deprecated options.
  (declare (ignore compute-gate-depth compute-gate-volume compute-runtime
                   compute-fidelity compute-2Q-gate-depth compute-unused-qubits
                   show-topological-overhead print-logical-schedule json-serialize))

  (warn-deprecated-options all-args)
  (warn-ignored-options all-args)

  ;;
  ;; Handle trivial print-then-exit arguments
  ;;

  (when help
    (show-help)
    (uiop:quit 0))

  (when version
    (show-version)
    (uiop:quit 0))

  (when list-backends
    (show-backends)
    (uiop:quit 0))

  (when check-libraries
    (check-libraries))

  ;;
  ;; Enable 'passive' feature arguments (logging, etc.)
  ;;

  (when log-level
    (setf *log-level* (log-level-string-to-symbol log-level)))

  ;; Start Swank if we were asked. Re-enable the debugger.
  #-forest-sdk
  (when swank-port
    (enable-debugger)
    (setf swank:*use-dedicated-output-stream* nil)
    (swank:create-server :port swank-port
                         :dont-close t))

  (setf *logger*
        (make-instance 'cl-syslog:rfc5424-logger
                       :app-name "quilc"
                       :facility ':local0
                       :maximum-priority *log-level*
                       :log-writer
                       #+windows (cl-syslog:stream-log-writer)
                       #-windows (cl-syslog:tee-to-stream
                                  (cl-syslog:syslog-log-writer "quilc" :local0)
                                  *error-output*)))


  ;;
  ;; Handle non-trivial print-then-exit arguments
  ;;

  #-forest-sdk
  (when benchmark
    (benchmarks))

  ;;
  ;; Validate argument values
  ;;

  (when (minusp time-limit)
    (error "A negative value (~D) was provided for the server time-limit." time-limit))

  (unless (or (null safe-include-directory)
              (uiop:directory-pathname-p safe-include-directory))
    (error "--safe-include-directory must point to a directory. Got ~S. Did you ~
            forget a trailing slash?"
           safe-include-directory))

  ;;
  ;; Now we're cooking with fire
  ;;

  (special-bindings-let*
      ((*log-level* (or (and log-level (log-level-string-to-symbol log-level))
                        *log-level*))
       (*logger* (make-instance 'cl-syslog:rfc5424-logger
                                :app-name "quilc"
                                :facility ':local0
                                :maximum-priority *log-level*
                                :log-writer
                                #+windows (cl-syslog:stream-log-writer)
                                #-windows (cl-syslog:tee-to-stream
                                           (cl-syslog:syslog-log-writer "quilc" :local0)
                                           *error-output*)))
       (quil::*prefer-ranged-gates-to-SWAPs* prefer-gate-ladders)
       (*without-pretty-printing* without-pretty-printing)
       (quil::*enable-state-prep-compression* enable-state-prep-reductions)
       ;; Null out the streams. If no server mode is requested, these bindings will be modified
       ;; before calling run-CLI-mode, below.
       (*human-readable-stream* (make-broadcast-stream))
       (*quil-stream* (make-broadcast-stream))
       (*protoquil* protoquil)
       (*state-aware* enable-state-prep-reductions)
       (quil::*safe-include-directory* safe-include-directory))

    (when check-sdk-version
      (asynchronously-indicate-update-availability +QUILC-VERSION+ :proxy proxy))
    ;;
    ;; At this point we know we're doing something. Strap in LAPACK.
    ;;

    (magicl:with-blapack
      (reload-foreign-libraries)

      (cond
        ;;
        ;; RPCQ server mode requested
        ;;
        (server-mode-rpc
         (unless quiet
           (show-banner))

         (cl-syslog:rfc-log (*logger* :info "Launching quilc.")
           (:msgid "LOG0001"))

         (when (or compile backend output)
           (cl-syslog:rfc-log (*logger* :warning "--backend and --output-file/-o options ~
                                                  don't make sense when using server mode. ~
                                                  Ignoring them.")))
         ;; launch the polling loop
         (start-rpc-server :host host
                           :port port
                           :logger *logger*
                           :time-limit time-limit))

        ;;
        ;; server modes not requested, so continue parsing arguments
        ;;
        (t
         (setf *human-readable-stream* *error-output*)
         (setf *quil-stream* *standard-output*)

         (let* ((program-text (slurp-lines))
                (program (safely-parse-quil program-text))
                (original-matrix (when (and protoquil compute-matrix-reps)
                                   (parsed-program-to-logical-matrix program)))
                (chip-spec (lookup-isa-descriptor-for-name isa)))
           (multiple-value-bind (processed-program statistics)
               (process-program program chip-spec
                                :protoquil protoquil
                                :verbose verbose
                                :gate-whitelist (and gate-whitelist
                                                     (split-sequence:split-sequence
                                                      #\,
                                                      (remove #\Space gate-whitelist)
                                                      :remove-empty-subseqs t))
                                :gate-blacklist (and gate-blacklist
                                                     (split-sequence:split-sequence
                                                      #\,
                                                      (remove #\Space gate-blacklist)
                                                      :remove-empty-subseqs t)))

             ;; NOTE: This flow is deprecated and will be merged with
             ;; quil-backend in the future.
             (unless compile
               ;; If we want circuit definitons, keep them. Otherwise
               ;; delete so they don't get printed.
               (unless print-circuit-definitions
                 (setf (parsed-program-circuit-definitions processed-program) nil))

               ;; Print the program to stdout
               (print-program processed-program *quil-stream*)

               ;; If we are using protoquil (no control flow), then we
               ;; can print statistics about the program as comments at
               ;; the end of the output.
               (when (and protoquil print-statistics)
                 (print-statistics statistics *quil-stream*))

               ;; If we are using protoquil (no control flow), then we
               ;; can print the input and output unitary matrix as
               ;; comments at the end of the output.
               (when (and protoquil compute-matrix-reps)
                 (let* ((processed-program-matrix (parsed-program-to-logical-matrix processed-program :compress-qubits t)))
                   (print-matrix-comparision original-matrix
                                             (quil::scale-out-matrix-phases processed-program-matrix
                                                                            original-matrix)))))

             ;; New and improved flow
             (when compile
               (unless backend
                 (error "Backend must be provided when compilation is enabled. For a list of available backends, run 'quilc --list-backends'."))

               (unless output
                 (error "Output must be provided when compilation is enabled. Specify an output file with -o or --output."))

               (let ((backend-class (quil:find-backend backend)))
                 (unless backend-class
                   (error "The backend value '~a' does not name an available backend. For a list of available backends, run 'quilc --list-backends'." backend))

                 ;; TODO: Initialize backend with options. This is
                 ;;       currently blocked by the command line
                 ;;       argument parser not supporting multiple of
                 ;;       the same argument as a list (like gcc does)

                 ;; TODO: Compute and pass statistics to backend when
                 ;; using protoquil
                 (let ((backend (make-instance backend-class)))
                   (unless (quil:backend-supports-chip-p backend chip-spec)
                     (error "The backend provided does not support this ISA."))
                   (backend-compile processed-program chip-spec backend output)))))))))))

(defun backend-compile (program chip-spec backend output)
  "Compile the processed program PROGRAM for BACKEND, writing to OUTPUT."
  (let ((stream))
    (unwind-protect
         (progn
           (setf stream (open output :direction ':output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists ':supersede
                                     :if-does-not-exist ':create))
           (let ((executable (quil:backend-compile program chip-spec backend)))
             (quil:write-executable executable stream)))
      (when stream
        (close stream)))))

(defun process-program (program chip-specification
                        &key
                          protoquil
                          state-aware
                          verbose
                          gate-whitelist
                          gate-blacklist)
  "Compile PROGRAM for the chip CHIP-SPECIFICATION. Optionally calculate statistics described by the keyword arguments. All require :PROTOQUIL T.

Note: PROGRAM is mutated by the compilation process. To avoid this, use COPY-INSTANCE.

Returns a values tuple (PROCESSED-PROGRAM, STATISTICS), where PROCESSED-PROGRAM is the compiled program, and STATISTICS is a HASH-TABLE whose keys are the slots of the RPCQ::|NativeQuilMetadata| class."
  (let* ((statistics (make-hash-table :test #'equal))
         (quil::*compiler-noise* (and verbose *human-readable-stream*))
         (*random-state* (make-random-state t))
         (quil::*enable-state-prep-compression* state-aware))
    ;; do the compilation
    (multiple-value-bind (processed-program topological-swaps)
        (compiler-hook program chip-specification :protoquil protoquil :destructive t)

      (when protoquil
        ;; if we're supposed to output protoQuil, we strip circuit and gate definitions
        (setf (parsed-program-circuit-definitions processed-program) nil
              (parsed-program-gate-definitions processed-program) nil)

        ;; if we're supposed to output protoQuil, we also need to strip the final HALT
        ;; instructions from the output
        (setf (parsed-program-executable-code processed-program)
              (strip-final-halt-respecting-rewirings processed-program))

        ;; Compute statistics for protoquil program
        (compute-statistics processed-program chip-specification statistics :gate-whitelist gate-whitelist :gate-blacklist gate-blacklist)
        (setf (gethash "topological_swaps" statistics) topological-swaps))

      (values processed-program statistics))))

;;
;; Functions for compiling to protoquil
;;

(defun compute-statistics (processed-program chip-specification statistics &key gate-whitelist gate-blacklist)
  "Compute statistics about protoquil program PROCESSED-PROGRAM.

This function will have undefined behavior when PROCESSED-PROGRAM is not protoquil."
  (setf (gethash "final_rewiring" statistics)
        (quil::extract-final-exit-rewiring-vector processed-program))

  (let ((lschedule (quil::make-lscheduler)))
    (loop :for instr :across (parsed-program-executable-code processed-program)
          :unless (typep instr 'pragma)
            :do (quil::append-instruction-to-lschedule lschedule instr))
    (setf (gethash "logical_schedule" statistics)
          lschedule))

  ;; gate depth, gate volume, duration, and fidelity stats can
  ;; all share an lschedule
  (let ((lschedule (quil::make-lscheduler)))
    (loop :for instr :across (parsed-program-executable-code processed-program)
          :when (and (typep instr 'gate-application)
                     (not (member (cl-quil::application-operator-root-name instr)
                                  gate-blacklist
                                  :test #'string=))
                     (or (null gate-whitelist)
                         (member (cl-quil::application-operator-root-name instr)
                                 gate-whitelist
                                 :test #'string=)))
            :do (quil::append-instruction-to-lschedule lschedule instr))

    (setf (gethash "gate_depth" statistics)
          (quil::lscheduler-calculate-depth lschedule))

    (setf (gethash "gate_volume" statistics)
          (quil::lscheduler-calculate-volume lschedule))

    (setf (gethash "program_duration" statistics)
          (quil::lscheduler-calculate-duration lschedule chip-specification))

    (setf (gethash "program_fidelity" statistics)
          (quil::lscheduler-calculate-fidelity lschedule chip-specification))

    (let* ((lscheduler-resources
             (let ((collect (quil::make-null-resource)))
               (quil::lscheduler-walk-graph
                lschedule
                :bump-value (lambda (instr value)
                              (setf collect
                                    (quil::resource-union collect
                                                          (quil::instruction-resources instr)))
                              value))
               collect))
           (unused-qubits
             (loop :for i :below (quil::chip-spec-n-qubits chip-specification)
                   :unless (quil::resources-intersect-p (quil::make-qubit-resource i)
                                                        lscheduler-resources)
                     :collect i)))
      (setf (gethash "unused_qubits" statistics)
            unused-qubits)))

  ;; multiq gate depth requires a separate lschedule
  (let ((lschedule (quil::make-lscheduler)))
    (loop :for instr :across (parsed-program-executable-code processed-program)
          :when (and (typep instr 'gate-application)
                     (<= 2 (length (application-arguments instr))))
            :do (quil::append-instruction-to-lschedule lschedule instr)
          :finally
             (setf (gethash "multiqubit_gate_depth" statistics)
                   (quil::lscheduler-calculate-depth lschedule))))

  statistics)

(defun strip-final-halt-respecting-rewirings (processed-program)
  "Remove the final HALT instruction, if any, from PROCESSED-PROGRAM, retaining any attached rewiring comments."
  (let* ((instructions (parsed-program-executable-code processed-program))
         (last-instruction (and (plusp (length instructions))
                                (quil::nth-instr 0 processed-program :from-end t)))
         (penultimate-instruction (and (< 1 (length instructions))
                                       (quil::nth-instr 1 processed-program :from-end t)))
         (must-transfer-comment-p (and (not (null penultimate-instruction))
                                       (comment last-instruction))))

    (unless (quil::haltp last-instruction)
      (return-from strip-final-halt-respecting-rewirings instructions))

    (when must-transfer-comment-p
      ;; Transfer the rewiring comment from LAST-INSTRUCTION to PENULTIMATE-INSTRUCTION.
      (multiple-value-bind (last-entering last-exiting)
          (quil::instruction-rewirings last-instruction)
        (multiple-value-bind (penultimate-entering penultimate-exiting)
            (quil::instruction-rewirings penultimate-instruction)
          (flet ((assert-rewirings-compatible (rewiring-type last-rewiring penultimate-rewiring)
                   ;; This bit of hoop-jumping guards against the unlikely event that both
                   ;; PENULTIMATE-INSTRUCTION and LAST-INSTRUCTION have rewiring comments attached
                   ;; which might be incompatible. We check to ensure that either one of the
                   ;; rewirings is NULL, or else they are EQUALP and can safely be merged.
                   (assert (or (or (null last-rewiring)
                                   (null penultimate-rewiring))
                               (equalp last-rewiring penultimate-rewiring))
                           ()
                           "Failed to strip final HALT. Instructions have incompatible ~A rewirings:~@
                           LAST: ~A ~A~@
                           PREV: ~A ~A"
                           rewiring-type last-instruction last-rewiring
                           penultimate-instruction penultimate-rewiring)))
            (assert-rewirings-compatible ':ENTERING last-entering penultimate-entering)
            (assert-rewirings-compatible ':EXITING last-exiting penultimate-exiting))
          ;; Consider the following cases for the :ENTERING rewirings (the same case analysis
          ;; applies to the :EXITING rewiring pair as well).
          ;;
          ;; 1) If both the rewirings are non-NIL, then the ASSERT-REWIRINGS-COMPATIBLE check above
          ;;    guarantees that they are EQUALP, and it doesn't matter which one we select.
          ;;
          ;; 2) If only one is non-NIL, the OR selects it.
          ;;
          ;; 3) If both are NIL, then MAKE-REWIRING-COMMENT just ignores that keyword argument, and
          ;;    returns an :EXITING rewiring.
          ;;
          ;; Finally, (COMMENT LAST-INSTRUCTION) is non-NIL (otherwise MUST-TRANSFER-COMMENT-P would
          ;; be NIL), so at least one of LAST-ENTERING and LAST-EXITING is non-NIL, which means that
          ;; at least one of the :ENTERING and :EXITING keyword args to MAKE-REWIRING-COMMENT is
          ;; non-NIL and hence the call will produce a rewiring comment.
          (setf (comment penultimate-instruction)
                (quil::make-rewiring-comment :entering (or last-entering penultimate-entering)
                                             :exiting (or last-exiting penultimate-exiting))))))

    ;; Strip the final HALT instruction.
    (subseq instructions 0 (1- (length instructions)))))
