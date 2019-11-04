;;;; entry-point.lisp
;;;;
;;;; Binary app interface entry point.
;;;;
;;;; Author: Eric Peterson
;;;;

(in-package #:quilc)

(defparameter *option-spec*
  '((("prefer-gate-ladders")
     :type boolean
     :optional t
     :documentation "uses gate ladders rather than SWAPs to implement long-ranged gates")

    (("gate-blacklist")
     :type string
     :optional t
     :documentation "when calculating statistics, ignore these (comma-separated) gates")

    (("gate-whitelist")
     :type string
     :optional t
     :documentation "when calculating statistics, consider only these (comma-separated) gates")

    (("without-pretty-printing")
     :type boolean
     :optional t
     :documentation "turns off pretty-printing features")

    (("verbose")
     :type boolean
     :optional t
     :documentation "verbose compiler trace output")

    (("isa")
     :type string
     :optional t
     :initial-value "8Q"
     :documentation "set ISA to one of \"8Q\", \"20Q\", \"16QMUX\", \"bristlecone\", \"ibmqx5\", or path to QPU description file")

    (("enable-state-prep-reductions")
     :type boolean
     :optional t
     :documentation "assume that the program starts in the ground state")

    (("protoquil" #\P)
     :type boolean
     :optional t
     :documentation "restrict input/output to ProtoQuil")

    (("print-statistics"
      :type boolean
      :optional t
      :documentation "print program statistics.  Requires -P."))

    (("compute-matrix-reps" #\m)
     :type boolean
     :optional t
     :documentation "prints matrix representations for comparison  Requires -P.  This is deprecated and will eventually be removed.")
    
    (("help" #\h)
     :type boolean
     :optional t
     :documentation "print this help information and exit")

    (("server-mode-rpc" #\R #\S)
     :type boolean
     :optional t
     :documentation "run as an RPCQ server")

    (("host")
     :type string
     :initial-value "*"
     :optional t
     :documentation "host on which to run the RPCQ server")

    (("port" #\p)
     :type integer
     :initial-value 5555
     :optional t
     :documentation "port to run the RPCQ server on")

    (("time-limit")
     :type integer
     :initial-value 0
     :documentation "time limit (in seconds) for server requests (0 => unlimited)")

    (("version" #\v)
     :type boolean
     :optional t
     :documentation "print version information")

    (("check-libraries")
     :type boolean
     :optional t
     :documentation "check that foreign libraries are adequate")

    #-forest-sdk
    (("benchmark")
     :type boolean
     :optional t
     :documentation "run benchmarks and print results")

    (("log-level")
     :type string
     :optional t
     :initial-value "info"
     :documentation "maximum logging level (\"debug\", \"info\", \"notice\", \"warning\", \"err\", \"crit\", \"alert\", or \"emerg\") (default \"info\")")

    (("quiet")
     :type boolean
     :optional t
     :initial-value nil
     :documentation "Disable all non-logging output (banner, etc.)")

    (("check-sdk-version")
     :type boolean
     :optional t
     :initial-value t
     :documentation "Check for a new SDK version at launch.")

    (("proxy")
     :type string
     :optional t
     :initial-value nil
     :documentation "Proxy to use when checking for an SDK update."))
  "Supported and non-deprecated options.")

(defparameter *deprecated-option-spec*
  '((("compute-gate-depth" #\d)
     :type boolean
     :optional t
     :documentation "prints compiled circuit gate depth (longest subsequece of data-sharing compiled instructions).  Requires -P.  This is deprecated and will eventually be removed. See --print-statistics.")

    (("compute-gate-volume")
     :type boolean
     :optional t
     :documentation "prints compiled circuit gate volume (number of gates).  Requires -P.  This is deprecated and will eventually be removed.  See --print-statistics.")

    (("compute-runtime" #\r)
     :type boolean
     :optional t
     :documentation "prints compiled circuit expected runtime.  Requires -P.  This is deprecated and will eventually be removed.  See --print-statistics.")

    (("compute-fidelity" #\f)
     :type boolean
     :optional t
     :documentation "prints approximate compiled circuit fidelity.  Requires -P.  This is deprecated and will eventually be removed.  See --print-statistics.")
    
    (("compute-2Q-gate-depth" #\2)
     :type boolean
     :optional t
     :documentation "prints compiled circuit multiqubit gate depth; ignores white/blacklists. Requires -P.  This is deprecated and will eventually be removed.  See --print-statistics.")
    
    (("compute-unused-qubits" #\u)
     :type boolean
     :optional t
     :documentation "prints unused qubits.  Requires -P.  This is deprecated and will eventually be removed.  See --print-statistics.")
    
    (("show-topological-overhead" #\t)
     :type boolean
     :optional t
     :documentation "prints the number of SWAPs incurred for topological reasons.  This is deprecated and will eventually be removed.  See --print-statistics."))
  "Supported and deprecated options.")

(defparameter *ignored-option-spec*
  '((("print-logical-schedule" #\s)
     :type boolean
     :optional t
     :documentation "include logically parallelized schedule in output.  Requires -P.  This is inactive and will eventually be removed.")
    
    (("json-serialize" #\j)
     :type boolean
     :optional t
     :documentation "serialize output as a JSON object"))
  "Inactive and deprecated options.")

(defparameter *retired-option-spec*
  '()
  "Invalid options.")

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
  (format t "~%Copyright (c) 2016-2019 Rigetti Computing.~2%")
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

(defun check-libraries ()
  "Check that the foreign libraries are adequate. Exits with status
  0 if so, 1 if not."
  #+sbcl
  (format t "Loaded libraries:~%~{  ~A~%~}~%"
          (mapcar 'sb-alien::shared-object-pathname sb-sys:*shared-objects*))
  (unless (magicl.foreign-libraries:foreign-symbol-available-p "zuncsd_"
                                                               'magicl.foreign-libraries:liblapack)
    (format t "The loaded version of LAPACK is missing necessary functionality.~%")
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
  #-win32
  (handler-bind ((warning #'muffle-warning))
    (uiop:symbol-call ':cl-quil.tweedledum '#:load-tweedledum))
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
                          (enable-state-prep-reductions nil)
                          (protoquil nil)
                          (print-statistics nil)
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
                          (proxy nil))
  ;; Deprecated options.
  (declare (ignore compute-gate-depth compute-gate-volume compute-runtime
                   compute-fidelity compute-2Q-gate-depth compute-unused-qubits
                   show-topological-overhead print-logical-schedule json-serialize))

  (warn-deprecated-options all-args)
  (warn-ignored-options all-args)

  (when help
    (show-help)
    (uiop:quit 0))

  (when version
    (show-version)
    (uiop:quit 0))

  (when check-libraries
    (check-libraries))

  (when log-level
    (setf *log-level* (log-level-string-to-symbol log-level)))

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

  (when check-sdk-version
    (multiple-value-bind (available-p version)
        (sdk-update-available-p +QUILC-VERSION+ :proxy proxy)
      (when available-p
        (format t "An update is available to the SDK. You have version ~A. ~
Version ~A is available from https://downloads.rigetti.com/~%"
                +QUILC-VERSION+ version))))

  #-forest-sdk
  (when benchmark
    (benchmarks))

  (when (minusp time-limit)
    (error "A negative value (~D) was provided for the server time-limit." time-limit))

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
       (*verbose* (make-broadcast-stream))
       (*protoquil* protoquil))
    ;; at this point we know we're doing something. strap in LAPACK.
    (magicl:with-blapack
      (reload-foreign-libraries)

      (cond
        ;; RPCQ server mode requested
        (server-mode-rpc
         (unless quiet
           (show-banner))

         (cl-syslog:rfc-log (*logger* :info "Launching quilc.")
           (:msgid "LOG0001"))
         ;; launch the polling loop
         (start-rpc-server :host host
                           :port port
                           :logger *logger*
                           :time-limit time-limit))

        ;; server modes not requested, so continue parsing arguments
        (t
         (setf *human-readable-stream* *error-output*)
         (setf *quil-stream* *standard-output*)

         (when verbose
           (setf *verbose* *human-readable-stream*))

         (let* ((program-text (slurp-lines))
                (program (parse-quil program-text))
                (original-matrix (when (and protoquil compute-matrix-reps)
                                   (parsed-program-to-logical-matrix program))))
           (multiple-value-bind (processed-program statistics)
               (process-program program (lookup-isa-descriptor-for-name isa)
                                :protoquil protoquil
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
             (print-program processed-program *quil-stream*)
             (when print-statistics
               (print-statistics statistics *quil-stream*))
             (when (and protoquil compute-matrix-reps)
               (let* ((processed-program-matrix (parsed-program-to-logical-matrix processed-program :compress-qubits t)))
                 (print-matrix-comparision original-matrix
                                           (quil::scale-out-matrix-phases processed-program-matrix
                                                                          original-matrix)))))))))))

(defun process-program (program chip-specification
                        &key
                          protoquil
                          gate-whitelist
                          gate-blacklist)
  "Compile PROGRAM for the chip CHIP-SPECIFICATION. Optionally calculate statistics described by the keyword arguments. All require :PROTOQUIL T.

Returns a values tuple (PROCESSED-PROGRAM, STATISTICS), where PROCESSED-PROGRAM is the compiled program, and STATISTICS is a HASH-TABLE whose keys are the slots of the RPCQ::|NativeQuilMetadata| class."
  (let* ((statistics (make-hash-table :test #'equal))
         (quil::*compiler-noise-stream* *verbose*)
         (*random-state* (make-random-state t)))
    ;; do the compilation
    (multiple-value-bind (processed-program topological-swaps)
        (compiler-hook program chip-specification :protoquil protoquil)

      ;; if we're supposed to output protoQuil, we need to strip the final HALT
      ;; instructions from the output
      (when protoquil
        (setf (gethash "topological_swaps" statistics) topological-swaps)
        (setf (gethash "final_rewiring" statistics)
              (quil::extract-final-exit-rewiring-vector processed-program))
        (setf (parsed-program-executable-code processed-program)
              (%strip-halts-respecting-rewirings processed-program))

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
                         (quil::lscheduler-calculate-depth lschedule)))))

      (values processed-program statistics))))

(defun %strip-halts-respecting-rewirings (processed-program)
  "Remove HALT instructions from PROCESSED-PROGRAM, retaining any rewirings."
  (flet ((assert-rewirings-compatible (rewiring-type instr prev-instr this-rewiring prev-rewiring)
           ;; Either one of the rewirings is NULL, or they are EQUALP.
           (assert (or (or (null this-rewiring)
                           (null prev-rewiring))
                       (equalp this-rewiring prev-rewiring))
                   ()
                   "Instructions have incompatible ~A rewirings:~@
                           THIS: ~A ~A~@
                           PREV: ~A ~A"
                   rewiring-type instr this-rewiring prev-instr prev-rewiring)))
    (loop
      :for j :from 0
      :for instr :across (parsed-program-executable-code processed-program)
      :for prev-instr := (and (plusp j) (quil::nth-instr (1- j) processed-program))
      :when (and (plusp j) (typep instr 'halt) (comment instr)) :do
        (multiple-value-bind (this-entering this-exiting) (quil::instruction-rewirings instr)
          (multiple-value-bind (prev-entering prev-exiting) (quil::instruction-rewirings prev-instr)
            (assert-rewirings-compatible ':ENTERING instr prev-instr this-entering prev-entering)
            (assert-rewirings-compatible ':EXITING instr prev-instr this-exiting prev-exiting)
            ;; Because of the (COMMENT INSTR) in the above :WHEN loop clause, we know that at least
            ;; one of THIS-ENTERING and THIS-EXITING is non-NIL. Furthermore, due to the
            ;; ASSERT-REWIRINGS-COMPATIBLE calls we know that either at least one the this/prev
            ;; rewirings are non-NIL, or else they are equal. If they are equal, it doesn't matter
            ;; which one we pick. If they are both NIL, then MAKE-REWIRING-FROM-STRING will do the
            ;; right thing and return either an :ENTERING or :EXITING rewiring comment.
            (setf (comment prev-instr)
                  (quil::make-rewiring-comment :entering (or this-entering prev-entering)
                                               :exiting (or this-exiting prev-exiting)))))
      :unless (typep instr 'halt)
        :collect instr :into new-instructions
      :finally
         (return (coerce new-instructions 'vector)))))
