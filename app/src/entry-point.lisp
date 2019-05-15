;;;; entry-point.lisp
;;;;
;;;; Binary app interface entry point.
;;;;
;;;; Author: Eric Peterson
;;;;

(in-package #:quilc)


(defparameter *program-name* "quilc")
(defparameter *print-logical-schedule* nil)
(defparameter *compute-gate-depth* nil)
(defparameter *compute-runtime* nil)
(defparameter *compute-fidelity* nil)
(defparameter *compute-matrix-reps* nil)
(defparameter *topological-swaps* nil)
(defparameter *compute-gate-volume* nil)
(defparameter *compute-2Q-gate-depth* nil)
(defparameter *compute-unused-qubits* nil)
(defparameter *gate-whitelist* nil)
(defparameter *gate-blacklist* nil)
(defparameter *without-pretty-printing* nil)
(defparameter *verbose* (make-broadcast-stream))
(defparameter *protoquil* nil)
(defparameter *log-level* ':info)


;; NOTE: these can't have default values b/c they don't survive serialization
(defparameter *json-stream* (make-broadcast-stream))
(defparameter *human-readable-stream* (make-broadcast-stream))
(defparameter *quil-stream* (make-broadcast-stream))

(defparameter *logger* (make-instance 'cl-syslog:rfc5424-logger
                                      :app-name "quilc"
                                      :facility ':local0
                                      :maximum-priority ':info
                                      :log-writer (cl-syslog:null-log-writer)))

(defparameter *statistics-dictionary* (make-hash-table :test #'equal))

(defparameter *option-spec*
  '((("prefer-gate-ladders") :type boolean :optional t :documentation "uses gate ladders rather than SWAPs to implement long-ranged gates")
    (("compute-gate-depth" #\d) :type boolean :optional t :documentation "prints compiled circuit gate depth (longest subsequece of data-sharing compiled instructions); requires -P")
    (("compute-gate-volume") :type boolean :optional t :documentation "prints compiled circuit gate volume (number of gates); requires -P")
    (("compute-runtime" #\r) :type boolean :optional t :documentation "prints compiled circuit expected runtime; requires -P")
    (("compute-fidelity" #\f) :type boolean :optional t :documentation "prints approximate compiled circuit fidelity; requires -P")
    (("compute-2Q-gate-depth" #\2) :type boolean :optional t :documentation "prints compiled circuit multiqubit gate depth; ignores white/blacklists, requires -P")
    (("compute-matrix-reps" #\m) :type boolean :optional t :documentation "prints matrix representations for comparison; requires -P")
    (("compute-unused-qubits" #\u) :type boolean :optional t :documentation "prints unused qubits; requires -P")
    (("show-topological-overhead" #\t) :type boolean :optional t :documentation "prints the number of SWAPs incurred for topological reasons")
    (("gate-blacklist") :type string :optional t :documentation "when calculating statistics, ignore these (comma-separated) gates")
    (("gate-whitelist") :type string :optional t :documentation "when calculating statistics, consider only these (comma-separated) gates")
    (("without-pretty-printing") :type boolean :optional t :documentation "turns off pretty-printing features")
    #-forest-sdk
    (("verbose") :type boolean :optional t :documentation "verbose compiler trace output")
    #-forest-sdk
    (("json-serialize" #\j) :type boolean :optional t :documentation "serialize output as a JSON object")
    #-forest-sdk
    (("print-logical-schedule" #\s) :type boolean :optional t :documentation "include logically parallelized schedule in JSON output; requires -P")
    (("isa") :type string :optional t :initial-value "8Q" :documentation "set ISA to one of \"8Q\", \"20Q\", \"16QMUX\", \"bristlecone\", \"ibmqx5\", or path to QPU description file")
    (("enable-state-prep-reductions") :type boolean :optional t :documentation "assume that the program starts in the ground state")
    (("protoquil" #\P) :type boolean :optional t :documentation "restrict input/output to ProtoQuil")
    (("help" #\h) :type boolean :optional t :documentation "print this help information and exit")
    (("server-mode-http" #\S) :type boolean :optional t :documentation "run as a web server *and* an RPCQ server. ignores --port and uses 6000 and 5555 respectively")
    (("server-mode-rpc" #\R) :type boolean :optional t :documentation "run as an RPCQ server")
    (("host") :type string :initial-value "*" :optional t :documentation "host on which to run the RPCQ server")
    (("port" #\p) :type integer :initial-value 5555 :optional t :documentation "port to run the RPCQ server on")
    (("time-limit") :type integer :initial-value 0 :documentation "time limit (in seconds) for server requests (0 => unlimited)")
    (("version" #\v) :type boolean :optional t :documentation "print version information")
    (("check-libraries") :type boolean :optional t :documentation "check that foreign libraries are adequate")
    #-forest-sdk
    (("benchmark") :type boolean :optional t :documentation "run benchmarks and print results")
    (("log-level") :type string :optional t :initial-value "info" :documentation "maximum logging level (\"debug\", \"info\", \"notice\", \"warning\", \"err\", \"crit\", \"alert\", or \"emerg\") (default \"info\")")
    (("quiet") :type boolean :optional t :initial-value nil :documentation "Disable all non-logging output (banner, etc.)")
    (("check-sdk-version") :type boolean :optional t :initial-value nil :documentation "Check for a new SDK version at launch.")
    (("proxy") :type string :optional t :initial-value nil :documentation "Proxy to use when checking for an SDK update.")))

(defun slurp-lines (&optional (stream *standard-input*))
  (flet ((line () (read-line stream nil nil nil)))
    (with-output-to-string (s)
      (loop :for line := (line) :then (line)
         :while line
         :do (write-line line s)))))

(defun print-quil-list (executable-code stream)
  (loop :for instr :in executable-code :do
     (progn
       (quil::print-instruction instr stream)
       (format stream "~%"))))

(defun reload-foreign-libraries ()
  (locally
        (declare #+sbcl (sb-ext:muffle-conditions style-warning))
      (handler-bind (#+sbcl (style-warning #'muffle-warning))
        (cffi:load-foreign-library 'magicl.foreign-libraries::libblas)
        (cffi:load-foreign-library 'magicl.foreign-libraries::liblapack))))

(defun print-matrix-with-comment-hashes (matrix &optional (stream *standard-output*))
  (format stream "~d"
          (cl-ppcre:regex-replace-all
           (coerce #(#\Newline) 'string)
           (with-output-to-string (s)
             (princ matrix s))
           (coerce #(#\Newline #\#) 'string))))

(defun lookup-isa-descriptor-for-name (isa)
  (alexandria:switch (isa :test #'string=)
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
  (alexandria:eswitch (log-level :test #'string=)
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
  (command-line-arguments:show-option-help *option-spec* :sort-names t))

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
     *option-spec*
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
                           :collect `(cons ',name ,name))
                   bordeaux-threads:*default-special-bindings*)))
     ,@body))

(defun process-options (&key
                          (prefer-gate-ladders nil)
                          (compute-gate-depth nil)
                          (compute-gate-volume nil)
                          (compute-runtime nil)
                          (compute-fidelity nil)
                          (compute-matrix-reps nil)
                          (compute-2Q-gate-depth nil)
                          (compute-unused-qubits nil)
                          (show-topological-overhead nil)
                          (gate-blacklist nil)
                          (gate-whitelist nil)
                          (without-pretty-printing nil)
                          (print-logical-schedule nil)
                          (verbose nil)
                          (json-serialize nil)
                          (isa nil)
                          (enable-state-prep-reductions nil)
                          (protoquil nil)
                          (version nil)
                          (check-libraries nil)
                          #-forest-sdk
                          (benchmark nil)
                          (server-mode-http nil)
                          (server-mode-rpc nil)
                          (host nil)
                          (port nil)
                          time-limit
                          (help nil)
                          (log-level nil)
                          (quiet nil)
                          (check-sdk-version nil)
                          (proxy nil))
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
                       :app-name *program-name*
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
Version ~A is available from https://www.rigetti.com/forest~%"
                +QUILC-VERSION+ version))
      (uiop:quit (if (and available-p version) 0 1))))

  #-forest-sdk
  (when benchmark
    (benchmarks))

  (when (minusp time-limit)
    (error "A negative value (~D) was provided for the server time-limit." time-limit))
  
  (special-bindings-let*
      ((*log-level* (or (and log-level (log-level-string-to-symbol log-level))
			*log-level*))
       (*logger* (make-instance 'cl-syslog:rfc5424-logger
				:app-name *program-name*
				:facility ':local0
				:maximum-priority *log-level*
				:log-writer
				#+windows (cl-syslog:stream-log-writer)
				#-windows (cl-syslog:tee-to-stream
					   (cl-syslog:syslog-log-writer "quilc" :local0)
					   *error-output*)))
       (*time-limit* time-limit)
       (quil::*prefer-ranged-gates-to-SWAPs* prefer-gate-ladders)
       (*compute-gate-depth* compute-gate-depth)
       (*compute-gate-volume* compute-gate-volume)
       (*compute-runtime* compute-runtime)
       (*compute-fidelity* compute-fidelity)
       (*compute-matrix-reps* compute-matrix-reps)
       (*compute-2Q-gate-depth* compute-2Q-gate-depth)
       (*compute-unused-qubits* compute-unused-qubits)
       (*without-pretty-printing* without-pretty-printing)
       (*print-logical-schedule* print-logical-schedule)
       (*gate-blacklist* (and gate-blacklist
			      (split-sequence:split-sequence #\, (remove #\Space gate-blacklist))))
       (*gate-whitelist* (and gate-whitelist
			      (split-sequence:split-sequence #\, (remove #\Space gate-whitelist))))
       (*topological-swaps* show-topological-overhead)
       (*protoquil* protoquil)
       (quil::*enable-state-prep-compression* enable-state-prep-reductions)
       ;; Null out the streams. If no server mode is requested, these bindings will be modified
       ;; before calling run-CLI-mode, below.
       (*json-stream* (make-broadcast-stream))
       (*human-readable-stream* (make-broadcast-stream))
       (*quil-stream* (make-broadcast-stream))
       (*verbose* (make-broadcast-stream)))
    ;; at this point we know we're doing something. strap in LAPACK.
    (magicl:with-blapack
      (reload-foreign-libraries)

      (cond
	;; web server mode requested.
	;; currently provides both web and RPCQ server as a transition.
	(server-mode-http
         ;; launch the polling loop
	 (unless quiet
           (show-banner))

	 (unless quiet
           (format t "~%
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> IMPORTANT NOTICE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
The HTTP endpoint has been deprecated in favor of the RPCQ endpoint.  In the
future, it will be removed.  In the meanwhile, we are launching *both* an HTTP
server and an RPCQ server.  You're advised to modify your client code to talk
to the RPCQ version instead, so that it continues to operate when we disable the
HTTP server for good.
>>>>>>>>>>>>>>>>>>>>>>>>>>>>> END IMPORTANT NOTICE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
~%~%"))

	 (when port
           (cl-syslog:rfc-log (*logger* :warning "WARNING: -p and -S are incompatible. Dropping -p.")
             (:msgid "LOG0001")))

	 ;; start the RPCQ server in parallel
	 (cl-syslog:rfc-log (*logger* :info "Launching quilc.")
           (:msgid "LOG0001"))
	 (bt:make-thread (lambda ()
                           (start-rpc-server :host host :port 5555 :logger *logger*)))

	 (start-web-server))

	;; RPCQ server mode requested
	(server-mode-rpc
         (unless quiet
	   (show-banner))

	 (cl-syslog:rfc-log (*logger* :info "Launching quilc.")
	   (:msgid "LOG0001"))
	 ;; launch the polling loop
	 (start-rpc-server :host host
			   :port port
			   :logger *logger*))

	;; server modes not requested, so continue parsing arguments
	(t
	 (cond
	   (json-serialize
	    (setf *json-stream* *standard-output*))
	   (t
	    (setf *human-readable-stream* *error-output*)
	    (setf *quil-stream* *standard-output*)))
         (when verbose
           (setf *verbose* *human-readable-stream*))
         (run-CLI-mode (lookup-isa-descriptor-for-name isa)))))))

(defun run-CLI-mode (isa-descriptor)
  (let* ((program-text (slurp-lines))
         (program (quil::parse-quil program-text)))
    (process-program program isa-descriptor)))

(defun process-program (program chip-specification)
  (let* ((original-matrix
           (when (and *protoquil* *compute-matrix-reps*)
             (quil::parsed-program-to-logical-matrix program)))
         (quil::*compiler-noise-stream* *verbose*)
         (*statistics-dictionary* (make-hash-table :test 'equal))
         (*random-state* (make-random-state t)))
    ;; do the compilation
    (multiple-value-bind (processed-program topological-swaps)
        (quil::compiler-hook program chip-specification :protoquil *protoquil*)

      ;; if we're supposed to output protoQuil, we need to strip the final HALT
      ;; instructions from the output
      (when *protoquil*
        (setf (quil::parsed-program-executable-code processed-program)
              (coerce
               (loop :for instr :across (quil::parsed-program-executable-code processed-program)
                     :for j :from 0
                     ;; if there's a comment, consider extracting from it the final_rewiring
                     ;; entry for use in the statistics dictionary.
                     :when (quil::comment instr)
                       :do (cond
                             ((uiop:string-prefix-p "Exiting rewiring: " (quil::comment instr))
                              (let ((*read-eval* nil))
                                (setf (gethash "final_rewiring" *statistics-dictionary*)
                                      (read-from-string
                                       (subseq (quil::comment instr) (length "Exiting rewiring: "))))))
                             ((uiop:string-prefix-p "Entering/exiting rewiring: " (quil::comment instr))
                              (let ((comment (quil::comment instr))
                                    (length (length "Entering/exiting rewiring: ("))
                                    (*read-eval* nil))
                                (setf (gethash "final_rewiring" *statistics-dictionary*)
                                      (read-from-string
                                       (subseq (quil::comment instr) length (- (length comment)
                                                                               (/ (- (length comment) length) 2)
                                                                               2))))))
                             (t nil))
                     ;; if there's a HALT instruction with a rewiring comment on
                     ;; it, we need to migrate the comment up one instruction
                     ;; (and perhaps merge it with any comment already present)
                     :when (and (< 0 j)
                                (typep instr 'quil::halt)
                                (quil::comment instr))
                       :do (cond
                             ((quil::comment (aref (quil::parsed-program-executable-code processed-program)
                                                   (1- j)))
                              (let ((*print-pretty* nil)
                                    (prev-rewiring (subseq (quil::comment (aref (quil::parsed-program-executable-code processed-program)
                                                                                (1- j)))
                                                           (length "Entering rewiring: ")))
                                    (this-rewiring (subseq (quil::comment instr)
                                                           (length "Exiting rewiring: "))))
                                (setf (quil::comment (aref (quil::parsed-program-executable-code processed-program)
                                                           (1- j)))
                                      (format nil "Entering/exiting rewiring: (~a . ~a)"
                                              prev-rewiring this-rewiring))))
                             (t
                              (setf (quil::comment (aref (quil::parsed-program-executable-code processed-program)
                                                         (1- j)))
                                    (quil::comment instr))))
                     :unless (typep instr 'quil::halt)
                       :collect instr)
               'vector)))

      ;; now that we've compiled the program, we have various things to output
      ;; one thing we're always going to want to output is the program itself.
      (print-program processed-program *quil-stream*)

      (when *topological-swaps*
        (print-topological-swap-count topological-swaps))

      (when (and *protoquil*
                 *print-logical-schedule*)
        (let ((lschedule (quil::make-lscheduler)))
          ;; fill out the lschedule
          (loop :for instr :across (quil::parsed-program-executable-code processed-program)
                :unless (typep instr 'quil::pragma)
                  :do (quil::append-instruction-to-lschedule lschedule
                                                             instr))
          ;; stuff it in the dictionary for later serialization
          (setf (gethash "logical_schedule" *statistics-dictionary*)
                lschedule)))

      (when (and *protoquil*
                 (or *compute-gate-depth*
                     *compute-gate-volume*
                     *compute-runtime*
                     *compute-fidelity*
                     *compute-unused-qubits*))
        ;; calculate some statistics based on logical scheduling
        (let ((lschedule (quil::make-lscheduler)))
          (loop :for instr :across (quil::parsed-program-executable-code processed-program)
                :when (and (typep instr 'quil::gate-application)
                           (not (member (quil::application-operator instr)
                                        *gate-blacklist*
                                        :test #'string=))
                           (or (null *gate-whitelist*)
                               (member (quil::application-operator instr)
                                       *gate-whitelist*
                                       :test #'string=)))
                  :do (quil::append-instruction-to-lschedule lschedule instr))
          (when *compute-gate-depth*
            (print-gate-depth lschedule))
          (when *compute-gate-volume*
            (print-gate-volume lschedule))
          (when *compute-runtime*
            (print-program-runtime lschedule chip-specification))
          (when *compute-fidelity*
            (print-program-fidelity lschedule chip-specification))
          (when *compute-unused-qubits*
            (print-unused-qubits lschedule chip-specification))))

      (when (and *protoquil* *compute-2Q-gate-depth*)
        (let ((lschedule (quil::make-lscheduler)))
          (loop :for instr :across (quil::parsed-program-executable-code processed-program)
                :when (and (typep instr 'quil::gate-application)
                           (<= 2 (length (quil::application-arguments instr))))
                  :do (quil::append-instruction-to-lschedule lschedule instr))
          (print-2Q-gate-depth lschedule)))

      (when (and *protoquil* *compute-matrix-reps*)
        (let* ((processed-program-matrix (quil::parsed-program-to-logical-matrix processed-program :compress-qubits t))
               (same-same-but-different (quil::scale-out-matrix-phases processed-program-matrix
                                                                       original-matrix)))
          (format *human-readable-stream* "~%#Matrix read off from input code~%")
          (print-matrix-with-comment-hashes original-matrix *human-readable-stream*)
          (format *human-readable-stream* "~%#Matrix read off from compiled code~%")
          (print-matrix-with-comment-hashes same-same-but-different *human-readable-stream*)
          (format *human-readable-stream* "~%")
          (format *human-readable-stream* "#Matrices are~a equal~%" (if (quil::matrix-equals-dwim original-matrix same-same-but-different) "" " not"))
          (finish-output *human-readable-stream*)))
      (publish-json-statistics))))
