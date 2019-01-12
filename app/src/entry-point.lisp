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
(defparameter *ISA-descriptor* nil)
(defparameter *verbose* (make-broadcast-stream))
(defparameter *protoquil* nil)


;; NOTE: these can't have default values b/c they don't survive serialization
(defparameter *json-stream* (make-broadcast-stream))
(defparameter *human-readable-stream* (make-broadcast-stream))
(defparameter *quil-stream* (make-broadcast-stream))

(defparameter *statistics-dictionary* (make-hash-table :test #'equal))

(defparameter *option-spec*
  '((("prefer-gate-ladders") :type boolean :optional t :documentation "uses gate ladders rather than SWAPs to implement long-ranged gates")
    (("compute-gate-depth" #\d) :type boolean :optional t :documentation "prints compiled circuit gate depth; requires -P")
    (("compute-gate-volume") :type boolean :optional t :documentation "prints compiled circuit gate volume; requires -P")
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
    (("isa") :type string :optional t :documentation "set ISA to one of \"8Q\", \"20Q\", \"16QMUX\", \"bristlecone\", \"ibmqx5\", or path to QPU description file")
    (("enable-state-prep-reductions") :type boolean :optional t :documentation "assume that the program starts in the ground state")
    (("protoquil" #\P) :type boolean :optional t :documentation "restrict input/output to ProtoQuil")
    (("help" #\h) :type boolean :optional t :documentation "print this help information and exit")
    (("server-mode-http" #\S) :type boolean :optional t :documentation "run as a web server")
    (("server-mode-rpc" #\R) :type boolean :optional t :documentation "run as an RPCQ server")
    (("port" #\p) :type integer :optional t :documentation "port to run the server on")
    (("time-limit") :type integer :initial-value 0 :documentation "time limit for server requests (0 => unlimited, ms)")
    (("version" #\v) :type boolean :optional t :documentation "print version information")
    (("check-libraries") :type boolean :optional t :documentation "check that foreign libraries are adequate")
    #-forest-sdk
    (("benchmark") :type boolean :optional t :documentation "run benchmarks and print results")))

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
  (format t "~A (library: ~A) [~A]~%" +QUILC-VERSION+ +CL-QUIL-VERSION+ +GIT-HASH+))

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
  (setup-debugger)
  
  ;; grab the CLI arguments
  (setf *program-name* (pop argv))
  
  (handler-case
      (command-line-arguments:handle-command-line
       *option-spec*
       'process-options
       :command-line argv
       :name "quilc"
       :positional-arity 0
       :rest-arity nil)
    (interactive-interrupt (c)
      (declare (ignore c))
      (format *error-output* "~&! ! ! Caught keyboard interrupt. Exiting.~%")
      (uiop:quit 0))
    (error (c)
      (format *error-output* "~&! ! ! Error: ~A~%" c)
      (uiop:quit 1))))

(defun %entry-point (argv)
  ;; grab the CLI arguments
  (setf *program-name* (pop argv))
  
  (command-line-arguments:handle-command-line
       *option-spec*
       'process-options
       :command-line argv
       :name "quilc"
       :positional-arity 0
       :rest-arity nil))

(defun process-options (&key (prefer-gate-ladders nil)
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
                             (port nil)
                             time-limit
                             (help nil))
  (when help
    (show-help)
    (uiop:quit 0))
  (when version
    (show-version)
    (uiop:quit 0))
  (when check-libraries
    (check-libraries))
  #-forest-sdk
  (when benchmark
    (benchmarks))

  (when (plusp time-limit)
    (setf *time-limit* (/ time-limit 1000.0d0)))
  
  (setf quil::*prefer-ranged-gates-to-SWAPs* prefer-gate-ladders)
  (setf *compute-gate-depth* compute-gate-depth)
  (setf *compute-gate-volume* compute-gate-volume)
  (setf *compute-runtime* compute-runtime)
  (setf *compute-fidelity* compute-fidelity)
  (setf *compute-matrix-reps* compute-matrix-reps)
  (setf *compute-2Q-gate-depth* compute-2Q-gate-depth)
  (setf *compute-unused-qubits* compute-unused-qubits)
  (setf *without-pretty-printing* without-pretty-printing)
  (setf *print-logical-schedule* print-logical-schedule)
  (setf *gate-blacklist* 
        (when gate-blacklist
          (split-sequence:split-sequence #\, (remove #\Space gate-blacklist))))
  (setf *gate-whitelist* 
        (when gate-whitelist
          (split-sequence:split-sequence #\, (remove #\Space gate-whitelist))))
  (setf *topological-swaps* show-topological-overhead)
  (setf *protoquil* protoquil)
  (setf quil::*enable-state-prep-compression* enable-state-prep-reductions)
  
  ;; at this point we know we're doing something. strap in LAPACK.
  (magicl:with-blapack
    (reload-foreign-libraries)
    
    (cond
      ((and server-mode-http server-mode-rpc)
       (format t "quilc can only run in either RPCQ or HTTP server mode. Select only one.")
       (uiop:quit 0))
      ;; web server mode requested
      (server-mode-http
       ;; null out the streams
       (setf *json-stream* (make-broadcast-stream)
             *human-readable-stream* (make-broadcast-stream)
             *quil-stream* (make-broadcast-stream))
       
       ;; configure the server
       (when port
         (setf *server-port* port))
       
       ;; launch the polling loop
       (show-banner)
       (format t "~%
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> IMPORTANT NOTICE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
The HTTP endpoint has been deprecated in favor of the RPCQ endpoint.  In the
future, it will be removed.  You're advised to modify your client code to talk
to the RPCQ version instead.
>>>>>>>>>>>>>>>>>>>>>>>>>>>>> END IMPORTANT NOTICE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
~%~%")
       (start-web-server))
      
      ;; RPCQ server mode requested
      (server-mode-rpc
       ;; null out the streams
       (setf *json-stream* (make-broadcast-stream)
             *human-readable-stream* (make-broadcast-stream)
             *quil-stream* (make-broadcast-stream))
       
       ;; configure the server
       (if port
           (setf *server-port* port)
           (setf *server-port* 5555))
       
       ;; launch the polling loop
       (show-banner)
       (let ((logger (make-instance 'cl-syslog:rfc5424-logger
                                    :app-name *program-name*
                                    :facility ':local0
                                    :log-writer
                                    #+windows (cl-syslog:stream-log-writer)
                                    #-windows (cl-syslog:tee-to-stream
                                               (cl-syslog:syslog-log-writer "quilc" :local0)))))
         (cl-syslog:rfc-log (logger :info "Launching quilc.")
           (:msgid "LOG0001"))
         (start-rpc-server :port *server-port*
                           :logger logger)))
      
      ;; server modes not requested, so continue parsing arguments
      (t
       (cond
         (json-serialize
          (setf *json-stream* *standard-output*)
          (setf *human-readable-stream* (make-broadcast-stream))
          (setf *quil-stream* (make-broadcast-stream)))
         (t
          (setf *json-stream* (make-broadcast-stream))
          (setf *human-readable-stream* *error-output*)
          (setf *quil-stream* *standard-output*)))
       (setf *isa-descriptor*
             (cond
               ((or (null isa)
                    (string= isa "8Q"))
                (quil::build-8Q-chip))
               ((string= isa "20Q")
                (quil::build-skew-rectangular-chip 0 4 5))
               ((string= isa "16QMUX")
                (quil::build-nQ-trivalent-chip 1 1 8 4))
               ((string= isa "bristlecone")
                (quil::build-bristlecone-chip))
               ((string= isa "ibmqx5")
                (quil::build-ibm-qx5))
               ((probe-file isa)
                (quil::read-chip-spec-file isa))
               (t
                (error "ISA descriptor does not name a known template or an extant file."))))
       (setf *verbose*
             (cond
               (verbose *human-readable-stream*)
               (t (make-broadcast-stream))))
       (run-CLI-mode)))))

(defun run-CLI-mode ()
  (let* ((program-text (slurp-lines))
         (program (quil::parse-quil-string program-text)))
    (process-program program *isa-descriptor*)))

(defun process-program (program chip-specification)
  (let* ((original-matrix
           (when (and *protoquil* *compute-matrix-reps*)
             (quil::make-matrix-from-quil (coerce (quil::parsed-program-executable-code program) 'list) program)))
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
                     :when (and (typep instr 'quil::pragma-current-rewiring))
                       :do (setf (gethash "final-rewiring" *statistics-dictionary*)
                                 (quil::pragma-rewiring instr))
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
        (let ((processed-quil (quil::parsed-program-executable-code processed-program))
              (initial-l2p (quil::pragma-rewiring
                            (aref (quil::parsed-program-executable-code processed-program) 0)))
              (final-l2p (quil::pragma-rewiring
                          (aref (quil::parsed-program-executable-code processed-program)
                                    (1- (length (quil::parsed-program-executable-code processed-program)))))))
          (print-matrix-representations initial-l2p
                                        (coerce processed-quil 'list)
                                        final-l2p
                                        original-matrix)))
      
      (publish-json-statistics))))
