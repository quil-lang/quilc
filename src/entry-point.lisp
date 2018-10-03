;;;; pipe-compiler-entry-points.lisp
;;;;
;;;; fast entry point for compilation methods
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; eventually, we will want this to look at argv for a filename from which it
;;;; can parse a chip/ISA specification. for now, we're going to bake such a
;;;; specification in.
;;;; ===sample input===
;;;; ANY CLASSICAL-FREE QUIL PROGRAM
;;;; ^D

(in-package #:quilc)


;; load and store bits of version information at compile time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun system-version (system-designator)
    (let ((sys (asdf:find-system system-designator nil)))
      (if (and sys (slot-boundp sys 'asdf:version))
          (asdf:component-version sys)
          "unknown")))

  (defun git-hash (system)
    "Get the short git hash of the system SYSTEM."
    (let ((sys-path (namestring (asdf:system-source-directory system))))
      (multiple-value-bind (output err-output status)
          (uiop:run-program `("git" "-C" ,sys-path "rev-parse" "--short" "HEAD")
                            :output '(:string :stripped t)
                            :ignore-error-status t)
        (declare (ignore err-output))
        (if (not (zerop status))
            "unknown"
            output)))))

(eval-when (:compile-toplevel :load-toplevel)
  (alexandria:define-constant +QUILC-VERSION+
      (system-version '#:quilc)
    :test #'string=
    :documentation "The version of the quilc application.")

  (alexandria:define-constant +CL-QUIL-VERSION+
      (system-version '#:cl-quil)
    :test #'string=
    :documentation "The version of the CL-Quil library.")

  (alexandria:define-constant +GIT-HASH+
      (git-hash '#:quilc)
    :test #'string=
    :documentation "The git hash of the quilc repo.")
  )




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
    (("compute-gate-depth" #\d) :type boolean :optional t :documentation "prints compiled circuit gate depth; requires -p")
    (("compute-gate-volume") :type boolean :optional t :documentation "prints compiled circuit gate volume; requires -p")
    (("compute-runtime" #\r) :type boolean :optional t :documentation "prints compiled circuit expected runtime; requires -p")
    (("compute-fidelity" #\f) :type boolean :optional t :documentation "prints approximate compiled circuit fidelity; requires -p")
    (("compute-2Q-gate-depth" #\2) :type boolean :optional t :documentation "prints compiled circuit multiqubit gate depth; ignores white/blacklists, requires -p")
    (("compute-matrix-reps" #\m) :type boolean :optional t :documentation "prints matrix representations for comparison; requires -p")
    (("compute-unused-qubits" #\u) :type boolean :optional t :documentation "prints unused qubits; requires -p")
    (("show-topological-overhead" #\t) :type boolean :optional t :documentation "prints the number of SWAPs incurred for topological reasons")
    (("gate-blacklist") :type string :optional t :documentation "when calculating statistics, ignore these gates")
    (("gate-whitelist") :type string :optional t :documentation "when calculating statistics, consider only these gates")
    (("without-pretty-printing") :type boolean :optional t :documentation "turns off pretty-printing features")
    #-forest-sdk
    (("verbose") :type boolean :optional t :documentation "verbose compiler trace output")
    #-forest-sdk
    (("json-serialize" #\j) :type boolean :optional t :documentation "serialize output as a JSON object")
    #-forest-sdk
    (("print-logical-schedule" #\s) :type boolean :optional t :documentation "include logically parallelized schedule in JSON output; requires -p")
    (("isa") :type string :optional t :documentation "set ISA to one of \"8Q\", \"20Q\", \"16QMUX\", or path to QPU description file")
    (("enable-state-prep-reductions") :type boolean :optional t :documentation "assume that the program starts in the ground state")
    (("protoquil" #\P) :type boolean :optional t :documentation "restrict input/output to ProtoQuil")
    (("help" #\h) :type boolean :optional t :documentation "print this help information and exit")
    (("server-mode" #\S) :type boolean :optional t :documentation "run as a server")
    (("port" #\p) :type integer :optional t :documentation "port to run the server on")
    (("time-limit") :type integer :initial-value 0 :documentation "time limit for server requests (0 => unlimited, ms)")
    (("version" #\v) :type boolean :optional t :documentation "print version information")))

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

(defun show-banner ()
  (format t "****************************************~%")
  (format t "* Welcome to the Rigetti Quil Compiler *~%")
  (format t "****************************************~%")
  (format t "Copyright (c) 2018 Rigetti Computing.~2%")
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
                             (server-mode nil)
                             (port *server-port*)
                             time-limit
                             (help nil))
  (when help
    (show-help)
    (uiop:quit 0))
  (when version
    (show-version)
    (uiop:quit 0))
  
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
      ;; server mode requested
      (server-mode
       ;; null out the streams
       (setf *json-stream* (make-broadcast-stream))
       (setf *human-readable-stream* (make-broadcast-stream))
       (setf *quil-stream* (make-broadcast-stream))
       
       ;; configure the server
       (when port
         (format t "port triggered: ~a.~%" port)
         (setf *server-port* port))
       
       ;; launch the polling loop
       (show-banner)
       (start-server))
      
      ;; server mode not requested, so continue parsing arguments
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
      ;; instructios from the output
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
