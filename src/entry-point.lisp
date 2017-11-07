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

(defparameter *program-name* "quilc")
(defparameter *compute-gate-depth* nil)
(defparameter *compute-runtime* nil)
(defparameter *compute-matrix-reps* nil)
(defparameter *topological-swaps* nil)
(defparameter *compute-gate-volume* nil)
(defparameter *gate-whitelist* nil)
(defparameter *gate-blacklist* nil)
(defparameter *without-pretty-printing* nil)
(defparameter *verbose* (make-broadcast-stream))

;; NOTE: these can't have default values b/c they don't survive serialization
(defparameter *json-stream* (make-broadcast-stream))
(defparameter *human-readable-stream* (make-broadcast-stream))
(defparameter *quil-stream* (make-broadcast-stream))

(defparameter *statistics-dictionary* (make-hash-table :test #'equal))

(defparameter *option-spec*
  '((("compute-gate-depth" #\d) :type boolean :optional t :documentation "prints compiled circuit gate depth")
    (("compute-gate-volume") :type boolean :optional t :documentation "prints compiled circuit gate volume")
    (("compute-runtime" #\r) :type boolean :optional t :documentation "prints compiled circuit expected runtime")
    (("compute-matrix-reps" #\m) :type boolean :optional t :documentation "prints matrix representations for comparison")
    (("show-topological-overhead" #\t) :type boolean :optional t :documentation "prints the number of SWAPs incurred for topological reasons")
    (("gate-blacklist") :type string :optional t :documentation "ignore these gates during count")
    (("gate-whitelist") :type string :optional t :documentation "include only these gates during count")
    (("without-pretty-printing") :type boolean :optional t :documentation "turns off pretty-printing features")
    (("verbose" #\v) :type boolean :optional t :documentation "verbose compiler trace output")
    (("json-serialize" #\j) :type boolean :optional t :documentation "serialize output as a JSON object")
    (("help" #\? #\h) :optional t :documentation "print this help information and exit")))

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
        (cffi:load-foreign-library 'magicl.foreign-libraries::libgfortran)
        (cffi:load-foreign-library 'magicl.foreign-libraries::libblas)
        (cffi:load-foreign-library 'magicl.foreign-libraries::liblapack)
        (cffi:load-foreign-library 'magicl.foreign-libraries::libexpokit))))

(defun print-matrix-with-comment-hashes (matrix &optional (stream *standard-output*))
  (format stream "~d"
          (cl-ppcre:regex-replace-all
           (coerce #(#\Newline) 'string)
           (with-output-to-string (s)
             (princ matrix s))
           (coerce #(#\Newline #\#) 'string))))

(defun show-help ()
  (format t "Usage:~%")
  (format t "  ~A [options]~%" *program-name*)
  (format t "Options:~%")
  (command-line-arguments:show-option-help *option-spec* :sort-names t))

(defun process-options (&key (compute-gate-depth nil)
                             (compute-gate-volume nil)
                             (compute-runtime nil)
                             (compute-matrix-reps nil)
                             (show-topological-overhead nil)
                             (gate-blacklist nil)
                             (gate-whitelist nil)
                             (without-pretty-printing nil)
                             (verbose nil)
                             (json-serialize nil)
                             (help nil))
  (when help
    (show-help)
    (uiop:quit 0))
  (setf *compute-gate-depth* compute-gate-depth)
  (setf *compute-gate-volume* compute-gate-volume)
  (setf *compute-runtime* compute-runtime)
  (setf *compute-matrix-reps* compute-matrix-reps)
  (setf *without-pretty-printing* without-pretty-printing)
  (setf *gate-blacklist* 
        (when gate-blacklist
          (split-sequence:split-sequence #\, (remove #\Space gate-blacklist))))
  (setf *gate-whitelist* 
        (when gate-whitelist
          (split-sequence:split-sequence #\, (remove #\Space gate-whitelist))))
  (setf *topological-swaps* show-topological-overhead)
  (cond
    (json-serialize
     (setf *json-stream* *standard-output*)
     (setf *human-readable-stream* (make-broadcast-stream))
     (setf *quil-stream* (make-broadcast-stream)))
    (t
     (setf *json-stream* (make-broadcast-stream))
     (setf *human-readable-stream* *error-output*)
     (setf *quil-stream* *standard-output*)))
  (setf *verbose*
        (when verbose *human-readable-stream*)))

(defun print-matrix-representations (initial-l2p processed-quil final-l2p program)
  (let* ((original-matrix (quil::make-matrix-from-quil (coerce (quil::parsed-program-executable-code program) 'list) program))
         (initial-l2p (quil::trim-rewiring initial-l2p))
         (final-l2p (quil::trim-rewiring final-l2p))
         (raw-new-matrix (quil::make-matrix-from-quil processed-quil program))
         (qubit-count (max (1- (integer-length (magicl:matrix-rows raw-new-matrix)))
                           (1- (integer-length (magicl:matrix-rows original-matrix)))
                           (length initial-l2p)
                           (length final-l2p)))
         (wire-out (apply #'quil::kq-gate-on-lines
                          (quil::rewiring-to-permutation-matrix-p2l final-l2p)
                          qubit-count
                          (alexandria:iota (length final-l2p) :start (1- (length final-l2p)) :step -1)))
         (wire-in (apply #'quil::kq-gate-on-lines
                         (quil::rewiring-to-permutation-matrix-l2p initial-l2p)
                         qubit-count
                         (alexandria:iota (length initial-l2p) :start (1- (length initial-l2p)) :step -1)))
         (stretched-raw-new-matrix (apply #'quil::kq-gate-on-lines
                                          raw-new-matrix
                                          qubit-count
                                          (alexandria:iota (1- (integer-length (magicl:matrix-rows raw-new-matrix)))
                                                           :start (- (integer-length (magicl:matrix-rows raw-new-matrix)) 2)
                                                           :step -1)))
         (stretched-original-matrix (apply #'quil::kq-gate-on-lines
                                           original-matrix
                                           qubit-count
                                           (alexandria:iota (1- (integer-length (magicl:matrix-rows original-matrix)))
                                                            :start (- (integer-length (magicl:matrix-rows original-matrix)) 2)
                                                            :step -1)))
         (new-matrix
           (reduce #'magicl:multiply-complex-matrices
                   (list
                    wire-out
                    stretched-raw-new-matrix
                    wire-in))))
    (setf new-matrix (quil::scale-out-matrix-phases new-matrix stretched-original-matrix))
    (format *human-readable-stream* "~%#Matrix read off from input code~%")
    (print-matrix-with-comment-hashes stretched-original-matrix *human-readable-stream*)
    (setf (gethash "original_matrix" *statistics-dictionary*)
          (with-output-to-string (s)
            (print-matrix-with-comment-hashes stretched-original-matrix s)))
    (format *human-readable-stream* "~%#Matrix read off from compiled code~%")
    (print-matrix-with-comment-hashes new-matrix *human-readable-stream*)
    (setf (gethash "compiled_matrix" *statistics-dictionary*)
          (with-output-to-string (s)
            (print-matrix-with-comment-hashes new-matrix s)))
    (format *human-readable-stream* "~%")
    (finish-output *standard-output*)
    (finish-output *human-readable-stream*)))

(defun print-gate-depth (lschedule)
  (let ((depth (quil::lscheduler-calculate-depth lschedule
                                                 :blacklist *gate-blacklist*
                                                 :whitelist *gate-whitelist*)))
    (setf (gethash "gate_depth" *statistics-dictionary*) depth)
    (format *human-readable-stream*
            "# Compiled gate depth: ~d~%"
            depth)))

(defun print-gate-volume (lschedule)
  (let ((volume (quil::lscheduler-calculate-volume lschedule
                                                   :blacklist *gate-blacklist*
                                                   :whitelist *gate-whitelist*)))
    (setf (gethash "gate_volume" *statistics-dictionary*) volume)
    (format *human-readable-stream*
            "# Compiled gate volume: ~d~%"
            volume)))

(defun print-program-runtime (lschedule chip-specification)
  (let ((duration (quil::lscheduler-calculate-duration lschedule
                                                       chip-specification)))
    (setf (gethash "program_duration" *statistics-dictionary*) duration)
    (format *human-readable-stream*
            "# Compiled program duration: ~5d~%"
            duration)))

(defun print-topological-swap-count (topological-swaps)
  (setf (gethash "topological_swaps" *statistics-dictionary*) topological-swaps)
  (format *human-readable-stream*
          "# SWAPs incurred by topological considerations: ~d~%"
          topological-swaps))

(defun print-program (initial-l2p processed-quil final-l2p &optional (stream *standard-output*))
  (let ((*print-pretty* nil))
    (format stream "PRAGMA EXPECTED_REWIRING \"~s\"~%" initial-l2p))
  (let ((quil::*print-fractional-radians* (not *without-pretty-printing*)))
    (print-quil-list processed-quil stream))
  (let ((*print-pretty* nil))
    (format stream "PRAGMA CURRENT_REWIRING \"~s\"~%" final-l2p)))

(defun publish-json-statistics ()
  (yason:encode *statistics-dictionary* *json-stream*))


(defun entry-point (argv)
  (handler-case (%entry-point argv)
    (sb-sys:interactive-interrupt (c)
      (declare (ignore c))
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
   :rest-arity nil)
  
  (princ *quil-stream*)
  (terpri)
  (princ *json-stream*)
  (terpri)
  (princ *human-readable-stream*)
  (terpri)
  
  ;; rebind the MAGICL libraries
  (magicl:with-blapack
    (reload-foreign-libraries)
    ;; slurp the program from *standard-in*
    (let* ((program-text (slurp-lines))
           (program (quil::parse-quil program-text))
           (reference-program (quil::parse-quil program-text))
           (chip-specification (quil::build-8Q-chip))
           (quil::*compiler-noise-stream* *verbose*))
      ;; do the compilation
      (multiple-value-bind (initial-l2p processed-quil final-l2p topological-swaps)
          (quil::compiler-hook program chip-specification)
        ;; now that we've compiled the program, we have various things to output
        ;; one thing we're always going to want to output is the program itself.
        (let ((program-as-string (with-output-to-string (s)
                                   (print-program initial-l2p processed-quil final-l2p s))))
          (setf (gethash "processed_program" *statistics-dictionary*)
                program-as-string)
          (write-string program-as-string *quil-stream*))
        
        
        (when *topological-swaps*
          (print-topological-swap-count topological-swaps))
        
        (when (or *compute-gate-depth*
                  *compute-gate-volume*
                  *compute-runtime*)
          ;; calculate some statistics based on logical scheduling
          (let ((lschedule (make-instance 'quil::lscheduler-empty)))
            (quil::append-instructions-to-lschedule lschedule processed-quil)
            (when *compute-gate-depth*
              (print-gate-depth lschedule))
            (when *compute-gate-volume*
              (print-gate-volume lschedule))
            (when *compute-runtime*
              (print-program-runtime lschedule chip-specification))))
        
        (when *compute-matrix-reps*
          (print-matrix-representations initial-l2p processed-quil final-l2p reference-program))
        
        (publish-json-statistics)))))
