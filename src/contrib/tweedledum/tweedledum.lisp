;;;; cl-tweedledum.lisp

(defpackage #:cl-quil.tweedledum
  (:use #:common-lisp
        #:cffi)
  (:local-nicknames (:a :alexandria))
  (:export #:synthesis-dbs))

(in-package #:cl-quil.tweedledum)

(cffi:define-foreign-library libtweedledum
  (:darwin (:or "libtweedledum.dylib" "tweedledum.dylib"))
  (:unix (:or "libtweedledum.so" "tweedledum.so"))
  (t (:default "libtweedledum")))

(defvar *tweedledum-libs-loaded* nil
  "T if the tweedledum shared library has been loaded by CFFI.")

(defcfun (%synthesis-dbs "tweedledum_synthesis_dbs")
    (:string :free-from-foreign t)
  (perm (:pointer :uint32))
  (size :int))

(defun synthesis-dbs (permutation)
  (with-foreign-object (perm :uint32 (length permutation))
    (loop :for i :below (length permutation)
          :for p_i :in permutation :do
            (setf (cffi:mem-aref perm :uint32 i) p_i))
    (%synthesis-dbs perm (length permutation))))

;; TODO  ecpeterson a minute ago
;;         As the overripe plum scent might indicate, some changes will probably need to be made
(defun compile-perm-gate-with-tweedledum (instr)
  "Compile the cl-quil instruction INSTR using tweedledum's permutation gate
compilation routines. Conforms to the cl-quil compiler interface, and calls
GIVE-UP-COMPILATION if INSTR is not a permutation gate."
  (unless (slot-boundp instr 'quil::name-resolution)
    (quil::give-up-compilation))
  (let ((res (quil::gate-application-resolution instr)))
    (when (typep res 'quil::gate-definition)
      (setf res (quil::gate-definition-to-gate res)))
    (let* ((perm-gate (funcall
                       (quil::operator-description-gate-lifter
                        (quil::application-operator instr))
                       res))
           (qubits (reverse (quil::application-arguments instr))))
      (cond
        ((and (typep perm-gate 'quil::permutation-gate)
              (> (length qubits) 2))
         (let* ((perm (coerce (quil::permutation-gate-permutation perm-gate) 'list))
                (synth (synthesis-dbs perm))
                (code (quil::parsed-program-executable-code
                       (quil::parse-quil synth)))
                (relabler (lambda (q)
                            (setf (quil::qubit-index q)
                                  (quil::qubit-index (nth (quil::qubit-index q) qubits))))))
           (map nil (a:rcurry #'quil::%relabel-qubits relabler) code)
           (coerce code 'list)))
        (t
         (quil::give-up-compilation))))))

(defun run-tweedledum-tests ()
  (unless *tweedledum-libs-loaded*
    (push (constantly 'cl-quil.tweedledum::compile-perm-gate-with-tweedledum)
          cl-quil::*global-compilers*))
  (uiop:symbol-call ':cl-quil-tests
                    '#:run-cl-quil-tests))

;; TODO Some error handling here
(unless *tweedledum-libs-loaded*
  (cffi:load-foreign-library 'libtweedledum)
  (push (constantly 'compile-perm-gate-with-tweedledum) cl-quil::*global-compilers*)
  (setf *tweedledum-libs-loaded* t))
