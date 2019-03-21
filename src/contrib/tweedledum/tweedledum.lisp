;;;; cl-tweedledum.lisp

(defpackage #:cl-quil.tweedledum
  (:use #:common-lisp
        #:cffi)
  (:export #:synthesis-dbs))

(in-package #:cl-quil.tweedledum)

(cffi:define-foreign-library libtweedledum
  (:darwin (:or #.(merge-pathnames "libtweedledum.dylib"
                                   (or *compile-file-truename*
                                       *load-truename*))
                "libtweedledum.dylib"
                "tweedledum.dylib"))
  (:unix  (:or #.(merge-pathnames "libtweedledum.so"
                                  (or *compile-file-truename*
                                      *load-truename*))
               "libtweedledum.so"
               "tweedledum.so"))

  (t (:default "libtweedledum")))

(defvar *tweedledum-libs-loaded* nil)

(unless *tweedledum-libs-loaded*
  (cffi:load-foreign-library 'libtweedledum)
  (setf *tweedledum-libs-loaded* t))


(defcfun (%synthesis-dbs "tweedledum_synthesis_dbs")
    :string
  (perm (:pointer :uint32))
  (size :int))

(defun synthesis-dbs (permutation)
  (with-foreign-object (perm :uint32 (length permutation)) 
    (loop :for i :below (length permutation)
          :for p_i :in permutation :do
            (setf (cffi:mem-aref perm :uint32 i) p_i))
    (%synthesis-dbs perm (length permutation))))

(defun compile-perm-gate-with-tweedledum (instr)
  (let ((perm-gate (quil::gate-application-gate instr)))
    (cond
      ((typep perm-gate 'quil::permutation-gate)
       (let* ((perm (coerce (quil::permutation-gate-permutation perm-gate) 'list))
              (synth (synthesis-dbs perm))
              (code (quil::parsed-program-executable-code
                     (quil::parse-quil-string synth))))
         (coerce code 'list)))
      (t
       (quil::give-up-compilation)))))

#+#:testcode
(progn
  (let ((lschedule (quil::make-lscheduler)))
    (loop :for instr
            :across (quil::parsed-program-executable-code
                     (quil::compiler-hook (quil::parse-quil-string "CCNOT 3 2 1")
                                          (quil::build-nQ-trivalent-chip 1 1 8 4 '(:cz :iswap))))
          :when (and (typep instr 'quil::gate-application)
                     (<= 2 (length (quil::application-arguments instr))))
            :do (quil::append-instruction-to-lschedule lschedule instr))
    (quil::lscheduler-calculate-depth lschedule))
  (let ((lschedule (quil::make-lscheduler)))
    (push (constantly #'cl-quil.tweedledum::compile-perm-gate-with-tweedledum) quil::*global-compilers*)
    (loop :for instr
            :across (quil::parsed-program-executable-code
                     (quil::compiler-hook (quil::parse-quil-string "CCNOT 3 2 1")
                                          (quil::build-nQ-trivalent-chip 1 1 8 4 '(:cz :iswap))))
          :when (and (typep instr 'quil::gate-application)
                     (<= 2 (length (quil::application-arguments instr))))
            :do (quil::append-instruction-to-lschedule lschedule instr))
    (pop quil::*global-compilers*)
    (quil::lscheduler-calculate-depth lschedule)))
