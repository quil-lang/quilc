
(in-package #:cl-user)

(unless *load-truename*
  (error "please load me"))

(setf *compile-verbose* t
      c::*compile-print* t
      c::*suppress-compiler-messages* 'c::compiler-debug-note)

(proclaim `(optimize (speed 3) (debug 0) (safety 1)))

(require 'cmp)
(require 'asdf)

#+ (or)
;; Non-monolithic build requires other dependency libraries to be
;; available compiled separately.
(asdf:make-build :cl-quil
                 :type :shared-library
                 :move-here #P"./"
                 :init-name "cl_quil__init")

;; Idea that UIOP is a preloaded system in ASDF is braindead. It means
;; that UIOP must be downloaded manually and put in some location
;; accessible by ASDF (QL won't download it, because system is already
;; present!). ASDF also assumes that it is present in the image at all
;; times - to put things on scale it is as if makefile were part of
;; all libraries which use it for building.
;;
;; Apparently cffi-toolchain depends on ASDF itself (i.e it calls
;; ASDF:WHATSOEVER in the lisp files at runtime), so we need to link
;; ASDF anyway (indirect dependency from magicl).
;;
;; There are three ways around that:
;; 
;; 1. Put (require "ASDF") in prologue code -- it has an advantage
;; that it "just works" for the average case. It finds UIOP built with
;; the implementation and loads it before the rest of the code. From
;; disadventages -- you must include ecl contribs in the distribution
;; (just as if you had installed libffi-dev package) and effectively
;; prevents fully static linking. That also increases the load time.
;;
;; 2. Build ECL with statically linked ASDF. Simple and effective but
;; conceptually broken, ASDF is a build utility not part of the
;; application. Also increases size a lot (libasdf is bigger than
;; libecl!). That also increases the load time.
;;
;; 3. My favourite - avoid dependency on uiop and asdf like fire
;; because of multiple problems caused by such
;; dependencies. Buildsystem should be required only at compilation
;; time.
;;
;; 4. [doesn't work well] make an artificial system depending on
;; cl-quil and prebuilt-asdf. It doesn't work becasue we don't know
;; the init function name for the static library libasdf.a. This may
;; be fixed in the future.

(defun build-shared ()
  (asdf:make-build :cl-quil
                   :type :shared-library
                   :move-here #P"."
                   :monolithic t
                   :init-name "quil_init"
                   :prologue-code '(progn
                                    (require "ASDF")
                                    (princ ";;; Initializing QUILC dependencies.")
                                    (terpri))))

(defun build-static ()
  (asdf:make-build :cl-quil
                   :type :static-library
                   :move-here #P"."
                   :monolithic t
                   :init-name "quil_init"
                   :prologue-code '(progn
                                    (require "ASDF")
                                    (princ ";;; Initializing QUILC dependencies.")
                                    (terpri))))
