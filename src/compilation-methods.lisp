;;;; compilation-methods.lisp
;;;;
;;;; Author: Eric Peterson
;;;;

(in-package #:cl-quil)

;;; A compilation method is a function with inputs
;;;   * a resolved gate-application object
;;;
;;;   * keyword arguments that further specialize the behavior of the
;;;     method and outputs a list of Quil instructions which encode
;;;     the matrix up to global phase
;;;
;;;   OR the signal of (a subcondition of) COMPILER-DOES-NOT-APPLY,
;;;      which means that the supplied instruction is cannot or
;;;      "should" not be compiled with this method. The preferred
;;;      method for signalling a condition is calling
;;;      GIVE-UP-COMPILATION, so that other quilc authors have the
;;;      opportunity to install more complicated error handling.
;;;
;;;   OR an error, which means that the compilation routine
;;;      experienced an error.
;;;
;;; Compilation methods should bring you *closer* to some desired
;;; output. If it doesn't, or if it can't, one of these conditions
;;; should be signalled.

(define-condition compiler-does-not-apply (serious-condition)
  ()
  (:documentation "A condition that is signalled anytime a compiler doesn't apply. In general, a sub-condition should be preferred over signalling this one."))

(define-condition compiler-invalid-domain (compiler-does-not-apply)
  ()
  (:documentation "This is signaled by a compilation method when the input matrix is outside the input domain of the method."))

(define-condition compiler-acts-trivially (compiler-does-not-apply)
  ()
  (:documentation "This is signaled when a compiler is technically applicable, but would act as an identity."))

(define-condition compiler-rewrite-does-not-apply (compiler-does-not-apply)
  ()
  (:documentation "This is signaled when a rewriting rule cannot be applied to an instruction sequence."))


(defun give-up-compilation (&key (because ':unknown))
  (ecase because
    (:invalid-domain         (error 'compiler-invalid-domain))
    (:acts-trivially         (error 'compiler-acts-trivially))
    (:unknown                (error 'compiler-does-not-apply))))


;;; Core routines governing how a chip-specification's compiler list is walked

(defun apply-translation-compilers (instruction chip-spec hardware-object)
  "Wrapper function that calls the compilers associated to HARDWARE-OBJECT and the generic compilers associated to CHIP-SPEC in precedence order, returning the first found expansion of INSTRUCTION as a sequence."
  (let ((context (make-compilation-context :chip-specification chip-spec)))
    (labels ((try-compiler (compilation-method)
               "Applies COMPILATION-METHOD to INSTRUCTION. If it succeeds, end
              the whole procedure and return the resulting instruction sequence.
              If it fails, cede control by returning NIL."
               (restart-case
                   (handler-case
                       (let ((result (funcall compilation-method instruction :context context)))
                         (let ((*print-pretty* nil))
                           (format-noise
                            "APPLY-TRANSLATION-COMPILERS: Applying ~A to ~/cl-quil:instruction-fmt/."
                            compilation-method instruction))
                         (format-noise "~{    ~/cl-quil:instruction-fmt/~%~}" result)
                         (return-from apply-translation-compilers result))
                     (compiler-does-not-apply () nil))
                 (try-next-compiler ()
                   :report "Ignore this error and try the next compiler in the list."))))
      ;; if this is a thread invocation, call its expander
      (when (typep instruction 'application-thread-invocation)
        (return-from apply-translation-compilers
          (application-thread-invocation-thread instruction)))
      ;; then try the compilers attached to the hardware object
      (when hardware-object
        (map nil #'try-compiler (hardware-object-compilation-methods hardware-object)))
      ;; if those fail, try the global compilers
      (map nil #'try-compiler (chip-specification-generic-compilers chip-spec))
      ;; if those failed too, there's really nothing more to do.
      (format-noise
       "APPLY-TRANSLATION-COMPILERS: Could not find a compiler for ~/cl-quil:instruction-fmt/."
       instruction)
      (give-up-compilation))))


;;; Core public-facing routine for a full compilation pass.

(define-condition not-protoquil (simple-error)
  ((program :reader not-protoquil-program :initarg :program)
   (index :reader not-protoquil-index :initarg :index))
  (:report (lambda (c s)
             (let ((j (not-protoquil-index c))
                   (p (parsed-program-executable-code (not-protoquil-program c))))
               (format s "Misplaced or illegal instruction in ProtoQuil program:~%  ")
               (cond ((zerop j)
                      (write-string "(BEGINNING OF PROGRAM)" s))
                     (t
                      (print-instruction (aref p (1- j)) s)))
               (format s "~%>>>")
               (print-instruction (aref p j) s)
               (format s "~%  ")
               (cond ((= (length p) (1+ j))
                      (write-string "(END OF PROGRAM)" s))
                     (t
                      (print-instruction (aref p (1+ j)) s))))))
  (:documentation "Error raised when a program does not validate as protoquil."))

(defun check-protoquil-program (program)
  "Checks that PROGRAM, an application-resolved parsed-program instance, conforms to the present definition of \"protoQuil\". Signals an error on failure."
  ;; a protoquil program carves up into 3 regions, each optional, of the form:
  ;; (1) a RESET instruction
  ;; (2) quantum gates, perhaps with classical memory references or classical arithmetic
  ;; (3) a collection of MEASURE instructions
  (let ((region-counter 1))
    (dotimes (j (length (parsed-program-executable-code program)))
      (let ((instr (aref (parsed-program-executable-code program) j)))
        (cond
          ;; PRAGMAs can appear anywhere.
          ((typep instr 'pragma)
           t)
          ((and (= 1 region-counter)
                (typep instr 'reset))
           (setf region-counter 2))
          ((and (>= 2 region-counter)
                (typep instr 'application))
           (setf region-counter 2))
          ((and (>= 3 region-counter)
                (typep instr 'measurement))
           (setf region-counter 3))
          (t
           (error 'not-protoquil :program program :index j)))))))

(defun protoquil-program-p (program)
  "Returns T if program satisfies the requirements of a protoquil program, NIL otherwise."
  (check-type program parsed-program)
  (handler-case (check-protoquil-program program)
    (not-protoquil () (return-from protoquil-program-p nil)))
  t)

;;; COMPILER-HOOK used to be here, but has moved to
;;; compiler-hook.lisp.

