;;;; type-safety.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

(define-condition quil-type-error (simple-error)
  ()
  (:documentation "Representation of an error type-checking Quil."))

(defun quil-type-error (format-control &rest format-args)
  "Signal a QUIL-TYPE-ERROR with a descriptive error message described by FORMAT-CONTROL and FORMAT-ARGS."
  (error 'quil-type-error :format-control format-control
                          :format-arguments format-args))

(define-transform type-check (type-check)
  "A transform which checks for type alignment on classical memory references, including conversion of constant literals to the appropriate type."
  expand-circuits)

;;; utilities ;;;

(defun find-descriptor-for-mref (mref memory-regions)
  (check-type mref (or memory-ref memory-name))
  (let* ((name (etypecase mref
                (memory-ref (memory-ref-name mref))
                (memory-name (memory-name-region-name mref))))
         (desc (find name memory-regions :key #'memory-descriptor-name :test #'string=)))
    (etypecase mref
      (memory-ref (setf (memory-ref-descriptor mref) desc))
      (memory-name (setf (memory-name-descriptor mref) desc)))
    desc))

(defun mref-available-length (mref memory-descriptor)
  (- (memory-descriptor-length memory-descriptor)
     (memory-ref-position mref)))

(defun enforce-mref-bounds (mref memory-descriptor)
  (when (and (typep mref 'memory-ref)
             (not (plusp (mref-available-length mref
                                                memory-descriptor))))
    (quil-type-error "Memory ref \"~/quil:instruction-fmt/\" exceeds region size ~A."
                     mref
                     (memory-descriptor-length memory-descriptor))))

(defun constant-or-mref-typep (obj quil-type memory-regions)
  (etypecase obj
    (memory-ref
     (let ((memory-region (find-descriptor-for-mref obj memory-regions)))
       (equalp quil-type (memory-descriptor-type memory-region))))
    (constant
     (equalp quil-type (constant-value-type obj)))))

(defun check-mref (obj)
  (unless (typep obj 'memory-ref)
    (quil-type-error "Argument expected to be a memory reference, but got ~/quil:instruction-fmt/."
                     obj)))

;;; helper functions for type-check-instr ;;;

(defun handle-binary-args-for-binary-classical-instr (instr memory-regions &key (object-to-type nil))
  "Performs type agreement and literal type coercion for operands to binary classical instructions. Returns T on success, error with QUIL-TYPE-ERROR on failure."
  (check-mref (classical-left-operand instr))
  (let ((mdesc (find-descriptor-for-mref (classical-left-operand instr) memory-regions)))
    (when (and object-to-type
               (equalp object-to-type (memory-descriptor-type mdesc)))
      (quil-type-error "Argument should be a binary memory reference, but got real ~/quil:instruction-fmt/."
                       (classical-left-operand instr)))
    (adt:match quil-type (memory-descriptor-type mdesc)
      (quil-real
       ;; quietly convert integer literals to real literals when assigned to real places
       (when (and (typep (classical-right-operand instr) 'constant)
                  (equal quil-integer (constant-value-type (classical-right-operand instr))))
         (setf (classical-right-operand instr)
               (constant (coerce (constant-value (classical-right-operand instr)) 'double-float)
                         quil-real)))
       (if (constant-or-mref-typep (classical-right-operand instr)
                                   quil-real
                                   memory-regions)
           t
           (quil-type-error "Expected REAL assignment based on ~/quil:instruction-fmt/, but got ~/quil:instruction-fmt/."
                            (classical-left-operand instr)
                            (classical-right-operand instr))))
      (quil-octet
       (if (or (constant-or-mref-typep (classical-right-operand instr)
                                       quil-octet
                                       memory-regions)
               (and (typep (classical-right-operand instr) 'constant)
                    (equal quil-integer (constant-value-type (classical-right-operand instr)))))
           t
           (quil-type-error "Expected OCTET assignment based on ~/quil:instruction-fmt/, but got ~/quil:instruction-fmt/."
                            (classical-left-operand instr)
                            (classical-right-operand instr))))
      (quil-bit
       (when (typep (classical-right-operand instr) 'constant)
         (case (constant-value (classical-right-operand instr))
           ((0 1)
            (setf (classical-right-operand instr)
                  (constant (coerce (constant-value (classical-right-operand instr)) 'bit)
                            quil-bit)))
           (otherwise
            (quil-type-error "Assignment to BIT field from non-BIT literal ~A."
                             (constant-value (classical-right-operand instr))))))
       (unless (constant-or-mref-typep (classical-right-operand instr)
                                       quil-bit
                                       memory-regions)
         (quil-type-error "Assignment to BIT field from non-BIT ~A."
                          (classical-right-operand instr))))
      (quil-integer
       (unless (constant-or-mref-typep (classical-right-operand instr)
                                       quil-integer
                                       memory-regions)
         (quil-type-error "Assignment to INTEGER field from non-INTEGER ~A."
                          (classical-right-operand instr)))))))

(defun typecheck-conditional-instruction (instr memory-regions)
  (check-mref (classical-target instr))
  (unless (constant-or-mref-typep (classical-target instr) quil-bit memory-regions)
    (quil-type-error "Conditional tests write to BIT memory, but got ~/quil:instruction-fmt/ instead."
                     (classical-target instr)))
  (check-mref (classical-left-operand instr))
  (let ((mdesc (find-descriptor-for-mref (classical-left-operand instr)
                                         memory-regions)))
    (unless (or (constant-or-mref-typep (classical-right-operand instr)
                                        (memory-descriptor-type mdesc)
                                        memory-regions)
                (and (equal quil-octet (memory-descriptor-type mdesc))
                     (constant-or-mref-typep (classical-right-operand instr)
                                             quil-integer
                                             memory-regions)))
      (quil-type-error "Conditional tests require type agreement in last two terms, but got ~/quil:instruction-fmt/ and ~/quil:instruction-fmt/."
                       (classical-left-operand instr)
                       (classical-right-operand instr)))))

(defun walk-parameter-for-real-values (param memory-regions)
  (etypecase param
    (number
     nil)
    (delayed-expression
     (walk-parameter-for-real-values
      (delayed-expression-expression param)
      memory-regions))
    (constant
     (unless (equal quil-real (constant-value-type param))
       (setf (constant-value param) (coerce (constant-value param) 'double-float))
       (setf (constant-value-type param) quil-real)))
    (cons
     (dolist (subparam (rest param))
       (walk-parameter-for-real-values subparam memory-regions)))
    (formal
     nil)
    (memory-ref
     (let ((mdesc (find-descriptor-for-mref param memory-regions)))
       (setf (memory-ref-descriptor param) mdesc)
       (unless (equal quil-real (memory-descriptor-type mdesc))
         (quil-type-error "Memory reference ~/quil:instruction-fmt/ is used as a gate parameter but is not a REAL value."
                          param))))))

(defun raw-capture-num-real-samples (instr)
  (check-type instr raw-capture)
  (let ((frame-defn (frame-name-resolution
                     (raw-capture-frame instr))))
    (if (frame-definition-sample-rate frame-defn)
        (* 2                            ; real, imag
           (constant-value (raw-capture-duration instr))
           (constant-value (frame-definition-sample-rate frame-defn)))
        nil)))

;;; real deal ;;;

(defgeneric type-check-instr (instr memory-regions)
  (:documentation "Ensures type alignment for classical memory references in an instruction.")

  ;; dummy methods

  (:method ((instr jump-target) memory-regions)
    (declare (ignore memory-regions))
    nil)

  (:method ((instr instruction) memory-regions)
    (declare (ignore memory-regions))
    nil)

  ;; all mref references have to be legal. may as well check them all in the same place
  (:method ((instr unary-classical-instruction) memory-regions)
    (let ((mref (classical-target instr)))
      (enforce-mref-bounds mref (find-descriptor-for-mref mref memory-regions))))

  (:method ((instr binary-classical-instruction) memory-regions)
    (let ((left-mref (classical-left-operand instr))
          (right-mref (classical-right-operand instr)))
      (enforce-mref-bounds left-mref (find-descriptor-for-mref left-mref memory-regions))
      (when (typep right-mref 'memory-ref)
        (enforce-mref-bounds right-mref (find-descriptor-for-mref right-mref memory-regions)))))

  (:method ((instr trinary-classical-instruction) memory-regions)
    (let ((target-mref (classical-target instr))
          (left-mref (classical-left-operand instr))
          (right-mref (classical-right-operand instr)))
      (enforce-mref-bounds target-mref (find-descriptor-for-mref target-mref memory-regions))
      (enforce-mref-bounds left-mref (find-descriptor-for-mref left-mref memory-regions))
      (when (typep right-mref 'memory-ref)
        (enforce-mref-bounds right-mref (find-descriptor-for-mref right-mref memory-regions)))))

  ;; NEG needs to be INT or REAL
  (:method ((instr classical-negate) memory-regions)
    (check-mref (classical-target instr))
    (let ((mdesc (find-descriptor-for-mref (classical-target instr) memory-regions)))
      (adt:match quil-type (memory-descriptor-type mdesc)
        (quil-real    (call-next-method))
        (quil-integer (call-next-method))
        (_            (quil-type-error "NEG argument should be a signed memory reference, but got ~/quil:instruction-fmt/."
                                       (classical-target instr))))))

  ;; NOT can't be REAL
  (:method ((instr classical-not) memory-regions)
    (check-mref (classical-target instr))
    (let ((mdesc (find-descriptor-for-mref (classical-target instr) memory-regions)))
      (adt:match quil-type (memory-descriptor-type mdesc)
        (quil-real
         (quil-type-error "NOT argument should be a binary memory reference, but got ~/quil:instruction-fmt/."
                          (classical-target instr)))
        (_
         (call-next-method)))))

  ;; binary bitwise ops requirements agreement and can't be REAL
  (:method :before ((instr classical-and) memory-regions)
    (handle-binary-args-for-binary-classical-instr instr memory-regions
                                              :object-to-type quil-real))

  (:method :before ((instr classical-inclusive-or) memory-regions)
    (handle-binary-args-for-binary-classical-instr instr memory-regions
                                              :object-to-type quil-real))

  (:method :before ((instr classical-exclusive-or) memory-regions)
    (handle-binary-args-for-binary-classical-instr instr memory-regions
                                              :object-to-type quil-real))

  ;; MOVE requires agreement
  (:method :before ((instr classical-move) memory-regions)
    (handle-binary-args-for-binary-classical-instr instr memory-regions))

  ;; EXCHANGE requires agreement
  (:method :before ((instr classical-exchange) memory-regions)
    (handle-binary-args-for-binary-classical-instr instr memory-regions))

  ;; CONVERT doesn't apply to OCTETs and doesn't allow literals.
  (:method :before ((instr classical-convert) memory-regions)
    (let ((args (arguments instr))
          (resolver (memory-descriptors->type-resolver memory-regions))
          (modes (mnemonic-addressing-modes "CONVERT")))
      (block type-check
        (dolist (mode modes)
          (when (argument-types-match-p resolver mode args)
            (return-from type-check t)))
        (quil-type-error "The convert instruction ~A didn't type check." instr))))

  ;; arithmetic requires type agreement
  (:method :before ((instr classical-addition) memory-regions)
    (handle-binary-args-for-binary-classical-instr instr memory-regions))

  (:method :before ((instr classical-subtraction) memory-regions)
    (handle-binary-args-for-binary-classical-instr instr memory-regions))

  (:method :before ((instr classical-multiplication) memory-regions)
    (handle-binary-args-for-binary-classical-instr instr memory-regions))

  (:method :before ((instr classical-division) memory-regions)
    (handle-binary-args-for-binary-classical-instr instr memory-regions))

  ;; LOAD requires agreement in first two, INT in last
  (:method :before ((instr classical-load) memory-regions)
    (check-mref (classical-target instr))
    (let ((mdesc
            (find-descriptor-for-mref (mref (memory-name-region-name (classical-left-operand instr)) 0)
                                      memory-regions)))
      (unless mdesc
        (quil-type-error "Memory region named ~A not found."
                         (memory-name-region-name (classical-left-operand instr))))
      (unless (constant-or-mref-typep (classical-target instr)
                                      (memory-descriptor-type mdesc)
                                      memory-regions)
        (quil-type-error "Memory region types do not match for ~A and ~/quil:instruction-fmt/."
                         (memory-name-region-name (classical-left-operand instr))
                         (classical-target instr)))
      (unless (constant-or-mref-typep (classical-right-operand instr)
                                      quil-integer
                                      memory-regions)
        (quil-type-error "LOAD right operand must be an integer, but got ~A."
                         (classical-right-operand instr)))))

  ;; STORE requires agreement in outer two, INT in middle
  (:method :before ((instr classical-store) memory-regions)
    (let ((mdesc
            (find-descriptor-for-mref (mref (memory-name-region-name (classical-target instr)) 0)
                                      memory-regions)))
      (unless mdesc
        (quil-type-error "Memory region named ~A not found"
                         (memory-name-region-name (classical-left-operand instr))))
      (unless (or (constant-or-mref-typep (classical-right-operand instr)
                                          (memory-descriptor-type mdesc)
                                          memory-regions)
                  (and (equal quil-octet (memory-descriptor-type mdesc))
                       (constant-or-mref-typep (classical-right-operand instr)
                                               quil-integer
                                               memory-regions)))
        (quil-type-error "Memory region types do not match for ~A and ~/quil:instruction-fmt/."
                         (memory-name-region-name (classical-target instr))
                         (classical-right-operand instr)))
      (unless (constant-or-mref-typep (classical-left-operand instr)
                                      quil-integer
                                      memory-regions)
        (quil-type-error "STORE middle operand must be an INTEGER, but got ~/quil:instruction-fmt/."
                         (classical-left-operand instr)))))

  ;; comparison operators require bit target in first, agreement in last two
  (:method :before ((instr classical-equality) memory-regions)
    (typecheck-conditional-instruction instr memory-regions))

  (:method :before ((instr classical-greater-than) memory-regions)
    (typecheck-conditional-instruction instr memory-regions))

  (:method :before ((instr classical-greater-equal) memory-regions)
    (typecheck-conditional-instruction instr memory-regions))

  (:method :before ((instr classical-less-than) memory-regions)
    (typecheck-conditional-instruction instr memory-regions))

  (:method :before ((instr classical-less-equal) memory-regions)
    (typecheck-conditional-instruction instr memory-regions))

  ;; MEASURE must target a BIT or INT
  (:method ((instr measure) memory-regions)
    (check-mref (measure-address instr))
    (let ((mdesc (find-descriptor-for-mref (measure-address instr)
                                           memory-regions)))
      (adt:match quil-type (memory-descriptor-type mdesc)
        (quil-bit
         t)
        (quil-integer
         t)
        (_
         (quil-type-error "MEASURE instruction target must be of type ~
                           BIT or INTEGER, but got ~/quil:instruction-fmt/ of type ~A."
                          (measure-address instr)
                          (quil-type-string (memory-descriptor-type mdesc)))))))

  ;; CAPTURE must target a REAL[2]
  (:method ((instr capture) memory-regions)
    (let* ((mref (capture-memory-ref instr))
           (mdesc (find-descriptor-for-mref mref memory-regions)))
      (enforce-mref-bounds mref mdesc)
      (adt:match quil-type (memory-descriptor-type mdesc)
        (quil-real
         (if (> 2 (mref-available-length mref mdesc))
             (quil-type-error "CAPTURE instruction target ~A must be a REAL ~
                               vector of length no less than 2."
                              (print-instruction-to-string mref))
             t))
        (_
         (quil-type-error "CAPTURE instruction target must be of type ~
                           REAL, but got ~A of type ~A"
                          (print-instruction-to-string mref)
                          (quil-type-string (memory-descriptor-type mdesc)))))))

  ;; RAW-CAPTURE must target a REAL[n] where n is 2*(the number of iq values)
  (:method ((instr raw-capture) memory-regions)
    (let* ((mref (raw-capture-memory-ref instr))
           (mdesc (find-descriptor-for-mref mref memory-regions))
           (frame-defn (frame-name-resolution
                        (raw-capture-frame instr))))
      (enforce-mref-bounds mref mdesc)
      (adt:match quil-type (memory-descriptor-type mdesc)
        (quil-real
         (a:if-let ((samples (raw-capture-num-real-samples instr)))
           (if (> samples (mref-available-length mref mdesc))
               (quil-type-error "RAW-CAPTURE instruction target ~A must be a REAL ~
                                 vector of length no less than ~A."
                                (print-instruction-to-string mref)
                                samples))
           (warn "RAW-CAPTURE on frame ~A with unknown sample rate."
                 (frame-definition-frame frame-defn))))
        (_
         (quil-type-error "RAW-CAPTURE instruction target must be of type ~
                           REAL, but got ~A of type ~A"
                          (print-instruction-to-string mref)
                          (quil-type-string (memory-descriptor-type mdesc)))))))

  ;; gate parameters must be REAL
  (:method ((instr application) memory-regions)
    (dolist (param (application-parameters instr))
      (walk-parameter-for-real-values param memory-regions))))

(defun type-check (parsed-program)
  "Ensure classical type safety of PARSED-PROGRAM, coercing literals where appropriate."
  (flet ((typecheck-instruction-sequence (seq)
           (map nil (lambda (thing)
                      (let ((memory-descriptors (parsed-program-memory-definitions parsed-program)))
                        (type-check-instr thing memory-descriptors)
                        (specialize-types thing memory-descriptors)))
                seq)))
    (typecheck-instruction-sequence (parsed-program-executable-code parsed-program)))
  parsed-program)
