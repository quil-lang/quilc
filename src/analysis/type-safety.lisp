;;;; type-safety.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil/frontend)

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

(defun memory-segment-length (memory-descriptor &key offset)
  "Get the length of the memory segment specified by MEMORY-DESCRIPTOR, relative to the offset indicated by the memory reference OFFSET."
  (check-type memory-descriptor memory-descriptor)
  (check-type offset (or null memory-ref))
  (if offset
      (- (memory-descriptor-length memory-descriptor)
         (memory-ref-position offset))
      (memory-descriptor-length memory-descriptor)))

(defun enforce-mref-bounds (mref memory-descriptor)
  (when (and (typep mref 'memory-ref)
             (not (plusp (memory-segment-length memory-descriptor :offset mref))))
    (quil-type-error "Memory ref \"~/cl-quil:instruction-fmt/\" exceeds region size ~A."
                     mref
                     (memory-descriptor-length memory-descriptor))))

(defun constant-or-mref-typep (obj quil-type memory-regions)
  "Test whether OBJ (a CONSTANT or a MEMORY-REF) has a value that has type QUIL-TYPE (or one of those types if QUIL-TYPE is a list)."
  (etypecase obj
    (memory-ref
     (let ((memory-region (find-descriptor-for-mref obj memory-regions)))
       #+allegro
       (member (memory-descriptor-type memory-region) (a:ensure-list quil-type) :test 'equalp)
       #-allegro
       (member (memory-descriptor-type memory-region) (a:ensure-list quil-type))))
    (constant
     #+allegro
     (member (constant-value-type obj) (a:ensure-list quil-type) :test 'equalp)
     #-allegro
     (member (constant-value-type obj) (a:ensure-list quil-type)))))

(defun check-mref (obj)
  (unless (typep obj 'memory-ref)
    (quil-type-error "Argument expected to be a memory reference, but got ~/cl-quil:instruction-fmt/."
                     obj)))

;;; helper functions for type-check-instr ;;;

(defun handle-binary-args-for-binary-classical-instr (instr memory-regions &key (object-to-type nil))
  "Performs type agreement and literal type coercion for operands to binary classical instructions. Returns T on success, error with QUIL-TYPE-ERROR on failure."
  (check-mref (classical-left-operand instr))
  (let ((mdesc (find-descriptor-for-mref (classical-left-operand instr) memory-regions)))
    (when (and object-to-type
               (equalp object-to-type (memory-descriptor-type mdesc)))
      (quil-type-error "Argument should be a binary memory reference, but got real ~/cl-quil:instruction-fmt/."
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
           (quil-type-error "Expected REAL assignment based on ~/cl-quil:instruction-fmt/, but got ~/cl-quil:instruction-fmt/."
                            (classical-left-operand instr)
                            (classical-right-operand instr))))
      (quil-octet
       (if (or (constant-or-mref-typep (classical-right-operand instr)
                                       quil-octet
                                       memory-regions)
               (and (typep (classical-right-operand instr) 'constant)
                    (equal quil-integer (constant-value-type (classical-right-operand instr)))))
           t
           (quil-type-error "Expected OCTET assignment based on ~/cl-quil:instruction-fmt/, but got ~/cl-quil:instruction-fmt/."
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

;;; From the Quil spec
;; # Comparison
;; EQ       r a b          # r := (a == b)
;; GT       r a b          # r := (a > b)
;; GE       r a b          # r := (a >= b)
;; LT       r a b          # r := (a < b)
;; LE       r a b          # r := (a <= b)
;;          <bit> <bit> <bit>
;;          <bit> <bit> <!int>
;;          <bit> <oct> <oct>
;;          <bit> <oct> <!int>
;;          <bit> <int> <int>
;;          <bit> <int> <!int>
;;          <bit> <real> <real>
;;          <bit> <real> <!real>
;;
;; We don't need to check if the RHS is a memref of a particular
;; type. That is done by DEFINE-CLASSICAL-INSTRUCTION.

(defun typecheck-comparison-instruction (instr memory-regions)
  (check-mref (classical-target instr))
  (unless (constant-or-mref-typep (classical-target instr) quil-bit memory-regions)
    (quil-type-error "Conditional tests write to BIT memory, but got ~/cl-quil:instruction-fmt/ instead."
                     (classical-target instr)))
  (check-mref (classical-left-operand instr))
  (let ((mdesc (find-descriptor-for-mref (classical-left-operand instr)
                                         memory-regions))
        (rop (classical-right-operand instr)))
    (adt:match quil-type (memory-descriptor-type mdesc)
      (quil-bit
       (unless (constant-or-mref-typep rop (list quil-bit quil-integer) memory-regions)
         (quil-type-error "Conditional test for left-hand operand of type BIT requires right-hand operand of type BIT or immediate INT but got ~/cl-quil:instruction-fmt/ and ~/cl-quil:instruction-fmt/."
                          (classical-left-operand instr)
                          rop )))
      (quil-real
       (unless (constant-or-mref-typep rop quil-real memory-regions)
         (quil-type-error "Conditional test for left-hand operand of type REAL requires right-hand operand of type REAL or immediate REAL but got ~/cl-quil:instruction-fmt/ and ~/cl-quil:instruction-fmt/."
                          (classical-left-operand instr)
                          rop)))
      (quil-octet
       (unless (constant-or-mref-typep rop (list quil-octet quil-integer) memory-regions)
         (quil-type-error "Conditional test for left-hand operand of type OCTET requires right-hand operand of type OCTET or immediate INTEGER but got ~/cl-quil:instruction-fmt/ and ~/cl-quil:instruction-fmt/."
                          (classical-left-operand instr)
                          rop)))
      (quil-integer
       (unless (constant-or-mref-typep rop quil-integer memory-regions)
         (quil-type-error "Conditional test for left-hand operand of type INTEGER requires right-hand operand of type INTEGER or immediate INTEGER but got ~/cl-quil:instruction-fmt/ and ~/cl-quil:instruction-fmt/."
                          (classical-left-operand instr)
                          rop))))))

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
         (quil-type-error "Memory reference ~/cl-quil:instruction-fmt/ is used as a gate parameter but is not a REAL value."
                          param))))))

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
        (_            (quil-type-error "NEG argument should be a signed memory reference, but got ~/cl-quil:instruction-fmt/."
                                       (classical-target instr))))))

  ;; NOT can't be REAL
  (:method ((instr classical-not) memory-regions)
    (check-mref (classical-target instr))
    (let ((mdesc (find-descriptor-for-mref (classical-target instr) memory-regions)))
      (adt:match quil-type (memory-descriptor-type mdesc)
        (quil-real
         (quil-type-error "NOT argument should be a binary memory reference, but got ~/cl-quil:instruction-fmt/."
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
        (quil-type-error "Memory region types do not match for ~A and ~/cl-quil:instruction-fmt/."
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
                                          (list (memory-descriptor-type mdesc)
                                                quil-integer)
                                          memory-regions)
                  (and (equal quil-octet (memory-descriptor-type mdesc))
                       (constant-or-mref-typep (classical-right-operand instr)
                                               quil-integer
                                               memory-regions)))
        (quil-type-error "Memory region types do not match for ~A and ~/cl-quil:instruction-fmt/."
                         (memory-name-region-name (classical-target instr))
                         (classical-right-operand instr)))
      (unless (constant-or-mref-typep (classical-left-operand instr)
                                      quil-integer
                                      memory-regions)
        (quil-type-error "STORE middle operand must be an INTEGER, but got ~/cl-quil:instruction-fmt/."
                         (classical-left-operand instr)))))

  ;; comparison operators require bit target in first, agreement in last two
  (:method :before ((instr classical-equality) memory-regions)
    (typecheck-comparison-instruction instr memory-regions))

  (:method :before ((instr classical-greater-than) memory-regions)
    (typecheck-comparison-instruction instr memory-regions))

  (:method :before ((instr classical-greater-equal) memory-regions)
    (typecheck-comparison-instruction instr memory-regions))

  (:method :before ((instr classical-less-than) memory-regions)
    (typecheck-comparison-instruction instr memory-regions))

  (:method :before ((instr classical-less-equal) memory-regions)
    (typecheck-comparison-instruction instr memory-regions))

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
                           BIT or INTEGER, but got ~/cl-quil:instruction-fmt/ of type ~A."
                          (measure-address instr)
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
