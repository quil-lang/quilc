;;;; rewrite-arithmetic.lisp
;;;;
;;;; Authors: Lauren Capelluto, Robert Smith

(in-package #:cl-quil/frontend)

;;; This file contains a transformation to rewrite the given program
;;; with general arithmetic in gate arguments each replaced by a
;;; single memory reference.

(defgeneric rewrite-arithmetic-for-isn (isn mref-name index)
  (:documentation "Handle arithmetic replacement for a single
instruction ISN. ISN is a single instruction from a parsed program.

Any gate parameter with nontrivial arithmetic will be replaced with a
single memory reference, which will be called MREF-NAME and will be
the nth position of that memory reference given by INDEX. MREF-NAME
must be a string designating the name of a declared memory region
which in turn is at least of size INDEX + 1, and holds REALs. (Trivial
arithmetic is arithmetic that only involves constants or parameter
expressions of a single memory reference with no real arithmetic.)

INDEX should be a non-negative integer that has not been used in the
memory referenced by MREF-NAME.

If a rewrite is necessary, then the values returned are:

    - the rewritten instruction, in the form:

          G(mref-name[index], ...) q ...

    - the alist of generated memory references to the recalculation
      function from the original gate parameter

    - index incremented by the number of parameters that were
      rewritten (the next index that would be available in the memory
      descriptor designated by MREF-NAME)

Otherwise, the original instruction is returned with an empty alist
and INDEX unchanged.

Classical instructions, which modify the expected value of a simple
memory reference during program execution, are not supported with this
rewrite scheme and will error.

NOTE: This function does *not* cause side effects.")

  (:method ((isn instruction) mref-name index)
    (values isn '() index))

  (:method ((isn jump-target) mref-name index)
    (values isn '() index))

  (:method ((isn application) mref-name index)
    (check-type mref-name string)
    (check-type index unsigned-byte)
    (let ((recalcs nil)
          (new-params nil)
          (new-isn (copy-instance isn)))
      (dolist (param (application-parameters isn))
        (etypecase param
          (constant
           ;; Arithmetic with only constants will already be a single
           ;; number, we don't have to do any more work.
           (push param new-params))
          (delayed-expression
           (typecase (delayed-expression-expression param)
             (memory-ref
              ;; Simple memory reference with no arithmetic, no extra
              ;; work to do
              (push param new-params))
             (t
              (let* ((new-mref (mref mref-name index))
                     (new-param (make-delayed-expression nil nil new-mref)))
                ;; General case where we have some arithmetic
                ;; expression with memory references Save the
                ;; arithmetic to the hash table
                (push (cons new-mref param) recalcs)
                (incf index)
                ;; Replace the full expression in the program with a
                ;; single memory reference
                (push new-param new-params)))))))
      (setf (application-parameters new-isn) (reverse new-params))
      (values new-isn recalcs index)))

  (:method ((isn unary-classical-instruction) mref-name index)
    (error "Classical instructions, which modify memory references, ~
            are not supported in conjunction with gate parameter ~
            arithmetic rewriting."))

  (:method ((isn binary-classical-instruction) mref-name index)
    (error "Classical instructions, which modify memory references, ~
            are not supported in conjunction with gate parameter ~
            arithmetic rewriting."))

  (:method ((isn trinary-classical-instruction) mref-name index)
    (error "Classical instructions, which modify memory references, ~
            are not supported in conjunction with gate parameter ~
            arithmetic rewriting.")))

(defun rewrite-arithmetic (parsed-prog)
  "Handle arithmetic replacement for all parameters in the parsed
program. New memory references are created to replace the arithmetic
expressions, as elements of a single new memory descriptor.

 This function returns:

  - A new parsed program with each gate parameter collapsed to a
    memory reference if it had previously contained nontrivial
    arithmetic. Iff a new memory descriptor is needed, it is added to
    the declared  memory definitions of this new program, with type
    QUIL-REAL and length given by the number of memory references which
    were needed

  - The original declared memory descriptors of the input program

  - A recalculation hash table from the new memory references to
    functions of the original memory references"
  (let* ((old-code (parsed-program-executable-code parsed-prog))
         (new-code (make-array (length old-code)))
         ;; We use a hash table in case we decide to use more than
         ;; just a single name (i.e., more than just __P). It adds
         ;; little bit of complexity in favor of a bit of additional
         ;; extensibility.
         (recalculation-table (make-hash-table :test 'memory-ref=
                                               :hash-function 'memory-ref-hash))
         (old-memory-descriptors (parsed-program-memory-definitions parsed-prog))
         (new-memory-descriptors old-memory-descriptors)
         ;; This isn't necessarily guaranteed to be unique, but the
         ;; chance that somebody has used this is around
         ;; 0.00000000000000002%. The odds are not in the adversary's
         ;; favor.
         (parameters-mref-name (format nil "__P~D" (random most-positive-fixnum))))
    (loop :with idx-mref := 0
          :for isn :across old-code
          :for i :from 0
          :do (multiple-value-bind (new-isn recalc next-idx)
                  (rewrite-arithmetic-for-isn isn parameters-mref-name idx-mref)
                (setf (aref new-code i) new-isn
                      idx-mref          next-idx)
                (loop :for (mref . func) :in recalc :do
                  (let ((hash-val (gethash mref recalculation-table)))
                    (assert (not hash-val) () "Generated the same arithmetic temporary ~
                                               more than once. This shouldn't happen.")
                    (setf (gethash mref recalculation-table) func))))
          :finally
             (when (plusp idx-mref)
               (push
                (make-memory-descriptor :name parameters-mref-name
                                        :type quil-real
                                        :length idx-mref)
                new-memory-descriptors)))
    (values (make-instance 
             'parsed-program
             :executable-code new-code
             :memory-definitions new-memory-descriptors
             :circuit-definitions (parsed-program-circuit-definitions parsed-prog)
             :gate-definitions (parsed-program-gate-definitions parsed-prog))
            old-memory-descriptors
            recalculation-table)))

(define-transform rewrite-arithmetic (rewrite-arithmetic)
  "This transformation allows for arbitrary arithmetic in Quil gate
parameters to be compiled out and later patched in as with binary
patching. Take, for instance:

    DECLARE a REAL[1]
    G(a, 3 * a, a + 5)

It may not be possible for the lower levels of the stack to handle any
arithmetic expression with the QPU control hardware. We rewrite the
arithmetic into the program by using designated memory references. The
above program would be translated in the following way:

    DECLARE a REAL[1]
    DECLARE __P<D> REAL[2]
    G(a, __P<D>[0], __P<D>[1])

where <D> is a random positive fixnum.  This program is returned with
a list of the original memory descriptors (to which the user can write
any value, which is not allowed for the new descriptor, _p) as well as
a mapping from _p[i] to the original delayed expression of the ith
gate with parameter arithmetic."
  expand-circuits)
