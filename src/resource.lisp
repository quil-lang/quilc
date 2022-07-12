;;;; resource.lisp
;;;;
;;;; Authors: Corwin de Boor
;;;;          Erik Davis

(in-package #:cl-quil/resource)


;;; Bit Sets

(deftype bit-set ()
  "Representation of a set of non-negative integers (specifically BIT-SET-ELEMENT) as bits. "
  'integer)

(deftype bit-set-element ()
  "An index into a bit set."
  '(and fixnum unsigned-byte))

(declaim (type bit-set +empty+ +full+))
(defconstant +empty+ 0
  "The null bit set. This is all 0's in two's complement.")
(defconstant +full+ -1
  "The full bit set. This is all 1's in two's complement.")

(macrolet ((define-inlineable (name args types &body body)
             (assert (= (+ 2 (length args)) (length types)))
             (let ((arg-types (subseq types 0 (length args)))
                   (ret-type  (subseq types (1+ (length args)))))
               `(progn
                  (declaim (inline ,name))
                  (declaim (ftype (function ,arg-types ,ret-type) ,name))
                  (defun ,name ,args ,@body)
                  (declaim (notinline ,name))))))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-inlineable integer-bits-equal (bits1 bits2)
        (bit-set bit-set -> boolean)
      (= bits1 bits2)))
  (define-inlineable infinite-integer-set-p (bits)
      (bit-set -> boolean)
    (minusp bits))
  (define-inlineable integer-bits-complement (bits)
      (bit-set -> bit-set)
    (lognot bits))
  (define-inlineable integer-bits-adjoin (i bits)
      (bit-set-element bit-set -> bit-set)
    (dpb 1 (byte 1 i) bits))
  (define-inlineable integer-bits-remove (i bits)
      (bit-set-element bit-set -> bit-set)
    (dpb 0 (byte 1 i) bits))
  (define-inlineable integer-bits-member (i bits)
      (bit-set-element bit-set -> boolean)
    (logbitp i bits))
  (define-inlineable integer-bits-union (bits1 bits2)
      (bit-set bit-set -> bit-set)
    (logior bits1 bits2))
  (define-inlineable integer-bits-intersection (bits1 bits2)
      (bit-set bit-set -> bit-set)
    (logand bits1 bits2))
  (define-inlineable integer-bits-empty-p (bits)
      (bit-set -> boolean)
    (integer-bits-equal bits +empty+))
  (define-inlineable integer-bits-difference (bits1 bits2)
      (bit-set bit-set -> bit-set)
    (logandc2 bits1 bits2))
  (define-inlineable integer-bits-subsetp (bits1 bits2)
      (bit-set bit-set -> boolean)
    "Is the bit set BITS1 is a subset of the bit set BITS2?"
    (integer-bits-empty-p (integer-bits-difference bits1 bits2)))

  (define-inlineable integer-bits-range (lo hi)
      (bit-set-element bit-set-element -> bit-set)
    "Returns a bit set representing the range [LO,HI)."
    (assert (<= lo hi) ()
            "The lower bound must be below the upper bound: ~A </= ~A" lo hi)
    (dpb +full+
         (byte (- hi lo) lo)
         +empty+)))

(defun bit-set-to-list (bits)
  "Return a list of integers corresponding to the bit set BITS."
  (cond ((infinite-integer-set-p bits)
         (error "Attempting to materialize an infinite set."))
        (t
         (loop :for i :below (integer-length bits)
               :when (integer-bits-member i bits)
                 :collect i))))


;;; Resource Collections
;;;
;;; These are used to manage resources for instructions or blocks of
;;; instructions. The two basic resources are qubits and classical
;;; memory regions. Qubit resources are simple: each qubit has an
;;; index and we can just track these using a bit set. Memory regions
;;; are have an associated name, and so we generally maintain an alist
;;; mapping region names to ranges (see below). The caveat is that at
;;; times it is useful to have a sentinel value indicating all quantum
;;; and classical resources. For this we use
;;; +IMPOSSIBLY-FULL-RESOURCE+, which simply has T for the memory
;;; region slot.

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct resource-collection
    "A collection of resources, representing some set of qubits and
classical memory regions. "
    (qubits +empty+ :type bit-set :read-only t)
    (memory-regions nil :read-only t))  ; Either an alist mapping region names to bit sets, or the sentinel value T

  (defmethod make-load-form ((object resource-collection) &optional env)
    (make-load-form-saving-slots object :environment env))

  (defun resource= (rc1 rc2)
    "Returns true if resource collections RC1 and RC2 contain the same qubit and region resources."
    (and (integer-bits-equal (resource-collection-qubits rc1)
                             (resource-collection-qubits rc2))
         (equal (resource-collection-memory-regions rc1)
                (resource-collection-memory-regions rc2)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (a:define-constant +impossibly-full-resource+
      (make-resource-collection
       :qubits +full+
       :memory-regions t)
    :test 'resource=
    :documentation "A representation of the largest possible resource collection, containing all qubits and classical memory."))


;;; Constructors


(defun make-qubit-resource (&rest indices)
  (declare (dynamic-extent indices)
           (inline integer-bits-adjoin))
  (let ((qubits +empty+))
    (declare (type bit-set qubits))
    (dolist (i indices)
      (declare (type bit-set-element i))
      (setf qubits (integer-bits-adjoin i qubits)))
    (make-resource-collection :qubits qubits)))

(defun make-null-resource ()
  (load-time-value (make-resource-collection) t))

(defun make-all-resource ()
  +impossibly-full-resource+)


(defun make-resource-range (name lo hi)
  "Make a resource collection consisting of the memory region named by
NAME with an inclusive lower bound LO and exclusive upper bound HI."
  (assert (<= lo hi) (lo hi)
          "The lower bound must be below the upper bound: ~A </= ~A" lo hi)
  (if (= lo hi)
      (make-null-resource)
      (make-resource-collection :memory-regions (list (cons name (list (cons lo hi)))))))


(defmethod print-object ((r (eql +impossibly-full-resource+)) stream)
  (print-unreadable-object (r stream :type t :identity nil)
    (format stream "IMPOSSIBLY FULL RESOURCE")))

(defmethod print-object ((r resource-collection) stream)
  (print-unreadable-object (r stream :type t :identity nil)
    (let ((qs (resource-collection-qubits r)))
      (format stream ":QUBITS {~{~D~^, ~}}~:[~;*~]~% :MEMORY-REGIONS ~W"
              (bit-set-to-list
               (if (infinite-integer-set-p qs)
                   (integer-bits-complement qs)
                   qs))
              (infinite-integer-set-p qs)
              (resource-collection-memory-regions r)))))


(defun resource-collection-adjoin-qubit (rc ind)
  "Given a resource collection RC, creates a new collection with the qubit index IND included."
  (make-resource-collection
   :qubits (integer-bits-adjoin ind (resource-collection-qubits rc))
   :memory-regions (resource-collection-memory-regions rc)))

(defun resource-collection-remove-qubit (rc ind)
  "Given a resource collection RC, creates a new collection with the qubit index IND excluded."
  (make-resource-collection
   :qubits (integer-bits-remove ind (resource-collection-qubits rc))
   :memory-regions (resource-collection-memory-regions rc)))

(defun resource-collection-contains-qubit (rc ind)
  "Check whether a resource collection RC contains the given qubit index IND."
  (integer-bits-member ind (resource-collection-qubits rc)))


;;; Predicates and Operations


(defun resource-null-p (rc)
  "Returns true if the resource collection RC is empty."
  (and (integer-bits-empty-p (resource-collection-qubits rc))
       (endp (resource-collection-memory-regions rc))))

(defun resource-all-p (rc)
  "Returns T if the resource collection RC contains all quantum and classical resources."
  (resource= rc +impossibly-full-resource+))


;;; There is some overhead in MAP-MEMORY-REGIONS. By isolating the qubit
;;; combination from the region combination, we can short-circuit in
;;; the later RESOURCE-UNION, RESOURCE-INTERSECTION, RESOURCE-SUBSET
;;; functions.
(declaim (inline combine-collections-default-regions))
(defun combine-collections-default-regions (qubit-combiner memory-regions rc1 rc2)
  "Combine resource collections RC1 and RC2 by applying QUBIT-COMBINER
  to their qubit bit sets. Instead of combining their regions, the
  default value of MEMORY-REGIONS is used for the result."
  (make-resource-collection
   :qubits (funcall qubit-combiner
                    (resource-collection-qubits rc1)
                    (resource-collection-qubits rc2))
   :memory-regions memory-regions))

(defun combine-collections (qubit-combiner region-combiner rc1 rc2)
  "Combine resource collections RC1 and RC2 by applying the function
QUBIT-COMBINER to their qubit bit sets, and the function
REGION-COMBINER to their commonly-named regions."
  (let* ((new-regions (map-sorted-memory-regions region-combiner
                                                 (resource-collection-memory-regions rc1)
                                                 (resource-collection-memory-regions rc2)
                                                 :default-value nil))
         (nonempty-regions (remove-if (lambda (named-region)
                                       (range-empty-p (cdr named-region)))
                                     new-regions)))
    (combine-collections-default-regions qubit-combiner
                                        nonempty-regions
                                        rc1
                                        rc2)))

(defun resource-union (rc1 rc2)
  "Returns a resource collection representing the union of RC1 and RC2."
  (cond ((or (resource-all-p rc1)
             (resource-all-p rc2))
         +impossibly-full-resource+)
        ;; Short circuit memory region combination, if possible
        ((null (resource-collection-memory-regions rc1))
         (combine-collections-default-regions #'integer-bits-union
                                              (resource-collection-memory-regions rc2)
                                              rc1
                                              rc2))
        ((null (resource-collection-memory-regions rc2))
         (combine-collections-default-regions #'integer-bits-union
                                              (resource-collection-memory-regions rc1)
                                              rc1
                                              rc2))
        (t
         (combine-collections #'integer-bits-union
                              #'range-union
                              rc1
                              rc2))))

(defun resource-intersection (rc1 rc2)
  "Returns a resource collection representing the intersection of RC1 and RC2."
  (cond ((resource-all-p rc1)
         rc2)
        ((resource-all-p rc2)
         rc1)
        ;; Short circuit memory region combination, if possible
        ((or (null (resource-collection-memory-regions rc1))
             (null (resource-collection-memory-regions rc2)))
         (combine-collections-default-regions #'integer-bits-intersection
                                              nil
                                              rc1
                                              rc2))
        (t
         (combine-collections #'integer-bits-intersection
                              #'range-intersection
                              rc1
                              rc2))))

(define-condition resource-collection-unbounded-names (error)
  ()
  (:report "Operation would result in a resource collection with an unbounded list of MEMORY-REGIONS."))

(defun resource-difference (rc1 rc2)
  "Returns a resource collection representing those resources in RC1 and not in RC2."
  (cond ((resource-all-p rc1)
         (error 'resource-collection-unbounded-names))
        ((resource-all-p rc2)
         (make-null-resource))
        ;; Short circuit memory region combination, if possible
        ((null (resource-collection-memory-regions rc1))
         (combine-collections-default-regions #'integer-bits-difference
                                              nil
                                              rc1
                                              rc2))
        ((null (resource-collection-memory-regions rc2))
         (combine-collections-default-regions #'integer-bits-difference
                                              (resource-collection-memory-regions rc1)
                                              rc1
                                              rc2))
        (t
         (combine-collections #'integer-bits-difference
                              #'range-difference
                              rc1
                              rc2))))


(defun resources-intersect-p (rc1 rc2)
  "Returns T if the resource collections RC1 and RC2 share a resource."
  (not (resource-null-p
        (resource-intersection rc1 rc2))))

(defun resource-subsetp (rc1 rc2)
  "Return T if all of the resources in RC1 are contained within RC2."
  (or (resource-all-p rc2)
      (and (not (resource-all-p rc1))
           (resource-null-p (resource-difference rc1 rc2)))))


(defun map-sorted-memory-regions (fn regions1 regions2 &key default-value)
  "Given memory regions REGIONS1 and REGIONS2, both of which are
sorted alists mapping names to bit sets, construct a new alist which,
for each name, has the value given by applying FN to the corresponding
two values in REGIONS1 and REGIONS2. If a value is missing,
DEFAULT-VALUE is used. "
  ;; This is a straightforward two way merge. We walk the alists in
  ;; tandem, and then when one has been exhausted we walk the other.
  (let ((results nil))
    (loop :while (and regions1 regions2)
          :for r1 := (first regions1)
          :for r2 := (first regions2)
          :do (cond ((string= (car r1) (car r2))
                     (push (cons (car r1)
                                 (funcall fn (cdr r1) (cdr r2)))
                           results)
                     (pop regions1)
                     (pop regions2))
                    ((string< (car r1) (car r2))
                     (push (cons (car r1)
                                 (funcall fn (cdr r1) default-value))
                           results)
                     (pop regions1))
                    (t
                     (push (cons (car r2)
                                 (funcall fn default-value (cdr r2)))
                           results)
                     (pop regions2))))
    ;; At most one of these two DOLIST steps will do anything nontrivial.
    (dolist (r1 regions1)
      (push (cons (car r1)
                  (funcall fn (cdr r1) default-value))
            results))
    (dolist (r2 regions2)
      (push (cons (car r2)
                  (funcall fn default-value (cdr r2)))
            results))
    (reverse results)))

;;; Ranges
;;;
;;; Ranges are ordered lists of intervals, where an interval [lo,hi)
;;; is represented by a cons cell (lo . hi).


(defun interval-lo (range) (car range))
(defun (setf interval-lo) (new-value range) (setf (car range) new-value))

(defun interval-hi (range) (cdr range))
(defun (setf interval-hi) (new-value range) (setf (cdr range) new-value))

(defun first-interval-lo (ranges) (interval-lo (first ranges)))
(defun first-interval-hi (ranges) (interval-hi (first ranges)))

(defun interval-empty-p (interval)
  (>= (interval-lo interval) (interval-hi interval)))

(defun range-empty-p (range)
  "Is the range empty?"
  (every #'interval-empty-p range))

(defconstant +range-lower-bound+ most-negative-fixnum)
(defconstant +range-upper-bound+ most-positive-fixnum)

(a:define-constant +full-range+
    (list (cons +range-lower-bound+ +range-upper-bound+))
  :test 'equal
  :documentation "The largest possible range.")

(defun make-full-region (name)
  "Make a resource collection representing the full use of a memory region named NAME."
  (make-resource-collection :memory-regions (list (cons name +full-range+))))

(defun range-merge-first (range ranges)
  "Merge the first range of RANGES with RANGE. Assumes that RANGE starts before
the first range of RANGES. Return T if and only if RANGE overlapped with
RANGES."
  (cond
    ((endp ranges)
     nil)
    ((>= (interval-hi range) (first-interval-lo ranges))
     (a:maxf (interval-hi range) (first-interval-hi ranges)))))

(defun range-union (r1 r2)
  (nconc
   (loop
     :while (and r1 r2)
     :for start := (min (first-interval-lo r1) (first-interval-lo r2))
     :for cell := (cons start start)
     :do (loop
           :for continue := nil
           :when (range-merge-first cell r1)
             :do (setf r1 (rest r1) continue t)
           :when (range-merge-first cell r2)
             :do (setf r2 (rest r2) continue t)
           :while continue)
     :unless (interval-empty-p cell)
       :collect cell)
   ;; note that at most one of r1 and r2 will be non-empty after the loop
   (or r1 r2)))

(defun range-intersection (r1 r2 &optional (result nil))
  (when (or (endp r1) (endp r2))
    (return-from range-intersection (nreverse result)))
  (when (> (first-interval-lo r1) (first-interval-lo r2))
    (rotatef r1 r2))
  ;; any ranges in r2 that are overlapped by the first in r1 are good
  (let ((end (first-interval-hi r1)))
    ;; only take the ones completely overlapped
    (loop
      :while (and r2 (<= (first-interval-hi r2) end))
      :do (push (pop r2) result))
    ;; include any partial overlap with the rest of the ranges (at most 1 can
    ;; partially overlap)
    (when (and r2
               (< (first-interval-lo r2) end))
      (push (cons (first-interval-lo r2) end) result))
    (range-intersection (rest r1) r2 result)))

(defun range-subsetp (r1 r2)
  (when (or (endp r1) (endp r2))
    (return-from range-subsetp (endp r1)))
  (if (> (first-interval-lo r1) (first-interval-hi r2))
      (range-subsetp r1 (rest r2))
      (and
       (<= (first-interval-lo r2) (first-interval-lo r1)
           (first-interval-hi r1) (first-interval-hi r2))
       (range-subsetp (rest r1) r2))))

(defun ranges-intersect-p (r1 r2)
  (when (or (endp r1) (endp r2))
    (return-from ranges-intersect-p nil))
  ;; ensure r1 starts below r2
  (when (> (first-interval-lo r1) (first-interval-lo r2))
    (rotatef r1 r2))
  (or
   ;; the first one overlaps
   (> (first-interval-hi r1) (first-interval-lo r2))
   ;; the rest overlap
   (ranges-intersect-p (rest r1) r2)))

(defun range-complement (r)
  ;; return ranges for all spaces around the ranges that exist in r
  (loop
    :for ((nil . lo) (hi . nil)) :on (acons 0 +range-lower-bound+ r)
    ;; on the last iteration, hi is NIL
    :when (< lo (or hi +range-upper-bound+))
      :collect (cons lo (or hi +range-upper-bound+))))

(defun range-difference (r1 r2)
  (range-intersection r1 (range-complement r2)))

(defun resource-qubits-list (r)
  (let ((qs (resource-collection-qubits r)))
    (bit-set-to-list
     (if (infinite-integer-set-p qs)
         (integer-bits-complement qs)
         qs))))
