;;;; src/classical-memory.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil/frontend)

(define-condition quil-memory-model-error (a:simple-parse-error)
  ()
  (:documentation "An error regarding the classical memory declarations."))

(defun quil-memory-model-error (format-control &rest format-args)
  "Signal a QUIL-MEMORY-MODEL-ERROR."
  (error 'quil-memory-model-error :format-control format-control
                                  :format-arguments format-args))

(adt:defdata quil-type
  "A valid data type for Quil memory."
  quil-bit
  quil-octet
  quil-integer
  quil-real)

(defun string-to-quil-type (name)
  "Convert a Quil string name of a data type to our internal representation."
  (a:switch (name :test #'string=)
    ("BIT"     quil-bit)
    ("OCTET"   quil-octet)
    ("INTEGER" quil-integer)
    ("REAL"    quil-real)
    (otherwise
     (error "Unrecognized type ~S" name))))

(defun quil-type-string (type)
  "Convert our internal representation of a Quil data type to a string name."
  (check-type type quil-type)
  (adt:match quil-type type
    (quil-bit     "BIT")
    (quil-octet   "OCTET")
    (quil-integer "INTEGER")
    (quil-real    "REAL")))

(defstruct memory-descriptor
  "A named region of classical memory, as parsed from a Quil program."
  (name nil :read-only t :type string)
  (type nil :read-only t :type quil-type)
  ;; Length = the number of native elements of this descriptor.
  (length 1 :read-only t :type unsigned-byte)
  ;; NOTE: we may want to make these writeable for a compiler that slots user-
  ;;       defined memory spaces into hardware-available memory spaces
  (sharing-parent nil :read-only t :type (or null string))
  (sharing-offset-alist nil :read-only t :type list)
  ;; Context token, available for later resolution
  (lexical-context nil :read-only t :type (or null token)))

(defmethod lexical-context ((obj memory-descriptor))
  (memory-descriptor-lexical-context obj))

(defun simple-memory-descriptor-p (desc)
  "Is the object DESC a memory descriptor that is \"simple\", i.e., does not alias?"
  (and (memory-descriptor-p desc)
       (null (memory-descriptor-sharing-parent desc))))

(defstruct memory-alias
  "A named piece of memory that indexes into some existing root piece of memory."
  ;; Name of the memory.
  (name nil :read-only t :type string)
  ;; The root memory being mapped.
  (root-memory nil :read-only t :type memory-descriptor)
  ;; The type of elements in this memory.
  (type nil :read-only t :type quil-type)
  ;; The length in the native element.
  (length nil :read-only t :type unsigned-byte)
  ;; The starting location (measured in bits) of this memory.
  (starting-bit nil :read-only t :type unsigned-byte)
  ;; The size of this map in bits.
  (size-in-bits nil :read-only t :type unsigned-byte))

(defstruct memory-model
  "A fully parsed-out model for the hierarchical memory of a program."
  ;; The alignment, sizeof(REAL), and sizeof(INTEGER), all measured in
  ;; bits.
  (alignment nil :read-only t :type (integer 1))
  (real-bits nil :read-only t :type (integer 1))
  (integer-bits nil :read-only t :type (integer 1))
  ;; SIZEOF takes a QUIL-TYPE and returns a non-negative integer
  (sizeof nil :read-only t :type function)
  ;; Hash table mapping string names to MEMORY-DESCRIPTORs for all
  ;; pieces of named memory.
  names
  ;; A list of MEMORY-DESCRIPTOR instances which are root memory.
  (roots nil :read-only t :type list)
  ;; A list of MEMORY-ALIAS instances which refer to the roots.
  (aliases nil :read-only t :type list))

(defun dividesp (d n)
  "Does D divide N?"
  (zerop (mod n d)))

(defun round-up-to-next-multiple (x multiple)
  "Round X up to the next multiple of MULTIPLE."
  (check-type x (integer 0))
  (check-type multiple (integer 1))
  (* multiple (ceiling x multiple)))

(defun memory-descriptors-to-model (descrs &key (alignment nil alignment-provided-p)
                                                (real-bits nil real-bits-provided-p)
                                                (integer-bits nil integer-bits-provided-p))
  "Given a list of MEMORY-DESCRIPTOR instances DESCRS, produce a MEMORY-MODEL.

This function will check for duplicate names, undefined names, as well
as aliases that overflow their shared region."
  (assert alignment-provided-p (alignment) "ALIGNMENT wasn't provided.")
  (assert real-bits-provided-p (real-bits) "REAL-BITS wasn't provided.")
  (assert integer-bits-provided-p (integer-bits) "INTEGER-BITS wasn't provided.")

  (check-type alignment (integer 1))
  (check-type real-bits (integer 1))
  (check-type integer-bits (integer 1))

  (let ((name->desc (make-hash-table :test 'equal))
        (name->children (make-hash-table :test 'equal)))
    ;; Process all of the descriptors.
    (dolist (d descrs)
      ;; Check that it isn't a dupe.
      (let ((name (memory-descriptor-name d)))
        (when (gethash name name->desc)
          (quil-memory-model-error "The name ~S was DECLAREd more than once." name))

        ;; Record it.
        (setf (gethash name name->desc) d)

        ;; Record whose child it is, if necessary.
        (unless (simple-memory-descriptor-p d)
          ;; We don't need to PUSHNEW because we've validated that
          ;; we've never seen NAME before.
          (push name (gethash (memory-descriptor-sharing-parent d) name->children)))))

    ;; We've recorded parent-children relationships, but we've never
    ;; actually checked that the parents exist. We do that here.
    (dohash ((parent-name childrens-names) name->children)
      ;; Check that the parent exists.
      (unless (gethash parent-name name->desc)
        (quil-memory-model-error
         "Each of the declared memories {~{~S~^, ~}} purportedly ~
                   share memory with ~S, but it was nowhere to be found."
         childrens-names
         parent-name)))

    ;; Package up everything we've learned into the memory model.
    (let ((roots (remove-if-not #'simple-memory-descriptor-p descrs)))
      (make-memory-model
       :alignment alignment
       :real-bits real-bits
       :integer-bits integer-bits
       :sizeof (size-of-function real-bits integer-bits)
       :names name->desc
       :roots roots
       :aliases (loop :for root :in roots
                      :append (compute-root-map root
                                                name->desc
                                                name->children
                                                alignment
                                                real-bits
                                                integer-bits))))))

(defun size-of-function (real-bits integer-bits)
  "Produce a function mapping QUIL-TYPE to size in bits. REAL-BITS and INTEGER-BITS should be the size of REAL and INTEGER in bits respectively."
  (lambda (type)
    (adt:match quil-type type
      (quil-bit     1)
      (quil-octet   8)
      (quil-integer integer-bits)
      (quil-real    real-bits))))

(defun descriptor-size-bits (desc sizeof)
  "How many bits does a descriptor DESC represent, in the context of a size-of function SIZEOF?"
  (* (memory-descriptor-length desc)
     (funcall sizeof (memory-descriptor-type desc))))

(defun descriptor-offset-bits (desc sizeof)
  "How many bits does a descriptor DESC offset from its shared parent by, in the context of a size-of function SIZEOF?"
  (loop :for (type . amount) :in (memory-descriptor-sharing-offset-alist desc)
        :sum (* amount (funcall sizeof type))))

;;; Alright, so here's the deal with alignment.
;;;
;;; All memory allocations (not accesses!) must be aligned, *except
;;; for BITs*. What this means is that the physical address of the
;;; start of every non-BIT allocation must be a multiple of ALIGNMENT
;;; BITs. If you OFFSET by some number of BITs that isn't a multiple,
;;; it will error! It is up to the programmer to get alignment
;;; correct. (The rationale for not rounding up is this: If we round
;;; up, then aliasing behavior can silently change if alignment
;;; changes. It's better to fail more than to be inconsistent.)
;;;
;;; Consider yourself spooked.
;;;
;;; If you want to ensure every *access* is aligned, then the
;;; following must be true:
;;;
;;;     * Your alignment should divide the size of your REAL and
;;;       INTEGER types.
;;;
;;;     * If your alignment is greater than 8 bits, you must not
;;;       access OCTETs.
;;;
;;; It's pretty reasonable to assume any architecture will have an
;;; alignment of 8 bits. A 1-bit aligned architecture is totally
;;; free. Every access is valid. Some architectures might have an
;;; alignment of greater than 8 bits (e.g., 32 or 64 bits).
;;;
;;; BIT is special, and in hardware, would be implemented with special
;;; primitives.

(defun compute-root-map (root name->desc name->children alignment real-bits integer-bits)
  "Given:

    - a root descriptor ROOT,

    - table NAME->DESC (mapping names to descriptors),

    - table NAME->CHILDREN (mapping names to names of children), and

    - the crucial memory properties ALIGNMENT, REAL-BITS, and INTEGER-BITS

Compute a list of MEMORY-ALIAS instances off of that root for each of its children.

This function will check if the aliases overflow the parent."
  (let* ((root-name (memory-descriptor-name root))
         (root-children (gethash root-name name->children))
         (sizeof (size-of-function real-bits integer-bits))
         (mem-map nil))
    (labels ((process-child (parent-desc global-offset child-name)
               (let* ((child-desc (gethash child-name name->desc))
                      (bit? (adt:match quil-type (memory-descriptor-type child-desc)
                              (quil-bit t)
                              (_        nil)))
                      (grandchildren (gethash child-name name->children))
                      (child-size (descriptor-size-bits child-desc sizeof))
                      (child-offset (descriptor-offset-bits child-desc sizeof))
                      (new-global-offset (+ global-offset child-offset)))
                 ;; We must check that the *global* offset is aligned,
                 ;; since ultimately that's where memory gets
                 ;; allocated.
                 (unless (or bit? (dividesp alignment new-global-offset))
                   (quil-memory-model-error
                    "The aliasing memory ~S as stated would start on an unaligned ~
                     memory boundary, which isn't allowed if the memory isn't of ~
                     type BIT. Consider adding an offset of ~D BIT, assuming that ~
                     that won't overflow the memory you're inheriting from."
                    child-name
                    (- (round-up-to-next-multiple new-global-offset alignment)
                       new-global-offset)))

                 ;; Check that we can be placed in the parent memory
                 ;; without overflowing.
                 (unless (<= (+ child-size child-offset)
                             (descriptor-size-bits parent-desc sizeof))
                   (quil-memory-model-error
                    "The memory ~S is offset by ~D bit~:P and has a size of ~D ~
                     bits, which is a total of ~D bit~:P that must fit in the ~
                     parent memory ~S, which is only ~D bit~:P long. It doesn't ~
                     fit."
                    child-name
                    child-offset
                    child-size
                    (+ child-offset child-size)
                    (memory-descriptor-name parent-desc)
                    (descriptor-size-bits parent-desc sizeof)))
                 (push (make-memory-alias
                        :name child-name
                        :root-memory root
                        :type (memory-descriptor-type child-desc)
                        :length (memory-descriptor-length child-desc)
                        :starting-bit new-global-offset
                        :size-in-bits child-size)
                       mem-map)
                 (dolist (grandchild-name grandchildren)
                   (process-child child-desc new-global-offset grandchild-name)))))
      (dolist (child-name root-children)
        (process-child root 0 child-name))
      (nreverse mem-map))))

(global-vars:define-global-var* **empty-memory-model**
    ;; Actual values don't/shouldn't matter. This won't allocate any
    ;; memory anyway.
    (make-memory-model :alignment 64
                       :real-bits 64
                       :integer-bits 64
                       :sizeof (size-of-function 64 64)
                       :names (make-hash-table)
                       :roots nil
                       :aliases nil)
  "A memory model that represents no memory.")
