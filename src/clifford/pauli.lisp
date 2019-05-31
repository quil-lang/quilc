;;;; src/clifford/pauli.lisp
;;;;
;;;; Author: Nik Tezak

(in-package #:cl-quil.clifford)

;;; This file implements an efficient representation of the Pauli
;;; group.

(a:define-constant +paulis+
    #(I X Z Y)
  :test #'equalp
  :documentation "The Pauli group symbols.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype base4 ()
    `(mod 4))

  (deftype pauli-components ()
    `(simple-array base4 (*)))

  (defstruct (pauli (:include qubit-algebra)
                    (:constructor %make-pauli))
    (components #() :type pauli-components))

  (defmethod num-qubits ((p pauli))
    (1- (length (pauli-components p))))

  (defun make-components (num-qubits)
    (declare (type unsigned-fixnum num-qubits))
    (make-array (1+ num-qubits) :element-type 'base4
                                :initial-element 0))

  (defun phase-factor (p)
    (aref (pauli-components p) 0))

  (defun base4-list (p)
    (coerce (subseq (pauli-components p) 1) 'list))

  ;; Make sure Pauli objects can be serialized in FASLs.
  (defmethod make-load-form ((self pauli) &optional environment)
    (make-load-form-saving-slots self
                                 :slot-names '(components)
                                 :environment environment))

  (declaim (ftype (function (pauli pauli) boolean) pauli=))
  (declaim (inline pauli=))
  (defun pauli= (p1 p2)
    "Test whether two Pauli's are equal."
    (every #'= (pauli-components p1) (pauli-components p2)))
  (declaim (notinline pauli=))

  (defun pauli-sym-p (pauli-sym)
    "Return PAULI-SYM if PAULI-SYM is a valid Pauli-symbol I, X Y or Z. Return NIL otherwise."
    (find pauli-sym +paulis+ :test #'string-equal))

  (defun base4-to-sym (b)
    "Convert a single base-4 encoded Pauli to the corresponding symbol."
    (check-type b base4)
    (svref +paulis+ b))

  (defun sym-to-base4 (s)
    "Convert a single Pauli symbol to its base-4 encoding."
    (position s +paulis+ :test #'string-equal))

  (defun base4-p (b)
    "Test whether B is properly represented as base-4."
    (typep b 'base4))

  (defun pack-base4 (x y)
    "Bit pack X and Y (Y must be base 4)"
    (check-type y base4)
    (logxor (ash x 2) y)))

(defun pauli-index (p)
  "What is the index of the Pauli operator P up to phase?"
  (loop :with idx := 0
        :for i :from 0
        :for c :in (reverse (base4-list p))
        :do (setf idx (logior c (ash idx 2)))
        :finally (return idx)))

(defmacro pair-membership (u v &rest cases)
  `(or ,@(loop :for (ui vi) :in cases
               :collect `(and (= ,u ,ui)
                              (= ,v ,vi)))))

(declaim (ftype (function (base4 base4) base4) levi-civita))
(defun levi-civita (u v)
  "Evaluate the phase factor resulting from multiplying to Paulis. "
  (declare (type base4 u v))
  (cond
    ((pair-membership u v (1 3) (3 2) (2 1))
     1)
    ((pair-membership v u (1 3) (3 2) (2 1))
     3)
    (t
     0)))


(declaim (inline %phase-mul))
(defun %phase-mul (a b)
  (declare (type base4 a b))
  (mod (+ a b) 4))

(declaim (ftype (function (pauli-components pauli-components) pauli-components) multiply-components))
(defun multiply-components (a b)
  (declare (type pauli-components a b))
  (let* ((n (length a))
         (c (make-components (1- n))))
    ;; Get the initial phase.
    (setf (aref c 0) (%phase-mul (aref a 0) (aref b 0)))
    ;; Get the components, modifying the phase along the way.
    (loop :for i :from 1 :below n
          :for ai :of-type base4 := (aref a i)
          :for bi :of-type base4 := (aref b i)
          :do (setf (aref c i) (logxor ai bi))
              (setf (aref c 0) (%phase-mul (aref c 0) (levi-civita ai bi))))
    ;; Return c
    c))


(defmethod group-mul ((a pauli) (b pauli))
  (%make-pauli
   :components (multiply-components (pauli-components a)
                                    (pauli-components b))))

(defun print-pauli (p &optional (stream nil))
  "If STREAM is NIL (by default), return simple string representation of a Pauli P.
If STREAM is T, print to standard output. Otherwise print to STREAM."
  (format stream "~A~A"
          (ecase (phase-factor p)
            (0 "")
            (1 "i")
            (2 "-")
            (3 "-i"))
          (apply
           #'concatenate
           'string
           (mapcar (a:compose #'symbol-name #'base4-to-sym)
                   (base4-list p)))))

(defmethod print-object ((p pauli) stream)
  (print-unreadable-object (p stream :type t)
    (print-pauli p stream)))

(defun make-pauli (b &optional (p 0))
  "Make Pauli operator with base-4 representation B and base-4 phase factor P."
  (unless (and (every #'base4-p b) (base4-p p))
    (error "Malformed base-4 representation of either B or P."))
  (let ((c (make-components (list-length b))))
    (setf (aref c 0) p)
    (replace c b :start1 1)
    (%make-pauli :components c)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pauli-from-symbols (s &optional (p 0))
    "Make Pauli operator from list of Pauli symbols S (and optionally a base-4
phase factor)."
    (unless (and (every #'pauli-sym-p s) (base4-p p))
      (error "Malformed Pauli-string S or phase-factor P."))
    (let ((c (make-components (list-length s))))
      (setf (aref c 0) p)
      (prog1 (%make-pauli :components c)
        (loop :for i :from 1
              :while s
              :do (setf (aref c i) (sym-to-base4 (pop s)))
              ))))

  (defun pauli-from-string (s)
    "Make Pauli operator from a string, e.g., \"iXYZ\", \"-iZ\", \"Z\"
    or \"I\"."
    (let* ((r (nth-value 1 (ppcre:scan-to-strings "(-?)(i?)([IXYZ]+)" s)))
           (n (string= (elt r 0) "-"))
           (i (string= (elt r 1) "i"))
           (o (elt r 2))
           (p (+ (if i 1 0) (if n 2 0)))
           (l (mapcar (a:compose
                       #'intern
                       (lambda (x) (coerce (list x) 'string)))
                      (coerce o 'list))))
      (pauli-from-symbols l p)))

)                                       ; EVAL-WHEN


(a:define-constant +I+ (pauli-from-symbols '(I))
  :test #'pauli=)

(a:define-constant +X+ (pauli-from-symbols '(X))
  :test #'pauli=)

(a:define-constant +Y+ (pauli-from-symbols '(Y))
  :test #'pauli=)

(a:define-constant +Z+ (pauli-from-symbols '(Z))
  :test #'pauli=)

(defun pauli-identity (n &optional (p 0))
  "Make N-qubit identity Pauli (optionally with base-4 phase factor P)."
  (let ((c (make-components n)))
    (setf (aref c 0) p)
    (%make-pauli :components c)))

(defun pauli-identity-p (p)
  "Is P an identity?"
  (check-type p pauli)
  (every #'zerop (pauli-components p)))

(defmethod embed ((a pauli) n idxs)
  (let ((result (pauli-identity n (phase-factor a))))
    (loop
      :for i :from 1
      :for idx :in idxs
      :for ai := (aref (pauli-components a) i)
      :do (setf (aref (pauli-components result) (1+ idx)) ai))
    result))

(defun pauli-basis-decompose (pauli)
  "Decompose PAULI into a list of basis single qubit X or Z factors
times a phase. Returns multiple values, a list of pauli's and a base-4
phase."
  (let ((p (phase-factor pauli))
        (n (num-qubits pauli)))
    (loop :for idx :below (num-qubits pauli)
          :for base4-rep := (aref (pauli-components pauli) (1+ idx))
          :append (ecase base4-rep
                    (0 nil)
                    (1 (list (embed +X+ n (list idx))))
                    (2 (list (embed +Z+ n (list idx))))
                    (3
                     ;; side-effect: update the overall phase factor *= i
                     (setq p (%phase-mul p 1))
                     (list (embed +X+ n (list idx))
                           (embed +Z+ n (list idx)))))
            :into b
          :finally (return (values b p)))))

;;; HASH-MIX is taken from SBCL's public domain SB-INT:MIX. See
;;; sbcl/src/code/target-sxhash.lisp for more documentation of its
;;; design. It compiles to very efficient code on SBCL, somewhat less
;;; efficient code on other implementations.
;;;
;;; TODO: For best performance on not-SBCL, investigate
;;; platform-specific HASH-MIX code.
(declaim (ftype (function ((and fixnum unsigned-byte)
                           (and fixnum unsigned-byte))
                          (and fixnum unsigned-byte))
                mix))
(declaim (inline mix))
(defun hash-mix (x y)
  "Mix fixnums X and Y in a manner suitable for a hash table
hash-function."
  (declare (optimize (speed 3)))
  (declare (type (and fixnum unsigned-byte) x y))
  (let* ((mul (logand 3622009729038463111 most-positive-fixnum))
         (xor (logand 608948948376289905 most-positive-fixnum))
         (xy (logand (+ (* x mul) y) most-positive-fixnum)))
    (logand (logxor xor xy (ash xy -5)) most-positive-fixnum)))

(declaim (ftype (function (pauli) (and fixnum unsigned-byte)) pauli-hash)
         (inline pauli-hash))
(defun pauli-hash (p)
  "Hash function for pauli's."
  (declare (optimize speed (safety 0) (debug 0) (space 0))
           (type pauli p))
  (sxhash
   (loop :with h :of-type unsigned-fixnum := 0
         :for x :across (pauli-components p)
         :do (setf h (hash-mix h x))
         :finally (return h))))
(declaim (notinline pauli-hash))

(defun make-pauli-hash-table ()
  "Create a hash-table that supports pauli's as keys. "
  (make-hash-table :test 'pauli= :hash-function 'pauli-hash))

(defmethod tensor-mul ((a pauli) (b pauli))
  (let* ((na (num-qubits a))
         (n (+ na (num-qubits b)))
         (c (make-components n)))
    (setf (aref c 0) (%phase-mul (phase-factor a) (phase-factor b)))
    (replace c (pauli-components a) :start1 1 :start2 1)
    (replace c (pauli-components b) :start1 (+ 1 na) :start2 1)
    (%make-pauli :components c)))

(defmethod group-inv ((p pauli))
  (let ((new-phase (let ((pf (phase-factor p)))
                     (case pf
                       (1 3)
                       (3 1)
                       (otherwise pf))))
        (c (copy-seq (pauli-components p))))
    (setf (aref c 0) new-phase)
    (%make-pauli :components c)))
