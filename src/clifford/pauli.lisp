;;;; src/clifford/pauli.lisp
;;;;
;;;; Authors: Nik Tezak
;;;;          Robert Smith

(in-package #:cl-quil/clifford)

;;; This file implements an efficient representation of the Pauli
;;; group.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (a:define-constant +paulis+
      #(I X Z Y)
    :test #'equalp
    :documentation "The Pauli group symbols."))

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
    (declare (type cl-quil::unsigned-fixnum num-qubits))
    (make-array (1+ num-qubits) :element-type 'base4
                                :initial-element 0))

  (defun phase-factor (p)
    "Return the phase factor as an integer. This integer can be interpreted as a complex number via the function INTERPRET-PAULI-PHASE-FACTOR."
    (aref (pauli-components p) 0))

  (defun interpret-pauli-phase-factor (n)
    "Interpret the phase of a Pauli term (as given by PHASE-FACTOR) as a complex number."
    (expt #C(0 1) n))

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
    "Bit pack X and Y (Y must be base 4)."
    (check-type y base4)
    (logxor (ash x 2) y)))

(defun pauli-index (p)
  "What is the index of the Pauli operator P up to phase?"
  (loop :with idx := 0
        :for i :from 0
        :for c :in (reverse (base4-list p))
        :do (setf idx (logior c (ash idx 2)))
        :finally (return idx)))

(defun pauli-integer (p)
  "Give a non-negative integer representation of the Pauli P, which is unique among only the Pauli operators of the same number of qubits.

(Implementation details: This function is implemented so that a phase of {1, i} determines the LSB, and a phase of {+1, -1} determines the second LSB.

Tensoring an I to the left does not affect the value. Assuming a phase of +1, tensoring an I to the right has the effect of shifting the value right by two bits.)"
  (reduce (lambda (x acc)
            (+ x (* 4 acc)))
          (pauli-components p)
          :from-end t
          :initial-value 0))

(defun integer-pauli (int n)
  "Convert a non-negative integer INT to a Pauli operator of order N. (See PAULI-INTEGER for more details.)"
  (loop :with c := (make-components n)
        :for i :to n :do
          (setf (values int (aref c i)) (floor int 4))
        :finally (return (%make-pauli :components c))))

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

(declaim (inline multiply-components-into))
(defun multiply-components-into (a b c)
  "Multiplies two pauli-component vectors and writes the result into a pauli-components vector C."
  (declare (type pauli-components a b c))
  (let* ((n (length a)))
    ;; Get the initial phase.
    (setf (aref c 0) (%phase-mul (aref a 0) (aref b 0)))
    ;; Get the components, modifying the phase along the way.
    (loop :for i :from 1 :below n
          :for ai :of-type base4 := (aref a i)
          :for bi :of-type base4 := (aref b i)
          :do (setf (aref c i) (logxor ai bi))
              (setf (aref c 0) (%phase-mul (aref c 0) (levi-civita ai bi))))))

(declaim (ftype (function (pauli-components pauli-components) pauli-components) multiply-components))
(defun multiply-components (a b)
  "Multiplies two pauli-component vectors, returning the result as a new pauli-components vector."
  (declare (type pauli-components a b))
  (let* ((c (make-components (1- (length a)))))
    (multiply-components-into a b c)
    c))

(defmethod group-mul ((a pauli) (b pauli))
  (%make-pauli
   :components (multiply-components (pauli-components a)
                                    (pauli-components b))))

;;; Paulis are printed in a fashion that is consistent with the
;;; ordering of the computational basis. For example, a pauli operator
;;; represented as #(0 A B C) is printed as CBA, which applies A on q0, B
;;; on q1, C on q2.
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
           (reverse (mapcar (alexandria:compose #'symbol-name #'base4-to-sym)
                            (base4-list p))))))

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
    (let ((c (make-components (list-length s)))
          (s (reverse s)))
      (setf (aref c 0) p)
      (prog1 (%make-pauli :components c)
        (loop :for i :from 1
              :while s
              :do (setf (aref c i) (sym-to-base4 (pop s)))))))

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
      :for idx :in (reverse idxs)
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
   (loop :with h :of-type cl-quil::unsigned-fixnum := 0
         :for x :across (pauli-components p)
         :do (setf h (hash-mix h x))
         :finally (return h))))
(declaim (notinline pauli-hash))

(defun make-pauli-hash-table ()
  "Create a hash-table that supports pauli's as keys. "
  (make-hash-table :test 'pauli= :hash-function 'pauli-hash))

(defmethod tensor-mul ((a pauli) (b pauli))
  (let* ((nb (num-qubits b))
         (n (+ nb (num-qubits a)))
         (c (make-components n)))
    (setf (aref c 0) (%phase-mul (phase-factor a) (phase-factor b)))
    (replace c (pauli-components b) :start1 1 :start2 1)
    (replace c (pauli-components a) :start1 (+ 1 nb) :start2 1)
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

;;; Exponentiation of a Pauli
;;;
;;; Reference:
;;;
;;;     Section VIII ("Trotterization") of "The Bravyi-Kitaev
;;;     transformation for quantum computation of electronic
;;;     structure" by Seeley, Richard, and
;;;     Love. (https://arxiv.org/pdf/1208.5986.pdf)
(defun build-cnot-ladder (qubits)
  (let ((reverse nil)
        (forward nil))
    (map nil (lambda (from to)
               ;; We make copies because quilc doens't like EQ things.
               (push (cl-quil:build-gate "CNOT" () from to) reverse)
               (push (cl-quil:build-gate "CNOT" () from to) forward))
         qubits
         (subseq qubits 1))
    (values (nreverse forward)
            reverse)))

(defvar *exp-pauli-toggles* 'build-cnot-ladder
  "A function designator which takes a list of qubits Q and builds a two sequences of instructions S and S' such that S \"toggles\" the last qubit of Q through the previous elements of Q. S' should be the mathematical inverse of S.")

(defun exp-pauli (p x)
  "Compute exp(-ixp) as a list of Quil instructions.

Note that this function is somewhat informed by the variable *EXP-PAULI-COLLECT-TOGGLES* variable; see its documentation. which is a function which takes a list of qubits and \"collects all toggles\" on those qubits
"
  (let ((phase (interpret-pauli-phase-factor (phase-factor p))))
    (assert (realp phase) (p) "The given Pauli term has a non-real phase of ~A ~
                               which isn't allowed in EXP-PAULI."
            phase)
    (setf phase (coerce (realpart phase) 'double-float))
    (cl-quil:with-inst ()
      (cond
        ((pauli-identity-p p)
         ;; PyQuil would generate
         ;;
         ;;   X 0
         ;;   PHASE(-phase) 0
         ;;   X 0
         ;;   PHASE(-phase) 0
         ;;
         ;; We are just going to generate nothing.
         nil)

        ;; A simple case so we can generate a single rotation.
        ((= 1 (count-if #'plusp (pauli-components p) :start 1))
         (multiple-value-bind (op qubit)
             (loop :for i :from 1
                   :for op := (base4-to-sym (aref (pauli-components p) i))
                   :unless (eq 'I op)
                     :return (values op (1- i)))
           (ecase op
             (X (cl-quil:inst "RX" (list (cl-quil::param-* (* 2.0d0 phase) x)) qubit))
             (Y (cl-quil:inst "RY" (list (cl-quil::param-* (* 2.0d0 phase) x)) qubit))
             (Z (cl-quil:inst "RZ" (list (cl-quil::param-* (* 2.0d0 phase) x)) qubit)))))

        ;; The general case.
        (t
         (let* ((n (num-qubits p)))
           (loop :for qubit :below n
                 :for pauli-component := (base4-to-sym (aref (pauli-components p) (1+ qubit)))
                 :unless (eq 'I pauli-component)
                   :collect qubit :into toggle-qubits
                 ;; This collect builds pairs of basis-changing gates.
                 :when (eq 'X pauli-component)
                   :collect      (cl-quil:build-gate "H" () qubit) :into to-z
                   :and :collect (cl-quil:build-gate "H" () qubit) :into from-z
                 :when (eq 'Y pauli-component)
                   :collect      (cl-quil:build-gate "RX" (list cl-quil:pi/2) qubit)  :into to-z
                   :and :collect (cl-quil:build-gate "RX" (list cl-quil:-pi/2) qubit) :into from-z
                 :finally
                    (multiple-value-bind (forward reverse) (funcall *exp-pauli-toggles* toggle-qubits)
                      (mapc #'cl-quil:inst to-z)
                      (mapc #'cl-quil:inst forward)
                      (cl-quil:inst "RZ" (list (cl-quil::param-* (* 2.0d0 phase) x)) (first (last toggle-qubits)))
                      (mapc #'cl-quil:inst reverse)
                      (mapc #'cl-quil:inst from-z)))))))))
