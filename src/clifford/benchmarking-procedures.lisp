;;;; benchmarking-procedures.lisp
;;;;
;;;; Author: Anthony Polloreno
;;;;
;;;; Procedures to respond to quilc requests about cliffords, specifically for benchmarking quantum gate sets and circuits.

(in-package :cl-quil/clifford)

(defconstant +god-table-cache-limit+ 10)
(global-vars:define-global-var **god-tables** (make-gateset-hash-table))

(defun find-or-make-god-table (gateset)
  (flet ((add-entry (key val)
           (when (<= +god-table-cache-limit+ (hash-table-count **god-tables**))
             (clrhash **god-tables**))
           (setf (gethash key **god-tables**) val)))
    (declare (inline add-entry))
    (or (gethash gateset **god-tables**)
        (add-entry gateset (make-god-table gateset)))))


;; TODO improve the error message when the provided Cliffords are non-spanning.
(defun rb-sequence (length n cliffords &optional interleaver)
  "Computes a randomized benchmarking sequence of length LENGTH with N qubit CLIFFORDs, decomposed into CLIFFORDS. CLIFFORDS should be a collection of elements of the Clifford group used to generate each element. This returns a list of lists, each list being a decomposed CLIFFORD. The sequence will be given from right to left, with the first CLIFFORD being the rightmost."
  (assert (>= length 2))
  (let* ((prod (clifford-identity n))
         (seq
           (loop :repeat (1- length)
                 :for cliff-el := (random-clifford n)
                 :when interleaver
                   :do (setq prod (group-mul prod interleaver))
                 :when interleaver
                   :collect interleaver
                 :do (setq prod (group-mul prod cliff-el))
                 :collect cliff-el))
         (gate-set (make-instance 'gateset :cliffords cliffords))
         (god-table (find-or-make-god-table gate-set))
         (inverse (group-inv prod))
         ;; We also need to decompose the identity, and so we pick the minimal such decomposition.
         (identity-decompositions
           (loop :for generator :in cliffords
                 :collect (let ((inverse (reconstruct (group-inv generator) god-table)))
                            (cons generator inverse))))
         (min-identity-decomposition (a:extremum identity-decompositions #'< :key #'length)))
    (push inverse seq)
    (loop :for s :in seq
          :collect (if (clifford-identity-p s)
                       min-identity-decomposition
                       (reconstruct s god-table)))))

(defun serialize-clifford (c)
  "Serialize a CLIFFORD element C to a list of strings representing the PAULI elements they map to."
  (map 'list #'print-pauli (basis-map c)))

(defun serialize-clifford-sequence (s)
  "Given a LIST of CLIFFORD elements S, serialize them to a LIST of LISTs of serialized CLIFFORDs."
  (mapcar #'serialize-clifford s))

(defmacro define-factorize (name pauli-string diagonal-test anti-diagonal-test)
  "Given a function NAME, DIAGONAL-TEST, an S-expression checking TOP-LEFT and BOTTOM-RIGHT for some property, and ANTI-DIAGONAL-TEST an S-expression checking TOP-RIGHT and BOTTOM-LEFT for some property, generate a function that checks those properties and returns PAULI-STRING if they're T. NIL otherwise."
  `(defun ,name (M)
     (factorize M
                (lambda (top-left bottom-right) ,diagonal-test)
                (lambda (top-right bottom-left) ,anti-diagonal-test) ,pauli-string)))

(defun factorize (p diagonal-test anti-diagonal-test pauli-string)
  "Given a square matrix P of even dimension, and functions DIAGONAL-TEST and ANTI-DIAGONAL-TEST, evaluate DIAGONAL-TEST elementwise on the top left and bottom right submatrices of P, and ANTI-DIAGONAL-TEST elementwise on the top right and bottom left submatrices. If both evaluate to true, return PAULI-STRING, else NIL."
  (let ((m (magicl:nrows p))
        (n (magicl:ncols p)))
    (if
     (loop :for i :below (/ m 2)
           :always (loop :for j :below (/ n 2)
                         :always (and (funcall diagonal-test
                                               (magicl:tref p i j)
                                               (magicl:tref p (+ i (/ m 2)) (+ j (/ n 2))))
                                      (funcall anti-diagonal-test
                                               (magicl:tref p i (+ j (/ n 2)))
                                               (magicl:tref p (+ i (/ m 2)) j)))))
     pauli-string
     nil)))

(define-factorize factor-I "I"
  (cl-quil::double= top-left bottom-right)
  (and (cl-quil::double= top-right 0)
       (cl-quil::double= top-right bottom-left)))

(define-factorize factor-X "X"
  (and (cl-quil::double= top-left 0)
       (cl-quil::double= top-left bottom-right))
  (cl-quil::double= top-right bottom-left))

(define-factorize factor-Y "Y"
  (and (cl-quil::double= top-left 0)
       (cl-quil::double= top-left bottom-right))
  (cl-quil::double= top-right (- bottom-left)))

(define-factorize factor-Z "Z"
  (cl-quil::double= top-left (- bottom-right))
  (and (cl-quil::double= top-right 0)
       (cl-quil::double= top-right bottom-left)))

(defun valid-pauli-dim (m n)
  "T if M and N are valid dimensions of a Pauli matrix, NIL otherwise."
  (and (= m n)
       (cl-quil::positive-power-of-two-p m)))

(defun concatenate-or-nil (a b)
  "If A and B are both not NIL, concatenate them and return a STRING."
  (when (and a b) (concatenate 'string a b)))

;; This function is just for testing purposes. There also exists an
;; almost identical function collinearp in src/matrix-operations.lisp
;; that handles arbitrary global phases, but it uses double=, which is
;; too strict for the contexts where this function is used to test
;; equality.
(defun global-phase~ (wfa wfb)
  "Tests that two wavefunctions are equal up to a global phase of an eighth root of unity."
  (let ((phase-factor (cis (/ pi 4)))
        (phase-wf (copy-seq wfa)))
    (loop :for i :below 8
          :do (map-into phase-wf (lambda (x) (* x phase-factor)) phase-wf)
          :when (every #'cl-quil::double~ phase-wf wfb)
            :return t
          :finally (return nil))))

(defun phase-to-string (phase)
  "Returns a string representation of a fourth root of unity, given by PHASE, NIL otherwise."
  (cond ((cl-quil::double~ phase 1) "")
        ((cl-quil::double~ phase -1) "-")
        ((cl-quil::double~ phase #C(0 1)) "i")
        ((cl-quil::double~ phase #C(0 -1)) "-i")
        (t (error "Invalid phase number: ~A~%" phase))))

(defun string-to-phase (phase-str)
  "Returns the phase corresponding to a string form of a fourth root of unity, given by PHASE-STR, NIL otherwise."
  (cond ((string= phase-str "") 1)
        ((string= phase-str "-") -1)
        ((string= phase-str "i") #C(0 1))
        ((string= phase-str "-i") #C(0 -1))
        (t (error "Invalid phase string: ~A~%" phase-str))))

(defun pauli-matrix-p (p)
  "Returns two strings. The first is a string representation of the phase of P, assuming P is a Pauli matrix, which will be a fourth root of unity. The second return value is a string representation of P as a tensor product of single qubit Pauli operators. If P is not a Pauli operator, then the second value will be NIL."
  (check-type p magicl:matrix)
  (let* ((m (magicl:nrows p))
         (n (magicl:ncols p))
         (pauli-checks (mapcar (lambda (factor) (funcall factor p))
                               '(factor-I factor-Z factor-X factor-Y)))
         (pauli-phases (list (magicl:tref p 0 0)
                             (magicl:tref p 0 0)
                             (magicl:tref p 0 1)
                             (* #C(0 1) (magicl:tref p 0 1))))
         (pauli-and-phase (find-if #'car (mapcar #'list pauli-checks pauli-phases)))
         (pauli (first pauli-and-phase))
         (phase (second pauli-and-phase)))
    (cond
      ((not (valid-pauli-dim m n)) NIL)
      ((= m n 2)
       (if (not (null pauli))
           (handler-case (values (phase-to-string phase) pauli) (error () nil))
           NIL))
      ((or (factor-I p) (factor-Z p))
       (multiple-value-bind (coeff next-pauli)
           (pauli-matrix-p (magicl:slice p '(0 0) (list (/ m 2) (/ n 2))))
         (values coeff (concatenate-or-nil pauli next-pauli))))
      ((or (factor-Y p) (factor-X p))
       (multiple-value-bind (coeff next-pauli)
           (pauli-matrix-p (magicl:slice p (list (/ m 2) 0) (list m (/ n 2))))
         (values (phase-to-string (* (string-to-phase coeff)
                                     (if (factor-Y p) #C(0 -1) 1)))
                 (concatenate-or-nil pauli next-pauli))))
      (t nil))))

(let ((memo-table (make-hash-table :test #'equal)))
  (defun %clear-memo-table ()
    (clrhash memo-table))
  (defun n-qubit-pauli-basis-matrices (n)
    "Return a list of the n qubit pauli basis matrices. Note that this is the basis for group action, consisting of strings with one X or Z, not the basis for the vector space of complex matrices."
    (or (gethash n memo-table)
        (setf (gethash n memo-table)
              (let ((X (cl-quil::from-list '(0 1 1 0) '(2 2)))
                    (Z (cl-quil::from-list '(1 0 0 -1) '(2 2))))
                (loop :for i :below n
                      :collect (cl-quil::kq-gate-on-lines X n `(,i))
                      :collect (cl-quil::kq-gate-on-lines Z n `(,i))))))))

(defun matrix-to-clifford (gate)
  "Convert a matrix GATE into a CLIFFORD object."
  (let ((num-qubits (cl-quil:ilog2 (magicl:ncols gate))))
    (make-clifford
     :num-qubits num-qubits
     :basis-map (make-array
                 (* 2 num-qubits)
                 :initial-contents
                 (loop :for pauli :in (n-qubit-pauli-basis-matrices num-qubits)
                       :collect
                       (multiple-value-bind (phase conj)
                           (pauli-matrix-p
                            (reduce #'magicl:@
                                    (list gate
                                          pauli
                                          (magicl:conjugate-transpose gate))))
                         (assert (not (null conj)) ()
                                 "The given matrix does not represent a Clifford element.")
                         (pauli-from-string (concatenate 'string phase conj))))))))

(defun apply-pauli-to-wavefunction (ph index q wf)
  "Apply the pauli specified by index (0 = I, 1 = X, 2 = Z, 3 = Y) to qubit Q of the wavefunction WF, with a phase PH."
  (assert (cl-quil::positive-power-of-two-p (length wf))
          (wf)
          "The provided wavefunction must have a positive power-of-two length.")
  (multiple-value-bind (b a) (floor index 2)
    (let* ((phase (expt #C(0.0d0 1.0d0) (b* a b)))
           (n (cl-quil::ilog2 (length wf))))
      (declare (type (integer 1) n))    ; Due to ASSERT above.
      (flet ((xz (w0 w1)
               (setf w0 (* ph phase w0))
               (setf w1 (* ph phase w1 (expt #C(-1.0d0 0.0d0) b)))
               (if (zerop a)
                   (values w0 w1)
                   (values w1 w0))))
        (declare (inline xz))
        (dotimes (i (expt 2 (1- n)) wf)
          (let* ((addr0 (ash i 1))
                 (addr1 (dpb 1 (byte 1 0) addr0)))
            (setf addr0 (cl-quil::rotate-byte q (byte n 0) addr0))
            (setf addr1 (cl-quil::rotate-byte q (byte n 0) addr1))
            (multiple-value-bind (w0 w1) (xz (aref wf addr0) (aref wf addr1))
              (setf (aref wf addr0) w0
                    (aref wf addr1) w1))))))))

;;; This function converts a clifford of arbitrary arity to its
;;; corresponding matrix representation. How would one do this, you
;;; ask? It is done in these steps:
;;;
;;;     1) Create a blank square matrix of size 2^n.
;;;
;;;     2) Find the wavefunction image of the zero state under this
;;;     clifford, by using (tableau-function cliff) and applying it on
;;;     a tableau in the zero state, then extracting its
;;;     wavefunction. Thus, this wavefunction is the first column of
;;;     our result matrix.
;;;
;;;     3) Next, we calculate the columns of the matrix in succession,
;;;     which each represent the image of a basis state under the
;;;     clifford. We already know C|0...0>; we want to find C|x>. We
;;;     can get to any basis state from |0> by applying some
;;;     combination of single qubit X gates, and we know the images of
;;;     each of these Xs under conjugation by the clifford, so we can
;;;     apply each image to C|0...0> to get (C*X0*C')(C*X1*C')...C|0> =
;;;     C(X0*X1...)|0> = C|x>. Looping over all 2^n - 1 remaining basis
;;;     states and doing that for each one, and setting the appropriate
;;;     column of the matrix for each one, we get our final clifford
;;;     matrix.
(defun clifford-to-matrix (cliff)
  "Converts a clifford element into its matrix form, operating on the usual computational basis Bn x B(n-1) x ... x B0."
  (let* ((n (num-qubits cliff))
         (mat (cl-quil::zeros (list (expt 2 n) (expt 2 n))))
         (scratch-wf (make-array (expt 2 n) :element-type '(complex double-float) :initial-element #C(0.0d0 0.0d0)))
         (zero-image-tab (make-tableau-zero-state n))
         (pauli-map (clifford-basis-map cliff)))
    ;; Use a tableau-function to find the image of the zero state
    (apply (tableau-function cliff) zero-image-tab (alexandria:iota n :start (1- n) :step -1))
    (setf scratch-wf (tableau-wavefunction zero-image-tab))
    ;; Write the image to the first column of MAT
    (dotimes (row (expt 2 n))
      (setf (magicl:tref mat row 0) (aref scratch-wf row)))
    ;; For each subsequent basis state,
    (dotimes (curr-state (1- (expt 2 n)))
      ;; Apply the appropriate paulis to find its image under CLIFF
      (let ((x (logxor curr-state (1+ curr-state))))
        (dotimes (i n)
          (when (logbitp i x)
            ;; Apply the pauli on zeroth qubit, with phase
            (apply-pauli-to-wavefunction (if (zerop (phase-factor (aref pauli-map (* 2 i)))) 1 -1)
                                         (aref (pauli-components (aref pauli-map (* 2 i))) 1) 0 scratch-wf)
            ;; Apply the paulis on the rest of the qubits
            (loop :for p :from 2 :to n
                  :do (apply-pauli-to-wavefunction 1 (aref (pauli-components (aref pauli-map (* 2 i))) p) (1- p) scratch-wf))))
        ;; Write the basis state's image to the corresponding column of MAT
        (dotimes (row (expt 2 n))
          (setf (magicl:tref mat row (1+ curr-state)) (aref scratch-wf row)))))
    mat))

(defun extract-cliffords (parsed-quil)
  "Given PARSED-QUIL generate a list of pairs (CLIFFORD_i QUBITS_i) where CLIFFORD_i is the clifford for the ith instruction in PARSED-QUIL and QUBITS_i is a list of the qubits used in that instruction.

Note: will raise an error if PARSED-QUIL contains instruction types
other than APPLICATION, PRAGMA, or UNRESOLVED-APPLICATION."
  (loop :for instr :across (cl-quil:parsed-program-executable-code parsed-quil)
        :for qubits-used := (cl-quil::qubits-used instr)
        :unless (or (typep instr 'cl-quil:application)
                    (typep instr 'cl-quil:pragma)
                    (and cl-quil::*allow-unresolved-applications*
                         (typep instr 'cl-quil:unresolved-application))) :do
                           (error "Cannot extract clifford from the instr ~/cl-quil:instruction-fmt/"
                                  instr)
        :collect (list (matrix-to-clifford (cl-quil:gate-matrix instr))
                       qubits-used)))

(defun clifford-circuit-p (parsed-quil)
  "If the parsed circuit PARSED-QUIL is a clifford circuit, return the CLIFFORD corresponding to it. Otherwise return NIL. This will generate a clifford that acts on the number of qubits in the program, rather than a number of qubits that is the difference between the maximum and minimum index."
  (let* ((cliffords (extract-cliffords parsed-quil))
	 (qubits (sort (cl-quil::qubits-used parsed-quil) #'<))
	 (num-qubits (length qubits)))
    (reduce #'group-mul (loop :for (clifford targets) :in (reverse cliffords)
                              :collect (embed clifford num-qubits
                                              (loop :for qubit :in targets
                                                    :collect (position qubit qubits)))))))

(defun clifford-from-quil (quil)
  "Given a STRING of quil, produce the associated CLIFFORD element. If QUIL does not represent a Clifford circuit, return NIL. "
  (clifford-circuit-p (cl-quil::safely-parse-quil quil)))
