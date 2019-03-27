;;;; benchmarking-procedures.lisp
;;;;
;;;; Author: Anthony Polloreno
;;;;
;;;; Procedures to respond to quilc requests about cliffords, specifically for benchmarking quantum gate sets and circuits.

(in-package :cl-quil.clifford)

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
         (min-identity-decomposition (alexandria:extremum identity-decompositions #'< :key #'length)))
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
  (let ((m (magicl:matrix-rows p))
        (n (magicl:matrix-cols p)))
    (if
     (loop :for i :below (/ m 2)
           :always (loop :for j :below (/ n 2)
                         :always (and (funcall diagonal-test
                                               (magicl:ref p i j)
                                               (magicl:ref p (+ i (/ m 2)) (+ j (/ n 2))))
                                      (funcall anti-diagonal-test
                                               (magicl:ref p i (+ j (/ n 2)))
                                               (magicl:ref p (+ i (/ m 2)) j)))))
     pauli-string
     nil)))

(define-factorize factor-I "I"
  (complex= top-left bottom-right)
  (and (complex= top-right 0)
       (complex= top-right bottom-left)))

(define-factorize factor-X "X"
  (and (complex= top-left 0)
       (complex= top-left bottom-right))
  (complex= top-right bottom-left))

(define-factorize factor-Y "Y"
  (and (complex= top-left 0)
       (complex= top-left bottom-right))
  (complex= top-right (- bottom-left)))

(define-factorize factor-Z "Z"
  (complex= top-left (- bottom-right))
  (and (complex= top-right 0)
       (complex= top-right bottom-left)))

(defun valid-pauli-dim (m n)
  "T if M and N are valid dimensions of a Pauli matrix, NIL otherwise."
  (and (= m n) (>= m n 2)
       (cl-quil::power-of-two-p m)
       (cl-quil::power-of-two-p n)))

(defun concatenate-or-nil (a b)
  "If A and B are both not NIL, concatenate them and return a STRING."
  (when (and a b) (concatenate 'string a b)))

(defun complex= (a b)
  (and (quil::double= (realpart a) (realpart b))
       (quil::double= (imagpart a) (imagpart b))))

(defun phase-to-string (phase)
  "Returns a string representation of a fourth root of unity, given by PHASE, NIL otherwise."
  (cond ((complex= phase 1) "")
        ((complex= phase -1) "-")
        ((complex= phase #C(0 1)) "i")
        ((complex= phase #C(0 -1)) "-i")
        (t NIL)))

(defun pauli-matrix-p (p)
  "Returns two strings. The first is a string representation of the phase of P, assuming P is a Pauli matrix, which will be a fourth root of unity. The second return value is a string representation of P as a tensor product of single qubit Pauli operators. If P is not a Pauli operator, then the second value will be NIL."
  (check-type p magicl:matrix)
  (let* ((m (magicl:matrix-rows p))
         (n (magicl:matrix-cols p))
         (pauli-checks (mapcar (lambda (factor) (funcall factor p))
                               '(factor-I factor-Z factor-X factor-Y)))
         (pauli-phases (list (magicl:ref p 0 0)
                             (magicl:ref p 0 0)
                             (magicl:ref p 0 1)
                             (* #C(0 1) (magicl:ref p 0 1))))
         (pauli-and-phase (find-if #'car (mapcar #'list pauli-checks pauli-phases)))
         (pauli (first pauli-and-phase))
         (phase (second pauli-and-phase)))
    (cond
      ((not (valid-pauli-dim m n)) NIL)
      ((= m n 2)
       (if (not (null pauli))
           (values (phase-to-string phase) pauli)
           NIL))
      ((or (factor-I p) (factor-Z p))
       (multiple-value-bind (coeff second-pauli)
           (pauli-matrix-p (magicl::slice p 0 (/ m 2) 0 (/ n 2)))
         (values coeff (concatenate-or-nil pauli second-pauli))))
      ((or (factor-Y p) (factor-X p))
       (multiple-value-bind (coeff second-pauli) (pauli-matrix-p
                                                  (magicl::slice p (/ m 2) m 0 (/ n 2)))
         (values coeff (concatenate-or-nil pauli second-pauli))))
      (t nil))))

(let ((memo-table (make-hash-table :test #'equal)))
  (defun %clear-memo-table ()
    (clrhash memo-table))
  (defun n-qubit-pauli-basis-matrices (n)
    "Return a list of the n qubit pauli basis matrices. Note that this is the basis for group action, consisting of strings with one X or Z, not the basis for the vector space of complex matrices."
    (or (gethash n memo-table)
        (setf (gethash n memo-table)
              (let ((X   (magicl:make-complex-matrix 2 2 '(0 1 1 0)))
                    (Z   (magicl:make-complex-matrix 2 2 '(1 0 0 -1))))
                (loop :for i :from (- n 1) :downto 0
                      :collect (quil::kq-gate-on-lines X n `(,i))
                      :collect (quil::kq-gate-on-lines Z n `(,i))))))))

(defun extract-cliffords (parsed-quil)
  "Given PARSED-QUIL generate the CLIFFORD for each gate"
  (loop :for gate-application :across (quil::parsed-program-executable-code parsed-quil)
        :collect
        (let* ((gate (quil:gate-matrix gate-application))
               (num-qubits (quil:ilog2 (magicl:matrix-cols gate))))
          (make-clifford
           :num-qubits num-qubits
           :basis-map (make-array
                       (* 2 num-qubits)
                       :initial-contents
                       (loop :for pauli :in (n-qubit-pauli-basis-matrices num-qubits)
                             :collect
                             (multiple-value-bind (phase conj)
                                 (pauli-matrix-p
                                  (reduce #'magicl:multiply-complex-matrices
                                          (list gate
                                                pauli
                                                (magicl:conjugate-transpose gate))))
                               (assert (not (null conj)) () "The given program does not represent a Clifford element.")
                               (pauli-from-string (concatenate 'string phase conj)))))))))


(defun extract-qubits-used (parsed-quil)
  "Given PARSED-QUIL return the indices of the qubits used. The result is given as a list of the qubits used per instruction in the PARSED-QUIL."
  (loop :for parsed-clifford :across (quil::parsed-program-executable-code parsed-quil)
        :collect (mapcar #'quil::qubit-index (quil::application-arguments parsed-clifford))))

(defun clifford-circuit-p (parsed-quil)
  "If the parsed circuit PARSED-QUIL a clifford circuit, return the CLIFFORD corresponding to it. Otherwise return NIL. This will generate a clifford that acts on the number of qubits in the program, rather than a number of qubits that is the difference between the maximum and minimum index."
  (let* ((cliffords (extract-cliffords parsed-quil))
         (qubit-targets (extract-qubits-used parsed-quil))
	 (qubits (sort (remove-duplicates (alexandria:flatten qubit-targets)) #'<))
	 (num-qubits (length qubits)))
    (reduce #'group-mul (loop :for clifford :in (reverse cliffords)
			   :for target :in (reverse qubit-targets)
			   :collect (embed clifford num-qubits (loop :for qubit :in target :collect (position qubit qubits)))))))

(defun clifford-from-quil (quil)
  "Given a STRING of quil, produce the associated CLIFFORD element. If QUIL does not represent a Clifford circuit, return NIL. "
  (clifford-circuit-p (quil::safely-parse-quil quil)))

