;;;; stabilizer.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil.clifford)

;;; Most of this is from or inspired by https://arxiv.org/pdf/quant-ph/0406196.pdf
;;;
;;; Gottesman's paper is also helpful https://arxiv.org/pdf/quant-ph/9807006.pdf

(deftype tableau-index ()
  '(integer 0 (#.array-total-size-limit)))

(deftype tableau ()
  ;; In all, this is a (2N + 1) x (2N + 1) square matrix of bits.
  ;;
  ;; Rows 0 to N - 1 are "destabilizer generators"
  ;; Rows N to 2N - 1 are "stabilizer generators"
  ;; Row 2N is for scratch space, as suggested by the Aaronson et al. paper.
  ;;
  ;; Columns 0 to N are Pauli X factors (x_i)
  ;; Columns N to 2N - 1 are Pauli Z factors (z_i)
  ;; Column 2N are the phases r
  '(simple-array bit (* *)))

(defun make-blank-tableau (n)
  (check-type n (integer 1))
  (let ((size (1+ (* 2 n))))
    (make-array (list size size) :element-type 'bit
                                 :initial-element 0)))

(declaim (inline tableau-qubits
                 tableau-x (setf tableau-x)
                 tableau-z (setf tableau-z)
                 tableau-r (setf tableau-r)
                 tableau-scratch (setf tableau-scratch)))

(defun tableau-qubits (tab)
  "How many qubits does the tableau TAB represent?"
  (declare (type tableau tab))
  (the tableau-index (ash (1- (array-dimension tab 0)) -1)))

(defun tableau-x (tab i j)
  (declare (type tableau tab)
           (type tableau-index i j))
  (aref tab i j))
(defun (setf tableau-x) (new-bit tab i j)
  (declare (type tableau tab)
           (type tableau-index i j)
           (type bit new-bit))
  (setf (aref tab i j) new-bit))

(defun tableau-z (tab i j)
  (declare (type tableau tab)
           (type tableau-index i j))
  (aref tab i (+ j (tableau-qubits tab))))
(defun (setf tableau-z) (new-bit tab i j)
  (declare (type tableau tab)
           (type tableau-index i j)
           (type bit new-bit))
  (setf (aref tab i (+ j (tableau-qubits tab))) new-bit))

(defun tableau-r (tab i)
  (declare (type tableau tab)
           (type tableau-index i))
  (aref tab i (* 2 (tableau-qubits tab))))
(defun (setf tableau-r) (new-bit tab i)
  (declare (type tableau tab)
           (type tableau-index i)
           (type bit new-bit))
  (setf (aref tab i (* 2 (tableau-qubits tab))) new-bit))

(defun tableau-scratch (tab i)
  (declare (type tableau tab)
           (type tableau-index i))
  (aref tab (* 2 (tableau-qubits tab)) i))
(defun (setf tableau-scratch) (new-bit tab i)
  (declare (type tableau tab)
           (type tableau-index i)
           (type bit new-bit))
  (setf (aref tab (* 2 (tableau-qubits tab)) i) new-bit))

(defun display-row (tab i)
  (cond
    ((= 1 (tableau-r tab i))
     (write-string "-"))
    (t
     (write-string "+")))
  (loop :for j :below (tableau-qubits tab) :do
    (let ((x (tableau-x tab i j))
          (z (tableau-z tab i j)))
      (cond
        ((and (= 0 x) (= 0 z)) (write-string "I"))
        ((and (= 1 x) (= 0 z)) (write-string "X"))
        ((and (= 0 x) (= 1 z)) (write-string "Z"))
        ((and (= 1 x) (= 1 z)) (write-string "Y"))))))

(defun display-tableau (tab)
  "Display the tableau TAB a la Aaronson's CHP program."
  (let ((n (tableau-qubits tab)))
    (loop :for i :below (* 2 n) :do
      (display-row tab i)
      (terpri)
      (when (= n (1+ i))
        (loop :repeat (1+ n) :do
          (write-char #\-))
        (terpri)))))

(defun zero-out-tableau (tab)
  "Bring the tableau to the zero state."
  (dotimes (i (array-total-size tab))
    (setf (row-major-aref tab i) 0))
  (dotimes (i (* 2 (tableau-qubits tab)))
    (setf (aref tab i i) 1)))

(defun make-tableau-zero-state (n)
  "Create a tableau of N qubits in the zero state."
  (let ((zero (make-blank-tableau n)))
    (dotimes (i (* 2 n) zero)
      (setf (aref zero i i) 1))))

(declaim (inline phase-of-product))
(defun phase-of-product (x1 z1 x2 z2)
  ;; This function is called "g" in the Aaronson et al. paper.
  (declare (type bit x1 z1 x2 z2))
  (levi-civita (logior x1 (ash z1 1))
               (logior x2 (ash z2 1))))

(declaim (inline %band b* %bior bmax %bxor b+ bnot))
(defun %band (a b)
  (declare (type bit a b))
  (the bit (logand a b)))
(defun b* (&rest bits)
  (declare (dynamic-extent bits))
  (let ((result 1))
    (declare (type bit result))
    (dolist (bit bits result)
      (setf result (%band result bit)))))
(defun %bior (a b)
  (declare (type bit a b))
  (the bit (logior a b)))
(defun bmax (&rest bits)
  (declare (dynamic-extent bits))
  (let ((result 0))
    (declare (type bit result))
    (dolist (bit bits result)
      (setf result (%bior result bit)))))
(defun %bxor (a b)
  (declare (type bit a b))
  (the bit (logxor a b)))
(defun b+ (&rest bits)
  (declare (dynamic-extent bits))
  (let ((result 0))
    (declare (type bit result))
    (dolist (bit bits result)
      (setf result (%bxor result bit)))))
(defun bnot (b)
  (declare (type bit b))
  (the bit (- 1 b)))

(define-modify-macro xorf (x) %bxor)

(defun incurred-phase-from-row-product (tab h i)
  "Calculate the incurred phase by multiplying row H and row I together."
  (let ((sum (+ (* 2 (tableau-r tab h))
                (* 2 (tableau-r tab i))
                (loop :with sum :of-type fixnum := 0
                      :for j :below (tableau-qubits tab)
                      :for ph := (phase-of-product
                                  (tableau-x tab i j)
                                  (tableau-z tab i j)
                                  (tableau-x tab h j)
                                  (tableau-z tab h j))
                      :do (incf sum ph)
                      :finally (return sum)))))
    (setf sum (mod sum 4))
    (unless (or (= sum 0) (= sum 2))
      (error "invalid prod between row ~D and ~D (got ~D):~%~A~%~A~%" h i sum
             (with-output-to-string (*standard-output*)
               (display-row tab h))
             (with-output-to-string (*standard-output*)
               (display-row tab i))))
    sum))

(declaim (inline row-product))
(defun row-product (tab h i)
  ;; Set generator h to h + i, where + is the Pauli group operation.
  (let ((sum (incurred-phase-from-row-product tab h i)))
    ;; Step 1
    (cond
      ((= 0 sum) (setf (tableau-r tab h) 0))
      ((= 2 sum) (setf (tableau-r tab h) 1))
      (t (error "Unreacheable: ~A." sum)))
    ;; Step 2
    (dotimes (j (tableau-qubits tab))
      (xorf (tableau-x tab h j) (tableau-x tab i j))
      (xorf (tableau-z tab h j) (tableau-z tab i j)))))

;;; This is intended to be a helper function for
;;; COMPILE-TABLEAU-OPERATION. Look at that function to see if it
;;; helps understanding this one.
(defun clifford-stabilizer-action (c)
  "Compute a symbolic representation of the action of the Clifford element C on the stabilizer representation of a state, namely the \"tableau representation\".

Given a Clifford C, calculate three values:

1. A list of variable names representing X-Z pairs on qubits 0, 1, .... A typical value will look like

    (X0 Z0 X1 Z1 ... Xn Zn),

but the symbols may be uninterned or named differently.

2. A Boolean expression which calculates the additional phase. If this boolean function were evaluated with the variables of (1) bound, then this would produce the action's contribution to the phase.

3. A list of updates to the variables of (1), represented as Boolean expressions. Similarly to (2), these are expressions in 1-1 correspondence with (1) which, if evaluated, would produce the updates to said variables."
  (let* ((num-qubits (clifford-num-qubits c))
         (num-variables (* 2 num-qubits))
         (variables (loop :for i :below num-variables
                          :collect (if (evenp i)
                                       (alexandria:format-symbol nil "X~D" (floor i 2))
                                       (alexandria:format-symbol nil "Z~D" (floor i 2)))))
         (phase-factor nil)
         (new-variables (make-array num-variables :initial-element nil)))
    ;; We will be mapping over all Paulis operating on a single
    ;; qubit. We will compute the action on the Pauli and use this to
    ;; produce part of the action.
    (map-all-paulis num-qubits
                    (lambda (i p)
                      (let* ((cp (apply-clifford c p))
                             (cp-phase (phase-factor cp)))
                        (assert (zerop (mod cp-phase 2)))
                        ;; We normalize the phase to be as follows:
                        ;;
                        ;;     0 - phase of 1
                        ;;     1 - phase of -1
                        ;;
                        ;; Note that this will _always_ be the case; a
                        ;; Clifford will maintain a real
                        ;; eigenvalue. So the factors +/-i can be
                        ;; thrown out.
                        (setf cp-phase (ash cp-phase -1))
                        ;; If we did accumulate a phase, we need to
                        ;; note how this phase gets accumulated.
                        ;;
                        ;; Below, BITS starts off as the bitwise
                        ;; representation of the Pauli P. This loop is
                        ;; iterating through the bits and variables in
                        ;; parallel, and we are building up a
                        ;; corresponding Boolean expression. This is
                        ;; the age-old trick of building a Boolean
                        ;; expression corresponding to a truth
                        ;; table.
                        ;;
                        ;; In the end, we want PHASE-FACTOR to have a
                        ;; conjunctive term which contributes a -1
                        ;; phase factor when the variables look like
                        ;; the Pauli P.
                        (when (= 1 cp-phase)
                          (loop :with bits := i
                                :for variable :in variables
                                :collect (if (zerop (ldb (byte 1 0) bits)) ; Equiv. to EVENP
                                             `(bnot ,variable)
                                             variable)
                                  :into conjunction
                                :do (setf bits (ash bits -1))
                                :finally (push `(b* ,@conjunction) phase-factor)))
                        ;; When the Pauli P is a generator (i.e., X or
                        ;; Z), then we want to record which other
                        ;; variables see an effect from this generator
                        ;; under C. Similarly to the last loop, we
                        ;; loop over the bitwise representation of the
                        ;; Pauli _acted on by the Clifford_ (CP
                        ;; instead of P). When we find any bit, we
                        ;; record the X or Z variable in that bit's
                        ;; running list. In the end, the running list
                        ;; represents a disjunction of contributions.
                        (when (power-of-two-p i) ; X and Z's will have power of two indexes.
                          (let ((var (nth (1- (integer-length i)) variables)))
                            ;; The var that contributes to X- and Z-FACTORS
                            (loop :with bits := (pauli-index cp)
                                  :for i :below num-variables
                                  :when (= 1 (ldb (byte 1 0) bits)) ; Equiv. to ODDP
                                    :do (push var (aref new-variables i))
                                  :do (setf bits (ash bits -1))))))))
    ;; We didn't actually construct the disjunction yet out of the
    ;; running lists, so do that here.
    (map-into new-variables (lambda (sum) `(b+ ,@sum)) new-variables)
    ;; Return the values.
    (values
     variables
     `(b+ ,@phase-factor)
     (coerce new-variables 'list))))

(defun compile-tableau-operation (c)
  "Compile a Clifford element into a tableau operation. This will be a lambda form whose first argument is the tableau to operate on, and whose remaining arguments are the qubit indexes to operate on. Upon applying this function to a tableau, it will be mutated "
  (check-type c clifford)
  (let* ((num-qubits (num-qubits c))
         ;; Gensyms
         (i      (gensym "I-"))
         (tab    (gensym "TAB-"))
         (qubits (loop :for i :below num-qubits
                       :collect (alexandria:format-symbol nil "Q~D" i))))
    (multiple-value-bind (variables phase-kickback new-variables)
        (clifford-stabilizer-action c)
      ;; Most of this lambda can be understood by understanding the
      ;; return values of CLIFFORD-STABILIZER-ACTION. See the
      ;; documentation of that function.
      `(lambda (,tab ,@qubits)
         (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0))
                  (type tableau ,tab)
                  (type tableau-index ,@qubits))
         (dotimes (,i (* 2 (tableau-qubits ,tab)))
           (declare (type tableau-index ,i))
           ;; First, we construct bindings to these variables
           ;; according to the qubits we are operating on.
           (let (,@(loop :for qubit :in qubits
                         :for (x z) :on variables :by #'cddr
                         :collect `(,x (tableau-x ,tab ,i ,qubit))
                         :collect `(,z (tableau-z ,tab ,i ,qubit))))
             (declare (type bit ,@variables))
             ;; Calculate the new phase.
             (xorf (tableau-r ,tab ,i) ,phase-kickback)
             ;; Update the tableau with the new values.
             (setf
              ,@(loop :for qubit :in qubits
                      :for (x z) :on new-variables :by #'cddr
                      ;; Set this...
                      :collect `(tableau-x ,tab ,i ,qubit)
                      ;; ...to this...
                      :collect x
                      ;; And set this...
                      :collect `(tableau-z ,tab ,i ,qubit)
                      ;; to this...
                      :collect z))))))))

;;; These functions: TABLEAU-APPLY-{CNOT, H, PHASE} are hard-coded
;;; from the Aaronson et al. paper. These can be readily generated
;;; from the above procedure.
(defun tableau-apply-cnot (tab a b)
  (declare (type tableau tab)
           (type tableau-index a b))
  (dotimes (i (* 2 (tableau-qubits tab)))
    (xorf (tableau-r tab i)   (logand (tableau-x tab i a)
                                      (tableau-z tab i b)
                                      (logxor 1
                                              (tableau-x tab i b)
                                              (tableau-z tab i a))))
    (xorf (tableau-x tab i b) (tableau-x tab i a))
    (xorf (tableau-z tab i a) (tableau-z tab i b))))

(defun tableau-apply-h (tab q)
  (declare (type tableau tab)
           (type tableau-index q))
  (dotimes (i (* 2 (tableau-qubits tab)))
    (xorf (tableau-r tab i) (logand (tableau-x tab i q) (tableau-z tab i q)))
    (rotatef (tableau-x tab i q) (tableau-z tab i q))))

(defun tableau-apply-phase (tab q)
  (declare (type tableau tab)
           (type tableau-index q))
  (dotimes (i (* 2 (tableau-qubits tab)))
    (xorf (tableau-r tab i) (logand (tableau-x tab i q) (tableau-z tab i q)))
    (xorf (tableau-z tab i q) (tableau-x tab i q))))

(declaim (inline tableau-set-row-to-pauli tableau-copy-row))
(defun tableau-set-row-to-pauli (tab i b)
  "Given a tableau TAB, set row I to be the single qubit Pauli term B, where if 0 <= B < N, the X_B will be set, and if N <= B < 2*N, then Z_(B - N) will be set."
  (declare (type tableau tab)
           (type tableau-index i b))
  (dotimes (j (array-dimension tab 1))
    (setf (aref tab i j) 0))
  (setf (aref tab i b) 1)
  nil)

(defun tableau-copy-row (tab i k)
  "Given a tableau TAB, overwrite row I with row K."
  (declare (type tableau tab)
           (type tableau-index i k))
  ;; set row i to row k
  (dotimes (j (array-dimension tab 1))
    (setf (aref tab i j) (aref tab k j))))

;;; Robert: For some reason that I'm not sure about, the procedure for
;;; non-deterministic measurement from the Aaronson et al. paper
;;; didn't work. I had to look at their C code for inspiration, which
;;; oddly does work (and doesn't seem to do precisely what the paper
;;; says.)
;;;
;;; The general idea is a two step operation: (1) determine if the
;;; measurement would produce a deterministic result, (2) determine
;;; the result.
(defun tableau-measure (tab q)
  (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0))
           (type tableau tab)
           (type tableau-index q))
  ;; return measurement outcome, and boolean indicating if it was
  ;; deterministic
  (let* ((n (tableau-qubits tab))
         (p (loop :for p :below n
                  ;; Look at the stabilizer generators, hence the + N.
                  :when (= 1 (tableau-x tab (+ p n) q))
                    :do (return p)
                  :finally (return nil))))
    (declare (type (or tableau-index null) p)
             (type tableau-index n))
    ;; The value of P is a reference to the Pth stabilizer
    ;; generator such that a Pauli X_q is present.
    (cond
      ;; Case I: Outcome is not deterministic.
      (p
       (tableau-copy-row tab p (+ p n))
       (tableau-set-row-to-pauli tab (+ p n) (+ q n))
       (setf (tableau-r tab (+ p n)) (random 2))
       (dotimes (i (* 2 n))
         (when (and (/= i p)
                    (= 1 (tableau-x tab i q)))
           (row-product tab i p)))
       (values (tableau-r tab (+ p n)) nil))
      ;; Case II: Outcome is determined.
      ;;
      ;; The work below will not modify the tableau (except for the
      ;; scratch space). This is used simply to compute what the
      ;; measurement would be.
      (t
       ;; Zero out the scratch space.
       (dotimes (i (* 2 n))
         (setf (tableau-scratch tab i) 0))
       (let ((m (loop :for m :below n
                      :when (= 1 (tableau-x tab m q))
                        :do (return m)
                      :finally (return n))))
         (tableau-copy-row tab (* 2 n) (+ m n))
         (loop :for i :from (1+ m) :below n :do
           (when (= 1 (tableau-x tab i q))
             (row-product tab (* 2 n) (+ i n))))
         (values (tableau-r tab (* 2 n)) t))))))


;;; The .chp file format is an input to Aaronson's chp program written
;;; in C. Below is a parser and interpreter for it.

(defun interpret-chp (code tab)
  (loop :for i :from 1
        :for isn :in code
        :do
           (format t "~D: ~A" i isn)
           (alexandria:destructuring-ecase isn
             ((H q)       (tableau-apply-h tab q))
             ((PHASE q)   (tableau-apply-phase tab q))
             ((CNOT p q)  (tableau-apply-cnot tab p q))
             ((MEASURE q) (multiple-value-bind (outcome determ?)
                              (tableau-measure tab q)
                            (format t " => ~D~:[ (random)~;~]" outcome determ?))))
           (terpri)))

(defun read-chp-file (file)
  "Parse a .chp file as defined by Aaronson."
  (let ((max-qubit 0))
    (labels ((parse-gate (gate-name)
               (cond
                 ((string= gate-name "h") 'H)
                 ((string= gate-name "c") 'CNOT)
                 ((string= gate-name "p") 'PHASE)
                 ((string= gate-name "m") 'MEASURE)
                 (t (error "Invalid gate name: ~S" gate-name))))
             (parse-qubits (qubits)
               (loop :with seen := 0
                     :for qubit-string :in qubits
                     :for qubit := (parse-integer qubit-string :junk-allowed nil)
                     :collect (progn
                                (when (minusp qubit)
                                  (error "Qubits must be non-negative integers"))
                                (when (logbitp qubit seen)
                                  (error "Can't have duplicate qubits: ~A" qubits))
                                (setf seen (dpb 1 (byte 1 qubit) seen))
                                (setf max-qubit (max max-qubit qubit))
                                qubit)))
             (parse-line (line)
               (let ((split (split-sequence:split-sequence #\Space line :remove-empty-subseqs t)))
                 (cond
                   ((null split) 
                    nil)
                   (t
                    (destructuring-bind (gate . qubits) split
                      (when (null qubits)
                        (error "Invalid line, need qubits: ~S" line))
                      (cons (parse-gate gate) (parse-qubits qubits))))))))
      (with-open-file (s file :direction ':input)
        ;; Read until the # line
        (loop :for line := (read-line s nil ':eof)
              :do (cond
                    ((eq ':eof line) (error "Didn't find a # line"))
                    ;; We can start processing lines.
                    ((string= line "#") (loop-finish))))
        ;; Read the actual gate lines.
        (values (loop :for line := (read-line s nil ':eof)
                      :until (eq ':eof line)
                      :for parsed-line := (parse-line line)
                      :when parsed-line
                        :collect parsed-line)
                (1+ max-qubit))))))

(defun interpret-chp-file (file)
  "Execute one of Aaronson's .chp files."
  (multiple-value-bind (code num-qubits)
      (read-chp-file file)
    (let ((tab (make-tableau-zero-state num-qubits)))
      (interpret-chp code tab))))
