;;;; stabilizer.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil.clifford)

(declaim (optimize (speed 0) safety debug))

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
  ;; Row 2N is for scratch space, as suggested by the paper.
  ;; Columns 0 to N are x
  ;; Columns N to 2N - 1 are z
  ;; Column 2N is are the phases r
  '(simple-array bit (* *)))

(defun make-blank-tableau (n)
  (check-type n (integer 1))
  (let ((size (1+ (* 2 n))))
    (make-array (list size size) :element-type 'bit
                                 :initial-element 0)))

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
  ;; This function is called "g" in the Aaronson paper.
  (declare (type bit x1 z1 x2 z2))
  (levi-civita (logior x1 (ash z1 1))
               (logior x2 (ash z2 1))))

(declaim (inline %band b* %bior bmax %bxor b+ bnot))
(defun %band (a b)
  (declare (type bit a b))
  (the bit (logand a b)))
(defun b* (&rest bits)
  (declare (dynamic-extent bits))
  (the bit (reduce #'%band bits :initial-value 1)))
(defun %bior (a b)
  (declare (type bit a b))
  (the bit (logior a b)))
(defun bmax (&rest bits)
  (declare (dynamic-extent bits))
  (the bit (reduce #'%bior bits :initial-value 0)))
(defun %bxor (a b)
  (declare (type bit a b))
  (the bit (logxor a b)))
(defun b+ (&rest bits)
  (declare (dynamic-extent bits))
  (the bit (reduce #'%bxor bits :initial-value 0)))
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
    (unless (member sum '(0 2))
      (error "invalid prod between row ~D and ~D (got ~D):~%~A~%~A~%" h i sum
             (with-output-to-string (*standard-output*)
               (display-row tab h))
             (with-output-to-string (*standard-output*)
               (display-row tab i))))
    sum))

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

(defun clifford-symplectic-action (c)
  "Given a Clifford C, calculate three values:

1. A list of variable names representing X-Z pairs on qubits 0, 1, ....

2. A Boolean expression which calculates the additional phase.

3. A list of updates to the variables of (1), represented as Boolean expressions."
  (let* ((num-qubits (clifford-num-qubits c))
         (num-variables (* 2 num-qubits))
         (variables (loop :for i :below num-variables
                          :collect (if (evenp i)
                                       (alexandria:format-symbol nil "X~D" (floor i 2))
                                       (alexandria:format-symbol nil "Z~D" (floor i 2)))))
         (phase-factor nil)
         (new-variables (make-array num-variables :initial-element nil)))
    (map-all-paulis num-qubits
                    (lambda (i p)
                      (let* ((cp (apply-clifford c p))
                             (cp-phase (phase-factor cp)))
                        (assert (zerop (mod cp-phase 2)))
                        (setf cp-phase (ash cp-phase -1))
                        ;; Collect phase contribution.
                        (when (= 1 cp-phase)
                          (loop :with bits := i
                                :for variable :in variables
                                :collect (if (zerop (ldb (byte 1 0) bits)) ; EVENP
                                             `(bnot ,variable)
                                             variable)
                                  :into conjunction
                                :do (setf bits (ash bits -1))
                                :finally (push `(b* ,@conjunction) phase-factor)))
                        ;; When we have X or Z (i = a power of 2),
                        ;; check out where they get shuffled.
                        (when (power-of-two-p i)
                          (let ((var (nth (1- (integer-length i)) variables)))
                            ;; The var that contributes to X- and Z-FACTORS
                            (loop :with bits := (pauli-index cp)
                                  :for i :below num-variables
                                  :when (= 1 (ldb (byte 1 0) bits)) ; ODDP
                                    :do (push var (aref new-variables i))
                                  :do (setf bits (ash bits -1))))
                          ;(format t "~A -> ~A~%" p cp)
                          ;(format t "~2,'0B -> ~2,'0B~%" (pauli-index p) (pauli-index cp))
                          )
                        ;(format t "  ~v,'0B -> ~B~%" num-variables i cp-phase)
                        )))
    (map-into new-variables (lambda (sum) `(b+ ,@sum)) new-variables)
    (values
     variables
     `(b+ ,@phase-factor)
     (coerce new-variables 'list))))

(defun compile-tableau-operation (c)
  "Compile a Clifford element into a tableau operation. This will be a lambda form whose first argument is the tableau to operate on, and whose remaining arguments are the qubit indexes to operate on."
  (check-type c clifford)
  (let* ((num-qubits (num-qubits c))
         ;; Gensyms
         (i      (gensym "I-"))
         (tab    (gensym "TAB-"))
         (qubits (loop :for i :below num-qubits
                       :collect (alexandria:format-symbol nil "Q~D" i))))
    (multiple-value-bind (variables phase-kickback new-variables)
        (clifford-symplectic-action c)
      `(lambda (,tab ,@qubits)
         (declare (type tableau ,tab)
                  (type tableau-index ,@qubits))
         (dotimes (,i (* 2 (tableau-qubits ,tab)))
           (declare (type tableau-index ,i))
           (let (,@(loop :for qubit :in qubits
                         :for (x z) :on variables :by #'cddr
                         :collect `(,x (tableau-x ,tab ,i ,qubit))
                         :collect `(,z (tableau-z ,tab ,i ,qubit))))
             ;; Calculate the new phase.
             (xorf (tableau-r ,tab ,i) ,phase-kickback)
             (setf
              ,@(loop :for qubit :in qubits
                      :for (x z) :on new-variables :by #'cddr
                      ;; Set this...
                      :collect `(tableau-x ,tab ,i ,qubit)
                      ;; to this...
                      :collect x
                      ;; And set this...
                      :collect `(tableau-z ,tab ,i ,qubit)
                      ;; to this...
                      :collect z))))))))

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

;;; For some reason that I'm not sure about, the procedure for
;;; non-deterministic measurement from the Aaronson et al. paper
;;; didn't work. I had to look at their C code for inspiration, which
;;; oddly does work (and doesn't do precisely what the paper says.)
(defun tableau-measure (tab q)
  (declare (type tableau tab)
           (type tableau-index q))
  ;; return measurement outcome, and boolean indicating if it was
  ;; deterministic
  (let* ((n (tableau-qubits tab))
         (p (loop :for p :below n
                  :when (= 1 (tableau-x tab (+ p n) q))
                    :do (return p)
                  :finally (return nil))))
    (declare (type (or tableau-index null) p)
             (type tableau-index n))
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
       (tableau-r tab (+ p n)))
      ;; Case II: Outcome is determined.
      (t
       ;; Zero out the scratch space.
       (dotimes (i (* 2 n))
         (setf (tableau-scratch tab i) 0))
       ;; Update the scratch space. Sum up the stabilizers into the
       ;; scratch space.
       (dotimes (i n)
         (when (= 1 (tableau-x tab i q))
           (row-product tab (* 2 n) (+ i n))))
       ;; Extract the answer from the phase bit of the scratch space.
       (values (tableau-r tab (* 2 n)) t)))))


;;; For testing

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
                    ((string= line "#") (loop-finish))
                    #+ig
                    ((cl-ppcre:scan "^#\\s*$"  line) (loop-finish))))
        ;; Read the actual gate lines.
        (values (loop :for line := (read-line s nil ':eof)
                      :until (eq ':eof line)
                      :for parsed-line := (parse-line line)
                      :when parsed-line
                        :collect parsed-line)
                (1+ max-qubit))))))

(defun interpret-chp-file (file)
  (multiple-value-bind (code num-qubits)
      (read-chp-file file)
    (let ((tab (make-tableau-zero-state num-qubits)))
      (time (interpret-chp code tab)))))
