;;;; stabilizer.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil.clifford)

(declaim (optimize speed (safety 0) (debug 0)))
;;; Most of this is from or inspired by https://arxiv.org/pdf/quant-ph/0406196.pdf

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

(declaim (inline tableau-qubits
                 tableau-x
                 tableau-z
                 tableau-r
                 tableau-scratch
                 (setf tableau-x)
                 (setf tableau-z)
                 (setf tableau-r)
                 (setf tableau-scratch)))

(defun tableau-qubits (tab)
  (declare (type tableau tab))
  (the tableau-index (ash (1- (array-dimension tab 0)) -1)))

(defun check-row-index (tab i)
  (assert (<= 0 i (* 2 (tableau-qubits tab)))))

(defun tableau-x (tab i j)
  (declare (type tableau tab)
           (type tableau-index i j))
;  (check-row-index tab i)
;  (check-index tab j)
  (aref tab i j))
(defun (setf tableau-x) (new-bit tab i j)
  (declare (type tableau tab)
           (type tableau-index i j)
           (type bit new-bit))
;  (check-row-index tab i)
;  (check-index tab j)
;  (check-type new-bit bit)
  (setf (aref tab i j) new-bit))

(defun tableau-z (tab i j)
  (declare (type tableau tab)
           (type tableau-index i j))
;  (check-row-index tab i)
;  (check-index tab j)
  (aref tab i (+ j (tableau-qubits tab))))
(defun (setf tableau-z) (new-bit tab i j)
  (declare (type tableau tab)
           (type tableau-index i j)
           (type bit new-bit))
;  (check-row-index tab i)
;  (check-index tab j)
;  (check-type new-bit bit)
  (setf (aref tab i (+ j (tableau-qubits tab))) new-bit))

(defun tableau-r (tab i)
  (declare (type tableau tab)
           (type tableau-index i))
;  (check-row-index tab i)
  (aref tab i (* 2 (tableau-qubits tab))))
(defun (setf tableau-r) (new-bit tab i)
  (declare (type tableau tab)
           (type tableau-index i)
           (type bit new-bit))
;  (check-row-index tab i)
;  (check-type new-bit bit)
  (setf (aref tab i (* 2 (tableau-qubits tab))) new-bit))

(defun tableau-scratch (tab i)
  (declare (type tableau tab)
           (type tableau-index i))
;  (assert (<= 0 i (* 2 (tableau-qubits tab))))
  (aref tab (* 2 (tableau-qubits tab)) i))
(defun (setf tableau-scratch) (new-bit tab i)
  (declare (type tableau tab)
           (type tableau-index i)
           (type bit new-bit))
;  (assert (<= 0 i (* 2 (tableau-qubits tab))))
;  (check-type new-bit bit)
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
  (let ((n (tableau-qubits tab)))
    (loop :for i :below (* 2 n) :do
      (display-row tab i)
      (terpri)
      (when (= n (1+ i))
        (loop :repeat (1+ n) :do
          (write-char #\-))
        (terpri)))))

(defun make-tableau-zero-state (n)
  (let ((zero (make-blank-tableau n)))
    (dotimes (i (* 2 n) zero)
      (setf (aref zero i i) 1))))

;; This is called "g" in the paper.
(declaim (inline phase-of-product))
(defun phase-of-product (x1 z1 x2 z2)
  (declare (type bit x1 z1 x2 z2))
  (levi-civita (logior x1 (ash z1 1))
               (logior x2 (ash z2 1))))

(declaim (inline xor))
(defun xor (a b)
  (declare (type bit a b))
  (the bit (logxor a b)))

(define-modify-macro xorf (x) xor)

(defun calc-phase-offset (tab h i)
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

(defun row-sum (tab h i)
  ;; Set generator h to h + i, where + is the Pauli group operation.
  (let ((sum (calc-phase-offset tab h i)))
    ;; Step 1
    (cond
      ((= 0 sum) (setf (tableau-r tab h) 0))
      ((= 2 sum) (setf (tableau-r tab h) 1))
      (t (error "Unreacheable: ~A." sum)))
    ;; Step 2
    (dotimes (j (tableau-qubits tab))
      (xorf (tableau-x tab h j) (tableau-x tab i j))
      (xorf (tableau-z tab h j) (tableau-z tab i j)))))

(defun tableau-apply-cnot (tab a b)
  (declare (type tableau tab)
           (type fixnum a b))
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
           (type fixnum q))
  (dotimes (i (* 2 (tableau-qubits tab)))
    (xorf (tableau-r tab i) (logand (tableau-x tab i q) (tableau-z tab i q)))
    (rotatef (tableau-x tab i q) (tableau-z tab i q))))

(defun tableau-apply-phase (tab q)
  (declare (type tableau tab)
           (type fixnum q))
  (dotimes (i (* 2 (tableau-qubits tab)))
    (xorf (tableau-r tab i) (logand (tableau-x tab i q) (tableau-z tab i q)))
    (xorf (tableau-z tab i q) (tableau-x tab i q))))

(defun rowset (tab i b)
  (declare (type tableau tab)
           (type fixnum i b))
  ;(assert (<= 0 b (1- (* 2 (tableau-qubits tab)))))
  (dotimes (j (array-dimension tab 1))
    (setf (aref tab i j) 0))
  (setf (aref tab i b) 1)
  nil)

(defun rowcopy (tab i k)
  (declare (type tableau tab)
           (type fixnum i k))
  ;; set row i to row k
  (dotimes (j (array-dimension tab 1))
    (setf (aref tab i j) (aref tab k j))))

(defun tableau-measure (tab q)
  (declare (type tableau tab)
           (type fixnum q))
  ;; return measurement outcome, and boolean indicating if it was
  ;; deterministic
  (let* ((n (tableau-qubits tab))
         (p (loop :for p :below n
                  :when (= 1 (tableau-x tab (+ p n) q))
                    :do (return p)
                  :finally (return nil))))
    (declare (type (or fixnum null) p)
             (type fixnum n))
    (cond
      ;; Case I: Outcome is not deterministic.
      (p
       (rowcopy tab p (+ p n))
       (rowset tab (+ p n) (+ q n))
       (setf (tableau-r tab (+ p n)) (random 2))
       (dotimes (i (* 2 n))
         (when (and (/= i p)
                    (= 1 (tableau-x tab i q)))
           (row-sum tab i p)))
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
           (row-sum tab (* 2 n) (+ i n))))
       ;; Extract the answer from the phase bit of the scratch space.
       (values (tableau-r tab (* 2 n)) t)))))

#+doesnt-work
(defun tableau-measure (tab q)
  ;; return measurement outcome, and boolean indicating if it was
  ;; deterministic
  (let* ((n (tableau-qubits tab))
         (p (loop :for p :from n :below (* 2 n)
                  :when (= 1 (tableau-x tab p q))
                    :do (return p)
                  :finally (return nil))))
    (cond
      ;; Case I: Outcome is not deterministic.
      (p
       (assert (= 1 (tableau-x tab p q)))
       (dotimes (i (* 2 n))
         (when (and (/= i p) (= 1 (tableau-x tab i q)))
           (row-sum tab i p)))
       ;; Set the (p - n)th row to the pth row, and zero out the pth
       ;; row.
       (let ((p-n (- p n)))
         (dotimes (column (array-dimension tab 1))
           (setf (aref tab p-n column) (aref tab p column)
                 (aref tab p column)   0)))
       ;; Non-deterministically choose r_p = {0, 1}. It's currently
       ;; set to 0 from the previous line.
       (when nil #+ig(zerop (random 2))
         (setf (tableau-r tab p) 1))
       ;; Set z_pq = 1.
       (setf (tableau-z tab p q) 1)
       ;; Return the answer.
       (values (tableau-r tab p) nil))
      ;; Case II: Outcome is determined.
      (t
       (values 0 t))
      #+ig
      (t
       ;; Zero out the scratch space.
       (dotimes (i (* 2 n))
         (setf (tableau-scratch tab i) 0))
       ;; Update the scratch space. Sum up the stabilizers into the
       ;; scratch space.
       (dotimes (i n)
         (when (= 1 (tableau-x tab i q))
           (row-sum tab (* 2 n) (+ i n))))
       ;; Extract the answer from the phase bit of the scratch space.
       (values (tableau-r tab (* 2 n)) t)))))

;;; For testing

(defun interpret-chp (num-qubits code)
  (loop :with tab := (make-tableau-zero-state num-qubits)
        :for i :from 1
        :for isn :in code
        :do
           (let ((*print-pretty* nil))
             ;(format t "~D: ~A~%" i isn)
             (alexandria:destructuring-ecase isn
               ((H q)       (tableau-apply-h tab q))
               ((PHASE q)   (tableau-apply-phase tab q))
               ((CNOT p q)  (tableau-apply-cnot tab p q))
               ((MEASURE q) (multiple-value-bind (outcome determ?)
                                (tableau-measure tab q)
                              #+ihn
                              (format t "~&qubit ~D measured to ~D~:[ (random)~;~]~%" q outcome determ?)))))
        :finally (return tab)))

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
    (time (interpret-chp num-qubits code))))
