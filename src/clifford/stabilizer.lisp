;;;; stabilizer.lisp
;;;;
;;;; Authors: Robert Smith, Andrew Shi

(in-package #:cl-quil/clifford)

;;; Most of this is from or inspired by https://arxiv.org/pdf/quant-ph/0406196.pdf
;;;
;;; Gottesman's paper is also helpful
;;; https://arxiv.org/pdf/quant-ph/9807006.pdf ...but BEWARE HIS PIQUANT
;;; QUBIT NOTATION!!  Correct qubit ordering is very important. Pay
;;; attention to which qubits are least/most significant and how they
;;; are ordered in the code with respect to this!!

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

(defun copy-tableau (tab)
  "Creates a copy of TAB and returns it."
  (let ((copy (make-tableau-zero-state (tableau-qubits tab))))
    (loop :for i :below (array-total-size copy)
          :do (setf (row-major-aref copy i)
                    (row-major-aref tab i)))
    copy))

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

(defun tableau-clear-scratch (tab)
  "Clear the scratch row of TAB."
  (dotimes (i (1+ (* 2 (tableau-qubits tab))))
    (setf (tableau-scratch tab i) 0)))

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

(defun tableau-copy-row (tab i k)
  "Given a tableau TAB, overwrite row I with row K."
  (declare (type tableau tab)
           (type tableau-index i k))
  ;; set row i to row k
  (dotimes (j (array-dimension tab 1))
    (setf (aref tab i j) (aref tab k j))))

(defun tableau-swap-row (tab i k)
  "Given a tableau TAB, swap row I with row K."
  (declare (type tableau tab)
           (type tableau-index i k))
  (dotimes (j (1+ (* 2 (tableau-qubits tab))))
    (rotatef (aref tab i j) (aref tab k j))))

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

(defun take-tableau-to-basis-state (tab x)
  "Bring the tableau to the specific basis state X. X is an integer interpreted as a collection of bits, so for example, |01000> = #b01000 = 8."
  (assert (>= (tableau-qubits tab) (integer-length x)))
  (let ((n (tableau-qubits tab)))
    (zero-out-tableau tab)
    (dotimes (i n)
      (when (logbitp i x)
        (setf (tableau-r tab (+ i n)) 1)))))

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
  (mod (+ (* 2 (tableau-r tab h))
          (* 2 (tableau-r tab i))
          (loop :with sum :of-type fixnum := 0
                :for j :below (tableau-qubits tab)
                :for ph := (phase-of-product
                            (tableau-x tab i j)
                            (tableau-z tab i j)
                            (tableau-x tab h j)
                            (tableau-z tab h j))
                :do (incf sum ph)
                :finally (return sum)))
       4))

(declaim (inline row-product))
(defun row-product (tab h i &key allow-dirty-phase)
  "Set generator h to h + i, where + is the Pauli group operation. When ALLOW-DIRTY-PHASE is true, phases are allowed to be i or -i."
  ;; We do this because while we are multiplying rows around to
  ;; generate the full stabilizer group, even though each stabilizer
  ;; will eventually have a phase of 1 or -1, the phase may take the
  ;; value of i or -i in intermediate steps (see below).
  (let ((sum (incurred-phase-from-row-product tab h i)))
    ;; Step 1
    (cond
      ((= 0 sum) (setf (tableau-r tab h) 0))
      ((= 2 sum) (setf (tableau-r tab h) 1))
      ((not allow-dirty-phase)
       (error "invalid prod between row ~D and ~D (got ~D, which is an invalid phase for a generator or antigenerator!):~%~A~%~A~%" h i sum
              (with-output-to-string (*standard-output*)
                (display-row tab h))
              (with-output-to-string (*standard-output*)
                (display-row tab i)))))
    ;; Step 2
    (dotimes (j (tableau-qubits tab))
      (xorf (tableau-x tab h j) (tableau-x tab i j))
      (xorf (tableau-z tab h j) (tableau-z tab i j)))
    ;; Return the phase
    sum))

;;; (Explained above in row-product, but re-explained here) While we
;;; are multiplying rows into the scratch row to generate the full
;;; stabilizer group, even though each stabilizer will eventually have
;;; a phase of 1 or -1, the phase may take the value of i or -i in
;;; intermediate steps. This function multiplies a row i into the
;;; scratch row, and returns the phase produced by the multiplication
;;; (can be 1, i, -1, or -i).
(defun multiply-into-scratch (tab i)
  "Multiplies row I of TAB into the scratch row, and return the phase produced by the multiplication (as 0, 1, 2, 3 for 1, i, -1, -i respectively)."
  (declare (notinline row-product))
  (let ((n (tableau-qubits tab)))
    (prog1 (row-product tab (* 2 n) i :allow-dirty-phase t)
      ;; This function should only be used where phase is tracked separately.
      (setf (tableau-r tab (* 2 n)) 0))))

;;; This is intended to be a helper function for
;;; COMPILE-TABLEAU-OPERATION. Look at that function to see if it
;;; helps understanding this one.
(defun clifford-stabilizer-action (c)
  "Compute a symbolic representation of the action of the Clifford element C on the stabilizer representation of a state, namely the \"tableau representation\".

Given a Clifford C, calculate two values:

1. A list of variable names representing X-Z pairs on qubits 0, 1, .... A typical value will look like

    (X0 Z0 X1 Z1 ... Xn Zn),

but the symbols may be uninterned or named differently.

2. A list of updates to the variables of (1), represented as Boolean expressions. Similarly to (2), these are expressions in 1-1 correspondence with (1) which, if evaluated, would produce the updates to said variables.

Note that no expressions calculating the phase update are created. This is because these phase updates are calculated by the function generate-clifford-image-phase instead."
  (let* ((num-qubits (clifford-num-qubits c))
         (num-variables (* 2 num-qubits))
         (variables (loop :for i :below num-variables
                          :collect (if (evenp i)
                                       (alexandria:format-symbol nil "X~D" (floor i 2))
                                       (alexandria:format-symbol nil "Z~D" (floor i 2)))))
         (new-variables (make-array num-variables :initial-element nil)))
    ;; We will be mapping over all Paulis operating on a single
    ;; qubit. We will compute the action on the Pauli and use this to
    ;; produce part of the action.
    (map-all-paulis num-qubits
                    (lambda (i p)
                      (let* ((cp (apply-clifford c p)))
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
                        (when (cl-quil::power-of-two-p i) ; X and Z's will have power of two indexes.
                          (let ((var (nth (1- (integer-length i)) variables)))
                            ;; The var that contributes to X- and Z-FACTORS
                            (loop :with bits := (pauli-index cp)
                                  :for i :below num-variables
                                  ;; :when (= 1 (ldb (byte 1 0) bits)) ; Equiv. to ODDP
                                  ;;   :do (push var (aref new-variables i))
                                  :do (push (if (= 1 (ldb (byte 1 0) bits)) var 0) (aref new-variables i)) (setf bits (ash bits -1))))))))
    ;; We didn't actually construct the disjunction yet out of the
    ;; running lists, so do that here.
    (map-into new-variables (lambda (sum) `(b+ ,@sum)) new-variables)
    ;; Return the values.
    (values
     variables
     (coerce new-variables 'list))))

;;; Many moons ago (04/2019), the phase updates produced by a clifford acting on
;;; a tableau via compile-tableau-operation (and
;;; clifford-stabilizer-action) were slightly incorrect. Before, phases
;;; were updated based on values in the un-clifforded tableau, but in
;;; reality, phase updates are dependent on the _updated_ X and Z bits
;;; of the tableau.
;;;
;;; This function calculates the correct phase update to each
;;; generator and antigenerator of a tableau acted on by a clifford.
;;; It does this (with a minor adjustment or two) by:
;;;
;;;     1) Considering the row (generator/antigenerator) as a sequence
;;;        of single-qubit X and Z paulis.
;;;
;;;     2) Mapping each of these paulis to their image under the clifford,
;;;        storing these new, more complicated paulis in contributing-ops.
;;;
;;;     3) Multiplying all these mapped paulis together to find the
;;;        image of the entire tableau row under the clifford.
;;;
;;; This tableau row image will now have an associated phase with it
;;; (which is a multiple of i). The minor adjustments refer to this
;;; detail: if the tableau row already has any Y paulis prior to the
;;; clifford action, the final phase value will have to be increased by
;;; 1 for each Y. This is because in the original row, the -i factor
;;; that comes from XZ = -iY is adjusted to 0, so we need to do the
;;; same for the resulting phase. Otherwise, a clifford that maps Y to
;;; Y, for example, would represent Y as X * Z, map them to themselves,
;;; multiply them together, and find that Y maps to -iY.
(defun generate-clifford-image-phase (tab-var c row-var qubit-vars)
  "Calculate the phase update to a specific row in the tableau TAB, when acted on by a clifford C on specific qubits."
  (let* ((cn (num-qubits c))
         (images (clifford-basis-map c)))
    (alexandria:with-gensyms (row-image y-phase-offset Xq Zq Yq)
      `(let ((,row-image (make-components ,cn))
             (,y-phase-offset 0))
         (declare (type fixnum ,y-phase-offset)
                  (type pauli-components ,row-image))
         ,@(loop :for i :below (length qubit-vars)
                 :for q :in (reverse qubit-vars)
                 :collect `(let* ((,Xq (tableau-x ,tab-var ,row-var ,q))
                                  (,Zq (tableau-z ,tab-var ,row-var ,q))
                                  (,Yq (logand ,Xq ,Zq)))
                             (declare (type bit ,Xq ,Yq ,Zq))
                             (when (= 1 ,Xq) (multiply-components-into ,row-image ,(pauli-components (aref images (* 2 i))) ,row-image))
                             (when (= 1 ,Zq) (multiply-components-into ,row-image ,(pauli-components (aref images (1+ (* 2 i)))) ,row-image))
                             (incf ,y-phase-offset ,Yq)))
         (floor (mod (the fixnum (+ ,y-phase-offset (aref ,row-image 0))) 4) 2)))))

(defun compile-tableau-operation (c)
  "Compile a Clifford element into a tableau operation. This will be a lambda form whose first argument is the tableau to operate on, and whose remaining arguments are the qubit indexes to operate on. Upon applying this function to a tableau, it will be mutated "
  (check-type c clifford)
  (let* ((num-qubits (num-qubits c))
         ;; Gensyms
         (i      (gensym "I-"))
         (tab    (gensym "TAB-"))
         (qubits (loop :for i :below num-qubits
                       :collect (alexandria:format-symbol nil "Q~D" (- num-qubits 1 i)))))
    (multiple-value-bind (variables new-variables)
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
           (let (,@(loop :for qubit :in (reverse qubits)
                         :for (x z) :on variables :by #'cddr
                         :collect `(,x (tableau-x ,tab ,i ,qubit))
                         :collect `(,z (tableau-z ,tab ,i ,qubit))))
             (declare (type bit ,@variables))
             ;; Calculate the new phase.
             (xorf (tableau-r ,tab ,i) ,(generate-clifford-image-phase tab c i qubits))
             ;; Update the tableau with the new values.
             (setf
              ,@(loop :for qubit :in (reverse qubits)
                      :for (x z) :on new-variables :by #'cddr
                      ;; Set this...
                      :collect `(tableau-x ,tab ,i ,qubit)
                      ;; ...to this...
                      :collect x
                      ;; And set this...
                      :collect `(tableau-z ,tab ,i ,qubit)
                      ;; to this...
                      :collect z))))))))

(defun tableau-function (c)
  "Given a Clifford C, return a function representing its action on a tableau."
  (compile nil (compile-tableau-operation c)))

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
       (tableau-clear-scratch tab)
       (let ((m (loop :for m :below n
                      :when (= 1 (tableau-x tab m q))
                        :do (return m)
                      :finally (return n))))
         (tableau-copy-row tab (* 2 n) (+ m n))
         (loop :for i :from (1+ m) :below n :do
           (when (= 1 (tableau-x tab i q))
             (row-product tab (* 2 n) (+ i n))))
         (values (tableau-r tab (* 2 n)) t))))))

(defun put-tableau-into-row-echelon-form (tab)
  "Perform Gaussian elimination to put the tableau in row echelon form. Specifically, the modified generators will be put in the form

    |1 * * * * * * * * * * *|
    |. 1 * * * * * * * * * *|
    |. . . 1 * * * * * * * *|
    |. . . . . 1 * * * * * *|
    |. . 0 . . . . 1 * * * *|
    |. . . . . . . . 1 * * *|
    ^-----X----^ ^----Z-----^

The upper generators comprise only X's and Y's while the lower generators comprise only Z's. This makes it easy to
 1) Determine how many nonzero basis states are present, based on the number of purely X/Y generators (which = log(# of nonzero basis states))
 2) Easily find a nonzero basis state using the purely Z generators (done in the find-nonzero-operator function below).

Returns the number of purely X/Y generators.
"
  (let* ((n (tableau-qubits tab))
         (curr-row n))
    (flet ((elim-generator (gen-type)
             (dotimes (column n)
               ;; Find a generator of gen-type in this column
               (let ((gen-row (loop :for gen-row :from curr-row :below (* 2 n)
                                    :when (= 1 (funcall gen-type tab gen-row column))
                                      :do (return gen-row)
                                    :finally (return nil))))
                 ;; Swap this generator up, and eliminate gen-type generators in this column from all other rows
                 (unless (null gen-row)
                   (tableau-swap-row tab curr-row gen-row)
                   (tableau-swap-row tab (- curr-row n) (- gen-row n))
                   (loop :for elim-row :from (1+ curr-row) :below (* 2 n)
                         ;; When there is a generator of gen-type in this column of elim-row...
                         :when (= 1 (funcall gen-type tab elim-row column))
                           ;; !!ELIMINATE!!
                           :do (row-product tab elim-row curr-row)
                               (row-product tab (- curr-row n) (- elim-row n)))
                   (incf curr-row))))))
      ;; Eliminate X's to get upper generators (X/Y only)
      (elim-generator #'tableau-x)
      ;; The number of rows we've used to eliminate is the log of how
      ;; many nonzero basis states we have. This is because the states
      ;; generated by these X/Y generators cannot destructively
      ;; interfere with each other: they each flip an unique bit.
      (prog1 (- curr-row n)
        ;; Eliminate Z's to get lower generators (Z only)
        (elim-generator #'tableau-z)))))

;;; The general procedure of extracting the wavefunction from the
;;; tableau is just generating all of the state's stabilizers (and
;;; applying them to some state). However, this state has to be chosen
;;; carefully, since it's possible that the stabilizer operator sum
;;; will cause everything to destructively interfere and return nothing
;;; (not the |0...0> state, literally nothing, as if the state is in
;;; the "nullspace" of the stabilizer sum). This function ensures we
;;; have a good state to operate on that gives us the correct
;;; stabilizer state.
(defun find-nonzero-operator (tab)
  "Given a tableau TAB, find a Pauli operator P such that P|0...0> has nonzero amplitude in the state represented by TAB. Write this operator to the scratch space of TAB. This function also puts TAB into REF (row echelon form), since P is found using the purely Z generators."
  (let ((n (tableau-qubits tab))
        (log-nonzero (put-tableau-into-row-echelon-form tab)))
    ;; Zero out scratch space
    (tableau-clear-scratch tab)
    ;; For each row, starting from the bottom of the Z generators
    (loop :for row :from (1- (* 2 n)) :downto (+ n log-nonzero)
          :do (let ((phase (tableau-r tab row))
                    (first-z n))
                ;; Find the leftmost Z in this generator
                (loop :for j :from (1- n) :downto 0
                      :when (= 1 (tableau-z tab row j))
                        :do (setf first-z j)
                            (when (= 1 (tableau-scratch tab j))
                              (xorf phase 1)))
                ;; Change the corresponding X in the P operator as necessary
                (when (= phase 1)
                  (xorf (tableau-scratch tab first-z) 1))))))

(defun read-basis-state-from-scratch-row (tab)
  "Given a tableau TAB, read off the operator on the scratch row as a basis state. Returns two values:
    1) the basis state as an integer
    2) the phase of the state as an integer."
  (let* ((n (tableau-qubits tab))
         (state 0)
         (phase 0))
    (dotimes (i n)
      (when (= 1 (tableau-x tab (* 2 n) i))
        (incf state (expt 2 i))
        (when (= 1 (tableau-z tab (* 2 n) i))
          (incf phase))))
    (values state phase)))

;;; The two nested dotimes in this function are a little mysterious,
;;; but they are just a way to cleverly loop over all possible
;;; combinations of generators.
(defun tableau-wavefunction (tab)
  "Given a tableau TAB, generate and return the wavefunction representation of the stabilizer state the tableau represents, using the normalized sum of all stabilizers.

Note: the scratch space is used as an area to write intermediate state values, and starts with the operator returned by find-nonzero-operator."
  (let* ((n (tableau-qubits tab))
         (log-nonzero (put-tableau-into-row-echelon-form tab))
         (norm (sqrt (expt 2 log-nonzero)))
         (wf (make-array (expt 2 n) :element-type '(complex double-float) :initial-element #C(0.0d0 0.0d0)))
         (running-phase 0))
    (find-nonzero-operator tab)
    (dotimes (i (expt 2 log-nonzero))
      (let ((x (logxor i (1+ i))))
        (dotimes (j log-nonzero)
          (when (logbitp j x)
            (incf running-phase (multiply-into-scratch tab (+ n j)))))
        (multiple-value-bind (state read-phase) (read-basis-state-from-scratch-row tab)
          (setf (aref wf state) (/ (expt #C(0.0d0 1.0d0) (+ read-phase running-phase)) norm)))))
    wf))

;;; The .chp file format is an input to Aaronson's chp program written
;;; in C. Below is a parser and interpreter for it.

(defun interpret-chp (code tab &key silent)
  (let ((*standard-output* (if silent (make-broadcast-stream) *standard-output*)))
    (loop :for i :from 1
          :for isn :in code
          :do
             (format t "~D: ~A" i isn)
             (alexandria:destructuring-ecase isn
               ((H q)       (tableau-apply-h tab q))
               ((S q)       (tableau-apply-phase tab q))
               ((CNOT p q)  (tableau-apply-cnot tab p q))
               ((MEASURE q) (multiple-value-bind (outcome determ?)
                                (tableau-measure tab q)
                              (format t " => ~D~:[ (random)~;~]" outcome determ?))))
             (terpri))))

(defun read-chp-file (file)
  "Parse a .chp file as defined by Aaronson."
  (let ((max-qubit 0))
    (labels ((parse-gate (gate-name)
               (cond
                 ((string= gate-name "h") 'H)
                 ((string= gate-name "c") 'CNOT)
                 ((string= gate-name "p") 'S)
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

(defun random-chp-and-quil (q n)
  "Generates a random CHP circuit on q qubits, of n gates.
   Outputs two values:
    1) The parsed CHP format of the random circuit
    2) The equivalent random circuit in quil.
   Intended for testing purposes."
  (let ((quil-str (make-string-output-stream)))
    (values
     (loop :repeat n
           :for q1 := (random q)
           :for q2 := (mod (+ q1 (1+ (random (1- q)))) q)
           :for chp-gate := (alexandria:whichever
                             `(H ,q1)
                             `(S ,q1)
                             `(CNOT ,q1 ,q2))
           :for quil-gate := (format nil "~{~A~^ ~}" chp-gate)
           :collect chp-gate
           :do (write-line quil-gate quil-str))
     (get-output-stream-string quil-str))))
