;;;; ucr-explode.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; Structure based off of expand-circuits.lisp
;;;; Algorithm based off of _Quantum Gate Decomposition Algorithms_ by Alexander Slepoy

(in-package #:cl-quil)

;; first, a helpful utility for constructing UCR gates
(defun build-UCR (roll-name params &rest args)
  (let ((op (named-operator roll-name)))
    (dolist (x (rest args))
      (declare (ignore x))
      (setf op (forked-operator op)))
    (apply #'build-gate op params args)))

;; this routine emits pretty efficient (but perhaps not optimal) decompositions
;; of certain FORKED gates. the input ingredients are:
;;
;;   + an parametric family G(theta) satisfying G(theta) G(phi) = G(theta + phi)
;;   + a gate F with the property F G(theta) = G(-theta) F
;;
;; given these two things, the gate
;; 
;;   FORKED G(theta, phi) control target
;; 
;; can be implemented in terms of G
;; and CONTROLLED F by an averaging scheme:
;;
;;   G((theta + phi) / 2) target
;;   # depending on the value of the control qubit, this next sequence will sum
;;   # with the above application of G to give either G(theta) or G(phi)
;;   CONTROLLED F         control target
;;   G((theta - phi) / 2) target
;;   CONTROLLED F         control target
;;
;; as a special case of this, we may set G = RY or G = RZ to recover "uniformly
;; controlled rolls" (UCRs), which appear in many common decomposition schemes
;; of generic unitaries, and so are highly compilation-relevant. they are also
;; highly hardware-relevant, because we can take CONTROLLED F = CNOT to get
;; native output on common devices. the function UCR-COMPILER below implements
;; the above scheme in this specific case.
;;
;; REM: this kind of decomposition scheme is especially relevant because of
;;      the observation that for a general multiplexed operator U0 (+) U1 acting
;;      on n qubits can be written an (n-1)Q operator VL, an (n-1)-FORKED RZ,
;;      and a second (n-1)Q operator VR.
;;
;;      for a scheme leveraging this idea, see src/compilers/qs-compile.lisp .
;;
;; REM: the decomposition of FORKED into CONTROLLED gates above can also be done
;;      in a slightly different order:
;;
;;   CONTROLLED F         control target
;;   G((theta - phi) / 2) target
;;   CONTROLLED F         control target
;;   G((theta + phi) / 2) target
;;
;;      applied judiciously, we can use both of these decompositions in tandem
;;      to place two CONTROLLED F instances next to each other in a recursive
;;      decomposition. in previous iterations of quilc, this was referred to as
;;      "chirality" and was implemented using non-local state; in the present
;;      incarnation of UCR-COMPILER, we do this interleaving purely locally.
;;
;;      *without interleaving the two decompositions*, the F-count of this
;;      decomposition routine satisfies r(n) = 2 + 2*r(n-1), and with the
;;      interleaving it is reduced by roughly a factor of 2.
;;
;; REM: we also provide an variant implementation of this scheme using ISWAP,
;;      which decomposes
;;
;;   FORKED RY(-phi, phi) control target
;;
;;      as
;;
;;   ISWAP control target
;;   RX(phi) control
;;   ISWAP control target
;;   Z control
;;   Z target
;;
;;      what's in use here is the decomposition of SWAP into ISWAP + CNOT,
;;      solving for CNOT, and commuting the SWAPs (and local gates) past the
;;      inner RX to convert it to an RY and change it from the target to the
;;      control qubit.  with this in hand, the decomposition scheme is much
;;      the same.
;;

(defun uniformly-controlled-roll-p (instr)
  "Generalized boolean predicate for detecting uniformly controlled rolls. Returns the gate name if true, NIL otherwise."
  (labels ((op-walker (op)
             (adt:match operator-description op
               ((named-operator str) (if (or (string= str "RY") (string= str "RZ"))
                                         str
                                         nil))
               ((forked-operator o)  (op-walker o))
               (_                    nil))))
    (op-walker (application-operator instr))))

(defun ucr-compiler (instr &key (arch ':cz))
  "Compiles a UCR into UCRs of one smaller order (or, in the base case, into plain rolls)."
  ;; if this isn't a UCR, skip it.
  (adt:match operator-description (application-operator instr)
    ((forked-operator op)
     (let* ((roll-type (uniformly-controlled-roll-p instr))
            (parameters (application-parameters instr))
            (arguments (mapcar #'qubit-index (application-arguments instr)))
            (control (first arguments))
            (target (first (last arguments)))
            (rest (rest arguments))
            (pi/2 (constant (/ pi 2)))
            (-pi/2 (constant (/ pi -2))))
       (unless roll-type
         (give-up-compilation))
       (labels ((ucr (params args)
                  (apply #'build-gate op (alexandria:ensure-list params) (alexandria:ensure-list args)))
                (iswap ()
                  (build-gate "ISWAP" () control target))
                (cnot ()
                  (build-gate "CNOT" () control target))
                (build-gates (gate-data)
                  (mapcar (lambda (arg-list) (apply #'build-gate arg-list)) gate-data))
                (but-last (ell)
                  (reverse (rest (reverse ell)))))
         ;; if all the UCR parameters are zero, return a NOP (or the CNOT we were meant to cancel with)
         (when (every (lambda (param) (double= 0d0 (constant-value param)))
                      parameters)
           (return-from ucr-compiler
             nil))
         ;; if all the UCR parameters are the same, return an uncontrolled roll
         (when (every (lambda (param) (double= (constant-value (first parameters))
                                               (constant-value param)))
                      parameters)
           (return-from ucr-compiler
             (list (build-gate roll-type (list (constant-value (first parameters))) target))))
         (let* ((high-order-params
                  (subseq parameters
                          0 (/ (length parameters) 2)))
                (low-order-params
                  (subseq parameters
                          (/ (length parameters) 2)))
                (averages
                  (mapcar (lambda (x y) (constant
                                         (/ (+ (constant-value x)
                                               (constant-value y))
                                            2)))
                          high-order-params
                          low-order-params))
                (differences
                  (mapcar (lambda (x y) (constant
                                         (/ (- (constant-value x)
                                               (constant-value y))
                                            2)))
                          high-order-params
                          low-order-params))
                (avgroll  (build-gate roll-type averages    target))
                (diffroll (build-gate roll-type differences target)))
           ;; we're ready to output the list of compiled instructions. we do case
           ;; analysis to decide whether we need to emit smaller UCRs or just rolls.
           (cond
             ((and (member arch '(:cnot :cz))
                   (= 1 (length averages)))
              ;; in this case, we just emit rolls, encoded with CNOTs
              (list avgroll (cnot) diffroll (cnot)))
             ;; otherwise, we need to emit shorter UCRs encoded with CNOTs
             ((member arch '(:cnot :cz))
              ;; see the comments above this function for an explanation of what's
              ;; going on here and why.
              ;;
              ;; future invocations of this function will again try to decompose the parameter
              ;; list into components of averages and differences. if we want them to come out
              ;; in the other order (as indicated in the comments above), we'll need to fuss
              ;; with the emitted parameter list now. we replace `differences` by `differences-prime`
              ;; with the property that the differences of differences matches the averages
              ;; of differences-prime, and the averages of differences matches the differences
              ;; of differences-prime.
              (let* ((param-count (length differences))
                     (differences-prime
                       (append (subseq differences 0 (/ param-count 2))
                               (mapcar (lambda (x)
                                         (constant (- (constant-value x))))
                                       (subseq differences (/ param-count 2))))))
                ;; by feeding it differences-prime, the second UCR will give the alternative
                ;; decomposition indicated in the comments, and the extra BUILD-GATEs will
                ;; move the controlled gate into place.
                (list
                 (ucr averages rest)
                 (build-gate "CNOT" () (first rest) target)
                 (cnot)
                 (ucr differences-prime rest)
                 (build-gate "CNOT" () (first rest) target)
                 (cnot))))
             ;; these next two are also just rolls, this time with ISWAPs.
             ;; the emitted code is different for RY and RZ, so we have to do case work.
             ((and (eql arch ':iswap)
                   (= 1 (length averages))
                   (string= "RY" roll-type))
              (build-gates `(("RY"    ,averages    ,target)
                             ("Z"     ()           ,control)
                             ("Z"     ()           ,target)
                             ("RZ"    (,pi/2)      ,target)
                             ("ISWAP" ()           ,target ,control)
                             ("RY"    ,differences ,control)
                             ("ISWAP" ()           ,target ,control)
                             ("RZ"    (,-pi/2)     ,target))))
             ((and (eql arch ':iswap)
                   (= 1 (length averages))
                   (string= "RZ" roll-type))
              (build-gates `(("RZ"    ,averages    ,target)
                             ("X"     ()           ,target)
                             ("Z"     ()           ,control)
                             ("RY"    (,-pi/2)     ,target)
                             ("ISWAP" ()           ,target ,control)
                             ("RY"    ,differences ,control)
                             ("ISWAP" ()           ,target ,control)
                             ("RY"    (,pi/2)      ,target))))
             ;; also shorter UCRs, this time with ISWAPs.
             ((and (eql arch ':iswap)
                   (string= "RY" roll-type))
              (list
               (ucr averages rest)      ; skip first control
               (build-gate "Z" nil control)
               (build-gate "Z" nil target)
               (build-gate "RZ" (list pi/2) target)
               (iswap)
               (ucr differences (append (but-last rest) (list control)))
               (iswap)
               (build-gate "RZ" (list -pi/2) target)))
             ((and (eql arch ':iswap)
                   (string= "RZ" roll-type))
              (list
               (ucr averages rest)
               (build-gate "X" nil target)
               (build-gate "Z" nil control)
               (build-gate "RY" (list -pi/2) target)
               (iswap)
               (build-gate "RX" (list pi/2) control)
               (ucr differences (append (but-last rest) (list control)))
               (build-gate "RX" (list -pi/2) control)
               (iswap)
               (build-gate "RY" (list pi/2) target)))
             (t
              (give-up-compilation)))))))
    (_
     (give-up-compilation))))


;;;
;;; Old routine for doing UCR expansion all-at-once.
;;;

(defun flip-bitstring (n bitcount)
  "Reads an unsigned binary number's bitstring backwards to form another number."
  (check-type n integer)
  (check-type bitcount integer)
  (loop
     :with ret = 0
     :for i :below bitcount
     :do (setf (ldb (byte 1 i) ret) (ldb (byte 1 (- bitcount i 1)) n))
     :finally (return ret)))

(defun gray-code-toggles (n)
  "Generates the moves that will do a Hamiltonian traversal of the Hamming graph of the numbers 0, ..., 2^(n-1)."
  (check-type n integer)
  (cond
    ((= n 1)
     (list 1))
    (t
     (let ((recurse (gray-code-toggles (1- n))))
       (append recurse
               (list n)
               recurse)))))

(defun gray-code-difference-matrix (n)
  "This generates the matrix used by the angle difference backsolver."
  (check-type n integer)
  (let* ((mat-size (expt 2 n))
         (operating-vector (make-array mat-size :initial-element (/ mat-size))))
    (make-row-major-matrix
     mat-size mat-size
     (loop
       :for index :in (nconc (gray-code-toggles n) (list n))
       :append (progn
                 (loop :for j :below mat-size
                       ;; apply (* -1) to those indices j which bitmask with index to 1
                       :when (logbitp (1- index) j)
                         :do (setf (aref operating-vector j)
                                   (- (aref operating-vector j))))
                 (coerce operating-vector 'list))))))

(defun ucr-explode-instr (instr)
  "Turn any UCR pseudo-instruction into an equivalent list of Quil instructions over the gate set RY, RZ, CNOT.  Leave all others alone."
  (unless (typep instr 'UCR-application)
    (return-from ucr-explode-instr instr))
  (cond
    ;; we deal with the trivial case separately.
    ((= 1 (length (application-arguments instr)))
     (list
      (build-gate (ucr-application-roll-type instr)
                  (application-parameters instr)
                  (first (application-arguments instr)))))
    ;; otherwise, we are dealing with a genuine controlled roll.
    (t
     (let*
         ((control-count (1- (length (application-arguments instr))))
          ;; compute the vector of deltas = M * alphas
          (matrix-deltas
            (magicl:multiply-complex-matrices
             ;; where M is the "Gray code difference matrix"
             (gray-code-difference-matrix control-count)
             ;; and we load alpha into a magicl matrix
             (magicl:make-complex-matrix
              (expt 2 control-count) 1
              (mapcar 'constant-value (application-parameters instr)))))
          ;; finally, we read deltas out of their magicl matrix.
          (deltas
            (loop :for i :below (magicl:matrix-rows matrix-deltas)
                  :collect (realpart (magicl:ref matrix-deltas i 0)))))
       ;; for each delta...
       (nreverse
        (mapcan (lambda (control-qubit delta)
                  (list
                   ;; flip the relevant bit
                   (build-gate "CNOT"
                               ()
                               (nth (- (length (application-arguments instr)) control-qubit)
                                    (application-arguments instr))
                               (first (application-arguments instr)))
                   ;; install the delta
                   (build-gate (ucr-application-roll-type instr)
                               (list delta)
                               (first (application-arguments instr)))))
                (append (gray-code-toggles control-count) (list control-count))
                deltas))))))
