;;;; ucr-explode.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; Structure based off of expand-circuits.lisp
;;;; Algorithm based off of _Quantum Gate Decomposition Algorithms_ by Alexander Slepoy

(in-package #:cl-quil)


;;; some external-facing utility functions / classes

;; this encodes a pseudo-instruction that performs different amounts of roll
;; depending on the (fine-resolution) state of a sequence of control qubits.
;;
;; just like we currently support CONTROLLED gates, it might be useful someday
;; to provide support for general "multiplexed" gates, of which CONTROLLED gates
;; and UCRs are both special instances.
(defclass UCR-application (gate-application)
  ((roll-type :initarg :roll-type
              :accessor UCR-application-roll-type
              :type string
              :documentation "Type of roll used in this application.")
   (chirality :initarg :chirality
              :accessor UCR-application-chirality
              :type (member :up :down nil)
              :documentation "Chirality, used for efficient UCR expansion. :UP indicates a prepended CNOT on the first control, :DOWN indicates a postpended CNOT on the first control, and NIL indicates no additional CNOT."))
  (:default-initargs :operator #.(named-operator "UCR")
                     :chirality nil
                     ;; XXX: Hack!
                     :gate nil)
  (:documentation "A pseudo-instruction representing an application of a uniformly controlled roll. The argument list is (action-qubit &rest control-qubits), and the parameter list is a descending-index-ordered list of control parameters of length 2**(length control-qubits)."))

(defmethod print-instruction-generic ((thing UCR-application) (s stream))
  (format s "UCR(~a) (~{~a~^, ~}) ~{~a ~}~a"
          (UCR-application-roll-type thing)
          (mapcar (lambda (param) (print-instruction param nil))
                  (application-parameters thing))
          (mapcar (lambda (arg) (print-instruction arg nil))
                  (rest (application-arguments thing)))
          (print-instruction (first (application-arguments thing)) nil)))

(defmethod gate-matrix ((gate ucr-application) &rest parameters)
  (unless (endp parameters)
    (warn "unexpected parameters given to GATE-MATRIX for UCR-APPLICATION"))
  (let* ((temp-gate (copy-instance gate)))
    (setf (application-arguments temp-gate)
          (mapcar #'qubit
                  (alexandria:iota (length (application-arguments gate))
                                   :start (1- (length (application-arguments gate)))
                                   :step -1)))
    (make-matrix-from-quil
     (append
      (when (eql ':up (ucr-application-chirality temp-gate))
        (list
         (build-gate "CNOT" () (second (application-arguments temp-gate))
                               (first (application-arguments temp-gate)))))
      (ucr-explode-instr temp-gate)
      (when (eql ':down (ucr-application-chirality temp-gate))
        (list
         (build-gate "CNOT" () (second (application-arguments temp-gate))
                               (first (application-arguments temp-gate)))))))))


;;; now some local utility functions

(defun flip-bitstring (n bitcount)
  "Reads an unsigned binary number's bitstring backwards to form another number."
  (check-type n integer)
  (check-type bitcount integer)
  (loop
     :with ret = 0
     :for i :below bitcount
     :do (setf (ldb (byte 1 i) ret) (ldb (byte 1 (- bitcount i 1)) n))
     :finally (return ret)))

;; some auxiliary functions
;;
;; this is used to form the sequence of CNOT indices.
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

;; this is used to compute the angle deltas that, given different sign
;; assignments encoded by gray-code-toggles, reassemble to a given sequence of
;; angle values.
;;
;; TODO: the :append in the main loop is probably a source of inefficiency. a
;; better version of this function would use arrays everywhere: an array can
;; house the matrix data, an array can house the column data, the column can get
;; scanned + updated + copied into the appropriate location in the output matrix
;; all at once, without any reallocation happening.
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


;;; now, the meat of UCR expansion

;; the idea here comes from the 2Q case: the single instruction
;;
;;     UCR(r, s) t (c)
;;
;; for angle values r, s; control qubit c; and target qubit t can be rewritten
;;
;;     UCR(a, a) t (c), CNOT c t, UCR(d, d) t (c), CNOT c t
;;
;; where a = (r+s)/2 is the average and d = (r-s)/2 is the difference. the CNOTs
;; toggle the sign between d and -d, and these satisfy a+d = r, a-d = s.
;; however, these forked UCRs are independent of c, and so can be re-rewritten
;;
;;     R(a) t, CNOT c t, R(d) t, CNOT c t.
;;
;; the general pattern is almost identical to this.
;;
;; the naive CNOT complexity of this method of compilation satisfies
;;
;;     r(n) = 2 + 2*r(n-1),
;;
;; which is slightly worse than the CNOT complexity of the gray code method's 2^n.
;; however, this can be ameliorated by toggling the expansion order of the UCRs:
;; the two instruction sequences
;;
;;     UCR(a, a) t (c), CNOT c t, UCR(d, d) t (c), CNOT c t
;;     CNOT c t, UCR(d, d) t (c), CNOT c t, UCR(a, a) t (c)
;;
;; are equivalent, and by alternating which expansion is used, we can cancel the
;; outer CNOT with its mate in the other half of the expansion one level up.
;; this is tracked by the UCR-application CHIRALITY slot. this brings the CNOT
;; complexity down roughly by a factor of 2, comparable to the gray code method.
;;
;; the other exchange is that recursive expansion allows for trampolined
;; compilation of the lower-order UCRs, which has better odds of making use of
;; differing hardware object native gate sets. (NOTE: it's unclear how these
;; complexities interact with the higher CNOT complexity imposed by chip topology.)
;;
;; this is also the place where UCR-to-iSWAP compilation gets done, according to
;;
;;     Z c, Z t, RZ(pi/2) t, ISWAP c t, RY(r) c, ISWAP c t, RZ(-pi/2) t
;;          == UCRY(r,-r) (c) t
;;
;;     X t, Z c, RY(pi/2) t, ISWAP c t, RY(r) c, ISWAP c t, RY(-pi/2) t
;;          == UCRZ(r,-r) (c) t 
;;
(defun ucr-compiler (instr &key (target ':cz))
  "Compiles a UCR into UCRs of one smaller order (or, in the base case, into plain rolls)."
  ;; if this isn't a UCR, skip it.
  (unless (typep instr 'ucr-application)
    (give-up-compilation))
  (let* ((roll-type (ucr-application-roll-type instr))
         (chirality (ucr-application-chirality instr))
         (parameters (application-parameters instr))
         (pa (first parameters))
         (arguments (application-arguments instr))
         (a (first arguments))
         (b (second arguments))
         (rest (rest arguments))
         (restrest (rest rest))
         (pi/2 (constant (/ pi 2)))
         (-pi/2 (constant (/ pi -2))))
    (labels ((ucr* (chirality roll params args)
               (make-instance 'ucr-application
                              :chirality chirality
                              :roll-type roll
                              :parameters (alexandria:ensure-list params)
                              :arguments (alexandria:ensure-list args)))
             (ucr (roll params args)
               (ucr* nil roll params args))
             (chiroll (chirality params)
               (ucr* chirality roll-type params (cons a restrest)))
             (iswap ()
               (build-gate "ISWAP" () a b))
             (cnot ()
               (build-gate "CNOT" () b a))
             (build-gates (gate-data)
               (mapcar (lambda (arg-list) (apply #'build-gate arg-list)) gate-data)))
      ;; if all the UCR parameters are zero, return a NOP (or the CNOT we were meant to cancel with)
      (when (every (lambda (param) (double= 0d0 (constant-value param)))
                   parameters)
        (return-from ucr-compiler
          (if (null chirality)
              (list (make-instance 'no-operation))
              (list (cnot)))))
      ;; if all the UCR parameters are the same, return an uncontrolled roll
      (when (loop :for param :in parameters
                  :always (double= (constant-value pa)
                                   (constant-value param)))
        (let ((roll (build-gate roll-type (list pa) a)))
          (return-from ucr-compiler
            (ecase chirality
              (:up
               (list (cnot) roll))
              (:down
               (list roll (cnot)))
              ((nil)
               (list roll))))))
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
             (avgroll  (build-gate roll-type averages    a))
             (diffroll (build-gate roll-type differences a)))
        ;; we're ready to output the list of compiled instructions. we do case
        ;; analysis to decide whether we need to emit smaller UCRs or just rolls.
        (cond
          ((and (member target '(:cnot :cz))
                (= 1 (length averages)))
           (ecase chirality
             (:down
              ;; in this case, we just emit rolls, encoded with CNOTs
              (list avgroll (cnot) diffroll))
             (:up
              ;; in this case, we just emit rolls, encoded with CNOTs
              (list diffroll (cnot) avgroll))
             ((nil)
              ;; in this case, we just emit rolls, encoded with CNOTs
              (list avgroll (cnot) diffroll (cnot)))))
          ;; otherwise, we need to emit shorter UCRs encoded with CNOTs
          ((member target '(:cnot :cz))
           (ecase chirality
             (:down
              (list (chiroll ':down averages) (cnot) (chiroll ':up differences)))
             (:up
              (list (chiroll ':down differences) (cnot) (chiroll ':up averages)))
             ((nil)
              (list
               (chiroll ':down averages) (cnot) (chiroll ':up differences) (cnot)))))
          ;; these next two are also just rolls, this time with ISWAPs.
          ;; the emitted code is different for RY and RZ, so we have to do case work.
          ((and (eql target ':iswap)
                (= 1 (length averages))
                (string= "RY" roll-type))
           (build-gates `(("RY"    ,averages    ,a)
                          ("Z"     ()           ,b)
                          ("Z"     ()           ,a)
                          ("RZ"    (,pi/2)      ,a)
                          ("ISWAP" ()           ,a ,b)
                          ("RY"    ,differences ,b)
                          ("ISWAP" ()           ,a ,b)
                          ("RZ"    (,-pi/2)     ,a))))
          ((and (eql target ':iswap)
                (= 1 (length averages))
                (string= "RZ" roll-type))
           (build-gates `(("RZ"    ,averages    ,a)
                          ("X"     ()           ,a)
                          ("Z"     ()           ,b)
                          ("RY"    (,-pi/2)     ,a)
                          ("ISWAP" ()           ,a ,b)
                          ("RY"    ,differences ,b)
                          ("ISWAP" ()           ,a ,b)
                          ("RY"    (,pi/2)      ,a))))
          ;; also shorter UCRs, this time with ISWAPs.
          ((and (eql target ':iswap)
                (string= "RY" roll-type))
           (list
            (ucr "RY" averages (cons a restrest))
            (build-gate "Z" nil b)
            (build-gate "Z" nil a)
            (build-gate "RZ" (list pi/2) a)
            (iswap)
            (ucr "RY" differences rest)
            (iswap)
            (build-gate "RZ" (list -pi/2) a)))
          ((and (eql target ':iswap)
                (string= "RZ" roll-type))
           (list
            (ucr "RZ" averages (cons a restrest))
            (build-gate "X" nil a)
            (build-gate "Z" nil b)
            (build-gate "RY" (list -pi/2) a)
            (iswap)
            (ucr "RY" differences rest)
            (iswap)
            (build-gate "RY" (list pi/2) a)))
          (t
           (give-up-compilation)))))))


;;;
;;; Old routine for doing UCR expansion all-at-once.
;;;
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
