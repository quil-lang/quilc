;;;; rewriting-rules.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)


;;; functions for dealing with mixed constant vs delayed-expression types

(defun param-binary-op (op arg1 arg2)
  "Binary operator that safely applies to possibly mixed arguments of NUMBER / DELAYED-EXPRESSION objects."
  (flet ((decompose (arg)
           "Returns a values tuple: PARAMS LAMBDA-PARAMS EXPRESSION DELAYEDP"
           (optima:match arg
             ((delayed-expression (params arg-params)
                                  (lambda-params arg-lambda-params)
                                  (expression arg-expression))
              (values
               arg-params
               arg-lambda-params
               arg-expression
               t))
             ((constant (value val))
              (values nil nil val nil))
             (_
              (values nil nil arg nil)))))
    (multiple-value-bind (params-1 lambda-params-1 expression-1 delayedp-1) (decompose arg1)
      (multiple-value-bind (params-2 lambda-params-2 expression-2 delayedp-2) (decompose arg2)
        (let ((params-zipped (union (mapcar #'list params-1 lambda-params-1)
                                    (mapcar #'list params-2 lambda-params-2)
                                    :test #'equal)))
          (loop :with zipped-tail := params-zipped
                :for head :in params-zipped
                :do (setf zipped-tail (rest zipped-tail))
                :when (find (second head) zipped-tail :key #'second :test #'equal)
                  :do (error "Duplicate definition of lambda-param: ~a" (second head)))
          (if (or delayedp-1 delayedp-2)
              (make-delayed-expression (mapcar #'first params-zipped)
                                       (mapcar #'second params-zipped)
                                       `(,op ,expression-1 ,expression-2))
              (funcall op expression-1 expression-2)))))))

(defun param-+ (arg1 arg2)
  (param-binary-op '+ arg1 arg2))

(defun param-* (arg1 arg2)
  (param-binary-op '* arg1 arg2))


;;; dumb little macros for cond guards that we were repeating a million times

(defmacro else-give-up-compilation (test &body body)
  "If TEST passes, proceed to execute BODY (and return the value of the final expression). If TEST fails, call GIVE-UP-COMPILATION."
  `(cond
     (,test
      ,@body)
     (t
      (give-up-compilation))))

(defmacro unpack-wf (instr context (psi qubits) &body body)
  "Extracts from CONTEXT the wavefunction component related to the instruction INSTR.  Upon success, bind PSI to the wavefunction contents, bind QUBITS to the qubit ordering convention of PSI, execute BODY, and return the result of the final expression.  Upon failure, call GIVE-UP-COMPILATION."
  `(destructuring-bind (,psi ,qubits)
       (aqvm-extract-state (compressor-context-aqvm ,context)
                           (mapcar #'qubit-index (application-arguments ,instr)))
     (else-give-up-compilation
         (not (eql ':not-simulated ,psi))
       ,@body)))


;;; rewriting rules in general

(defun global-rewriting-rules ()
  "Rewriting rules that may be useful in manipulating instruction sequences that do not have an underlying notion of 'native hardware'.  In particular, this includes canonicalizing the ordering of commuting instructions."
  (list
   (make-rewriting-rule "CZs commute"
       (_
        (("CZ" () p q) x)
        (("CZ" () r s) y))
     
     (else-give-up-compilation
         (< (+ (ash 1 p) (ash 1 q))
            (+ (ash 1 r) (ash 1 s)))
       (list y x)))
   
   (make-rewriting-rule "CPHASEs commute"
       (_
        (("CPHASE" (_) p q) x)
        (("CPHASE" (_) r s) y))
     
     (else-give-up-compilation
         (< (+ (ash 1 p) (ash 1 q))
            (+ (ash 1 r) (ash 1 s)))
       (list y x)))
   
   (make-rewriting-rule "Eigenvectors take no action"
       (context
        (_ instr))
     
     (unpack-wf instr context (psi qubit-indices)
       (let ((updated-wf (nondestructively-apply-instr-to-wf instr psi qubit-indices)))
         (else-give-up-compilation
             (collinearp psi updated-wf)
           (list)))))))


;; rewriting rules specialized to qubit types
(defmacro rewriting-rules-for-roll-template (roll-type)
  "The rewriting rules for a roll do not depend on the particular roll type and are instead valid for any single-parameter exponentiated family (which, incidentally, are all automatically 2pi-periodic). To cut down on repeated code, we provide this macro that slots the roll type in to the standard set of rules."
  `(list
    (make-rewriting-rule ,(format nil "~a(a) ~:*~a(b) -> ~:*~a(a + b)" roll-type)
        (_
         ((,roll-type (theta) q) x)
         ((,roll-type (phi)   q) y))
      
      (list (build-gate ,roll-type (list (param-+ theta phi)) q)))
    
    (make-rewriting-rule ,(format nil "~a(2 pi k) -> " roll-type)
        (_
         ((,roll-type (theta) _) x))
      
      (else-give-up-compilation
          (and (typep theta 'double-float)
               (double= (/ theta (* 2 pi))
                        (round (/ theta (* 2 pi)))))
        (list)))
    
    (make-rewriting-rule ,(format nil "~a(x + 2 pi k) -> ~:*~a(x)" roll-type)
        (_
         ((,roll-type (theta) q) x))
      
      (else-give-up-compilation
          (typep theta 'double-float)
        (let ((reduced-theta (- (mod (+ pi theta) (* 2 pi)) pi)))
          (else-give-up-compilation
              (and (< pi (abs theta))
                   (< (abs reduced-theta) (abs theta)))
            (list (build-gate ,roll-type (list reduced-theta) q))))))
    
    (make-rewriting-rule ,(format nil "~a(-pi) -> ~:*~a(pi)" roll-type)
        (_
         ((,roll-type (#.(- pi)) q) x))
      
      (list (build-gate ,roll-type (list pi) q)))))

(defun rewriting-rules-for-roll-RX ()
  "Generates a list of rewriting rules for simplifying expressions involving RX."
  (rewriting-rules-for-roll-template "RX"))

(defun rewriting-rules-for-roll-RZ ()
  "Generates a list of rewriting rules for simplifying expressions involving RZ."
  (rewriting-rules-for-roll-template "RZ"))

(defmacro rewriting-rules-preferring-rollA-to-rollB-template (rollA rollB)
  "The rewriting rules for a pair of distinct rolls do not depend on the particular roll types and are instead valid for any single-parameter exponentiated families for rolls about orthogonal axes. To cut down on repeated code, we provide this macro that slots the roll types in to the standard set of rules. Here we require a notion of \"preference\" of ROLLA over ROLLB: some of the rules trade rolls of one type for rolls of another, and so we prefer to rewrite the \"less natural\" ROLLB into the \"more natural\" ROLLA."
  `(list
    ;; rollB(X) rollA(pi) --> rollA(pi) rollB(-x)
    (make-rewriting-rule ,(format nil "~a(x) ~a(pi) -> ~:*~a(pi) ~:*~:*~a(-x)" rollB rollA)
        (_
         ((,rollB (theta) q) x)
         ((,rollA (#.pi)  q) y))
      
      (list (build-gate ,rollA (list pi)                   q)
            (build-gate ,rollB (list (param-* -1d0 theta)) q)))
    
    ;; rollB(pi) rollA(x) --> rollA(-x) rollB(pi)
    (make-rewriting-rule ,(format nil "~a(pi) ~a(x) -> ~:*~a(-x) ~:*~:*~a(pi)" rollB rollA)
        (_
         ((,rollB (#.pi)  q) x)
         ((,rollA (theta) q) y))
      
      (list (build-gate ,rollA (list (param-* -1d0 theta)) q)
            (build-gate ,rollB (list pi)                   q)))
    
    ;; -rollB/2 rollA/2 rollB/2 --> rollA/2 rollB/2 -rollA/2 (with positionally reversed signs)
    (make-rewriting-rule ,(format nil "-~a/2 ~a/2 ~:*~:*~a/2 -> ~a/2 ~:*~:*~a/2 -~a/2, etc" rollB rollA)
        (_
         ((,rollB (theta1) q) x)
         ((,rollA (theta2) q) y)
         ((,rollB (theta3) q) z))
      
      (else-give-up-compilation
          (and (typep theta1 'double-float)
               (double= (/ pi 2) (abs theta1))
               (typep theta2 'double-float)
               (double= (/ pi 2) (abs theta2))
               (typep theta3 'double-float)
               (double= (/ pi 2) (abs theta3)))
        (list (build-gate ,rollA (list theta3) q)
              (build-gate ,rollB (list theta2) q)
              (build-gate ,rollA (list theta1) q))))))

(defun rewriting-rules-preferring-RX-to-RZ ()
  "Generates a list of rewriting rules for simplifying expressions involving RX and RZ, with RX preferred."
  (rewriting-rules-preferring-rollA-to-rollB-template "RX" "RZ"))

(defun rewriting-rules-preferring-RZ-to-RX ()
  "Generates a list of rewriting rules for simplifying expressions involving RZ and RX, with RZ preferred."
  (rewriting-rules-preferring-rollA-to-rollB-template "RZ" "RX"))


;; rewriting rules specialized to link type

(defun rewriting-rules-for-link-of-ISWAP-type ()
  "Generates a list of rewriting rules for simplifying expressions involving ISWAP and standard single-qubit operations."
  (list
   (make-rewriting-rule "ISWAP ISWAP -> Z (x) Z"
       (_
        (("ISWAP" () p q) x)
        (("ISWAP" () r s) y))
     
     (else-give-up-compilation
         (subsetp (list p q) (list r s))
       (list (build-gate "RZ" (list pi) p)
             (build-gate "RZ" (list pi) q))))
   
   (make-rewriting-rule "(RZ (x) I) ISWAP -> ISWAP (I (x) RZ)"
       (_
        (("RZ"    (theta) q)     x)
        (("ISWAP" ()      q1 q2) y))
     
     (else-give-up-compilation
         (or (= q q1) (= q q2))
       (list (build-gate "ISWAP" ()           q1 q2)
             (build-gate "RZ"    (list theta) (if (= q q1) q2 q1)))))))


(defun rewriting-rules-for-link-of-PISWAP-type ()
  "Generates a list of rewriting rules for simplifying expressions involving PISWAP and standard single-qubit operations."
  (list
   (make-rewriting-rule "PISWAP(phi) PISWAP(theta) -> PISWAP(phi + theta)"
       (_
        (("PISWAP" (theta) p q) x)
        (("PISWAP" (phi)   r s) y))
     
     (else-give-up-compilation
         (subsetp (list p q) (list r s))
       (list (build-gate "PISWAP" (list (param-+ theta phi)) p q))))
   
   (make-rewriting-rule "PISWAP(phi) -> PISWAP(phi mod 4 pi)"
       (_
        (("PISWAP" (theta) p q) x))
     
     (else-give-up-compilation
         (typep theta 'double-float)
       (let ((reduced-theta (mod theta (* 4 pi))))
         (else-give-up-compilation
             (not (double= theta reduced-theta))
           (list (build-gate "PISWAP" (list reduced-theta) p q))))))
   
   (make-rewriting-rule "PISWAP(2pi) -> Z (x) Z"
       (_
        (("PISWAP" (#.(* 2 pi)) p q) x))
     
     (list (build-gate "RZ" (list pi) p)
           (build-gate "RZ" (list pi) q)))
   
   (make-rewriting-rule "PISWAP(4pi) -> NOP"
       (_
        (("PISWAP" (#.(* 4 pi)) _ _) x))
     
     (list))
   
   (make-rewriting-rule "(RZ (x) I) PISWAP(pi) -> PISWAP(pi) (I (x) RZ)"
       (_
        (("RZ"     (theta) q)     x)
        (("PISWAP" (phi)   q1 q2) y))
     
     (else-give-up-compilation
         (and (typep phi 'double-float)
              (double= 0d0 (mod (abs phi) pi))
              (or (= q q1) (= q q2)))
       (list (build-gate "PISWAP" (list phi)   q1 q2)
             (build-gate "RZ"     (list theta) (if (= q q1) q2 q1)))))))


(defun rewriting-rules-preferring-ISWAP-to-PISWAP ()
  "Generates a list of rewriting rules that simplify expressions involving ISWAP and PISWAP, with a preference for rewriting into ISWAP over PISWAP."
  (list
   (make-rewriting-rule "PISWAP(odd * pi) -> ISWAP PISWAP(even * pi)"
       (_
        (("PISWAP" (theta) p q) x))
     
     (else-give-up-compilation
         (and (typep theta 'double-float)
              (double= (* pi (floor theta pi)) theta))
       (list (build-gate "ISWAP"  ()  p)
             (build-gate "PISWAP" (list (- theta pi)) q))))
   
   (make-rewriting-rule "ISWAP PISWAP(theta) -> PISWAP(pi + theta)"
       (_
        (("ISWAP"  ()    p q) x)
        (("PISWAP" (phi) r s) y))
     
     (else-give-up-compilation
         (subsetp (list p q) (list r s))
       (list (build-gate "PISWAP" (list (param-+ pi phi)) p q))))
   
   (make-rewriting-rule "PISWAP(phi) ISWAP -> PISWAP(phi + pi)"
       (_
        (("PISWAP" (theta) p q) x)
        (("ISWAP"  ()      r s) y))
     
     (else-give-up-compilation
         (subsetp (list p q) (list r s))
       (list (build-gate "PISWAP" (list (param-+ theta pi)) p q))))))


(defun rewriting-rules-preferring-PISWAP-to-ISWAP ()
  "Generates a list of rewriting rules that simplify expressions involving ISWAP and PISWAP, with a preference for rewriting into PISWAP over ISWAP."
  (list
   (make-rewriting-rule "ISWAP -> PISWAP"
       (_
        (("ISWAP" () p q) x))
     
     (list (build-gate "PISWAP" (list pi) p q)))))


(defun rewriting-rules-for-link-of-CNOT-type ()
  (list
   (make-rewriting-rule "CNOT CNOT -> NOP"
       (_
        (("CNOT" () p q) x)
        (("CNOT" () r s) y))
     
     (else-give-up-compilation
         (subsetp (list p q) (list r s))
       (list)))
   
   (make-rewriting-rule "(RZ (x) I) CNOT -> CNOT (RZ (x) I)"
       (_
        (("RZ"   (theta) p)   x)
        (("CNOT" ()      p q) y))
     
     (list (build-gate "CNOT" ()        p q)
           (build-gate "RZ"   `(,theta) p)))
   
   (make-rewriting-rule "(I (x) RX) CNOT -> CNOT (I (x) RX)"
       (_
        (("RX"   (theta) q)   x)
        (("CNOT" ()      p q) y))
     
     (list (build-gate "CNOT" ()        p q)
           (build-gate "RX"   `(,theta) q)))))


(defun rewriting-rules-for-link-of-CZ-type ()
  "Generates a list of rewriting rules for simplifying expressions involving CZ and standard single-qubit operations."
  (list
   (make-rewriting-rule "CZ CZ -> NOP"
       (_
        (("CZ" () p q) x)
        (("CZ" () r s) y))
     
     (else-give-up-compilation
         (subsetp (list p q) (list r s))
       (list)))
   
   (make-rewriting-rule "RZ CZ -> CZ RZ"
       (_
        (("RZ" (_) q)    x)
        (("CZ" () q1 q2) y))
     
     (else-give-up-compilation
         (or (= q q1) (= q q2))
       (list y x)))
   
   (make-rewriting-rule "(X (x) I) CZ -> CZ (X (x) Z)"
       (_
        (("RX" (#.pi) q)     x)
        (("CZ" ()     q1 q2) y))
     
     (else-give-up-compilation
         (or (= q q1) (= q q2))
       (list (build-gate "CZ" ()        q1 q2)
             (build-gate "RZ" (list pi) (if (= q q1) q2 q1))
             (build-gate "RX" (list pi) (if (= q q1) q1 q2)))))
   (make-rewriting-rule "Control bit off: no action"
       (context
        (("CZ" () control _) x))
     
     (unpack-wf x context (psi qubit-indices)
       (let* ((control-bit (- (length qubit-indices)
                              1
                              (position control qubit-indices)))
              (control-bit-weight
                (loop :for i :below (array-total-size psi)
                      :when (logbitp control-bit i)
                        :sum (expt (abs (aref psi i)) 2))))
         (else-give-up-compilation
             (double= 0d0 (sqrt control-bit-weight))
           (list)))))
   
   (make-rewriting-rule "Control bit on: apply Z gate"
       (context
        (("CZ" () control target) x))
     
     (unpack-wf x context (psi qubit-indices)
       (let* ((control-bit (- (length qubit-indices)
                              1
                              (position control qubit-indices)))
              (control-bit-weight
                (loop :for i :below (array-total-size psi)
                      :when (logbitp control-bit i)
                        :sum (expt (abs (aref psi i)) 2))))
         (else-give-up-compilation
             (double= 1d0 (sqrt control-bit-weight))
           (list (build-gate "RZ" (list pi) target))))))
   
   (make-rewriting-rule "Partially supported wf on CZ"
       (context
        (("CZ" () control target) x))
     
     (unpack-wf x context (psi qubit-indices)
       (let* ((zero-position (- (length qubit-indices)
                                1
                                (position target qubit-indices)))
              (one-position (- (length qubit-indices)
                               1
                               (position control qubit-indices)))
              (wf-components (make-array 4 :initial-element 0d0)))
         (flet ((position-type (i)
                  (+ (* 2 (ldb (byte 1 one-position) i))
                     (ldb (byte 1 zero-position) i))))
           (dotimes (i (array-total-size psi))
             (incf (aref wf-components (position-type i))
                   (expt (abs (aref psi i)) 2)))
           (else-give-up-compilation
               (not (double= 0d0 (aref wf-components 3)))
             (let ((wf-signature
                     (+ (if (double= 0d0 (aref wf-components 0))
                            0 1)
                        (if (double= 0d0 (aref wf-components 1))
                            0 2)
                        (if (double= 0d0 (aref wf-components 2))
                            0 4)
                        (if (double= 0d0 (aref wf-components 3))
                            0 8))))
               (case wf-signature
                 (#b1001
                  (list (build-gate "RZ" (list pi) target)))
                 (#b1010
                  (list (build-gate "RZ" (list pi) control)))
                 (#b1011
                  (list (build-gate "RZ" (list pi) control)))
                 (#b1101
                  (list (build-gate "RZ" (list pi) control)))
                 (#b1110
                  (list (build-gate "RZ" (list pi) control)
                        (build-gate "RZ" (list pi) target)))
                 ;; cases we don't care about include:
                 ;; the third bit is zero, so we live in an eigenspace: 0000 0001 0010 0011 0100 0101 0110 0111
                 ;; only the third bit is nonzero, so we live in an eigenspace: 1000
                 ;; the control bit is on: 1100
                 ;; all the bits are nonzero, so we can't reduce: 1111
                 (otherwise
                  (give-up-compilation)))))))))))

(defun rewriting-rules-for-link-of-CPHASE-type ()
  "Generates a list of rewriting rules for simplifying expressions involving CPHASE and standard single-qubit operations."
  (list*
   (make-rewriting-rule "CPHASE(theta) CPHASE(phi) -> CPHASE(theta + phi)"
       (_
        (("CPHASE" (theta) p q) x)
        (("CPHASE" (phi)   r s) y))
     
     (else-give-up-compilation
         (subsetp (list p q) (list r s))
       (list (build-gate "CPHASE" (list (param-+ theta phi)) p q))))
   
   (make-rewriting-rule "CPHASE(2 pi k) -> NOP"
       (_
        (("CPHASE" (theta) _ _) x))
     
     (else-give-up-compilation
         (and (typep theta 'double-float)
              (double= 0d0 (mod theta (* 2 pi))))
       (list)))
   
   (make-rewriting-rule "CPHASE(x) -> CPHASE(x mod 2 pi)"
       (_
        (("CPHASE" (theta) p q) x))
     
     (else-give-up-compilation
         (and (typep theta 'double-float)
              (not (double= theta (mod theta (* 2 pi)))))
       (list (build-gate "CPHASE" (list (mod theta (* 2 pi))) p q))))
   
   (make-rewriting-rule "RZ CPHASE -> CPHASE RZ"
       (_
        (("RZ"     (_) _)   x)
        (("CPHASE" (_) _ _) y))
     
     (list y x))
   (make-rewriting-rule "Control bit off: no action"
       (context
        (("CPHASE" (_) control _) x))
     
     (unpack-wf x context (psi qubit-indices)
       (let* ((control-bit (- (length qubit-indices)
                              1
                              (position control qubit-indices)))
              (control-bit-weight
                (loop :for i :below (array-total-size psi)
                      :when (logbitp control-bit i)
                        :sum (expt (abs (aref psi i)) 2))))
         (else-give-up-compilation
             (double= 0d0 (sqrt control-bit-weight))
           (list)))))
   
   (make-rewriting-rule "Control bit on: apply RZ gate"
       (context
        (("CPHASE" (theta) control target) x))
     
     (unpack-wf x context (psi qubit-indices)
       (let* ((control-bit (- (length qubit-indices)
                              1
                              (position control qubit-indices)))
              (control-bit-weight
                (loop :for i :below (array-total-size psi)
                      :when (logbitp control-bit i)
                        :sum (expt (abs (aref psi i)) 2))))
         (else-give-up-compilation
             (double= 1d0 (sqrt control-bit-weight))
           (list (build-gate "RZ" (list theta) target))))))
   (rewriting-rules-for-link-of-CZ-type)))

(defun rewriting-rules-preferring-CPHASE-to-CZ ()
  "Generates a list of rewriting rules that simplify expressions involving CZ and CPHASE, with a preference for rewriting into CPHASE over CZ."
  (list
   (make-rewriting-rule "CZ -> CPHASE"
       (_
        (("CZ" () p q) x))
     
     (list (build-gate "CPHASE" (list pi) p q)))))

(defun rewriting-rules-preferring-CZ-to-CPHASE ()
  "Generates a list of rewriting rules that simplify expressions involving CZ and CPHASE, with a preference for rewriting into CZ over CPHASE."
  (list
   (make-rewriting-rule "CPHASE CZ -> CPHASE"
       (_
        (("CPHASE" (phi) p q) x)
        (("CZ"     ()    r s) y))
     
     (else-give-up-compilation
         (subsetp (list p q) (list r s))
       (list (build-gate "CPHASE" (list (param-+ phi pi)) p q))))
   
   (make-rewriting-rule
       "CZ CPHASE -> CPHASE"
       (_
        (("CZ"     ()    p q) x)
        (("CPHASE" (phi) r s) y))
     
     (else-give-up-compilation
         (subsetp (list p q) (list r s))
       (list (build-gate "CPHASE" (list (param-+ phi pi)) p q))))
   
   (make-rewriting-rule "CPHASE(pi) -> CZ"
       (_
        (("CPHASE" (theta) p q) x))
     
     (else-give-up-compilation
         (and (typep theta 'double-float)
              (double= 0d0 (mod (+ pi theta) (* 2 pi))))
       (list (build-gate "CZ" () p q))))))
