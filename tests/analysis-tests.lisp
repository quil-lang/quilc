;;;; analysis-tests.lisp
;;;;
;;;; Author: Robert Smith
;;;;         Joe Lin

(in-package #:cl-quil-tests)

(defun same-list-p (a b)
  (and (= (length a)
          (length b))
       (every #'=
              (sort (copy-seq a) #'<)
              (sort (copy-seq b) #'<))))

(a:define-constant +identity-defgate+ "
DEFGATE I:
    1, 0, 0, 0
    0, 1, 0, 0
    0, 0, 1, 0
    0, 0, 0, 1"
  :test #'string=)

(deftest test-compress-qubits ()
  "Test that qubits get compressed correctly."
  (let ((p1 (with-output-to-quil
              (write-line "H 0")
              (write-line "RX(1.0) 1")
              (write-line "MEASURE 1")
              (write-line "RESET 1")
              (write-line "CNOT 6 0")))
        (p2 (with-output-to-quil
              (write-line "RESET")
              (write-line "NOP"))))
    (is (= 7 (cl-quil::qubits-needed p1)))
    (is (= 0 (cl-quil::qubits-needed p2)))

    (is (same-list-p
         '(0 1 6)
         (cl-quil::qubits-used p1)))
    (is (null (cl-quil::qubits-used p2)))

    (cl-quil::transform 'cl-quil::compress-qubits p1)
    (cl-quil::transform 'cl-quil::compress-qubits p2)

    (is (= 3 (cl-quil::qubits-needed p1)))
    (is (= 0 (cl-quil::qubits-needed p2)))

    (is (same-list-p
         '(0 1 2)
         (cl-quil::qubits-used p1)))
    (is (null (cl-quil::qubits-used p2)))))

(deftest test-repeat-labels ()
  "Test that repeat labels are detected."
  (let ((pp (with-output-to-quil
              "LABEL @a"
              "LABEL @a")))
    (signals simple-error
      (cl-quil::transform 'cl-quil::patch-labels pp))))

(defun identity-test-program (quil-instr)
  (with-output-to-string (s)
    (write-string +identity-defgate+ s)
    (terpri s)
    (write-string quil-instr s)
    (terpri s)))

(deftest test-repeat-qubits ()
  "Test that repeated qubits on a gate are detected."
  (signals simple-error
    (cl-quil:parse-quil
     (identity-test-program "I 1 1"))))

(deftest test-too-few-qubits ()
  "Test that a gate applied to too few qubits is detected."
  (signals simple-error
    (cl-quil:parse-quil
     (identity-test-program "I 0"))))

(deftest test-too-many-qubits ()
  "Test that a gate applied to too many qubits is detected."
  (signals simple-error
    (cl-quil:parse-quil "I 1 2 3")))

(deftest test-standard-gate-resolution ()
  "Test that all standard gates resolve."
  (let ((quil (with-output-to-quil
                "I 0"
                "X 0"
                "Y 0"
                "Z 0"
                "H 0"
                "RX(0.0) 0"
                "RY(0.0) 0"
                "RZ(0.0) 0"
                "CNOT 0 1"
                "CCNOT 0 1 2"
                "S 0"
                "T 0"
                "PHASE(0.0) 0"
                "CPHASE00(0.0) 0 1"
                "CPHASE01(0.0) 0 1"
                "CPHASE10(0.0) 0 1"
                "CPHASE(0.0) 0 1"
                "CZ 0 1"
                "SWAP 0 1"
                "CSWAP 0 1 2"
                "ISWAP 0 1"
                "PSWAP(0.0) 0 1")))
    (is (every (lambda (isn)
                 (typep isn 'cl-quil:gate-application))
               (cl-quil:parsed-program-executable-code quil)))))

(deftest test-qubit-relabeler ()
  "Test that qubit relabeling seems to be sane."
  (let ((r1 (cl-quil/frontend::qubit-relabeler #(0 1 2)))
        (r2 (cl-quil/frontend::qubit-relabeler #(2 1 0)))
        (r3 (cl-quil/frontend::qubit-relabeler #(5)))
        (r4 (cl-quil/frontend::qubit-relabeler #())))
    (flet ((test-success (relabeler qubit-input qubit-output)
             (let ((q (cl-quil:qubit qubit-input)))
               (is (eq t (funcall relabeler q)))
               (is (= qubit-output (cl-quil:qubit-index q)))))
           (test-choke (relabeler bad-input)
             (signals simple-error (funcall relabeler (cl-quil:qubit bad-input))))
           (test-dont-choke (relabeler bad-input)
             (let ((q (cl-quil:qubit bad-input)))
               (is (eq nil (funcall relabeler q :dont-choke t)))
               (is (= bad-input (cl-quil:qubit-index q))))))
      ;; Identity map
      (test-success r1 0 0)
      (test-success r1 1 1)
      (test-success r1 2 2)
      (test-choke r1 3)
      (test-dont-choke r1 3)
      ;; Reverse map
      (test-success r2 0 2)
      (test-success r2 1 1)
      (test-success r2 2 0)
      (test-choke r2 3)
      (test-dont-choke r2 3)
      ;; Partial map
      (test-success r3 5 0)
      (test-choke r3 4)
      (test-dont-choke r3 4)
      ;; Empty map
      (test-choke r4 0)
      (test-dont-choke r4 0))))

(deftest test-kraus-stuff-rewritten-properly ()
  "Test that COMPRESS-QUBITS acts correctly on Kraus/POVM PRAGMAs."
  (let ((p (cl-quil:parse-quil "
DECLARE ro BIT[6]
PRAGMA ADD-KRAUS X 0 \"(0 0 0 0)\"
PRAGMA ADD-KRAUS X 5 \"(5 0 0 0)\"
PRAGMA READOUT-POVM 0 \"(0 0 0 0)\"
PRAGMA READOUT-POVM 5 \"(5 0 0 0)\"
X 5
MEASURE 5 ro[5]
")))
    (setf p (cl-quil::compress-qubits p))
    (let ((code (cl-quil:parsed-program-executable-code p)))
      (is (typep (aref code 0) 'cl-quil:no-operation))
      (is (typep (aref code 1) 'cl-quil::pragma-add-kraus))
      (is (typep (aref code 2) 'cl-quil:no-operation))
      (is (typep (aref code 3) 'cl-quil::pragma-readout-povm))
      (is (equal '(0) (cl-quil:pragma-qubit-arguments (aref code 1))))
      (is (zerop (cl-quil:pragma-qubit-index (aref code 3)))))))

(deftest test-parameter-arithmetic-rewriting ()
  "Test rewriting arithmetic for gates with and without parameters."
  (let ((in-p (let ((cl-quil:*allow-unresolved-applications* t))
                (cl-quil:parse-quil "
DECLARE a REAL
DECLARE b REAL[2]
A(1, 1 + 1, a, 1 + a)
B(a) 0
C(2) 0 1
D(2 * a) 0 1 2
E(a + 3 * b[1]) 0 1 2 3
"))))
    (multiple-value-bind (p mem-descriptors recalc-table)
        (rewrite-arithmetic in-p)
      (let ((__p (find-if (lambda (name) (eql 0 (search "__P" name)))
                          (cl-quil:parsed-program-memory-definitions p)
                          :key #'cl-quil::memory-descriptor-name)))
        ;; Do we have the memory descriptor?
        (is (not (null __p)))
        ;; Is it the right length?
        (is (= 3 (cl-quil::memory-descriptor-length __p)))
        ;; Is the recalc table of equal size?
        (is (= 3 (hash-table-count recalc-table)))
        ;; Are the members of the table correct?
        (let ((__p-name (memory-descriptor-name __p)))
          (dotimes (i 3)
            (is (not (null (gethash (mref __p-name i) recalc-table)))))
          ;; Are the actual expressions correct?
          (labels ((cleanse-mrefs (expr)
                     ;; delete the descriptors from any mrefs present in
                     ;; expr
                     (cond
                       ((typep expr 'cl-quil::memory-ref)
                        (setf (cl-quil::memory-ref-descriptor expr) nil)
                        expr)
                       ((atom expr)
                        expr)
                       (t (cons (cleanse-mrefs (car expr))
                                (cleanse-mrefs (cdr expr))))))
                   (get-mref (i)
                     (cleanse-mrefs
                      (cl-quil::delayed-expression-expression
                       (gethash (mref __p-name i) recalc-table)))))
            (let ((A (get-mref 0))
                  (D (get-mref 1))
                  (E (get-mref 2)))
              ;; A should be 1 + a[0]
              (is (equalp A `(+ 1.0d0 ,(mref "a" 0))))
              ;; D should be 2 * a[0]
              (is (equalp D `(* 2.0d0 ,(mref "a" 0))))
              ;; E should be a[0] + 3 * b[1]
              (is (equalp E `(+ ,(mref "a" 0) (* 3.0d0 ,(mref "b" 1))))))))
        ;; Now we go through the program to make sure that is correct.
        (let ((old-code (cl-quil:parsed-program-executable-code in-p))
              (new-code (cl-quil:parsed-program-executable-code p)))
          ;; Is it the same length as the old one?
          (is (= (length old-code) (length new-code)))
          ;; Are the untouched instructions' parameters the same?
          (is (equalp (application-parameters (aref old-code 1))
                      (application-parameters (aref new-code 1))))
          (is (equalp (application-parameters (aref old-code 2))
                      (application-parameters (aref new-code 2))))
          ;; Do the new instructions have rewritten parameters?
          (flet ((checkem (program-index parameter-index mref-index)
                   (let ((rewritten-param
                           (nth parameter-index
                                (application-parameters (aref new-code program-index)))))
                     (is (typep rewritten-param 'cl-quil::delayed-expression))
                     (let ((new-mref (cl-quil::delayed-expression-expression rewritten-param)))
                       (is (typep new-mref 'cl-quil::memory-ref))
                       (is (zerop (search "__P" (cl-quil::memory-ref-name new-mref))))
                       (is (= mref-index (cl-quil::memory-ref-position new-mref)))))))
            (checkem 0 3 0)
            (checkem 3 0 1)
            (checkem 4 0 2))
          ;; Are our returned memory descriptors the same as the old ones?
          (let ((old-defs (cl-quil:parsed-program-memory-definitions in-p)))
            (is (and (subsetp old-defs mem-descriptors)
                     (subsetp mem-descriptors old-defs)))))))))

(deftest test-plain-arithmetic-rewriting ()
  "Test rewriting arithmetic for a program without any parameters to rewrite."
  (let ((in-p (let ((cl-quil:*allow-unresolved-applications* t))
                (cl-quil:parse-quil "
DECLARE a REAL
DECLARE b REAL[2]
A(1, 1 + 1, a)
B(b) 0 1
"))))
    (multiple-value-bind (p mem-descriptors recalc-table)
        (rewrite-arithmetic in-p)
      ;; Is the recalc table empty?
      (is (= 0 (hash-table-count recalc-table)))
      (let ((old-code (cl-quil:parsed-program-executable-code in-p))
            (new-code (cl-quil:parsed-program-executable-code p)))
        ;; Is the old program the same length as the new one?
        (is (= (length old-code) (length new-code))))
      (let ((old-defs (cl-quil:parsed-program-memory-definitions in-p)))
        ;; Are our returned memory descriptors the same as the old ones?
        (is (and (subsetp old-defs mem-descriptors)
                 (subsetp mem-descriptors old-defs)))
        ;; Are there no new memory descriptors (no __P)?
        (is (= (length old-defs) (length mem-descriptors)))))))

;;; Fusion tests

(deftest test-grid-node ()
  "Tests on the grid node data structure and associated functions."
  (let ((q0 (cl-quil/frontend::make-grid-node 'q0 0))
        (q1 (cl-quil/frontend::make-grid-node 'q1 1))
        (q2 (cl-quil/frontend::make-grid-node 'q2 2))
        (q01 (cl-quil/frontend::make-grid-node 'q01 0 1))
        (q12 (cl-quil/frontend::make-grid-node 'q12 1 2))
        (q02 (cl-quil/frontend::make-grid-node 'q02 0 2)))
    ;; All nodes start as root nodes.
    (is (every #'cl-quil/frontend::root-node-p (list q0 q1 q2 q01 q12 q02)))
    ;; Set succeeding, check preceding.
    (setf (cl-quil/frontend::succeeding-node-on-qubit q0 0) q01)
    (is (eq q01 (cl-quil/frontend::succeeding-node-on-qubit q0 0)))
    ;; Set preceding, check succeeding
    (setf (cl-quil/frontend::preceding-node-on-qubit q12 2) q2)
    (is (eq q2 (cl-quil/frontend::preceding-node-on-qubit q12 2)))
    ;; Check that we can't set a preceding or succeeding node on
    ;; invalid qubits.
    (signals error (setf (cl-quil/frontend::succeeding-node-on-qubit q2 0) q02))
    (signals error (setf (cl-quil/frontend::preceding-node-on-qubit q02 1) q1))
    ;; Check trailer dealios.
    (is (cl-quil/frontend::trailer-node-on-qubit-p q01 0))
    (is (not (cl-quil/frontend::trailer-node-on-qubit-p q0 0)))))

(defclass dummy-node ()
  ((value :initarg :value
          :accessor dummy-node-value)))

(defun dummy-node (x)
  (make-instance 'dummy-node :value (a:ensure-list x)))

(defmethod cl-quil::fuse-objects ((a dummy-node) (b dummy-node))
  (make-instance 'dummy-node
                 :value (append
                         (a:ensure-list (dummy-node-value a))
                         (a:ensure-list (dummy-node-value b)))))

(deftest test-fuse-dummy-objects ()
  "Test that FUSE-OBJECTS behaves properly in MERGE-GRID-NODES."
  (is (equal '(a b)
             (dummy-node-value
              (cl-quil/frontend::grid-node-tag
               (cl-quil/frontend::merge-grid-nodes
                (cl-quil/frontend::make-grid-node (dummy-node 'a) 1)
                (cl-quil/frontend::make-grid-node (dummy-node 'b) 1)))))))

(defun build-grid (&rest proto-nodes)
  (loop :with pg := (make-instance 'cl-quil/frontend::program-grid)
        :for (x . qs) :in proto-nodes
        :for gn := (apply #'cl-quil/frontend::make-grid-node
                          (dummy-node x)
                          qs)
        :do (cl-quil/frontend::append-node pg gn)
        :finally (return pg)))

(deftest test-trivial-fusion ()
  "Test that a bunch of trivially fuseable gates can be fused."
  (flet ((test-it (&rest proto-nodes)
           (let ((output-pg (cl-quil/frontend::fuse-trivially (apply #'build-grid proto-nodes))))
             (is (= 1 (length (cl-quil/frontend::roots output-pg))))
             (let ((root (first (cl-quil/frontend::roots output-pg))))
               (is (cl-quil/frontend::root-node-p root))
               (is (every (a:curry #'eq root) (cl-quil/frontend::trailers output-pg)))
               (is (every #'null (cl-quil/frontend::grid-node-back root)))
               (is (every #'null (cl-quil/frontend::grid-node-forward root)))
               (is (equalp (mapcar #'first proto-nodes)
                           (dummy-node-value (cl-quil/frontend::grid-node-tag root))))))))
    (test-it '(a 0)
             '(b 0 1)
             '(c 0 1 2)
             '(d 0 1 2 3))
    (test-it '(a 0 1 2 3)
             '(b 0 1 2)
             '(c 0 1)
             '(d 0))
    (test-it '(a 0 1 2 3)
             '(b 0 1 2 3)
             '(c 0 1 2 3)
             '(d 0 1 2 3))
    (test-it '(a 0)
             '(b 0 1)
             '(c 0 1 2)
             '(d 0 1 2 3)
             '(e 0 1 2)
             '(f 0 1)
             '(g 0))))

;;; Simplify arithmetic tests

(deftest test-simplify-arithmetic-linear ()
  "Test that a linear expression can simplified"
  (let ((in-p (cl-quil:parse-quil "
DECLARE theta REAL[1]
RX(2.0+3.0*theta[0]-3.0*theta[0]/4.0-2.0) 0
"))
        (out-p (cl-quil::parse-quil "
DECLARE theta REAL[1]
RX(2.25*theta[0]) 0
")))
    (setf in-p (cl-quil::simplify-arithmetic in-p))
    (is (equalp (cl-quil::application-parameters (cl-quil::nth-instr 0 in-p))
                (cl-quil::application-parameters (cl-quil::nth-instr 0 out-p))))))

(deftest test-simplify-arithmetic-non-linear ()
  "Test that a non-linear expression is left alone"
  (let ((in-p (cl-quil:parse-quil "
DECLARE theta REAL[1]
RX(2.0+3.0*cos(theta[0])-3.0*theta[0]-2.0) 0
"))
        (out-p (cl-quil:parse-quil "
DECLARE theta REAL[1]
RX(2.0+3.0*cos(theta[0])-3.0*theta[0]-2.0) 0
")))
    (setf in-p (cl-quil::simplify-arithmetic in-p))
    (is (equalp (cl-quil::application-parameters (cl-quil::nth-instr 0 in-p))
                (cl-quil::application-parameters (cl-quil::nth-instr 0 out-p))))))

(deftest test-simplify-arithmetic-negative-references ()
  "Test that simplification works on negative-prefixed memory references e.g. RX(-theta[0] + 2.0*theta[0] + 2.0) 0 -> RX(2.0 + 1.0*theta[0]) 0"
  (let ((in-p (cl-quil:parse-quil "
DECLARE theta REAL[1]
RX(-theta[0]+2.0*theta[0]+2.0) 0
"))
        (out-p (cl-quil:parse-quil "
DECLARE theta REAL[1]
RX(2.0+theta[0]) 0
")))
    (setf in-p (cl-quil::simplify-arithmetic in-p))
    (is (equalp (cl-quil::application-parameters (cl-quil::nth-instr 0 in-p))
                (cl-quil::application-parameters (cl-quil::nth-instr 0 out-p))))))
