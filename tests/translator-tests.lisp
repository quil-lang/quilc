;;;; translator-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-tests)

(defun print-matrix (mat &optional (stream *standard-output*))
  (dotimes (i (magicl:matrix-rows mat))
    (dotimes (j (magicl:matrix-cols mat))
      (let ((z (magicl:ref mat i j)))
        (when (= j 0)
          (format stream "# "))
        (format stream "~6,3F+~6,3Fj" (realpart z) (imagpart z))
        (when (= j (1- (magicl:matrix-cols mat)))
          (format stream "~%")
          (format stream ", "))))))

(defun rescale-matrix-against-reference (mat ref-mat)
  (magicl:scale
   (cond
     ((> (abs (magicl:ref mat 0 0)) 1/32) (/ (magicl:ref ref-mat 0 0) (magicl:ref mat 0 0)))
     ((> (abs (magicl:ref mat 1 0)) 1/32) (/ (magicl:ref ref-mat 1 0) (magicl:ref mat 1 0)))
     ((> (abs (magicl:ref mat 2 0)) 1/32) (/ (magicl:ref ref-mat 2 0) (magicl:ref mat 2 0)))
     ((> (abs (magicl:ref mat 3 0)) 1/32) (/ (magicl:ref ref-mat 3 0) (magicl:ref mat 3 0)))
     (t (error "Matrix has a degenerate column.")))
   mat))

(deftest test-euler-translations ()
  "Ensures that the different Euler decompositions all work."
  (let ((master-matrix (quil::random-special-unitary 2)))
    (dolist (compiler '(quil::euler-zyz-compiler
                        quil::euler-zxz-compiler
                        quil::euler-yzy-compiler
                        quil::euler-yxy-compiler
                        quil::euler-xyx-compiler
                        quil::euler-xzx-compiler))
      (let* ((compiled-program (funcall compiler
                                        (build-anonymous-gate master-matrix 0)))
             (compiled-matrix (quil::make-matrix-from-quil compiled-program)))
        (is (quil::matrix-equals-dwim master-matrix compiled-matrix)
            "Euler translation test failed: ~A~%" compiler)))))

(define-condition test-case-too-complicated (error)
  ((reason :initarg :reason :reader test-case-too-complicated-reason))
  (:documentation "Signaled when a test case generator is applied to a scenario that is too complicated for it to generate.")
  (:report (lambda (c s)
             (princ (test-case-too-complicated-reason c) s))))

(defun generate-translator-test-case-for-simple-compiler (bindings)
  ;; build an adjacency graph of bound qubit variables that aren't allowed to collide
  (let ((adjacency-table (make-hash-table))
        qubit-bound
        parameter-names
        (parameter-assignments (make-hash-table)))
    (labels ((random-with-exceptions (bound exceptions)
               (assert (> bound (length exceptions)))
               (let ((val (random bound)))
                 (if (member val exceptions)
                     (random-with-exceptions bound exceptions)
                     val)))
             (add-clique (qubits)
               (cond
                 ((endp qubits)
                  t)
                 ((eql '_ (first qubits))
                  (add-clique (rest qubits)))
                 (t
                  (unless (gethash (first qubits) adjacency-table)
                    (setf (gethash (first qubits) adjacency-table) nil))
                  (dolist (distant-qubit (rest qubits))
                    (unless (eq '_ distant-qubit)
                      (push (first qubits) (gethash distant-qubit adjacency-table))
                      (push distant-qubit (gethash (first qubits) adjacency-table))))
                  (add-clique (rest qubits))))))
      (dolist (binding bindings)
        (etypecase binding
          (quil::wildcard-binding
           (error 'test-case-too-complicated
                  :reason "I don't know how to make tests for permissive compilers."))
          (quil::measure-binding
           (error 'test-case-too-complicated
                  :reason "I don't know how to make tests for MEASURE compilers."))
          (quil::gate-binding
           (when (quil::gate-binding-options binding)
             (error 'test-case-too-complicated
                    :reason "I don't know how to make test cases for guarded compilers."))
           (add-clique (quil::gate-binding-arguments binding))
           (when (listp (quil::gate-binding-parameters binding))
             (setf parameter-names
                   (union parameter-names
                          (loop :for param :in (quil::gate-binding-parameters binding)
                                :when (and (typep param 'symbol)
                                           (not (eql '_ param)))
                                  :collect param)))))))
      ;; color the graph with qubit indices. it'd be :cool: if this were randomized.
      (quil::dohash ((qubit collision-names) adjacency-table)
        (let ((collision-qubits (mapcar (lambda (name) (gethash name adjacency-table))
                                        collision-names)))
          (loop :for j :from 0
                :unless (member j collision-qubits)
                  :do (setf (gethash qubit adjacency-table) j)
                      (return))))
      ;; save the max of the largest instruction qubit number & the k in the k-coloring
      (setf qubit-bound (max (loop :for binding :in bindings
                                   :maximize (1+ (length (quil::gate-binding-arguments binding))))
                             (loop :for name :being :the :hash-keys :of adjacency-table
                                     :using (hash-value qubit-assignment)
                                   :maximize (1+ qubit-assignment))))
      (dolist (n parameter-names)
        (setf (gethash n parameter-assignments)
              ;; If a named parameter exists, let's use a memory
              ;; reference for it.
              (mref (symbol-name n) 0)))
      ;; walk the instructions, doing parameter/argument instantiation
      (values (loop :for binding :in bindings
                    :collect
                    (let* ((params (when (listp (quil::gate-binding-parameters binding))
                                     (mapcar (lambda (param)
                                               (cond
                                                 ((numberp param)
                                                  (constant param))
                                                 ((typep param 'symbol)
                                                  (gethash param parameter-assignments))
                                                 (t
                                                  (error 'test-case-too-complicated
                                                         :reason "I don't know how to generate this parameter."))))
                                             (quil::gate-binding-parameters binding))))
                           (qubits (mapcar (lambda (qubit)
                                             (cond
                                               ((numberp qubit)
                                                qubit)
                                               ((eql qubit (intern "_"))
                                                nil)
                                               ((symbolp qubit)
                                                (gethash qubit adjacency-table))))
                                           (quil::gate-binding-arguments binding)))
                           (occupied-qubits (remove-if #'symbolp qubits))
                           (qubits
                             (let ((output-qubits nil))
                               (dolist (qubit qubits (reverse output-qubits))
                                 (cond
                                   ((numberp qubit)
                                    (push qubit output-qubits))
                                   ((null qubit)
                                    (push (random-with-exceptions qubit-bound
                                                                  (append output-qubits occupied-qubits))
                                          output-qubits)))))))
                      (cond
                        ((symbolp (quil::gate-binding-operator binding))
                         (apply #'quil::anon-gate "ANONYMOUS-INPUT"
                                (quil::random-special-unitary (ash 1 (length qubits)))
                                qubits))
                        (t
                         (apply #'quil::build-gate (quil::gate-binding-operator binding) params qubits)))))
              parameter-assignments))))

(defun %patch-mref-values (instr table)
  (unless (and (typep instr 'gate-application)
               (quil::application-parameters instr))
    (return-from %patch-mref-values instr))
  (let ((instr-new (quil::copy-instance instr)))
    (setf (quil::application-parameters instr-new)
          (mapcar #'quil::copy-instance
                  (quil::application-parameters instr-new)))
    (labels ((treesplore (expression)
               (cond ((listp expression)
                      (mapcar #'treesplore expression))
                     ((quil::delayed-expression-p expression)
                      (quil::make-delayed-expression
                       nil nil (treesplore (quil::delayed-expression-expression expression))))
                     ((typep expression 'quil::memory-ref)
                      (let* ((name (quil::memory-ref-name expression)))
                        (gethash name table)))
                     (t
                      expression))))
      (map-into (quil::application-parameters instr-new)
                (lambda (param)
                  (if (quil::delayed-expression-p param)
                      (let ((res (treesplore param)))
                        (quil::evaluate-delayed-expression res))
                      param))
                (quil::application-parameters instr-new))
      instr-new)))

(defun %make-de-value-table (quil)
  (loop :with table := (make-hash-table :test 'equalp)
        :for instr :in quil
        :for params := (application-parameters instr) :do
          (loop :for param :in params
                :when (typep param 'quil::delayed-expression) :do
                  (let ((name (quil::memory-ref-name (quil::delayed-expression-expression param))))
                    (setf (gethash name table) (random (* 2 pi)))))
        :finally
           (return table)))

(deftest test-translators ()
  (labels ((do-compilation (compiler)
             (unless (quil::compiler-gateset-reducer-p compiler)
               (return-from do-compilation nil))
             (let (test-case test-case-patched input-matrix compiled-output output-matrix table)
               ;; building the test case can throw an error if the compiler itself is
               ;; too complicated. we don't intend for that to break the tests.
               (handler-case (setf test-case (generate-translator-test-case-for-simple-compiler
                                              (quil::compiler-bindings compiler)))
                 (test-case-too-complicated (c)
                   (format t "~&Compiler ~A not tested because ~A" compiler (test-case-too-complicated-reason c))
                   (return-from do-compilation nil)))
               (setf table (%make-de-value-table test-case))
               (setf test-case-patched (mapcar (a:rcurry #'%patch-mref-values table) test-case))
               (setf input-matrix (quil::make-matrix-from-quil test-case-patched))
               (setf compiled-output (mapcar (a:rcurry #'%patch-mref-values table)
                                             (apply compiler test-case)))
               (setf output-matrix (quil::make-matrix-from-quil compiled-output))
               (format t "~&    Testing simple compiler ~A" (quil::compiler-name compiler))
               (is (quil::matrix-equals-dwim input-matrix output-matrix))
               t)))
    (loop :for compiler :in (remove-if (a:rcurry #'typep 'quil::approximate-compiler)
                                       quil::**compilers-available**)
          :count t :into compiler-count
          :count (do-compilation compiler) :into hit-count
          :finally (let ((hit-rate (/ hit-count compiler-count)))
                     (format t "~&  Tested ~2,2F% of all compilers." (* 100 hit-rate))
                     (is (< 1/10 hit-rate))))))


(defun random-permutation (list)
  (unless (null list)
    (let ((index (random (length list))))
      (cons (nth index list)
            (random-permutation (remove (nth index list) list))))))

(deftest test-ucr-recognition ()
  (dolist (roll-type (list "RY" "RZ"))
    (let* ((qubit-count 4)
           (argument-list (random-permutation (mapcar #'qubit (a:iota qubit-count))))
           (angle-list (mapcar (lambda (x)
                                 (declare (ignore x))
                                 (constant (random (* 2 pi))))
                               (make-list (expt 2 (1- qubit-count)))))
           (ucr-instruction (apply #'quil::build-UCR roll-type angle-list argument-list))
           (ucr-matrix (quil::make-matrix-from-quil (list ucr-instruction)))
           (anonymous-instr (make-instance 'quil::gate-application
                                           :operator #.(named-operator "ANONYMOUS-UCR")
                                           :arguments (mapcar #'qubit (nreverse (a:iota qubit-count)))
                                           :gate ucr-matrix))
           (recognized-instruction (quil::recognize-ucr anonymous-instr))
           (recognized-matrix (quil::make-matrix-from-quil recognized-instruction)))
      (fiasco-assert-matrices-are-equal
       ucr-matrix
       recognized-matrix))))

(deftest test-global-instruction-expansion ()
  "Tests the expansion of gate applications on non-adjacent qubits by comparing the matrix of the gate before and afte expansion."
  (finish-output *debug-io*)
  (let ((chip-spec (cl-quil::build-8Q-chip)))
    (dolist (instr-type (list (list "CNOT"   nil #'cl-quil::CNOT-to-native-CNOTs)
                              (list "CZ"     nil #'cl-quil::CZ-to-native-CZs)
                              (list "ISWAP"  nil #'cl-quil::ISWAP-to-native-ISWAPs)
                              (list "SWAP"   nil #'cl-quil::SWAP-to-native-SWAPs)
                              (list "CPHASE" (list (random 1.0d0)) #'cl-quil::CPHASE-to-native-CPHASEs)
                              (list "PISWAP" (list (random 1.0d0)) #'cl-quil::PISWAP-to-native-PISWAPs)))
      (destructuring-bind (operator params expander) instr-type
        (format *debug-io* "~&    Testing global-to-local ~A expansion~%" operator)
        (let* ((instr (quil::build-gate operator params 0 3))
               (ref-mat (cl-quil::make-matrix-from-quil (list instr)))
               (mat (cl-quil::make-matrix-from-quil (funcall expander instr
                                                             :context (quil::make-compilation-context
                                                                       :chip-specification chip-spec)))))
          (is (cl-quil::matrix-equality ref-mat
                                        (cl-quil::scale-out-matrix-phases mat ref-mat))))))))

(defun %build-disconnected-chip-spec ()
  ;; chip spec with disconnected qubits 0 <-> 1 and 2 <-> 3
  (let ((chip-spec (cl-quil::make-chip-specification
                    :generic-rewriting-rules (coerce (cl-quil::global-rewriting-rules) 'vector))))
    (cl-quil::install-generic-compilers chip-spec ':cz)
    (loop :for j :below 4 :do
      (cl-quil::adjoin-hardware-object (cl-quil::build-qubit j :type '(:RZ :X/2 :MEASURE)) chip-spec))
    (cl-quil::install-link-onto-chip chip-spec 0 1)
    (cl-quil::install-link-onto-chip chip-spec 2 3)
    chip-spec))

(deftest test-find-directed-shortest-path-on-chip-spec ()
  (let (;; 1 <-> 2 <-> 3 <-> 4
        (undirected-nq (cl-quil::build-nq-linear-chip 4))
        ;; 1 <-> 2     3 <-> 4
        (pathological (%build-disconnected-chip-spec))
        (ring (cl-quil::build-8q-chip)))
    ;; good paths
    (is (equal (cl-quil::find-shortest-path-on-chip-spec undirected-nq 1 3) '(1 2 3)))
    (is (equal (cl-quil::find-shortest-path-on-chip-spec undirected-nq 1 0) '(1 0)))
    (is (equal (cl-quil::find-shortest-path-on-chip-spec undirected-nq 2 0) '(2 1 0)))
    (is (equal (cl-quil::find-shortest-path-on-chip-spec ring 1 7) '(1 0 7)))
    ;; bad paths
    (is (null (cl-quil::find-shortest-path-on-chip-spec pathological 2 1)))))

(deftest test-phase-compiles-to-rz ()
  "A PHASE gate should trivially compile to a RZ gate, preserving parameters."
  (let* ((cphase (quil::parse-quil "PHASE(0) 0"))
         (cphase-compiled (quil::compiler-hook cphase (quil::build-8q-chip)))
         (cphase-parametric (quil::parse-quil "DECLARE gamma REAL[1]
PHASE(2*gamma[0]) 0"))
         (cphase-parametric-compiled (quil::compiler-hook cphase-parametric (build-8q-chip)))
         (rz (quil::compiler-hook (quil::parse-quil "RZ(0) 0") (build-8q-chip)))
         (rz-parametric (quil::compiler-hook (quil::parse-quil "DECLARE gamma REAL[1]
RZ(2*gamma[0]) 0") (quil::build-8q-chip))))
    (fiasco-assert-matrices-are-equal
     (quil::make-matrix-from-quil (coerce (quil::parsed-program-executable-code cphase-compiled) 'list))
     (quil::make-matrix-from-quil (coerce (quil::parsed-program-executable-code rz) 'list)))
    ;; one phase should compile to one rz
    (is (= (length (quil::parsed-program-executable-code cphase-parametric-compiled))
           (length (quil::parsed-program-executable-code rz-parametric))))))

(deftest test-modifier-compilers ()
  (let ((text "DECLARE theta REAL ; DAGGER CONTROLLED FORKED RZ(1*theta, 2*theta) 1 0 2"))
    (dolist (architecture '(:cz :iswap))
      (let* ((quil::*compress-carefully* nil)
             (orig (coerce (parsed-program-executable-code (quil::parse-quil text)) 'list))
             (cpp (quil::expand-to-native-instructions (coerce (parsed-program-executable-code (quil::parse-quil text)) 'list)
                                                       (quil::build-nq-fully-connected-chip 3 :architecture architecture)))
             (table (a:plist-hash-table (list "theta" (random 2pi))
                                        :test #'equalp)))
        (is (quil::operator=
             (quil::make-matrix-from-quil (mapcar (a:rcurry #'%patch-mref-values table)
                                                  orig))
             (quil::make-matrix-from-quil (mapcar (a:rcurry #'%patch-mref-values table)
                                                  cpp))))))))
