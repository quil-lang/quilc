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
        (format stream "~6,3f+~6,3fj" (realpart z) (imagpart z))
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
    (dolist (compiler (list #'quil::euler-zyz-compiler
                            #'quil::euler-zxz-compiler
                            #'quil::euler-yzy-compiler
                            #'quil::euler-yxy-compiler
                            #'quil::euler-xyx-compiler
                            #'quil::euler-xzx-compiler))
      (let* ((compiled-program (funcall compiler
                                        (build-anonymous-gate master-matrix 0)))
             (compiled-matrix (magicl:diag 2 2 (list 1d0 1d0))))
        (loop :for instr :in compiled-program :do
          (setf compiled-matrix (quil::apply-gate compiled-matrix instr)))
        (is (loop :for i :from 0 :to 1
                  :always (loop :for j :from 0 :to 1
                                :always (< (abs (- (magicl:ref compiled-matrix i j)
                                                   (magicl:ref master-matrix i j)))
                                           0.01)))
            "Euler translation test failed: ~a~%" compiler)))))

(defun generate-test-case (bindings)
  ;; build an adjacency graph of bound qubit variables that aren't allowed to collide
  (let ((adjacency-table (make-hash-table))
        qubit-bound)
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
        (unless (typep binding 'cons)
          (error "I don't know how to make tests for permissive compilers."))
        (destructuring-bind (variable-name binding &rest options) binding
          (declare (ignore variable-name))
          (when options
            (error "I don't know how to make test cases for guarded compilers."))
          (add-clique (cddr binding))))
      ;; color the graph with qubit indices. it'd be :cool: if this were randomized.
      (quil::dohash ((qubit collision-names) adjacency-table)
        (let ((collision-qubits (mapcar (lambda (name) (gethash name adjacency-table))
                                        collision-names)))
          (loop :for j :from 0
                :unless (member j collision-qubits)
                  :do (setf (gethash qubit adjacency-table) j)
                      (return))))
      ;; save the max of the largest instruction qubit number & the k in the k-coloring
      (setf qubit-bound (max (loop :for (variable-name binding) :in bindings
                                   :maximize (1+ (length (cddr binding))))
                             (loop :for name :being :the :hash-keys :of adjacency-table
                                     :using (hash-value qubit-assignment)
                                   :maximize (1+ qubit-assignment))))
      ;; walk the instructions, doing parameter/argument instantiation
      (loop :for binding :in bindings
            :collect
            (destructuring-bind (variable-name (name params &rest qubits)) binding
              (declare (ignore variable-name))
              (let* ((params (mapcar (lambda (param)
                                       (cond
                                         ((numberp param)
                                          param)
                                         ((typep param 'symbol)
                                          (random (* 2 pi)))
                                         (t
                                          (error "I don't know how to generate this parameter."))))
                                     params))
                     (qubits (mapcar (lambda (qubit)
                                       (cond
                                         ((numberp qubit)
                                          qubit)
                                         ((eql '_ qubit)
                                          '_)
                                         ((symbolp qubit)
                                          (gethash qubit adjacency-table))))
                                     qubits))
                     (occupied-qubits (remove-if #'symbolp qubits))
                     (qubits
                       (let ((output-qubits nil))
                         (dolist (qubit qubits (reverse output-qubits))
                           (cond
                             ((numberp qubit)
                              (push qubit output-qubits))
                             ((eql '_ qubit)
                              (push (random-with-exceptions qubit-bound
                                                            (append output-qubits occupied-qubits))
                                    output-qubits)))))))
                (apply #'quil::build-gate name params qubits)))))))

(deftest test-translators ()
  (labels
      ((test-a-compiler (compiler)
         (let (test-case input-matrix compiled-output output-matrix)
           ;; building the test case can throw an error if the compiler itself is
           ;; too complicated. we don't intend for that to break the tests.
           (handler-case (setf test-case (generate-test-case (quil::compiler-bindings compiler)))
             (error ()
               (return-from test-a-compiler nil)))
           (setf input-matrix (quil::make-matrix-from-quil test-case))
           (handler-case (setf compiled-output (apply compiler test-case))
             (quil::compiler-does-not-apply ()
               (return-from test-a-compiler nil)))
           (setf output-matrix (quil::make-matrix-from-quil compiled-output))
           (format t "~&    Testing simple compiler ~a" (quil::compiler-name compiler))
           (is (quil::matrix-equals-dwim input-matrix output-matrix)))))
    (dolist (compiler quil::**compilers-available**)
      (test-a-compiler compiler))))


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
        (format *debug-io* "~&    Testing global-to-local ~a expansion~%" operator)
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
    (loop :repeat 4 :do
      (cl-quil::adjoin-hardware-object (cl-quil::build-qubit) chip-spec))
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


