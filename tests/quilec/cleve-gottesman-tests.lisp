;;;; tests/quilec/cleve-gottesman-tests.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:cl-quil/quilec-tests)

(defun pauli-to-program (pauli)
  "Return parsed program that applies PAULI to the current wavefunction."
  (cl-quil:with-output-to-quil
    (loop :for d :from 0
          :with components := (subseq (cl-quil/clifford::pauli-components pauli) 1)
          :for l :across components :do
            (format t "~[I~;X~;Z~;RY(pi)~] ~D~%" l d)
          :finally (terpri))))

(defun pauli-to-matrix (pauli)
  "Return matrix corresponding to PAULI (i.e., a Kronecker product of I, X, Z, and Y gates)."
  (cl-quil:parsed-program-to-logical-matrix (pauli-to-program pauli)))

(defun dist (u v)
  "Return $\ell_\infty$ distance between vectors U and V."
  (assert (= (magicl:size u) (magicl:size v)))
  (loop :for uk :across (magicl::storage u)
        :for vk :across (magicl::storage v)
        :maximizing (abs (- uk vk))))

(defun dot-product (u v)
  (assert (= (magicl:size u) (magicl:size v)))
  (loop :for uk :across (magicl::storage u)
        :for vk :across (magicl::storage v)
        :sum (* uk (conjugate vk))))

(defun rayleigh-quotient (matrix vector)
  (assert (= (first (magicl:shape matrix))
             (second (magicl:shape matrix))
             (magicl:size vector)))
  (dot-product vector (magicl:@ matrix vector)))

(defun get-amplitudes (qvm)
  (declare (type qvm:pure-state-qvm qvm))
  (let ((amplitudes (qvm::amplitudes (qvm::state qvm))))
    (cl-quil::from-list (coerce amplitudes 'list) (list (length amplitudes) 1))))

(defun encode-linear-algebra (stabilizer-group bits)
  "Use STABILIZER-GROUP to encode BITS. Return a vector of amplitudes and a QVM with the corresponding wavefunction."
  (let* ((n (qec::number-of-physical-qubits stabilizer-group))
         (2^n (expt 2 n)))

    (flet ((make-zero-ket ()
             (let ((vector (cl-quil::zeros (list 2^n 1))))
               (setf (magicl:tref vector 0 0) #c(1.0d0 0.0d0))
               vector))

           (wrap-into-qvm (vector)
             (let ((qvm (qvm:make-qvm (+ n (length (qec::generators stabilizer-group))))))
               (replace (qvm::amplitudes (qvm::state qvm)) (magicl::storage vector))
               qvm)))

      (let ((ket (make-zero-ket))
            (primary-generators (qec::primary-generators stabilizer-group))
            (seed-generators (qec::seed-generators stabilizer-group)))

        (when (plusp bits)
          (loop :for k :below (length seed-generators)
                :for seed := (elt seed-generators k)
                :for matrix := (pauli-to-matrix seed)
                :when (logbitp k bits) :do
                  (setf ket (magicl:@ matrix ket))))

        (loop :with b := (length primary-generators)
              :with result := (cl-quil::zeros (list 2^n 1))
              :for z :below (expt 2 b) :do
                (loop :with term := (magicl::copy-matrix/complex-double-float ket)
                      :for x :below b
                      :for generator := (elt primary-generators x)
                      :when (logbitp x z) :do
                        (let ((matrix (pauli-to-matrix generator)))
                          (setf term (magicl:@ matrix term)))
                      :finally (setf result (magicl:.+ result term)))
              :finally (let ((qvm (wrap-into-qvm (magicl:scale result (expt 2 (- (/ b 2)))))))
                         (return (values (get-amplitudes qvm) qvm))))))))

(defun load-program-and-run (qvm program)
  (qvm:load-program qvm program :supersede-memory-subsystem t)
  (qvm:run qvm))

(defun encode-circuit (stabilizer-group bits)
  "Encode BITS by running the encoder circuit of STABILIZER-GROUP in a QVM. Return the resulting QVM object."
  (let ((qvm (qvm:make-qvm (+ (qec::number-of-physical-qubits stabilizer-group)
                              (length (qec::generators stabilizer-group))))))
    (load-program-and-run qvm (qec::encode stabilizer-group bits))
    qvm))

(defun encode-circuit-vector (stabilizer-group bits)
  (let ((qvm (encode-circuit stabilizer-group bits)))
    (get-amplitudes qvm)))

(defun test-encoding (stabilizer-group)
  "Ensure the encoding circuit for STABILIZER-GROUP produces the correct wavefunction."
  (loop :for c :below (expt 2 (length (qec::seed-generators stabilizer-group)))
        ;; The Linear Algebra encoder is regarded as the gold standard here.
        :for v1 := (encode-linear-algebra stabilizer-group c)
        :for v2 := (encode-circuit-vector stabilizer-group c)
        :do (let ((d (dist v1 v2)))
              (is (<= d single-float-epsilon)))))

(defun test-orthogonality (stabilizer-group)
  "Ensure that all the codewords are orthogonal."
  (loop :with seed-generators := (qec::seed-generators stabilizer-group)
        :for c1 :below (expt 2 (length seed-generators)) :do
          (loop :for c2 :from c1 :below (expt 2 (length seed-generators)) :do
            (let ((value (dot-product (encode-linear-algebra stabilizer-group c1)
                                      (encode-linear-algebra stabilizer-group c2))))
              (is (if (= c1 c2)
                      (and (cl-quil::double~ (realpart value) 1.0d0)
                           (cl-quil::double~ (imagpart value) 0.0d0))
                      (cl-quil::double~ (abs value) 0.0d0)))))))

(defun test-idempotence (stabilizer-group)
  "Ensure that all the codewords are idempotent."
  (loop :for g :across (concatenate 'qec::generators
                                    (qec::primary-generators stabilizer-group)
                                    (qec::secondary-generators stabilizer-group))
        :for matrix := (pauli-to-matrix g) :do
          (is (magicl:identity-matrix-p (magicl:@ matrix matrix)
                                        (* 1.0d1 double-float-epsilon)))))

(defun test-eigenspace (stabilizer-group)
  "Verify that every codeword is in the +1 eigenspace of the generators using the encoder circuit to create the relevant codewords."
  (let* ((primary-generators (qec::primary-generators stabilizer-group))
         (secondary-generators (qec::secondary-generators stabilizer-group))
         (seed-generators (qec::seed-generators stabilizer-group))
         (primary-and-secondary-generators (concatenate 'qec::generators primary-generators secondary-generators))
         (matrices (loop :for g :across primary-and-secondary-generators
                         :for m := (pauli-to-matrix g)
                         :collect m)))
    (loop :with 2^n := (expt 2 (qec::number-of-physical-qubits stabilizer-group))
          :for c :below (expt 2 (length seed-generators))
          :for vector := (encode-linear-algebra stabilizer-group c) :do
            (loop :for x :from 0
                  :for matrix :in matrices
                  :for rq := (rayleigh-quotient matrix (magicl:slice vector '(0 0) (list 2^n 1)))
                  :do (is (and (cl-quil::double~ 1.0d0 (realpart rq))
                               (cl-quil::double~ 0.0d0 (imagpart rq))))))))

(defun test-eigenspace-linear-algebra (stabilizer-group)
  "Verify that every codeword is in the +1 eigenspace of the generators using linear algebra to create the relevant codewords."
  (let ((primary-generators (qec::primary-generators stabilizer-group))
        (secondary-generators (qec::secondary-generators stabilizer-group)))

    (labels ((make-zero-ket (n &optional (value #c(1.0d0 0.0d0)))
               (let ((vector (cl-quil::zeros (list (expt 2 n)))))
                 (setf (magicl:tref vector 0) value)
                 vector))

             (make-state ()
               (loop :with n := (qec::number-of-physical-qubits stabilizer-group)
                     :with b := (length primary-generators)
                     :with psi := (cl-quil::zeros (list (expt 2 n)))
                     :for a :below (expt 2 b) :do
                       (loop :with vector := (make-zero-ket n (/ (sqrt (expt 2 b))))
                             :for i :below b
                             :when (logbitp i a) :do
                               ;; (format t "~4,B~4TApplying ~A~%" a (elt primary-generators i))
                               (let ((matrix (pauli-to-matrix (elt primary-generators i))))
                                 (setf vector (magicl:@ matrix vector)))
                             :finally (setf psi (magicl:.+ psi vector)))
                     :finally (return psi))))

      (loop :for generator :across (concatenate 'qec::generators primary-generators secondary-generators)
            :for matrix := (pauli-to-matrix generator)
            :for vector := (make-state) :do
              (let ((rq (rayleigh-quotient matrix vector)))
                (is (and (cl-quil::double~ 1.0d0 (realpart rq))
                         (cl-quil::double~ 0.0d0 (imagpart rq)))))))))

(defun test-commutation (stabilizer-group)
  "Ensure all generators commute."
  (let* ((primary-and-secondary-generators (concatenate 'qec::generators
                                                        (qec::primary-generators stabilizer-group)
                                                        (qec::secondary-generators stabilizer-group)))
         (n (qec::number-of-physical-qubits stabilizer-group))
         (d (length primary-and-secondary-generators))
         (new-stabilizer-group (make-instance 'qec::stabilizer-group
                                  :n n
                                  :k (- n d)
                                  :generators primary-and-secondary-generators)))
    (is (qec::commutes-p new-stabilizer-group))))

(defun make-error-program (gate qubit)
  (cl-quil:parse-quil (format nil "~A ~D~%" gate qubit)))

(defun test-syndromes (stabilizer-group)
  "Verify that the errors lie in the orthogonal complement of the code space and that every error anticommutes with at least one generator."
  (loop :with primary-generators := (qec::primary-generators stabilizer-group)
        :with secondary-generators := (qec::secondary-generators stabilizer-group)
        :with generators := (concatenate 'list primary-generators secondary-generators)
        :with n := (qec::number-of-physical-qubits stabilizer-group)
        :with k := (length (qec::seed-generators stabilizer-group))
        :for i :below (expt 2 k) :do
          (loop ;; :initially (format t "Data: ~3,B~%" i)
                :for gate :in '("X" "RY(pi)" "Z") :do
                  (loop :with syndromes := nil
                        :for j :below n :do
                          (loop :with syndrome := 0
                                :for l :from 0
                                :for generator :in generators
                                :for program := (pauli-to-program generator)
                                :do (let ((qvm (encode-circuit stabilizer-group i)))
                                      (load-program-and-run qvm (qec::make-syndrome-measurement-circuit stabilizer-group))
                                      (is (zerop (qvm:memory-ref qvm "ancillae" l)))

                                      (load-program-and-run qvm (make-error-program gate j))
                                      (load-program-and-run qvm (qec::make-syndrome-measurement-circuit stabilizer-group))
                                      (setf (ldb (byte 1 l) syndrome) (qvm:memory-ref qvm "ancillae" l)))
                                :finally (pushnew syndrome syndromes))
                        :finally (is (every #'plusp syndromes))
                                 ;; (format t "Syndromes for ~2A: ~{~9,'0B~^ ~}~%" gate syndromes)
                        ))))

(defun test-syndromes-linear-algebra (stabilizer-group)
  "Verify that the errors lie in the orthogonal complement of the code space and that every error anticommutes with at least one generator."
  (loop :with primary-generators := (qec::primary-generators stabilizer-group)
        :with secondary-generators := (qec::secondary-generators stabilizer-group)
        :with n := (qec::number-of-physical-qubits stabilizer-group)
        :with k := (length (qec::seed-generators stabilizer-group))
        :for i :below (expt 2 k)
        :for u := (encode-linear-algebra stabilizer-group i) :do
          (loop :for gate :in '("X" "RY(pi)" "Z") :do
            (loop :for j :below n :do
              (loop :with should-break-p := nil
                    :for generator :in (concatenate 'list primary-generators secondary-generators)
                    :for matrix := (cl-quil:parsed-program-to-logical-matrix (pauli-to-program generator))
                    :unless should-break-p :do
                      (let ((qvm (nth-value 1 (encode-linear-algebra stabilizer-group i))))
                        (load-program-and-run qvm (make-error-program gate j))
                        (let* ((new-vector (get-amplitudes qvm))
                               (rq (rayleigh-quotient matrix new-vector)))
                          (when (minusp (realpart rq))
                            ;; (format t "~&✓~4T~A ~D applied to ~B (~A)~%" gate j i generator)
                            (setf should-break-p t))
                          (is (<= (abs (dot-product u new-vector)) double-float-epsilon))))
                    :finally (unless should-break-p
                               ;; (format t "~&✘~4T~A ~D applied to ~B (FAIL)~%" gate j i)
                               (signal 'quantum-error-not-detected)))))))

(defun test-code (input-stabilizer-group)
  (let ((stabilizer-group (cleve-gottesman input-stabilizer-group)))
    (test-encoding stabilizer-group)
    (test-orthogonality stabilizer-group)
    (test-idempotence stabilizer-group)
    (test-eigenspace stabilizer-group)
    (test-commutation stabilizer-group)
    (test-syndromes stabilizer-group)
    ;; These two tests are useful to test major changes to the
    ;; circuit-generation algorithm but not under normal circumstances.
    ;; ;; (test-eigenspace-linear-algebra stabilizer-group)
    ;; ;; (test-syndromes-linear-algebra stabilizer-group)
    ))

(deftest test-seven-qubit-code ()
  (test-code (make-stabilizer-group '((x x x x i i i)
                                      (x x i i x x i)
                                      (x i x i x i x)
                                      (z z z z i i i)
                                      (z z i i z z i)
                                      (z i z i z i z)))))

(deftest test-eight-qubit-code ()
  (test-code (make-stabilizer-group '((x x x x x x x x)
                                      (z z z z z z z z)
                                      (x i x i z y z y)
                                      (x i y z x i y z)
                                      (x z i y i y x z)))))

(deftest test-redundant-eight-qubit-code ()
  (let ((redundant-generators '((x x x x x x x x)
                                (y y y y y y y y)
                                (z z z z z z z z)
                                (x i x i z y z y)
                                (x i y z x i y z)
                                (x i x i z y z y)
                                (x z i y i y x z))))
    (signals warning (cleve-gottesman (make-group redundant-generators)))
    (handler-bind ((warning #'muffle-warning))
      (test-code (make-stabilizer-group redundant-generators)))))

;; ;;; Shor's [[9, 1, 3]] code is disabled only because it takes too long.
;; (deftest test-shor-9-1-3-code ()
;;   (test-code (make-stabilizer-group '((z z i i i i i i i)
;;                                       (i z z i i i i i i)
;;                                       (i i i z z i i i i)
;;                                       (i i i i z z i i i)
;;                                       (i i i i i i z z i)
;;                                       (i i i i i i i z z)
;;                                       (x x x x x x i i i)
;;                                       (i i i x x x x x x)))))
