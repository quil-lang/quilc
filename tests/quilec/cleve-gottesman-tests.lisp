;;;; tests/cleve-gottesman-tests.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:cl-quil.quilec-tests)

(defun pauli-to-program (pauli)
  "Return parsed program that applies PAULI to the current wavefunction."
  (quil:parse-quil
   (with-output-to-string (stream)
     (loop :for d :from 0
           :with components := (subseq (cl-quil.clifford::pauli-components pauli) 1)
           :for l :across components :do
             (format stream "~[I~;X~;Z~;YY~] ~D~%" l d)
           :finally (terpri stream)))))

(defun pauli-to-matrix (pauli)
  "Return matrix corresponding to PAULI (i.e., a Kronecker product of I, X, Z, and Y gates)."
  (quil:parsed-program-to-logical-matrix (pauli-to-program pauli)))

(defun dist (v1 v2)
  "Return $\ell_\infty$ distance between vectors V1 and V2."
  (loop :for v1k :across (magicl::matrix-data v1)
        :for v2k :across (magicl::matrix-data v2)
        :maximizing (abs (- v1k v2k))))

(defun dot-product (u v)
  (loop :for uk :across (magicl::matrix-data u)
        :for vk :across (magicl::matrix-data v)
        :sum (* uk (conjugate vk))))

 (defun rayleigh-quotient (matrix vector)
   (dot-product vector (quil::m* matrix vector)))

(defun get-amplitudes (qvm)
  (declare (type qvm:pure-state-qvm qvm))
  (let ((amplitudes (qvm::amplitudes (qvm::state qvm))))
    (magicl:make-complex-vector (coerce amplitudes 'list))))

(defun encode-linear-algebra (code c)
  "Use CODE to encode a bitstring C. Return a vector of amplitudes and a QVM with the corresponding wavefunction."
  (let* ((n (qec::number-of-physical-qubits code))
         (2**n (expt 2 n)))

    (flet ((make-zero-ket ()
             (let ((vector (magicl:make-zero-matrix 2**n 1)))
               (setf (magicl:ref vector 0 0) #c(1.0d0 0.0d0))
               vector))

           (wrap-into-qvm (vector)
             (let ((qvm (qvm:make-qvm (truncate (log (magicl:matrix-rows vector) 2)))))
               (setf (qvm::amplitudes (qvm::state qvm))
                     (magicl::matrix-data vector))
               qvm)))

      (let ((ket (make-zero-ket))
            (primary-generators (qec::primary-generators code))
            (seed-generators (qec::seed-generators code)))

        (when (plusp c)
          (loop :for k :below (length seed-generators)
                :for seed := (elt seed-generators k)
                :for matrix := (pauli-to-matrix seed)
                :when (logbitp k c) :do
                  (setf ket (quil::m* matrix ket))))

        (loop :with b := (length primary-generators)
              :with result := (magicl:make-zero-matrix 2**n 1)
              :for z :below (expt 2 b) :do
                (loop :with term := (magicl::copy-matrix ket)
                      :for x :below b
                      :for generator := (elt primary-generators x)
                      :when (logbitp x z) :do
                        (let ((matrix (pauli-to-matrix generator)))
                          (setf term (quil::m* matrix term)))
                      :finally (setf result (quil::m+ result term)))
              :finally (let ((qvm (wrap-into-qvm
                                   (magicl:scale (expt 2 (- (/ b 2)))
                                                 result))))
                         (return (values (get-amplitudes qvm) qvm))))))))

(defun load-program-and-run (qvm program)
  (qvm:load-program qvm program)
  (qvm:run qvm))

(defun encode-circuit (encoder bits)
  "Execute the circuit ENCODER in the QVM in order to encode the bitstring given in BITS. Return the resulting wavefunction and the QVM object."
  (flet ((make-bit-toggler-program (bits)
           "Returns a program that takes the |0> state to the |BITS> state."
           (declare (type a:non-negative-fixnum bits))
           (let ((quil (with-output-to-string (stream)
                         (format stream "RESET~%")
                         (when (plusp bits)
                           (loop :for i :upto (log bits 2)
                                 :when (logbitp i bits) :do
                                   (format stream "X ~D~%" i))))))
             (quil:parse-quil quil))))

    (let ((qvm (qvm:make-qvm (quil::qubits-needed encoder))))
      (load-program-and-run qvm (make-bit-toggler-program bits))
      (load-program-and-run qvm encoder)
      (values (get-amplitudes qvm)
              qvm))))

(defun test-encoding (code encoder)
  "Ensure the encoding circuit for CODE produces the correct wavefunction."
  (loop :for c :below (expt 2 (length (qec::seed-generators code)))
        ;; The Linear Algebra encoder is regarded as the gold standard here.
        :for v1 := (encode-linear-algebra code c)
        :for v2 := (encode-circuit encoder c)
        :do (let ((d (dist v1 v2)))
              (is (quil::double= d 0.0d0)))))

(defun test-orthogonality (code &optional encoder)
  "Ensure that all the codewords are orthogonal."
  (declare (ignore encoder))
  (loop :with seed-generators := (qec::seed-generators code)
        :for c1 :below (expt 2 (length seed-generators)) :do
          (loop :for c2 :from c1 :below (expt 2 (length seed-generators)) :do
            (let ((value (dot-product (encode-linear-algebra code c1)
                                      (encode-linear-algebra code c2))))
              (is (if (= c1 c2)
                      (and (quil::double= (realpart value) 1.0d0)
                           (quil::double= (imagpart value) 0.0d0))
                      (quil::double= (abs value) 0.0d0)))))))

(defun test-idempotence (code &optional encoder)
  "Ensure that all the codewords are idempotent."
  (declare (ignore encoder))
  (loop :for g :across (concatenate 'qec::generators
                                    (qec::primary-generators code)
                                    (qec::secondary-generators code))
        :for matrix := (pauli-to-matrix g) :do
          (is (magicl:identityp (magicl:multiply-complex-matrices matrix matrix)))))

(defun test-eigenspace (code &optional encoder)
  "Verify that every codeword is in the +1 eigenspace of the generators using the encoder circuit to create the relevant codewords."
  (declare (ignore encoder))
  (let* ((primary-generators (qec::primary-generators code))
         (secondary-generators (qec::secondary-generators code))
         (seed-generators (qec::seed-generators code))
         (primary-and-secondary-generators (concatenate 'qec::generators primary-generators secondary-generators))
         (matrices (loop :for g :across primary-and-secondary-generators
                         :for m := (pauli-to-matrix g)
                         :collect m)))
    (loop :for c :below (expt 2 (length seed-generators))
          :for vector := (encode-linear-algebra code c) :do
            (loop :for x :from 0
                  :for matrix :in matrices
                  :for rq := (rayleigh-quotient matrix vector)
                  :do (is (and (quil::double= 1.0d0 (realpart rq))
                               (quil::double= 0.0d0 (imagpart rq))))))))

(defun test-eigenspace-linear-algebra (code)
  "Verify that every codeword is in the +1 eigenspace of the generators using linear algebra to create the relevant codewords."
  (let ((primary-generators (qec::primary-generators code))
        (secondary-generators (qec::secondary-generators code)))

    (labels ((make-zero-ket (n &optional (value #c(1.0d0 0.0d0)))
               (let ((vector (magicl:make-zero-matrix (expt 2 n) 1)))
                 (setf (magicl:ref vector 0 0) value)
                 vector))

             (make-state ()
               (loop :with n := (qec::number-of-physical-qubits code)
                     :with b := (length primary-generators)
                     :with psi := (magicl:make-zero-matrix (expt 2 n) 1)
                     :for a :below (expt 2 b) :do
                       (loop :with vector := (make-zero-ket n (/ (sqrt (expt 2 b))))
                             :for i :below b
                             :when (logbitp i a) :do
                               ;; (format t "~4,B~4TApplying ~A~%" a (elt primary-generators i))
                               (let ((matrix (pauli-to-matrix (elt primary-generators i))))
                                 (setf vector (magicl:multiply-complex-matrices matrix vector)))
                             :finally (setf psi (magicl:add-matrix psi vector)))
                     :finally (return psi))))

      (loop :for generator :across (concatenate 'qec::generators primary-generators secondary-generators)
            :for matrix := (pauli-to-matrix generator)
            :for vector := (make-state) :do
              (let ((rq (magicl:ref
                         (magicl:multiply-complex-matrices
                          (magicl:transpose vector)
                          (magicl:multiply-complex-matrices matrix vector))
                         0 0)))
                (is (and (quil::double= 1.0d0 (realpart rq))
                         (quil::double= 0.0d0 (imagpart rq)))))))))

(defun test-commutation (code)
  "Ensure all generators commute."
  (let ((primary-and-secondary-generators (concatenate 'qec::generators
                                                       (qec::primary-generators code)
                                                       (qec::secondary-generators code))))
    (let ((new-code (make-instance 'qec::code
                                   :n (qec::number-of-physical-qubits code)
                                   :generators primary-and-secondary-generators)))
      (is (qec::commutes-p new-code)))))

(defun test-syndromes (code encoder)
  "Verify that the errors lie in the orthogonal complement of the code space and that every error anticommutes with at least one generator."
  (flet ((make-error-program (gate qubit)
           (quil:parse-quil (format nil "~A ~D~%" gate qubit))))

    (loop :with primary-generators := (qec::primary-generators code)
          :with secondary-generators := (qec::secondary-generators code)
          :with generators := (concatenate 'list primary-generators secondary-generators)
          :with k := (length (qec::seed-generators code))
          :with n := (qec::number-of-physical-qubits code)
          :for i :below (expt 2 k)
          :for u := (encode-circuit encoder i) :do
            (loop ;; :initially (format t "Data: ~3,B~%" i)
                  :for gate :in '("X" "YY" "Z") :do
                    (loop :with syndromes := nil
                          :for j :below n :do
                            (loop :with syndrome := 0
                                  :for l :from 0
                                  :for generator :in generators
                                  :for program := (pauli-to-program generator)
                                  :for matrix := (quil:parsed-program-to-logical-matrix program)
                                  :do
                                     (let ((qvm (nth-value 1 (encode-circuit encoder i))))
                                       (load-program-and-run qvm (make-error-program gate j))
                                       (let* ((new-vector (get-amplitudes qvm))
                                              (rq (rayleigh-quotient matrix new-vector)))
                                         (when (minusp (realpart rq))
                                           (setf (ldb (byte 1 l) syndrome) 1))
                                         (is (<= (abs (dot-product u new-vector))
                                                 double-float-epsilon))))
                                  :finally (pushnew syndrome syndromes))
                          :finally (is (every #'plusp syndromes))
                                   ;; (format t "Syndromes for ~2A: ~{~5,B~^ ~}~%" gate (stable-sort syndromes #'<=))
                          )))))

(defun test-syndromes-linear-algebra (code)
  "Verify that the errors lie in the orthogonal complement of the code space and that every error anticommutes with at least one generator."
  (flet ((make-error-program (gate qubit)
           (quil:parse-quil (format nil "~A ~D~%" gate qubit))))

    (loop :with primary-generators := (qec::primary-generators code)
          :with secondary-generators := (qec::secondary-generators code)
          :with n := (qec::number-of-physical-qubits code)
          :with k := (length (qec::seed-generators code))
          :for i :below (expt 2 k)
          :for u := (encode-linear-algebra code i) :do
            (loop :for gate :in '("X" "YY" "Z") :do
              (loop :for j :below n :do
                (loop :with should-break-p := nil
                      :for generator :in (concatenate 'list primary-generators secondary-generators)
                      :for matrix := (quil:parsed-program-to-logical-matrix (pauli-to-program generator))
                      :unless should-break-p :do
                        (let ((qvm (nth-value 1 (encode-linear-algebra code i))))
                          (load-program-and-run qvm (make-error-program gate j))
                          (let* ((new-vector (get-amplitudes qvm))
                                 (rq (rayleigh-quotient matrix new-vector)))
                            (when (minusp (realpart rq))
                              ;; (format t "~&✓~4T~A ~D applied to ~B (~A)~%" gate j i generator)
                              (setf should-break-p t))
                            (is (<= (abs (dot-product u new-vector)) double-float-epsilon))))
                      :finally (unless should-break-p
                                 ;; (format t "~&✘~4T~A ~D applied to ~B (FAIL)~%" gate j i)
                                 (signal 'quantum-error-not-detected))))))))

(defun test-code (input-code)
  (multiple-value-bind (code encoder)
      (cleve-gottesman input-code)
    (test-encoding code encoder)
    (test-orthogonality code)
    (test-idempotence code)
    (test-eigenspace code)
    ;; (test-eigenspace-linear-algebra code)
    (test-commutation code)
    (test-syndromes code encoder)
    ;; (test-syndromes-linear-algebra code)
    ))

(deftest test-eight-qubit-code ()
  (test-code (qec::make-code '((x x x x x x x x)
                               (z z z z z z z z)
                               (x i x i z y z y)
                               (x i y z x i y z)
                               (x z i y i y x z)))))

(deftest test-shor-9-1-3-code ()
  (test-code (make-code '((z z i i i i i i i)
                          (i z z i i i i i i)
                          (i i i z z i i i i)
                          (i i i i z z i i i)
                          (i i i i i i z z i)
                          (i i i i i i i z z)
                          (x x x x x x i i i)
                          (i i i x x x x x x)))))
