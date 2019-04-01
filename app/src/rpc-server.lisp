;;;; rpc-server.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:quilc)


(defun get-version-info ()
  (alexandria:alist-hash-table
   `(("quilc"   . ,+QUILC-VERSION+)
     ("githash" . ,+GIT-HASH+))))

(defun ensure-optional-real (x)
  (if x
      (coerce x 'double-float)
      nil))

;; TODO: rework the structure of process-program so that the JSON junk is only
;;       done in web-server.lisp, and this doesn't have to do back-translation.
(defun quil-to-native-quil (request)
  "Traditional QUILC invocation: compiles a Quil program to native Quil, as specified by an ISA."
  (check-type request rpcq::|NativeQuilRequest|)
  (let* ((quil-program (quil::parse-quil (rpcq::|NativeQuilRequest-quil| request)))
         (target-device (rpcq::|NativeQuilRequest-target_device| request))
         (qpu-hash (alexandria:plist-hash-table (list "isa" (rpcq::|TargetDevice-isa| target-device)
                                                      "specs" (rpcq::|TargetDevice-specs| target-device))
                                                :test #'equal))
         (chip-specification (cl-quil::qpu-hash-table-to-chip-specification qpu-hash))
         (dictionary (process-program quil-program chip-specification))
         (metadata (make-instance 'rpcq::|NativeQuilMetadata|
                                  :|final_rewiring| (gethash "final_rewiring" dictionary)
                                  :|gate_depth| (gethash "gate_depth" dictionary)
                                  :|gate_volume| (gethash "gate_volume" dictionary)
                                  :|multiqubit_gate_depth| (gethash "multiqubit_gate_depth" dictionary)
                                  :|program_duration| (ensure-optional-real (gethash "program_duration" dictionary))
                                  :|program_fidelity| (ensure-optional-real (gethash "program_fidelity" dictionary))
                                  :|topological_swaps| (gethash "topological_swaps" dictionary))))
    (make-instance 'rpcq::|NativeQuilResponse|
                   :|quil| (gethash "processed_program" dictionary)
                   :|metadata| metadata)))

(defun native-quil-to-binary (request)
  "Dummy invocation: this QUILC binary returns something QVM-executable, which is just the program again."
  (check-type request rpcq::|BinaryExecutableRequest|)
  (make-instance 'rpcq::|PyQuilExecutableResponse|
                 :|program| (rpcq::|BinaryExecutableRequest-quil| request)
                 :|attributes| (alexandria:plist-hash-table
                                `("num_shots" ,(rpcq::|BinaryExecutableRequest-num_shots| request)))))

(defun generate-rb-sequence (request)
  "Generates a randomized benchmarking sequence according to REQUEST."
  (check-type request rpcq::|RandomizedBenchmarkingRequest|)
  (let ((k (rpcq::|RandomizedBenchmarkingRequest-depth| request))
        (n (rpcq::|RandomizedBenchmarkingRequest-qubits| request))
        (gateset (rpcq::|RandomizedBenchmarkingRequest-gateset| request))
        (seed (rpcq::|RandomizedBenchmarkingRequest-seed| request))
        (interleaver (rpcq::|RandomizedBenchmarkingRequest-interleaver| request)))
    #-sbcl
    (when seed
      (error "Unable to seed the random number generator."))
    (when (and seed (not (typep seed 'unsigned-byte)))
      (error "Seed must be a positive integer."))
    (when (> n 2)
      (error "Currently no more than two qubit randomized benchmarking is supported."))
    (let* ((cliffords (mapcar #'quil.clifford::clifford-from-quil gateset))
           (qubits-used (mapcar (alexandria:compose
                                 (alexandria:curry #'reduce #'union)
                                 #'cl-quil.clifford::extract-qubits-used
                                 #'cl-quil:parse-quil-string)
                                gateset))
           (qubits-used-by-interleaver
             (when interleaver
               (reduce #'union
                       (cl-quil.clifford::extract-qubits-used
                        (cl-quil:parse-quil-string interleaver)))))
           (qubits (union qubits-used-by-interleaver (reduce #'union qubits-used)))
           (embedded-cliffords (loop :for clifford :in cliffords
                                     :for i :from 0
                                     :collect
                                     (quil.clifford:embed clifford n
                                                          (loop :for index :in (nth i qubits-used)
                                                                :collect (position index qubits)))))
           (embedded-interleaver
             (when interleaver
               (quil.clifford:embed (quil.clifford::clifford-from-quil interleaver)
                                    n
                                    (loop :for index :in qubits-used-by-interleaver
                                          :collect (position index qubits)))))
           (rb-sequence
             (let ((*random-state*
                     #+sbcl (if seed (sb-ext:seed-random-state seed) *random-state*)
                     #-sbcl *random-state*))
               (quil.clifford::rb-sequence k n embedded-cliffords embedded-interleaver)))
           (gateset-label-sequence
             (loop :for clifford-element :in rb-sequence
                   :collect (loop :for generator :in clifford-element
                                  :collect (position generator embedded-cliffords :test #'quil.clifford:clifford=)))))
      (make-instance 'rpcq::|RandomizedBenchmarkingResponse|
                     :|sequence| gateset-label-sequence))))

(defun conjugate-pauli-by-clifford (request)
  "Conjugates a Pauli operator by a Clifford operator, as specified by REQUEST."
  (check-type request rpcq::|ConjugateByCliffordRequest|)
  (let* ((pauli (rpcq::|ConjugateByCliffordRequest-pauli| request))
         (clifford-program (rpcq::|ConjugateByCliffordRequest-clifford| request))
         (pauli-indices (rpcq::|PauliTerm-indices| pauli))
         (pauli-terms (rpcq::|PauliTerm-symbols| pauli))
         (clifford-indices (sort (reduce #'union (cl-quil.clifford::extract-qubits-used (cl-quil:parse-quil-string clifford-program))) #'<))
         (qubits (sort (union (copy-seq pauli-indices) (copy-seq clifford-indices)) #'<))
         (pauli (quil.clifford:pauli-from-string
                 (with-output-to-string (s)
                   (dolist (i qubits)
                     (cond ((member i pauli-indices)
                            (write-string (nth (position i pauli-indices) pauli-terms) s))
                           (T (write-string "I" s)))))))
         (clifford (cl-quil.clifford::embed (quil.clifford::clifford-from-quil clifford-program)
                                            (length qubits)
                                            (loop :for index :in clifford-indices :collect (position index qubits))))
         (pauli-out (quil.clifford:apply-clifford clifford pauli)))
    (make-instance 'rpcq::|ConjugateByCliffordResponse|
                   :|phase| (quil.clifford::phase-factor pauli-out)
                   :|pauli| (apply #'concatenate 'string
                                   (mapcar (alexandria:compose #'symbol-name #'quil.clifford::base4-to-sym)
                                           (quil.clifford::base4-list pauli-out))))))

(defun rewrite-arithmetic (request)
  "Rewrites the request program without arithmetic in gate parameters."
  (check-type request rpcq::|RewriteArithmeticRequest|)
  (let ((program (quil::parse-quil (rpcq::|RewriteArithmeticRequest-quil| request))))
    (multiple-value-bind (rewritten-program original-memory-descriptors recalculation-table)
        (cl-quil::rewrite-arithmetic program)
      (let ((reformatted-rt (make-hash-table)))
        (maphash (lambda (key val)
                   (setf (gethash (make-instance 'rpcq::|ParameterAref|
                                                 :|name| (quil::memory-ref-name key)
                                                 :|index| (quil::memory-ref-position key))
                                  reformatted-rt)
                         (quil::print-instruction val nil)))
                 recalculation-table)
        (make-instance 'rpcq::|RewriteArithmeticResponse|
                       :|quil|
                       (with-output-to-string (s)
                         (quil::print-parsed-program rewritten-program s))
                       :|original_memory_descriptors|
                       (alexandria:alist-hash-table
                        (mapcar (lambda (memory-defn)
                                  (cons (quil::memory-descriptor-name memory-defn)
                                        (make-instance 'rpcq::|ParameterSpec|
                                                       :|type| (quil::quil-type-string (quil::memory-descriptor-type memory-defn))
                                                       :|length| (quil::memory-descriptor-length memory-defn))))
                                original-memory-descriptors))
                       :|recalculation_table|
                       reformatted-rt)))))


(declaim (special *program-name*))
(defun start-rpc-server (&key
                           (port 5555)
                           (logger (make-instance 'cl-syslog:rfc5424-logger
                                                  :log-writer (cl-syslog:null-log-writer))))
  (let ((dt (rpcq:make-dispatch-table)))
    (rpcq:dispatch-table-add-handler dt 'quil-to-native-quil)
    (rpcq:dispatch-table-add-handler dt 'native-quil-to-binary)
    (rpcq:dispatch-table-add-handler dt 'generate-rb-sequence)
    (rpcq:dispatch-table-add-handler dt 'conjugate-pauli-by-clifford)
    (rpcq:dispatch-table-add-handler dt 'rewrite-arithmetic)
    (rpcq:dispatch-table-add-handler dt 'get-version-info)
    (rpcq:start-server :dispatch-table dt
                       :listen-addresses (list (format nil "tcp://*:~a" port))
                       :logger logger
                       :timeout *time-limit*)))
