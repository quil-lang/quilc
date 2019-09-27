;;;; rpc-server.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:quilc)


(defun get-version-info-handler ()
  (a:alist-hash-table
   `(("quilc"   . ,+QUILC-VERSION+)
     ("githash" . ,+GIT-HASH+))))

(defun ensure-optional-real (x)
  (if x
      (coerce x 'double-float)
      nil))

(defun runtime-estimation (parsed-protoquil-program)
  "Estimated QPU runtime of PARSED-PROTOQUIL-PROGRAM. Likely to be an over-estimate for small depth programs, where runtime is dominated by network latency and compilation, etc. Take these results with a grain of salt."
  (when (and (typep parsed-protoquil-program 'quil:parsed-program)
             (quil:protoquil-program-p parsed-protoquil-program))
    ;; These opaque numbers come from an analysis of the runtimes of a
    ;; large number of randomly generated programs targeting a 16Q
    ;; lattice. Those programs were a random mixture of 1- and 2Q
    ;; gates, followed by MEASUREs on all 16 qubits.
    (loop :with coeff-oneq := 1.6291
          :with coeff-twoq := 1.701
          :with runtime := 181.89
          :for instr :across (quil:parsed-program-executable-code parsed-protoquil-program)
          :when (typep instr 'quil:application) :do
            (case (length (quil:application-arguments instr))
              (1 (incf runtime coeff-oneq))
              (2 (incf runtime coeff-twoq)))
          :finally (return runtime))))

(defun statistics-to-metadata (statistics)
  (check-type statistics hash-table)
  (make-instance 'rpcq::|NativeQuilMetadata|
                 :|final_rewiring| (gethash "final_rewiring" statistics)
                 :|gate_depth| (gethash "gate_depth" statistics)
                 :|gate_volume| (gethash "gate_volume" statistics)
                 :|multiqubit_gate_depth| (gethash "multiqubit_gate_depth" statistics)
                 :|program_duration| (ensure-optional-real (gethash "program_duration" statistics))
                 :|program_fidelity| (ensure-optional-real (gethash "program_fidelity" statistics))
                 :|topological_swaps| (gethash "topological_swaps" statistics)
                 :|qpu_runtime_estimation| (gethash "qpu_runtime_estimation" statistics)))

;; TODO: rework the structure of process-program so that the JSON junk is only
;;       done in web-server.lisp, and this doesn't have to do back-translation.
(defun quil-to-native-quil-handler (request &key protoquil state-prep-compression)
  "Traditional QUILC invocation: compiles a Quil program to native Quil, as specified by an ISA."
  (check-type request rpcq::|NativeQuilRequest|)
  (let* ((quil-program (quil::parse-quil (rpcq::|NativeQuilRequest-quil| request)))
         (target-device (rpcq::|NativeQuilRequest-target_device| request))
         (qpu-hash (a:plist-hash-table (list "isa" (rpcq::|TargetDevice-isa| target-device)
                                             "specs" (rpcq::|TargetDevice-specs| target-device))
                                       :test #'equal))
         (chip-specification (cl-quil::qpu-hash-table-to-chip-specification qpu-hash))
         ;; Allow endpoint to override server's -P
         (protoquil
          (ecase protoquil
            ((nil) *protoquil*)
            (:false nil)
            (t t)))
         (quil::*enable-state-prep-compression*
          (ecase state-prep-compression
            ((nil) quil::*enable-state-prep-compression*)
            (:false nil)
            (t t))))
    (multiple-value-bind (processed-program statistics-dict)
        (process-program quil-program chip-specification :protoquil protoquil)
      (when protoquil
        (setf (gethash "qpu_runtime_estimation" statistics-dict)
              (runtime-estimation processed-program)))
      (make-instance 'rpcq::|NativeQuilResponse|
                     :|quil| (print-program processed-program nil)
                     :|metadata| (when protoquil
                                   (statistics-to-metadata statistics-dict))))))

(defun native-quil-to-binary-handler (request)
  "Dummy invocation: this QUILC binary returns something QVM-executable, which is just the program again."
  (check-type request rpcq::|BinaryExecutableRequest|)
  (make-instance 'rpcq::|PyQuilExecutableResponse|
                 :|program| (rpcq::|BinaryExecutableRequest-quil| request)
                 :|attributes| (a:plist-hash-table
                                `("num_shots" ,(rpcq::|BinaryExecutableRequest-num_shots| request)))))

(defun generate-rb-sequence-handler (request)
  "Generates a randomized benchmarking sequence according to REQUEST."
  (check-type request rpcq::|RandomizedBenchmarkingRequest|)
  (let ((k (rpcq::|RandomizedBenchmarkingRequest-depth| request))
        (n (rpcq::|RandomizedBenchmarkingRequest-qubits| request))
        (gateset (coerce (rpcq::|RandomizedBenchmarkingRequest-gateset| request) 'list))
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
           (qubits-used (mapcar (a:compose
                                 #'cl-quil:qubits-used
                                 #'cl-quil:parse-quil)
                                gateset))
           (qubits-used-by-interleaver
             (when interleaver
               (cl-quil:qubits-used (cl-quil:parse-quil interleaver))))
           (qubits (union qubits-used-by-interleaver (reduce #'union qubits-used)))
           (embedded-cliffords (loop :for clifford :in cliffords
                                     :for i :from 0
                                     :collect
                                     (quil.clifford:embed clifford n
                                                          ;; See below
                                                          (reverse (loop :for index :in (nth i qubits-used)
                                                                         :collect (position index qubits))))))
           (embedded-interleaver
             (when interleaver
               (quil.clifford:embed (quil.clifford::clifford-from-quil interleaver)
                                    n
                                    ;; XXX: the embedding ordering has
                                    ;; been reversed to comply with
                                    ;; the computational basis
                                    ;; convention, hence the reverse
                                    ;; here. We could use a better fix
                                    ;; for this.
                                    (reverse (loop :for index :in qubits-used-by-interleaver
                                                   :collect (position index qubits))))))
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

(defun conjugate-pauli-by-clifford-handler (request)
  "Conjugates a Pauli operator by a Clifford operator, as specified by REQUEST."
  (check-type request rpcq::|ConjugateByCliffordRequest|)
  (let* ((pauli (rpcq::|ConjugateByCliffordRequest-pauli| request))
         (clifford-program (rpcq::|ConjugateByCliffordRequest-clifford| request))
         (pauli-indices (coerce (rpcq::|PauliTerm-indices| pauli) 'list))
         (pauli-terms (coerce (rpcq::|PauliTerm-symbols| pauli) 'list))
         (clifford-indices (sort (cl-quil:qubits-used (cl-quil:parse-quil clifford-program)) #'<))
         (qubits (sort (union (copy-seq pauli-indices) (copy-seq clifford-indices)) #'<))
         (pauli (quil.clifford:pauli-from-string
                 ;; XXX: the pauli-from-string and embedding orderings
                 ;; have been reversed to comply with the
                 ;; computational basis convention, hence the reverse
                 ;; here. We could use a better fix for this.
                 (reverse (with-output-to-string (s)
                            (dolist (i qubits)
                              (cond ((member i pauli-indices)
                                     (write-string (nth (position i pauli-indices) pauli-terms) s))
                                    (T (write-string "I" s))))))))
         (clifford (cl-quil.clifford::embed (quil.clifford::clifford-from-quil clifford-program)
                                            (length qubits)
                                            ;; Likewise to the above.
                                            (reverse (loop :for index :in clifford-indices :collect (position index qubits)))))
         (pauli-out (quil.clifford:apply-clifford clifford pauli)))
    (make-instance 'rpcq::|ConjugateByCliffordResponse|
                   :|phase| (quil.clifford::phase-factor pauli-out)
                   :|pauli| (apply #'concatenate 'string
                                   (mapcar (a:compose #'symbol-name #'quil.clifford::base4-to-sym)
                                           (quil.clifford::base4-list pauli-out))))))

(defun rewrite-arithmetic-handler (request)
  "Rewrites the request program without arithmetic in gate parameters."
  (check-type request rpcq::|RewriteArithmeticRequest|)
  (let ((program (quil::parse-quil (rpcq::|RewriteArithmeticRequest-quil| request)
                                   :transforms nil)))
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
                       (a:alist-hash-table
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
                           (protocol "tcp")
                           (host "*")
                           (port 5555)
                           (logger (make-instance 'cl-syslog:rfc5424-logger
                                                  :log-writer (cl-syslog:null-log-writer)))
                           (time-limit 0))
  (let ((dt (rpcq:make-dispatch-table)))
    (rpcq:dispatch-table-add-handler dt 'quil-to-native-quil-handler :name "quil-to-native-quil")
    (rpcq:dispatch-table-add-handler dt 'native-quil-to-binary-handler :name "native-quil-to-binary")
    (rpcq:dispatch-table-add-handler dt 'generate-rb-sequence-handler :name "generate-rb-sequence")
    (rpcq:dispatch-table-add-handler dt 'conjugate-pauli-by-clifford-handler :name "conjugate-pauli-by-clifford")
    (rpcq:dispatch-table-add-handler dt 'rewrite-arithmetic-handler :name "rewrite-arithmetic")
    (rpcq:dispatch-table-add-handler dt 'get-version-info-handler :name "get-version-info")
    (rpcq:start-server :dispatch-table dt
                       :listen-addresses (list (format nil "~a://~a~@[:~a~]" protocol host port))
                       :logger logger
                       :timeout time-limit)))
