;;;; rpcq-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:quilc-tests)

(defmacro with-random-rpc-client ((client) &body body)
  (let* ((protocol "inproc")
         (host (format nil "~a" (uuid:make-v4-uuid)))
         (endpoint (concatenate 'string protocol "://" host)))
    `(let* ((server-function (lambda ()
                              (quilc::start-rpc-server :protocol ,protocol
                                                       :host ,host
                                                       :port nil)))
            (server-thread (bt:make-thread server-function)))
       (sleep 1)
       (unwind-protect
            (rpcq:with-rpc-client (,client ,endpoint)
              ,@body)
         (bt:destroy-thread server-thread)))))

(deftest test-easy-version-call ()
  (with-random-rpc-client (client)
    (let ((hash (rpcq:rpc-call client "get-version-info")))
      (is (typep hash 'hash-table)))))

(deftest test-quil-roundtrip ()
  (with-random-rpc-client (client)
    (let* ((quil "H 0")
           (isa (a:plist-hash-table
                 (list
                  "1Q" (a:plist-hash-table
                        (list
                         "0" (make-hash-table)))
                  "2Q" (make-hash-table))))
           (specs (make-hash-table))
           (target-device (make-instance 'rpcq::|TargetDevice|
                                         :|isa| isa
                                         :|specs| specs))
           (server-payload (make-instance 'rpcq::|NativeQuilRequest|
                                          :|quil| quil
                                          :|target_device| target-device))
           (server-response (rpcq:rpc-call client "quil-to-native-quil" server-payload))
           (pp (quil::parse-quil quil))
           (cpp (quil::parse-quil (rpcq::|NativeQuilResponse-quil| server-response))))
      (multiple-value-bind (mat1 mat2)
          (quil::matrix-rescale (quil::make-matrix-from-quil
                                 (coerce (quil:parsed-program-executable-code pp) 'list))
                                (quil::make-matrix-from-quil
                                 (coerce (quil:parsed-program-executable-code cpp) 'list)))
        (setf mat1 (quil::scale-out-matrix-phases mat1 mat2))
        (is (quil::matrix-equality mat1 mat2))))))

(deftest test-native-quil-to-binary-endpoint ()
  (with-random-rpc-client (client)
    (let* ((quil "H 0")
           (num-shots 10)
           (server-payload (make-instance 'rpcq::|BinaryExecutableRequest|
                                          :|quil| quil
                                          :|num_shots| num-shots))
           (server-response (rpcq:rpc-call client "native-quil-to-binary" server-payload)))
      (is (string= quil (rpcq::|PyQuilExecutableResponse-program| server-response))
          (= num-shots (gethash "num_shots"
                                (rpcq::|PyQuilExecutableResponse-attributes| server-response)))))))

;; This test is copied wholesale from pyQuil's test_api.py, random
;; seed and all.
(deftest test-generate-rb-sequence-endpoint ()
  (with-random-rpc-client (client)
    (let* ((quil "PHASE(pi/2) 0
H 0")
           (gateset (list "PHASE(pi/2) 0" "H 0"))
           (server-payload (make-instance 'rpcq::|RandomizedBenchmarkingRequest|
                                          :|quil| quil
                                          :|depth| 2
                                          :|seed| 52
                                          :|qubits| 1
                                          :|gateset| gateset))
           (server-response (rpcq:rpc-call client "generate-rb-sequence" server-payload))
           (response (rpcq::|RandomizedBenchmarkingResponse-sequence| server-response)))
      (loop :for a :across response :do
        (is (equalp a #(0 0 1 0 1)))))))

(deftest test-conjugate-pauli-by-clifford-endpoint ()
  (with-random-rpc-client (client)
    (let* ((pauli (make-instance 'rpcq::|PauliTerm| :|indices| '(0) :|symbols| '("X")))
           (clifford "H 0")
           (request (make-instance 'rpcq::|ConjugateByCliffordRequest|
                                   :|pauli| pauli
                                   :|clifford| clifford))
           (response (rpcq:rpc-call client "conjugate-pauli-by-clifford" request)))
      (is (zerop (rpcq::|ConjugateByCliffordResponse-phase| response)))
      (is (string= "Z" (rpcq::|ConjugateByCliffordResponse-pauli| response))))))

(deftest test-rewrite-arithmetic-endpoint ()
  (with-random-rpc-client (client)
    (let* ((quil "RX((2+1/2)*pi/7) 0")
           (request (make-instance 'rpcq::|RewriteArithmeticRequest|
                                   :|quil| quil))
           (response (rpcq:rpc-call client "rewrite-arithmetic" request)))
      (is (quil::matrix-equality
           (quil:parsed-program-to-logical-matrix (quil:parse-quil quil))
           (quil:parsed-program-to-logical-matrix
            (quil:parse-quil (rpcq::|RewriteArithmeticResponse-quil| response))))))))


(deftest test-generate-rb-sequence-endpoint ()
  (let* ((server-function (lambda ()
                            (quilc::start-rpc-server)))
         (server-thread (bt:make-thread server-function)))
    (sleep 1)
    (unwind-protect
         (rpcq:with-rpc-client (client "tcp://127.0.0.1:5555")
           (let* ((quil "PHASE(pi/2) 0
H 0")
                  (gateset (list "PHASE(pi/2) 0" "H 0"))
                  (server-payload (make-instance 'rpcq::|RandomizedBenchmarkingRequest|
                                                 :|quil| quil
                                                 :|depth| 2
                                                 :|seed| 52
                                                 :|qubits| 1
                                                 :|gateset| gateset))
                  (server-response (rpcq:rpc-call client "generate-rb-sequence" server-payload))
                  (response (rpcq::|RandomizedBenchmarkingResponse-sequence| server-response)))
             (loop :for a :across response :do
               (is (equalp a #(0 0 1 0 1))))))
      (bt:destroy-thread server-thread))))
