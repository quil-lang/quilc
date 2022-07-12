;;;; rpcq-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:quilc-tests)

;; TODO FIXME XXX The below relies on some funky/hacky/unsightly code
;; to forcefully kill threads. See open issues
;; https://github.com/rigetti/rpcq/issues/61
;; https://github.com/rigetti/rpcq/issues/75

;; Stolen from RPCQ-TESTS
(defun kill-thread-slowly (thread)
  #+ccl
  (loop :while (bt:thread-alive-p thread)
        :do (sleep 1) (bt:destroy-thread thread))
  #-ccl
  (when (bt:thread-alive-p thread)
    (bt:destroy-thread thread))
  (sleep 1))

(defmacro with-random-rpc-client ((client) &body body)
  "Bind CLIENT to an RPCQ client object for the duration of BODY."
  (let* ((protocol "inproc")
         (host (format nil "~A" (uuid:make-v4-uuid)))
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
         (kill-thread-slowly server-thread)))))

(deftest test-easy-version-call ()
  "Test that the \"get-version-info\" endpoint works."
  (with-random-rpc-client (client)
    (let ((hash (rpcq:rpc-call client "get-version-info")))
      (is (typep hash 'hash-table)))))

(deftest test-quil-to-native-quil-endpoint ()
  "Test that the \"quil-to-native-quil\" endpoint compiles to a program whose matrix representation is equivalent to the input program." 
  (flet ((plist-isa-subtable (&rest p) (a:plist-hash-table p :test #'equalp)))
    (with-random-rpc-client (client)
      (let* ((quil "H 0")
             (isa (plist-isa-subtable
                   "1Q" (plist-isa-subtable
                         "0" (make-hash-table)
                         "1" (make-hash-table))
                   "2Q" (plist-isa-subtable
                         "0-1" (make-hash-table))))
             (specs (plist-isa-subtable
                     "1Q" (plist-isa-subtable
                           "0" (plist-isa-subtable "f1QRB" 0.98)
                           "1" (plist-isa-subtable "f1QRB" 0.98))
                     "2Q" (plist-isa-subtable
                           "0-1" (make-hash-table))))
             (target-device (make-instance 'rpcq::|TargetDevice|
                                           :|isa| isa
                                           :|specs| specs))
             (server-payload (make-instance 'rpcq::|NativeQuilRequest|
                                            :|quil| quil
                                            :|target_device| target-device))
             (server-response (rpcq:rpc-call client "quil-to-native-quil" server-payload :protoquil t))
             (pp (cl-quil::parse-quil quil))
             (cpp (cl-quil::parse-quil (rpcq::|NativeQuilResponse-quil| server-response))))
        (multiple-value-bind (mat1 mat2)
            (cl-quil::matrix-rescale (cl-quil::make-matrix-from-quil
                                   (coerce (cl-quil:parsed-program-executable-code pp) 'list))
                                  (cl-quil::make-matrix-from-quil
                                   (coerce (cl-quil:parsed-program-executable-code cpp) 'list)))
          (setf mat1 (cl-quil::scale-out-matrix-phases mat1 mat2))
          (is (> 1d0 (rpcq::|NativeQuilMetadata-program_fidelity|
                            (rpcq::|NativeQuilResponse-metadata| server-response))))
          (is (cl-quil::matrix-equality mat1 mat2)))))))

(deftest test-quil-to-native-quil-protoquil-endpoint ()
  "Test that the \"quil-to-native-quil\" endpoint will compile protoquil when given :PROTOQUIL T."
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
           (server-response (rpcq:rpc-call client "quil-to-native-quil" server-payload :protoquil nil))
           (server-response-protoquil (rpcq:rpc-call client "quil-to-native-quil" server-payload :protoquil t))
           (cpp (cl-quil:parse-quil (rpcq::|NativeQuilResponse-quil| server-response)))
           (cpp-protoquil (cl-quil:parse-quil (rpcq::|NativeQuilResponse-quil| server-response-protoquil))))
      (is (not (cl-quil:protoquil-program-p cpp)))
      (is (cl-quil:protoquil-program-p cpp-protoquil)))))

(deftest test-quil-to-native-quil-endpoint-overrides-server ()
  "Test that the \"quil-to-native-quil\" endpoint can override a server that has been started with -P."
  (quilc::special-bindings-let* ((quilc::*protoquil* t))
    ;; Bindings added to BT:*DEFAULT-SPECIAL-BINDINGS* are rebound in any threads created directly
    ;; by the binding thread. Since BT:*DEFAULT-SPECIAL-BINDINGS* does not appear in as member of
    ;; BT:*DEFAULT-SPECIAL-BINDINGS*, it won't be bound in child threads! If you want your bindings
    ;; to persist in grandchildren threads (and beyond), then you must ask for it explicitly.
    (quilc::special-bindings-let* ((bt:*default-special-bindings* bt:*default-special-bindings*))
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
                                              :|target_device| target-device)))
          (flet ((parse-response (protoquil)
                   (cl-quil:parse-quil (rpcq::|NativeQuilResponse-quil| (rpcq:rpc-call client "quil-to-native-quil" server-payload :protoquil protoquil)))))
            ;; :protoquil nil means defer to server, i.e. this should produce protoquil
            (is (cl-quil:protoquil-program-p (parse-response nil)))
            ;; :protoquil ':false means no protoquil, override server's -P
            (is (not (cl-quil:protoquil-program-p (parse-response ':false))))
            ;; :protoquil t means yes protoquil, regardless of what the server says
            (is (cl-quil:protoquil-program-p (parse-response t))))))))

  ;; Same tests but for *protoquil* = nil
  (quilc::special-bindings-let* ((quilc::*protoquil* nil))
    (quilc::special-bindings-let* ((bt:*default-special-bindings* bt:*default-special-bindings*))
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
                                              :|target_device| target-device)))
          (flet ((parse-response (protoquil)
                   (cl-quil:parse-quil (rpcq::|NativeQuilResponse-quil| (rpcq:rpc-call client "quil-to-native-quil" server-payload :protoquil protoquil)))))
            ;; :protoquil nil means defer to server, i.e. this should not produce protoquil
            (is (not (cl-quil:protoquil-program-p (parse-response nil))))
            ;; :protoquil ':false means no protoquil, override server's -P
            (is (not (cl-quil:protoquil-program-p (parse-response ':false))))
            ;; :protoquil t means yes protoquil, regardless of what the server says
            (is (cl-quil:protoquil-program-p (parse-response t)))))))))

(deftest test-native-quil-to-binary-endpoint ()
  "Test that the \"native-quil-to-binary\" endpoint works."
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
  "Test that the \"generate-rb-sequence\" endpoint works."
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
  "Test that the \"conjugate-pauli-by-clifford\" endpoint works."
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
  "Test that the \"rewrite-arithmetic\" endpoint works."
  (with-random-rpc-client (client)
    (let* ((quil "RX((2+1/2)*pi/7) 0")
           (request (make-instance 'rpcq::|RewriteArithmeticRequest|
                                   :|quil| quil))
           (response (rpcq:rpc-call client "rewrite-arithmetic" request)))
      (is (cl-quil::matrix-equality
           (cl-quil:parsed-program-to-logical-matrix (cl-quil:parse-quil quil))
           (cl-quil:parsed-program-to-logical-matrix
            (cl-quil:parse-quil (rpcq::|RewriteArithmeticResponse-quil| response))))))))
(deftest test-quil-to-native-quil-on-nontrivial-features ()
  "Test that non-trivial language features (such as DEFCIRCUIT) are processed correctly."
  (with-random-rpc-client (client)
    (let* ((quil "
DEFCIRCUIT TEST a b:
    H a
    CNOT a b

TEST 0 1
")
           (isa (yason:parse "{\"1Q\": {\"0\": {}, \"1\": {}}, \"2Q\": {\"0-1\": {}}}"))
           (specs (make-hash-table))
           (target-device (make-instance 'rpcq::|TargetDevice|
                                         :|isa| isa
                                         :|specs| specs))
           (server-payload (make-instance 'rpcq::|NativeQuilRequest|
                                          :|quil| quil
                                          :|target_device| target-device))
           (server-response (rpcq:rpc-call client "quil-to-native-quil" server-payload))
           (pp (cl-quil::parse-quil quil))
           (cpp (cl-quil::parse-quil (rpcq::|NativeQuilResponse-quil| server-response))))
      (multiple-value-bind (mat1 mat2)
          (cl-quil::matrix-rescale (cl-quil::make-matrix-from-quil
                                 (coerce (cl-quil:parsed-program-executable-code pp) 'list))
                                (cl-quil::make-matrix-from-quil
                                 (coerce (cl-quil:parsed-program-executable-code cpp) 'list)))
        (setf mat1 (cl-quil::scale-out-matrix-phases mat1 mat2))
        (is (cl-quil::matrix-equality mat1 mat2))))))

(deftest test-quil-safely-resolve ()
  "Test that the \"quil-to-native-quil\" endpoint raises an error when including a file outside of cl-quil::*safe-include-directory*."
  (quilc::special-bindings-let* ((cl-quil::*safe-include-directory* "./"))
    (quilc::special-bindings-let* ((bt:*default-special-bindings* bt:*default-special-bindings*))
      (with-random-rpc-client (client)
        (let* ((quil "INCLUDE \"../test\"")
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
                                              :|target_device| target-device)))
          (signals rpcq:rpc-error (rpcq:rpc-call client "quil-to-native-quil" server-payload)))))))
