;;;; rpcq-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:quilc-tests)


(deftest test-easy-version-call ()
  (let* ((server-thread (bt:make-thread #'quilc::start-rpc-server)))
    (sleep 1)
    (unwind-protect
         (rpcq:with-rpc-client (client "tcp://127.0.0.1:5555")
           (let ((hash (rpcq:rpc-call client "get-version-info")))
             (is (typep hash 'hash-table))))
      (bt:destroy-thread server-thread))))

(deftest test-quil-roundtrip ()
  (let* ((server-function (lambda ()
                            (quilc::start-rpc-server)))
         (server-thread (bt:make-thread server-function)))
    (sleep 1)
    (unwind-protect
         (rpcq:with-rpc-client (client "tcp://127.0.0.1:5555")
           (let* ((quil "H 0")
                  (isa (alexandria:plist-hash-table
                        (list
                         "1Q" (alexandria:plist-hash-table
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
                  (pp (quil::parse-quil-string quil))
                  (cpp (quil::parse-quil-string (rpcq::|NativeQuilResponse-quil| server-response))))
             (multiple-value-bind (mat1 mat2)
                 (quil::matrix-rescale (quil::make-matrix-from-quil
                                        (coerce (quil:parsed-program-executable-code pp) 'list))
                                       (quil::make-matrix-from-quil
                                        (coerce (quil:parsed-program-executable-code cpp) 'list)))
               (setf mat1 (quil::scale-out-matrix-phases mat1 mat2))
               (is (quil::matrix-equality mat1 mat2)))))
      (bt:destroy-thread server-thread))))
