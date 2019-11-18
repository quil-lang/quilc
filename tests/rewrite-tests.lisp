(in-package #:cl-quil-tests)

(deftest test-rz-full-rotation-elimination ()
  ;; Test the compiler directly.
  (is (null (quil::eliminate-full-rz-rotations (quil::build-gate "RZ" (list 0d0) 0))))
  (is (null (quil::eliminate-full-rz-rotations (quil::build-gate "RZ" (list (quil::constant 0d0)) 0))))

  ;; Test that the compiler takes effect via compiler-hook.
  (let ((chip (quil::build-nq-fully-connected-chip 2))
        (pps (list (quil::parse-quil "DECLARE theta REAL[1]; RZ(0.0) 0;")
                   (quil::parse-quil "DECLARE theta REAL[1]; RZ(2*pi) 0;")
                   (quil::parse-quil "DECLARE theta REAL[1]; RZ(-2*pi) 0;"))))
    (dolist (pp pps)
      (is (zerop (length (parsed-program-executable-code
                          (quilc::process-program pp chip :protoquil t))))))))

(deftest test-rz-agglutination-elimination ()
  (let ((chip (quil::build-nq-fully-connected-chip 2))
        (pps (list
              (parse-quil "RZ(1) 0; RZ(-1) 0;")
              (parse-quil "DECLARE theta REAL[1]; RZ(theta) 0; RZ(-theta) 0"))))
    (dolist (pp pps)
      (is (zerop (length (parsed-program-executable-code
                          (quilc::process-program pp chip :protoquil t))))))))


