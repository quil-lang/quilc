(in-package #:cl-quil-tests)

(defun %filter-halt (pp)
  (remove-if #'cl-quil::haltp
             (parsed-program-executable-code pp)))

(deftest test-rz-full-rotation-elimination ()
  ;; Test the compiler directly.
  (is (null (cl-quil::eliminate-full-rz-rotations (cl-quil::build-gate "RZ" (list 0d0) 0))))
  (is (null (cl-quil::eliminate-full-rz-rotations (cl-quil::build-gate "RZ" (list (cl-quil::constant 0d0)) 0))))

  ;; Test that the compiler takes effect via compiler-hook.
  (let ((chip (cl-quil::build-nq-fully-connected-chip 2))
        (pps (list (cl-quil::parse-quil "DECLARE theta REAL[1]; RZ(0.0) 0;")
                   (cl-quil::parse-quil "DECLARE theta REAL[1]; RZ(2*pi) 0;")
                   (cl-quil::parse-quil "DECLARE theta REAL[1]; RZ(-2*pi) 0;"))))
    (dolist (pp pps)
      (is (zerop (length (%filter-halt (cl-quil::compiler-hook pp chip))))))))

(deftest test-rz-agglutination-elimination ()
  (let ((chip (cl-quil::build-nq-fully-connected-chip 2))
        (pps (list
              (parse-quil "RZ(1) 0; RZ(-1) 0;")
              (parse-quil "DECLARE theta REAL[1]; RZ(theta) 0; RZ(-theta) 0"))))
    (dolist (pp pps)
      (is (zerop (length (%filter-halt (cl-quil::compiler-hook pp chip))))))))


