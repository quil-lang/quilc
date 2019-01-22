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
    (dolist (type (list :zyz :zxz :yzy :yxy :xyx :xzx))
      (let* ((compiled-program (cl-quil::euler-compiler (build-anonymous-gate master-matrix 0)
                                                        :target type))
             (compiled-matrix (magicl:diag 2 2 (list 1d0 1d0))))
        (loop :for instr :in compiled-program :do
           (setf compiled-matrix (quil::apply-gate compiled-matrix instr)))
        (is (loop :for i :from 0 :to 1 :always
               (loop :for j :from 0 :to 1 :always
                  (< (abs (- (magicl:ref compiled-matrix i j) (magicl:ref master-matrix i j))) 0.01)))
            "Euler translation test failed: ~a~%" type)))))

(defmacro build-translator-test (translator-fn input-instr)
  "Designs a test for TRANSLATOR-FN, which swallows the single INPUT-INSTR gate and emits an equivalent list of instructions."
  `(let* ((input-instr ,input-instr)
          (old-matrix (quil::apply-gate (magicl:diag 2 2 (list 1d0 1d0)) input-instr))
          (translator-output (,translator-fn input-instr))
          (new-matrix (quil::make-matrix-from-quil translator-output)))
     (setf new-matrix (quil::scale-out-matrix-phases new-matrix old-matrix))
     (is (loop :for i :below (magicl:matrix-rows new-matrix) :always
            (loop :for j :below (magicl:matrix-cols new-matrix) :always
               (< (abs (- (magicl:ref new-matrix i j) (magicl:ref old-matrix i j))) 0.01)))
         "On pass: ~a~%Goal matrix:~%~a~%Constructed matrix:~%~a~%"
         (symbol-name ',translator-fn)
         (with-output-to-string (s)
           (print-matrix old-matrix s))
         (with-output-to-string (s)
           (print-matrix new-matrix s)))))

(deftest test-translators ()
  "Tests the various hard-coded translators."
  (build-translator-test quil::H-to-YX        (cl-quil::build-gate "H"      ()             0))
  (build-translator-test quil::Z-to-XYX       (cl-quil::build-gate "Z"      ()             0))
  (build-translator-test quil::Yhalf-to-HX    (cl-quil::build-gate "RY"     '(#.(/ pi 2))  0))
  (build-translator-test quil::RY-to-XZX      (cl-quil::build-gate "RY"     '(#.(/ pi -9)) 0))
  (build-translator-test quil::CZ-to-CPHASE   (cl-quil::build-gate "CZ"     ()             0 1))
  (build-translator-test quil::CPHASE-to-CNOT (cl-quil::build-gate "CPHASE" '(#.(/ pi 7))  0 1))
  (build-translator-test quil::CNOT-to-CZ     (cl-quil::build-gate "CNOT"   ()             0 1))
  (build-translator-test quil::iSWAP-to-CNOT  (cl-quil::build-gate "ISWAP"  ()             0 1))
  (build-translator-test quil::CZ-to-CNOT     (cl-quil::build-gate "CZ"     ()             0 1))
  (build-translator-test quil::iSWAP-to-PSWAP (cl-quil::build-gate "ISWAP"  ()             0 1))
  (build-translator-test quil::PSWAP-to-CNOT  (cl-quil::build-gate "PSWAP"  '(#.(/ pi 5))  0 1))
  (build-translator-test quil::CNOT-to-iSWAP  (cl-quil::build-gate "CNOT"   ()             0 1))
  (build-translator-test quil::SWAP-to-CNOT   (cl-quil::build-gate "SWAP"   ()             0 1))
  (build-translator-test quil::SWAP-to-CZ     (cl-quil::build-gate "SWAP"   ()             0 1))
  (build-translator-test quil::SWAP-to-PSWAP  (cl-quil::build-gate "SWAP"   ()             0 1))
  (build-translator-test quil::SWAP-to-iSWAP  (cl-quil::build-gate "SWAP"   ()             0 1)))

(defun random-permutation (list)
  (unless (null list)
    (let ((index (random (length list))))
      (cons (nth index list)
            (random-permutation (remove (nth index list) list))))))

(deftest test-ucr-recognition ()
  (dolist (roll-type (list "RY" "RZ"))
    (let* ((qubit-count 4)
           (argument-list (random-permutation (mapcar #'qubit (alexandria:iota qubit-count))))
           (angle-list (mapcar (lambda (x)
                                 (declare (ignore x))
                                 (constant (random (* 2 pi))))
                               (make-list (expt 2 (1- qubit-count)))))
           (ucr-instruction (make-instance 'quil::ucr-application
                                           :roll-type roll-type
                                           :arguments argument-list
                                           :parameters angle-list))
           (ucr-matrix (quil::make-matrix-from-quil (list ucr-instruction)))
           (anonymous-instr (make-instance 'quil::gate-application
                                           :operator #.(named-operator "ANONYMOUS-UCR")
                                           :arguments (mapcar #'qubit (nreverse (alexandria:iota qubit-count)))
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
        (format *debug-io* "    Testing global-to-local ~a expansion~%" operator)
        (let* ((instr (quil::build-gate operator params 0 3))
               (ref-mat (cl-quil::make-matrix-from-quil (list instr)))
               (mat (cl-quil::make-matrix-from-quil (funcall expander chip-spec instr))))
          (is (cl-quil::matrix-equality ref-mat
                                        (cl-quil::scale-out-matrix-phases mat ref-mat))))))))
