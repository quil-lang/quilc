;;;; permutation-tests.lisp
;;;;
;;;; Author: Charles Zhang

(in-package #:cl-quil-tests)

(a:define-constant +prime+ #(0 2 3 5 7 1 4 6) :test #'equalp)

(defun matrix-from-permutation (permutation)
  (let* ((size (length permutation))
         (matrix (magicl:zeros (list size size) :type '(complex double-float))))
    (loop :for i :from 0
          :for j :across permutation
          :do (setf (magicl:tref matrix i j) 1))
    matrix))

(defun qubits-in-computational-order (n)
  (nreverse (coerce (a:iota n) 'vector)))

(defun permutation-synthesis-as-parsed-program (permutation)
  (make-instance 'cl-quil::parsed-program
    :executable-code (coerce (cl-quil::synthesize-permutation
                              permutation
                              (qubits-in-computational-order (cl-quil/frontend::ilog2 (length permutation))))
                             'vector)))

;;; Test that the synthesized permutation when simulated performs the
;;; action of the permutation.
(defun synthesize-and-check-permutation (permutation)
  (cl-quil::operator=
   (cl-quil:parsed-program-to-logical-matrix
    (permutation-synthesis-as-parsed-program permutation)
    :compress-qubits nil)
   (matrix-from-permutation permutation)))

(deftest test-permutation-gates-logical-matrix-equivalent-examples ()
  (let ((perms (list #(0 1)
                     #(1 0)
                     #(3 0 1 2)
                     #(2 0 3 1)
                     #(1 0 3 2)
                     #(0 2 3 1)
                     #(2 1 3 0)
                     #(3 1 2 0)
                     +prime+)))
    (dolist (p perms)
      (is (synthesize-and-check-permutation p)))))

(defun cl-perm-to-vec (p)
  (map 'vector #'1- (cl-permutation:perm-to-vector p)))

(deftest test-permutation-gates-logical-matrix-equivalent-big ()
  (loop :for i :from 1 :to 3
        :for n := (expt 2 i)
        :for start := (get-internal-real-time)
        :do (format t "~&Testing permutations of dimension ~D..." n)
            (cl-permutation:doperms (p n)
              (is (synthesize-and-check-permutation (cl-perm-to-vec p))))
            (format t " done [~D ms]~%" (round (* 1000 (- (get-internal-real-time) start))
                                               internal-time-units-per-second))))

(deftest test-perm-compilation-gh805 ()
  (let* ((chip (cl-quil::build-nq-linear-chip 3 :architecture ':cnot))
         (orig-prog (cl-quil::parse-quil "
DEFGATE PERM AS PERMUTATION:
    5, 1, 2, 6, 7, 0, 4, 3

X 0
PERM 0 1 2
"))
         (orig-matrix (cl-quil:parsed-program-to-logical-matrix orig-prog))
         (proc-prog (cl-quil::compiler-hook orig-prog chip))
         (proc-matrix (cl-quil:parsed-program-to-logical-matrix proc-prog))
         (2q-code (program-2q-instructions proc-prog)))
    (is (cl-quil::matrix-equals-dwim orig-matrix proc-matrix))
    (is (every (link-nativep chip) 2q-code))))

(deftest test-random-3q-perm-compilations ()
  (flet ((perm->prog (perm)
           (format nil "
DEFGATE PERM AS PERMUTATION:
    ~{~D~^, ~}

X 0
PERM 0 1 2
"
                   perm)))
    (let ((chip (cl-quil::build-nq-linear-chip 3 :architecture ':cnot))
          (*print-pretty* nil))
      (loop :repeat 16 :do
        (let* ((perm (alexandria:shuffle (alexandria:iota 8)))
               (orig-prog (cl-quil::parse-quil (perm->prog perm)))
               (orig-matrix (cl-quil:parsed-program-to-logical-matrix orig-prog))
               (proc-prog (cl-quil::compiler-hook orig-prog chip))
               (proc-matrix (cl-quil:parsed-program-to-logical-matrix proc-prog))
               (2q-code (program-2q-instructions proc-prog)))
          (format t "    Testing compiling perm ~A...~%" perm)
          (is (cl-quil::matrix-equals-dwim orig-matrix proc-matrix))
          (is (every (link-nativep chip) 2q-code)))))))

