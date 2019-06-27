;;;; sk-compile.lisp
;;;;
;;;; Author: Mark Skilbeck

(in-package :cl-quil)

(defun matrix-trace (m)
  (loop :for i :below (magicl:matrix-cols m)
        :sum (magicl:ref m i i)))

(defun fidelity (m)
  (let ((p (* 2 (log (magicl:matrix-cols m) 2))))
    (/ (+ (expt (abs (matrix-trace m)) 2) p)
       (+ (expt p 2) p))))

(defun d (u s)
  (- 1 (fidelity (magicl:multiply-complex-matrices
                  (magicl:conjugate-transpose s)
                  u))))

(defvar +native-gateset+
  (alexandria:alist-hash-table
   (list
    (cons "H" (gate-matrix (gate-definition-to-gate (lookup-standard-gate "H"))))
    (cons "T" (gate-matrix (gate-definition-to-gate (lookup-standard-gate "T"))))
    (cons "T'" (magicl:conjugate-transpose
                (gate-matrix (gate-definition-to-gate (lookup-standard-gate "T"))))))
   :test #'equalp)
  "native gateset")

(defun gate-string-to-matrix-sequence (gate-name)
  (loop :for gate :in (uiop:split-string gate-name)
        :unless (string= gate " ")
          :collect (gethash gate +native-gateset+)))

(defun operator-string (op &optional preop)
  (concatenate 'string op " " preop))

(defun operator-string-to-matrix (op)
  (reduce #'magicl:multiply-complex-matrices
          (gate-string-to-matrix-sequence op)))

(defun all-operator-combinations-strings (max-length)
  (loop :with native := '("H" "T" "T'")
        :with result := '()
        :with prev := (copy-list native)
        :repeat (1- max-length) :do
          (setf prev (loop :for p :in prev
                           :append (mapcar (lambda (s) (operator-string s p))
                                           native)))
          (setf result (append result prev))
        :finally (return (append native result))))

(defun sk-brutus-basic-approximation (u &key (gateset +native-gateset+) (max-length 8) (e0 0.001) (early t))
  (declare (optimize (debug 3)))
  (loop :with smallest-diff := most-positive-fixnum
        :with best-gate := "I"
        :for gate-string :in (all-operator-combinations-strings max-length)
        :for gate := (operator-string-to-matrix gate-string)
        :for diff := (d u gate)
        :when (< diff smallest-diff) :do
          (setf smallest-diff diff)
          (setf best-gate gate-string)
        :when (and early (< smallest-diff e0)) :do
          (return (list :diff smallest-diff :gate-string best-gate))
        :finally
           (return (list :diff smallest-diff :gate-string best-gate))))

;; (defun sk-basic-approximation (u &keyword (gateset +native-gateset+) (e0 0.14) (l0 16))
;;   )
