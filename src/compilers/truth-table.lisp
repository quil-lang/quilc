;;;; truth-table.lisp
;;;; This file contains the data structure representation for
;;;; manipulating boolean functions.

(in-package #:cl-quil)

(defun missing-arg ()
  (error "Required argument missing."))

;;; This representation is not particularly optimized for efficiency.
(defstruct (truth-table
            (:constructor make-truth-table
                (n-vars
                 &key initial-contents
                 &aux (bits (make-array (ash 1 n-vars) :element-type 'bit :initial-contents initial-contents)))))
  "This structure represents a boolean function {0...2^n-1} -> {0, 1} using a bit vector."
  ;; Is this slot actually necessary?
  (n-vars (missing-arg) :type fixnum)
  (bits (missing-arg) :type simple-bit-vector))

(defun truth-table-has-var (truth-table var)
  "Check if truth table depends on VAR."
  (declare (type truth-table truth-table))
  (let ((bits (truth-table-bits truth-table)))
    (dotimes (i (length bits) nil)
      (when (/= (aref bits i)
                (aref bits (logxor i (ash 1 var))))
        (return t)))))

(defun truth-table-swap! (truth-table var1 var2)
  (declare (type truth-table truth-table))
  (unless (= var1 var2)
    (let ((bits (truth-table-bits truth-table)))
      (dotimes (i (length bits) nil)
        (when (and (logbitp var1 i)
                   (not (logbitp var2 i)))
          (rotatef (aref bits i)
                   (aref bits (logxor i (+ (ash 1 var1)
                                           (ash 1 var2)))))))))
  truth-table)

(defun truth-table-minimize-base! (truth-table)
  "Return the minimal functional support indices of TRUTH-TABLE and reorder the truth table such that the support are indexed by 0...k where k is the amount of support."
  (declare (type truth-table truth-table))
  (let ((supports '())
        (k 0))
    (dotimes (i (truth-table-n-vars truth-table))
      (when (truth-table-has-var truth-table i)
        (when (< k i)
          (truth-table-swap! truth-table k i))
        (push i supports)
        (incf k)))
    (setf (truth-table-n-vars truth-table) k)
    (setf (truth-table-bits truth-table)
          (adjust-array (truth-table-bits truth-table)
                        (ash 1 k)))
    (values truth-table supports)))

(defun truth-table-zero-p (truth-table)
  "Check if the truth table represents the constant 0 function."
  (declare (type truth-table truth-table))
  (not (find 1 (truth-table-bits truth-table))))

(defun truth-table-one-p (truth-table)
  "Check if the truth table represents the constant 1 function."
  (declare (type truth-table truth-table))
  (not (find 0 (truth-table-bits truth-table))))

(defun truth-table-cofactor0 (truth-table index)
  "Find the 0-cofactor of TRUTH-TABLE with respect to INDEX."
  (declare (type truth-table truth-table))
  (make-truth-table
   (truth-table-n-vars truth-table)
   :initial-contents
   (let ((bits (truth-table-bits truth-table)))
     (loop for k from 0 below (length bits)
           collect (aref bits (logandc2 k (ash 1 index)))))))

(defun truth-table-cofactor1 (truth-table index)
  "Find the 1-cofactor of the TRUTH-TABLE with respect to INDEX."
  (declare (type truth-table truth-table))
  (make-truth-table
   (truth-table-n-vars truth-table)
   :initial-contents
   (let ((bits (truth-table-bits truth-table)))
     (loop for k from 0 below (length bits)
           collect (aref bits (logior k (ash 1 index)))))))

(defun truth-table-xor (truth-table1 truth-table2)
  (declare (type truth-table truth-table1 truth-table2))
  (let ((n-vars (truth-table-n-vars truth-table1)))
    (assert (= n-vars (truth-table-n-vars truth-table2)))
    (make-truth-table
     n-vars
     :initial-contents (bit-xor (truth-table-bits truth-table1)
                                (truth-table-bits truth-table2)))))

(defun make-cube ()
  "Create a cube, representing the product of boolean variables. 0 means not present, +/-1 represent polarity or sign of the boolean variable in the cube."
  (make-array 0))

(defun cube-add-literal (cube index polarity)
  (declare (type (member -1 1) polarity))
  (let* ((new-length (max (length cube) (1+ index)))
         (new-cube (make-array new-length :initial-element 0)))
    (loop for i from 0
          for trit across cube
          do (setf (aref new-cube i) trit))
    (setf (aref new-cube index) polarity)
    new-cube))

;;; This algorithm applies recursively the positive Davio decomposition
;;; which eventually leads into the PPRM representation of a
;;; function. An ESOP (Exclusive Sum Of Products) is represented by a
;;; list of cubes.
(defun truth-table-esop-from-pprm (truth-table)
  "Given a truth table, return a list of cubes which when disjoined represent the PPRM representation of the boolean function encoded by TRUTH-TABLE."
  (let ((cubes (make-hash-table :test #'equalp)))
    (labels ((decompose (table index cube)
               ;; terminal cases
               (cond ((truth-table-zero-p table))
                     ((truth-table-one-p table)
                      (setf (gethash cube cubes) t))
                     (t
                      (let ((cofactor0 (truth-table-cofactor0 table index))
                            (cofactor1 (truth-table-cofactor1 table index)))
                        (decompose cofactor0 (1+ index) cube)
                        (decompose (truth-table-xor cofactor0 cofactor1)
                                   (1+ index)
                                   (cube-add-literal cube index 1)))))))
      (decompose truth-table 0 (make-cube))
      (a:hash-table-keys cubes))))
