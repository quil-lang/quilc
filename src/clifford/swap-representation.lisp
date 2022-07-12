;;;; src/clifford/swap-representation.lisp
;;;;
;;;; Author: Anthony Polloreno

(in-package #:cl-quil/clifford)

;;; This file contains functions to explore the Clifford group mod the swap group.

(defun clifford-as-vectors (clifford-element)
  "Given a CLIFFORD-ELEMENT return the basis vectors and the images of those basis vectors associated with the basis map it represents. Returns a VECTOR of basis vectors and a VECTOR of their images."
  (let ((basis-vector-images (map 'simple-vector #'identity (basis-map clifford-element)))
        (basis-vectors (map 'simple-vector #'identity (enumerate-pauli-basis (num-qubits clifford-element)))))
    (values basis-vectors basis-vector-images)))


(defun resolve-ambiguity (indices images)
  "Given a sequence of INDICES, return the subset whose pauli matrices have the highest base4 representation in all IMAGES."
  (loop :for image :across images :do
    (let* ((values (loop :for index :in indices :collect (nth index (base4-list image))))
           (max-indices '())
           (max-value (apply #'max values)))
      (loop :for index :in indices :do
        (when (= (nth index (base4-list image)) max-value)
            (push index max-indices)))
      (setq indices max-indices)))
  indices)

(defun first-indices (list)
  (find-if (lambda (sublist) (not (null sublist))) list))

(defun canonical-swap-representative (clifford-element)
  "Returns a canonical representative of this CLIFFORD-ELEMENT, as a coset of the swap group."
  (let* ((num-qubits (num-qubits clifford-element))
         (permutation (perm-identity num-qubits))
         ;; We add four spots to the end for sorting elements
         (new-permutation (make-array (+ 4 num-qubits) :initial-element '())))
    (multiple-value-bind (basis-vectors basis-vector-images)
        (clifford-as-vectors clifford-element)
      (declare (ignore basis-vectors))
      (loop :for image :across basis-vector-images
            :for basis-idx :from 0
            :for base4-list := (base4-list image)
            :for buffer := (make-array (length base4-list)
                                       :initial-contents (permute permutation base4-list))
            :do
               (if (evenp basis-idx)
                   (progn
                     (loop :for el :across buffer
                           :for idx :from 0 :do
                             (if (>= (* 2 idx) basis-idx)
                                 (push (1+ idx) (aref new-permutation (- (+ 4 num-qubits) el 1)))
                                 (push (1+ idx) (aref new-permutation idx))))
                     (let* ((first-idxs (first-indices (subseq new-permutation (1- num-qubits))))
                            (first-idx (car (resolve-ambiguity (map 'list #'1- first-idxs) (subseq basis-vector-images basis-idx))))
                            (permutation-list (perm-to-list permutation)))
                       (cond ((not (= (nth 0 permutation-list) (nth first-idx permutation-list)))
                              (setf permutation (perm-compose permutation (from-cycles (list (make-cycle (nth first-idx permutation-list) (nth 0 permutation-list)))))))
                             (T))))   
                   ;;Prepare for the next iteration
                   (fill new-permutation '() :end (length new-permutation)))
               ;;Now that the total permutation has been computed, apply it to all images of the basis map
               (loop :for image :across basis-vector-images
                     :for basis-idx :from 0 :do
                       (let ((result (copy-seq (base4-list image))))
                         (setf result (permute permutation result))
                         (setf (aref (basis-map clifford-element) basis-idx)
                               (make-pauli (coerce result 'list) (phase-factor image)))))))
    clifford-element))
