;;;; resource-tests.lisp
;;;;
;;;; Author: Corwin de Boor

(in-package #:cl-quil-tests)

(defun qr (idx) (cl-quil::make-qubit-resource idx))
(defun make-qubit-resources (indices)
  (reduce #'cl-quil::resource-union (mapcar #'qr indices)))
(defun mk-ranges (ranges &optional names)
  (unless names
    (setf names (make-list (length ranges) :initial-element "")))
  (reduce #'cl-quil::resource-union
          (mapcar (lambda (range name)
                    (cl-quil::make-resource-range name (car range) (cdr range)))
                  ranges
                  names)
          :initial-value (cl-quil::make-null-resource)))

(defparameter *test-ranges-cases*
  ;; first-arg         second-arg        intersect         union     difference
  '((((1 . 2))         ((0 . 1))         ()                ((0 . 2)) ((1 . 2)))
    (((1 . 2))         ((0 . 3))         ((1 . 2))         ((0 . 3)) ())
    (((1 . 3) (4 . 6)) ((2 . 5))         ((2 . 3) (4 . 5)) ((1 . 6)) ((1 . 2) (5 . 6)))
    (((1 . 2))         ((2 . 3))         ()                ((1 . 3)) ((1 . 2)))
    (((3 . 5) (7 . 9)) ((1 . 3) (4 . 7)) ((4 . 5))         ((1 . 9)) ((3 . 4) (7 . 9)))))

(deftest test-ranges ()
  (loop
    :for case :in *test-ranges-cases*
    :for (s1 s2 intersect union difference) := (mapcar #'mk-ranges case)
    :for intersect-p := (not (cl-quil::resource-null-p intersect))
    :for subset-p := (cl-quil::resource-null-p difference)
    :do (is (eql             intersect-p (cl-quil::resources-intersect-p s1 s2)))
        (is (eql             subset-p    (cl-quil::resource-subsetp      s1 s2)))
        (is (cl-quil::resource= intersect   (cl-quil::resource-intersection s1 s2)))
        (is (cl-quil::resource= union       (cl-quil::resource-union        s1 s2)))
        (is (cl-quil::resource= difference  (cl-quil::resource-difference   s1 s2)))))

(defparameter *test-ranges-subset-p-cases*
  '((((1 . 2))                 ((1 . 2))         T)
    (((1 . 2))                 ((0 . 2))         T)
    (((1 . 2))                 ((1 . 3))         T)
    (((1 . 2))                 ((0 . 3))         T)
    (((1 . 3))                 ((0 . 3))         T)
    (((1 . 4))                 ((0 . 3))         NIL)
    (((1 . 2) (3 . 4))         ((0 . 4))         T)
    (((1 . 2) (3 . 4) (6 . 8)) ((0 . 4) (5 . 9)) T)
    (((1 . 2) (3 . 4) (6 . 8)) ((0 . 4) (6 . 9)) T)
    (((1 . 2) (3 . 4) (5 . 8)) ((0 . 4) (6 . 9)) NIL)))

(deftest test-ranges-subset-p ()
  (loop
    :for (s1 s2 result) :in *test-ranges-subset-p-cases*
    :do (is (eql result (cl-quil::resource-subsetp (mk-ranges s1) (mk-ranges s2))))))

(defun resource-eq-check (resource indices)
  (is (cl-quil::resource= resource (make-qubit-resources indices)))
  (dolist (i indices)
    (is (cl-quil::resource-subsetp (qr i) resource))
    (is (cl-quil::resources-intersect-p (qr i) resource))))

(deftest test-resource-operations ()
  (loop
    :repeat 20
    :for l1 := (sort (delete-duplicates (loop :repeat 20 :collect (random 30))) #'<)
    :for l2 := (sort (delete-duplicates (loop :repeat 20 :collect (random 30))) #'<)
    :for r1 := (make-qubit-resources l1)
    :for r2 := (make-qubit-resources l2)
    :do (resource-eq-check (cl-quil::resource-intersection r1 r2)  (intersection   l1 l2))
        (resource-eq-check (cl-quil::resource-union        r1 r2)  (union          l1 l2))
        (resource-eq-check (cl-quil::resource-difference   r1 r2)  (set-difference l1 l2))
        (is (eql           (cl-quil::resources-intersect-p r1 r2)  (if (intersection l1 l2) t nil)))
        (is (eql           (cl-quil::resource-subsetp      r1 r2)  (subsetp l1 l2)))))


(deftest test-resource-difference ()
  (let ((universe (make-qubit-resources (a:iota 30))))
    (loop
      :repeat 20
      :for l := (loop :repeat 20 :collect (random 30))
      :for r := (cl-quil::resource-difference universe (make-qubit-resources l))
      :do (dolist (i l)
            (is (not (cl-quil::resource-subsetp (qr i) r)))))))

(defun check-identities (r1 r2 r3 universe)
  ;; A ∩ B = A - Bᶜ
  (is (cl-quil::resource= (cl-quil::resource-intersection r1 r2)
                       (cl-quil::resource-difference r1 (cl-quil::resource-difference universe r2))))
  ;; (A - B)ᶜ = A or Bᶜ
  (is (cl-quil::resource= (cl-quil::resource-difference universe (cl-quil::resource-difference r1 r2))
                       (cl-quil::resource-union r2 (cl-quil::resource-difference universe r1))))
  ;; A or B = (Aᶜ ∩ Bᶜ)ᶜ
  (is (cl-quil::resource= (cl-quil::resource-union r1 r2)
                       (cl-quil::resource-difference universe
                        (cl-quil::resource-intersection
                         (cl-quil::resource-difference universe r1)
                         (cl-quil::resource-difference universe r2)))))
  ;; A - ∅ = A
  (is (cl-quil::resource= (cl-quil::resource-difference r1 (cl-quil::make-null-resource)) r1))
  ;; (B - A) or C = (B or C) - (A - C)
  (is (cl-quil::resource= (cl-quil::resource-union
                        (cl-quil::resource-difference r2 r1) r3)
                       (cl-quil::resource-difference
                        (cl-quil::resource-union r2 r3)
                        (cl-quil::resource-difference r1 r3)))))

(defun random-resource (range-names)
  "Constructs a random resource, where named ranges have names selected from RANGE-NAMES."
  (reduce
   #'cl-quil::resource-union
   (list*
    (make-qubit-resources (remove-duplicates (loop :repeat (1+ (random 20)) :collect (random 30))))
    (loop :repeat (1+ (random 10))
          :for size := (random 10)
          :for lo := (random (- 50 size))
          :collect (cl-quil::make-resource-range (format nil "~D" (a:random-elt range-names)) lo (+ lo size))))))

(deftest test-resource-identities ()
  (let* ((names (list "a" "b" "c"))
         (universe (cl-quil/resource::make-resource-collection
                    :qubits cl-quil/resource::+full+
                    :memory-regions (mapcar (lambda (name)
                                              (cons name (list (cons most-negative-fixnum most-positive-fixnum))))
                                          names))))
    (loop
      :repeat 20
      :do (check-identities (random-resource names)
                            (random-resource names)
                            (random-resource names)
                            universe))))
