;;;; resource-tests.lisp
;;;;
;;;; Author: Corwin de Boor

(in-package #:cl-quil-tests)

(defun qr (idx) (quil::make-qubit-resource idx))
(defun make-qubit-resources (indices)
  (reduce #'quil::resource-union (mapcar #'qr indices)))
(defun mk-ranges (ranges &optional names)
  (unless names
    (setf names (make-list (length ranges) :initial-element "")))
  (reduce #'quil::resource-union
          (mapcar (lambda (range name)
                    (quil::make-resource-range name (car range) (cdr range)))
                  ranges
                  names)
          :initial-value (quil::make-null-resource)))

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
    :for intersect-p := (not (quil::resource-null-p intersect))
    :for subset-p := (quil::resource-null-p difference)
    :do (is (eql             intersect-p (quil::resources-intersect-p s1 s2)))
        (is (eql             subset-p    (quil::resource-subsetp      s1 s2)))
        (is (quil::resource= intersect   (quil::resource-intersection s1 s2)))
        (is (quil::resource= union       (quil::resource-union        s1 s2)))
        (is (quil::resource= difference  (quil::resource-difference   s1 s2)))))

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
    :do (is (eql result (quil::resource-subsetp (mk-ranges s1) (mk-ranges s2))))))

(defun resource-eq-check (resource indices)
  (is (quil::resource= resource (make-qubit-resources indices)))
  (dolist (i indices)
    (is (quil::resource-subsetp (qr i) resource))
    (is (quil::resources-intersect-p (qr i) resource))))

(deftest test-resource-operations ()
  (loop
    :repeat 20
    :for l1 := (sort (delete-duplicates (loop :repeat 20 :collect (random 30))) #'<)
    :for l2 := (sort (delete-duplicates (loop :repeat 20 :collect (random 30))) #'<)
    :for r1 := (make-qubit-resources l1)
    :for r2 := (make-qubit-resources l2)
    :do (resource-eq-check (quil::resource-intersection r1 r2)  (intersection   l1 l2))
        (resource-eq-check (quil::resource-union        r1 r2)  (union          l1 l2))
        (resource-eq-check (quil::resource-difference   r1 r2)  (set-difference l1 l2))
        (is (eql           (quil::resources-intersect-p r1 r2)  (if (intersection l1 l2) t nil)))
        (is (eql           (quil::resource-subsetp      r1 r2)  (subsetp l1 l2)))))


(deftest test-resource-difference ()
  (let ((universe (make-qubit-resources (alexandria:iota 30))))
    (loop
      :repeat 20
      :for l := (loop :repeat 20 :collect (random 30))
      :for r := (quil::resource-difference universe (make-qubit-resources l))
      :do (dolist (i l)
            (is (not (quil::resource-subsetp (qr i) r)))))))

(defun check-identities (r1 r2 r3 universe)
  ;; A ∩ B = A - Bᶜ
  (is (quil::resource= (quil::resource-intersection r1 r2)
                       (quil::resource-difference r1 (quil::resource-difference universe r2))))
  ;; (A - B)ᶜ = A or Bᶜ
  (is (quil::resource= (quil::resource-difference universe (quil::resource-difference r1 r2))
                       (quil::resource-union r2 (quil::resource-difference universe r1))))
  ;; A or B = (Aᶜ ∩ Bᶜ)ᶜ
  (is (quil::resource= (quil::resource-union r1 r2)
                       (quil::resource-difference universe
                        (quil::resource-intersection
                         (quil::resource-difference universe r1)
                         (quil::resource-difference universe r2)))))
  ;; A - ∅ = A
  (is (quil::resource= (quil::resource-difference r1 (quil::make-null-resource)) r1))
  ;; (B - A) or C = (B or C) - (A - C)
  (is (quil::resource= (quil::resource-union
                        (quil::resource-difference r2 r1) r3)
                       (quil::resource-difference
                        (quil::resource-union r2 r3)
                        (quil::resource-difference r1 r3)))))

(defun random-resource (range-names)
  "Constructs a random resource, where named ranges have names selected from RANGE-NAMES."
  (reduce
   #'quil::resource-union
   (list*
    (make-qubit-resources (remove-duplicates (loop :repeat (1+ (random 20)) :collect (random 30))))
    (loop :repeat (1+ (random 10))
          :for size := (random 10)
          :for lo := (random (- 50 size))
          :collect (quil::make-resource-range (format nil "~d" (alexandria:random-elt range-names)) lo (+ lo size))))))

(deftest test-resource-identities ()
  (let* ((names (list "a" "b" "c"))
         (universe (cl-quil.resource::make-resource-collection
                    :qubits cl-quil.resource::+full+
                    :memory-regions (mapcar (lambda (name)
                                              (cons name (list (cons most-negative-fixnum most-positive-fixnum))))
                                          names))))
    (loop
      :repeat 20
      :do (check-identities (random-resource names)
                            (random-resource names)
                            (random-resource names)
                            universe))))
