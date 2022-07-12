;;;; classical-memory-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil-tests)

(defun binary-memory-tree (n)
  (with-output-to-string (s)
    (labels ((rec (a b parent offset)
               (let ((current (format nil "x~D-~D" a b)))
                 (format s "DECLARE ~A OCTET[~D]~:[~; SHARING ~A~]" current (- b a) parent parent)
                 (unless (zerop offset)
                   (format s " OFFSET ~D OCTET" offset))
                 (terpri s)
                 (when (< 1 (- b a))
                   (rec a (floor (+ a b) 2) current 0)
                   (rec (floor (+ a b) 2) b current (- (floor (+ a b) 2) a))))))
      (rec 0 n nil 0))))

(deftest test-memory-tree-model-building ()
  "Test that we correctly build a model for an OCTET tree of memory."
  (let ((model (memory-descriptors-to-model
                (parsed-program-memory-definitions
                 (parse-quil
                  (binary-memory-tree 128)))
                :alignment 8
                :real-bits 64
                :integer-bits 64)))
    (is (= 1 (length (cl-quil::memory-model-roots model))))
    (is (= 254 (length (cl-quil::memory-model-aliases model))))
    (is (= 255 (hash-table-count (cl-quil::memory-model-names model))))))

(deftest test-bad-inherit-small-from-large ()
  "Test we can't inherit from a smaller memory region to a larger one."
  (let ((baddies '("
DECLARE b BIT
DECLARE ix INTEGER SHARING b
"
                   "
DECLARE o OCTET
DECLARE ox OCTET[2] SHARING o
"
                   "
DECLARE o OCTET
DECLARE b BIT SHARING o OFFSET 8 BIT
"
                   "
DECLARE o OCTET[4]
DECLARE ix INTEGER SHARING o
")))
    (dolist (bad baddies)
      (signals cl-quil:quil-memory-model-error
        (memory-descriptors-to-model
         (parsed-program-memory-definitions
          (parse-quil bad))
         :alignment 8
         :real-bits 64
         :integer-bits 64)))))

#+#:ignore                              ; Not sure if this is needed.
(deftest test-divisible-alignment ()
  "Test that we detect when data types are indivisible in their size to alignment."
  (signals cl-quil:quil-memory-model-error
    (memory-descriptors-to-model nil :alignment 8
                                     :real-bits 9
                                     :integer-bits 8))
  (signals cl-quil:quil-memory-model-error
    (memory-descriptors-to-model nil :alignment 8
                                     :real-bits 8
                                     :integer-bits 9)))

(deftest test-alignment ()
  "Test that alignment seems to work."
  (let* ((ok-programs (list
                       "DECLARE x BIT SHARING mem"
                       "DECLARE x BIT SHARING mem OFFSET 1 BIT"
                       "DECLARE x BIT[8] SHARING mem OFFSET 1 BIT"
                       "DECLARE x OCTET SHARING mem"
                       "DECLARE x INTEGER SHARING mem"
                       "DECLARE x REAL SHARING mem"
                       "DECLARE x OCTET SHARING mem OFFSET 8 BIT"
                       "DECLARE x INTEGER SHARING mem OFFSET 8 BIT"
                       "DECLARE x REAL SHARING mem OFFSET 8 BIT"
                       "DECLARE x OCTET SHARING mem OFFSET 8 BIT 1 OCTET"
                       "DECLARE x INTEGER SHARING mem OFFSET 8 BIT 1 OCTET"
                       "DECLARE x REAL SHARING mem OFFSET 8 BIT 1 OCTET"))
         (not-ok-programs (list
                           "DECLARE x OCTET SHARING mem OFFSET 1 BIT"
                           "DECLARE x INTEGER SHARING mem OFFSET 1 BIT"
                           "DECLARE x REAL SHARING mem OFFSET 1 BIT"
                           ;; what a jokester this guy
                           "
DECLARE x BIT[8] SHARING mem OFFSET 1 BIT   # ok
DECLARE y OCTET SHARING x                   # no no
"
                           )))
    ;; OK
    (dolist (p ok-programs)
      (not-signals cl-quil:quil-memory-model-error
        (memory-descriptors-to-model
         (parsed-program-memory-definitions
          (parse-quil
           (format nil "DECLARE mem OCTET[128]~%~A~%" p)))
         :alignment 8
         :real-bits 64
         :integer-bits 64)))
    ;; NOT OK
    (dolist (p not-ok-programs)
      (signals cl-quil:quil-memory-model-error
        (memory-descriptors-to-model
         (parsed-program-memory-definitions
          (parse-quil
           (format nil "DECLARE mem OCTET[128]~%~A~%" p)))
         :alignment 8
         :real-bits 64
         :integer-bits 64)))))
