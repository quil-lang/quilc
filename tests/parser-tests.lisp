;;;; tests/parser-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil-tests)

(defparameter *good-test-file-directory*
  (asdf:system-relative-pathname
   ':cl-quil-tests
   "tests/good-test-files/"))

(defparameter *bad-test-file-directory*
  (asdf:system-relative-pathname
   ':cl-quil-tests
   "tests/bad-test-files/"))

(deftest test-parsing-good-test-files ()
  "Test whether all valid test files parse."
  (dolist (file (uiop:directory-files *good-test-file-directory* #P"*.quil"))
    (format *debug-io* "~&    Testing good file ~A~%" (pathname-name file))
    (let ((cl-quil:*allow-unresolved-applications* t))
      (not-signals quil-parse-error
        (cl-quil:read-quil-file file)))))

(deftest test-parsing-bad-test-files ()
  "Test whether all invalid test files signal a parse error."
  (dolist (file (uiop:directory-files *bad-test-file-directory* #P"*.quil"))
    (format *debug-io* "~&    Testing bad file ~A~%" (pathname-name file))
    (let ((cl-quil:*allow-unresolved-applications* t))
      (signals quil:quil-parse-error
        (handler-case (cl-quil:read-quil-file file)
          ;; Re-signal all of the following errors as
          ;; QUIL-PARSE-ERRORs.
          (quil:quil-type-error (c)
            (declare (ignore c))
            (error 'quil:quil-parse-error))
          (yacc:yacc-parse-error (c)
            (declare (ignore c))
            (error 'quil:quil-parse-error))
          (alexa:lexer-match-error (c)
            (declare (ignore c))
            (error 'quil:quil-parse-error)))))))

(deftest test-semicolon-parsing ()
  (let ((pp1 (parse-quil "H 0 ; X 1"))
        (pp2 (parse-quil "DEFCIRCUIT foo:
    X 0 ; H 0")))
    (is (= 2 (length (parsed-program-executable-code pp1))))
    (is (= 2 (length (quil::circuit-definition-body
                      (first (parsed-program-circuit-definitions pp2))))))))

(deftest test-pragma-parsing ()
  (let* ((p (with-output-to-quil
              "PRAGMA a"
              "PRAGMA a b"
              "PRAGMA a b c \"str\""
              "PRAGMA a 5 b c"
              "PRAGMA a 5 \"str\""
              ))
         (code (coerce (parsed-program-executable-code p) 'list)))
    (is (= 5 (length code)))
    (destructuring-bind (one two three four five) code
      (is (equalp '("a") (pragma-words one)))
      (is (null (pragma-freeform-string one)))

      (is (equalp '("a" "b") (pragma-words two)))
      (is (null (pragma-freeform-string two)))

      (is (equalp '("a" "b" "c") (pragma-words three)))
      (is (string= "str" (pragma-freeform-string three)))

      (is (equalp '("a" 5 "b" "c") (pragma-words four)))
      (is (null (pragma-freeform-string four)))

      (is (equalp '("a" 5) (pragma-words five)))
      (is (string= "str" (pragma-freeform-string five))))))

(deftest test-pragma-specialization ()
  (let* ((p (with-output-to-quil
              "PRAGMA EXPECTED_REWIRING \"#(3 4 0 1 2)\""
              "PRAGMA READOUT-POVM 3 \"(0.9 0.2 0.1 0.8)\""
              "PRAGMA ADD-KRAUS X 2 \"(1.0 0.0 0.0 1.0)\""
              "PRAGMA ADD-KRAUS X 2 \"(1.0 0.0 0.0 -i)\""))
         (code (parsed-program-executable-code p)))

    ;; first pragma
    (let ((instr (aref code 0)))
      (is (typep instr 'quil::pragma-expected-rewiring))
      (is (and (typep (quil::pragma-rewiring instr) 'quil::rewiring))))

    ;; second pragma
    (let ((instr (aref code 1)))
      (is (typep instr 'quil::pragma-readout-povm))
      (is (typep (quil::pragma-qubit-index instr) 'integer))
      (is (and (typep (quil::pragma-matrix-entries instr) 'list)
               (every (lambda (e) (typep e 'double-float))
                      (quil::pragma-matrix-entries instr)))))

    ;; third pragma
    (let ((instr (aref code 2)))
      (is (typep instr 'quil::pragma-add-kraus))
      (is (typep (quil::pragma-operator-name instr) 'string))
      (is (and (typep (quil::pragma-qubit-arguments instr) 'list)
               (every #'integerp (quil::pragma-qubit-arguments instr))))
      (is (and (typep (quil::pragma-matrix-entries instr) 'list)
               (every (lambda (e) (typep e '(complex double-float)))
                      (quil::pragma-matrix-entries instr)))))

        ;; fourth pragma
    (let ((instr (aref code 3)))
      (is (typep instr 'quil::pragma-add-kraus))
      (is (typep (quil::pragma-operator-name instr) 'string))
      (is (and (typep (quil::pragma-qubit-arguments instr) 'list)
               (every #'integerp (quil::pragma-qubit-arguments instr))))
      (is (and (typep (quil::pragma-matrix-entries instr) 'list)
               (every (lambda (e) (typep e '(complex double-float)))
                      (quil::pragma-matrix-entries instr)))))))

(deftest test-parse-element-list ()
  (let* ((element-list "(1 2.0 3.0e3 4.0i 5+6i 7+8.0i 9.0E-10i -i i pi -pi)")
         (parsed-list (cl-quil::parse-element-list element-list 11)))
    (is (equalp
         parsed-list
         (list
          #C(1d0 0d0)
          #C(2d0 0d0)
          #C(3d3 0d0)
          #C(0d0 4d0)
          #C(5d0 6d0)
          #C(7d0 8d0)
          #C(0d0 9d-10)
          #C(0d0 -1d0)
          #C(0d0 1d0)
          (complex pi 0d0)
          (complex (- pi) 0d0))))))

(deftest test-parse-reset ()
  (let* ((p (with-output-to-quil
              "RESET"
              "RESET 5"
              "DEFCIRCUIT YO qq:"
              "    RESET qq"
              "RESET"))
         (code (parsed-program-executable-code p)))
    (is (typep (aref code 0) 'reset))
    (is (equalp (reset-qubit-target (aref code 1))
                (qubit 5)))))

(deftest test-parse-dagger-dagger ()
  (let* ((p (with-output-to-quil
              "DAGGER H 0"
              "DAGGER DAGGER H 0"
              "DAGGER DAGGER DAGGER H 0"))
         (code (parsed-program-executable-code p)))
    (destructuring-bind (instr-dagger^1
                         instr-dagger^2
                         instr-dagger^3)
        (mapcar (a:compose #'quil::operator-description-string
                           #'quil:application-operator)
                (coerce code 'list))
      (is (string= "DAGGER H" instr-dagger^1))
      (is (string= "H" instr-dagger^2))
      (is (string= "DAGGER H" instr-dagger^3)))))

(deftest test-defgate-as-matrix ()
  (let* ((quil "
DEFGATE TEST AS MATRIX:
    exp(2*i), 0
    0, exp(4*i)

TEST 0")
         (quil-parsed (not-signals quil-parse-error (parse-quil quil))))
    (let ((gates (parsed-program-gate-definitions quil-parsed)))
      (is (= 1 (length gates)))
      (is (typep (first gates) 'quil::static-gate-definition)))))

(deftest test-defgate-as-permutation ()
  (let ((quil "
DEFGATE TEST AS PERMUTATION:
    0, 1, 2, 3, 4, 5, 7, 6

TEST 0 1 2")
        (quil-bad "
DEFGATE TEST AS PERMUTATION:
    0, 1, 2, 3, 4, 5, 7, 6, 8

TEST 0 1 2"))
    (let* ((quil-parsed (not-signals quil-parse-error (quil::parse-quil quil)))
           (gates (parsed-program-gate-definitions quil-parsed)))
      (is (= 1 (length gates)))
      (is (typep (first gates) 'quil::permutation-gate-definition)))
    (signals quil-parse-error (parse-quil quil-bad))))

(deftest test-parsing-process-include ()
  (uiop:with-temporary-file (:stream stream :pathname path)
    (format stream "DECLARE foo BIT~@
                    MEASURE 0 foo")
    (force-output stream)
    (let* ((test-quil (format nil "H 0~@
                                   INCLUDE \"~A\"" path))
           (pp (parse-quil test-quil)))
      (is (= 1 (length (parsed-program-memory-definitions pp)))))))

(deftest test-parsing-include-declaration-collision ()
  (uiop:with-temporary-file (:stream stream :pathname path)
    (format stream "DECLARE foo BIT~@
                    MEASURE 0 foo")
    (force-output stream)
    (let* ((test-quil (format nil "DECLARE foo BIT~@
                                   H 0~@
                                   INCLUDE \"~A\"" path)))
      (signals quil-parse-error (parse-quil test-quil)))))

(deftest test-parsing-include-defgate-ambiguity ()
  (uiop:with-temporary-file (:stream stream :pathname path)
    (format stream "
DEFGATE FOO:
    1/sqrt(2), 1/sqrt(2)
    1/sqrt(2), -1/sqrt(2)")
    (force-output stream)
    (let* ((test-quil (format nil "
DEFGATE FOO:
    1/sqrt(2), 1/sqrt(2)
    1/sqrt(2), -1/sqrt(2)

INCLUDE \"~A\"" path)))
      (signals quil::ambiguous-gate-or-circuit-definition
        (parse-quil test-quil :ambiguous-definition-handler #'identity)))))

(deftest test-parsing-defgate-defcircuit-ambiguity ()
  (let ((test-quil "
DEFGATE FOO(%a):
    1/sqrt(2), 1/sqrt(2)
    1/sqrt(2), -1/sqrt(2)

DEFCIRCUIT FOO(%a) q v:
    X q
"))
    (signals quil::ambiguous-gate-or-circuit-definition
      (parse-quil test-quil :ambiguous-definition-handler #'identity))))

(deftest test-parsing-multiple-includes-good ()
  (uiop:with-temporary-file (:stream stream :pathname path)
    (format stream "X 0")
    (force-output stream)
    (let* ((test-quil (format nil "H 0~@
                                   INCLUDE \"~A\"~@
                                   INCLUDE \"~A\""
                              path
                              path))
           (pp (parse-quil test-quil)))
      (is (= 3 (length (parsed-program-executable-code pp)))))))

(deftest test-parsing-cyclic-include ()
  (uiop:with-temporary-file (:stream stream1 :pathname path1)
    (uiop:with-temporary-file (:stream stream2 :pathname path2)
      (format stream1 "X 0;INCLUDE \"~A\"" path2)
      (format stream2 "X 0;INCLUDE \"~A\"" path1)
      (force-output stream1)
      (force-output stream2)
      (signals quil-parse-error
        (parse-quil (read-quil-file path1) :originating-file path1))
      (signals quil-parse-error
        (parse-quil (read-quil-file path1))))))
