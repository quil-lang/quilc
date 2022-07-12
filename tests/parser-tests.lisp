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
    (format t "~&    Testing good file ~A~%" (pathname-name file))
    (let ((cl-quil:*allow-unresolved-applications* t))
      (not-signals quil-parse-error
        (cl-quil:read-quil-file file)))))

(deftest test-parsing-bad-test-files ()
  "Test whether all invalid test files signal a parse error."
  (dolist (file (uiop:directory-files *bad-test-file-directory* #P"*.quil"))
    (format t "~&    Testing bad file ~A~%" (pathname-name file))
    (let ((cl-quil:*allow-unresolved-applications* t))
      (signals cl-quil:quil-parse-error
        (handler-case (cl-quil:read-quil-file file)
          ;; Re-signal all of the following errors as
          ;; QUIL-PARSE-ERRORs.
          (cl-quil:quil-type-error (c)
            (declare (ignore c))
            (error 'cl-quil:quil-parse-error))
          (yacc:yacc-parse-error (c)
            (declare (ignore c))
            (error 'cl-quil:quil-parse-error))
          (alexa:lexer-match-error (c)
            (declare (ignore c))
            (error 'cl-quil:quil-parse-error)))))))

(deftest test-semicolon-parsing ()
  (let ((pp1 (parse-quil "H 0 ; X 1"))
        (pp2 (parse-quil "DEFCIRCUIT foo:
    X 0 ; H 0")))
    (is (= 2 (length (parsed-program-executable-code pp1))))
    (is (= 2 (length (cl-quil::circuit-definition-body
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
      (is (typep instr 'cl-quil::pragma-expected-rewiring))
      (is (and (typep (cl-quil::pragma-rewiring instr) 'cl-quil::rewiring))))

    ;; second pragma
    (let ((instr (aref code 1)))
      (is (typep instr 'cl-quil::pragma-readout-povm))
      (is (typep (cl-quil::pragma-qubit-index instr) 'integer))
      (is (and (typep (cl-quil::pragma-matrix-entries instr) 'list)
               (every (lambda (e) (typep e 'double-float))
                      (cl-quil::pragma-matrix-entries instr)))))

    ;; third pragma
    (let ((instr (aref code 2)))
      (is (typep instr 'cl-quil::pragma-add-kraus))
      (is (typep (cl-quil::pragma-operator-name instr) 'string))
      (is (and (typep (cl-quil::pragma-qubit-arguments instr) 'list)
               (every #'integerp (cl-quil::pragma-qubit-arguments instr))))
      (is (and (typep (cl-quil::pragma-matrix-entries instr) 'list)
               (every (lambda (e) (typep e '(complex double-float)))
                      (cl-quil::pragma-matrix-entries instr)))))

        ;; fourth pragma
    (let ((instr (aref code 3)))
      (is (typep instr 'cl-quil::pragma-add-kraus))
      (is (typep (cl-quil::pragma-operator-name instr) 'string))
      (is (and (typep (cl-quil::pragma-qubit-arguments instr) 'list)
               (every #'integerp (cl-quil::pragma-qubit-arguments instr))))
      (is (and (typep (cl-quil::pragma-matrix-entries instr) 'list)
               (every (lambda (e) (typep e '(complex double-float)))
                      (cl-quil::pragma-matrix-entries instr)))))))

(deftest test-parse-element-list ()
  (let* ((element-list "(1 2.0 3.0e3 4.0i 5+6i 7+8.0i 9.0E-10i -i i pi -pi)")
         (parsed-list (cl-quil/frontend::parse-element-list element-list 11)))
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

(deftest test-defgate-as-matrix ()
  (let* ((quil "
DEFGATE TEST AS MATRIX:
    exp(2*i), 0
    0, exp(4*i)

TEST 0")
         (quil-parsed (not-signals quil-parse-error (parse-quil quil))))
    (let ((gates (parsed-program-gate-definitions quil-parsed)))
      (is (= 1 (length gates)))
      (is (typep (first gates) 'cl-quil::static-gate-definition)))))

(deftest test-defgate-as-permutation ()
  (let ((quil "
DEFGATE TEST AS PERMUTATION:
    0, 1, 2, 3, 4, 5, 7, 6

TEST 0 1 2")
        (quil-bad "
DEFGATE TEST AS PERMUTATION:
    0, 1, 2, 3, 4, 5, 7, 6, 8

TEST 0 1 2"))
    (let* ((quil-parsed (not-signals quil-parse-error (cl-quil::parse-quil quil)))
           (gates (parsed-program-gate-definitions quil-parsed)))
      (is (= 1 (length gates)))
      (is (typep (first gates) 'cl-quil::permutation-gate-definition)))
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
      (signals cl-quil::ambiguous-definition-condition
        (parse-quil test-quil :ambiguous-definition-handler #'identity)))))

(deftest test-parsing-defgate-defcircuit-ambiguity ()
  (let ((test-quil "
DEFGATE FOO(%a):
    1/sqrt(2), 1/sqrt(2)
    1/sqrt(2), -1/sqrt(2)

DEFCIRCUIT FOO(%a) q:
    X q
"))
    (signals cl-quil::ambiguous-definition-condition
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

(deftest test-pauli-sum-parsing ()
  (let ((p (parse-quil-into-raw-program "
DEFGATE FOO(%a) q AS PAULI-SUM:
    X(%a) q

")))
    (is (listp p))
    (is (= 1 (length p)))
    (is (typep (first p) 'cl-quil::exp-pauli-sum-gate-definition))))




;;;; Tests for Split-Lines

;;; Test various odd corners of the split-lines subfunction of the
;;; parser.  Split-lines takes a list of the form
;;;
;;;   ( { token | break-symbol }* )
;;;
;;; where token is a token structure, and break-symbol is either of
;;; the symbols :NEWLINE or :SEMICOLON.
;;;
;;; The returned result is a list of sublists of every subseries of
;;; tokens up to a break symbol. The resulting sublists do not include
;;; any break symbols. The main tricky bit is when break-symbol is
;;; :SEMICOLON. In that case, it prepends the appropriate indentation.
;;;
;;; For purposes of testing, split-lines doesn't care what the tokens
;;; are, so test it with a couple of dummy tokens, one for a regular
;;; token and one for the special case of an indentation token. 

(defparameter *test-cases-for-split-lines*
  (let ((token (cl-quil::tok ':nop))
        (indentation (cl-quil::tok ':indentation 13)))
    ;; Use the same identical token for all the test lists
    `(;; lists of the form (input expected)

      ;; trivial no-token cases that result in an empty list
      (() ())
      ((:newline) ())
      ((:semicolon) ())
      ((:newline :newline) ())
      ((:newline :semicolon) ())

      ;; basic cases with tokens but without a break symbol
      ((,token) ((,token)))
      ((,token ,token) ((,token ,token)))
      ((,token ,token ,token) ((,token ,token  ,token)))
      ((,token ,indentation) ((,token ,indentation)))
      ((,indentation ,token) ((,indentation ,token)))

      ;; cases with a newline break
      ((:newline ,token) ((,token)))
      ((:newline :newline ,token) ((,token)))
      ((,token :newline) ((,token)))
      ((,token :newline :newline) ((,token)))
      ((:newline :newline ,token :newline :newline) ((,token)))
      ((:newline ,token :newline ,token) ((,token) (,token)))
      ((:newline ,token :newline ,token :newline ,token :newline :newline)
       ((,token) (,token) (,token)))
      ((,token ,token ,token :newline)
       ((,token ,token ,token)))
      ((:newline ,token  ,token  ,token
        :newline ,token ,token
        :newline :newline ,token
        :newline)
       ((,token ,token ,token)
        (,token ,token)
        (,token)))

      ;; cases with a semicolon break
      ((:semicolon ,token) ((,token)))
      ((:semicolon :semicolon ,token) ((,token)))
      ((,token :semicolon) ((,token)))
      ((,token :semicolon :semicolon) ((,token)))
      ((:semicolon :semicolon ,token :semicolon :semicolon) ((,token)))
      ((:semicolon ,token :semicolon ,token) ((,token) (,token)))
      ((:semicolon ,token :semicolon ,token :semicolon ,token)
       ((,token) (,token) (,token)))
      ((,token ,token ,token :semicolon)
       ((,token ,token ,token)))
      ((:semicolon ,token  ,token  ,token
        :semicolon ,token ,token
        :semicolon :semicolon ,token
        :semicolon)
       ((,token ,token ,token)
        (,token ,token)
        (,token)))

      ;; cases with a semicolon break and indentation
      ((:semicolon ,indentation ,token) ((,indentation ,token)))
      ((:semicolon :semicolon ,token ,indentation) ((,token ,indentation)))
      ((,indentation ,token :semicolon)
       ((,indentation ,token)
        (,indentation)))
      ((,indentation ,token :semicolon :semicolon)
       ((,indentation ,token)
        (,indentation)
        (,indentation)))

      ;; Here are cases that involve mixing of break characters and
      ;; indentation. The interesting case is where a subsequence that
      ;; ends in a semicolon starts with an indentation token, and
      ;; there's a following subsequence. In that case, the
      ;; indentation token is copied into the following subsequence by
      ;; consing it onto the front.
      ((:semicolon :semicolon ,indentation ,token :semicolon :semicolon)
       ((,indentation ,token) (,indentation) (,indentation)))
      ((:semicolon :semicolon ,indentation ,token
        :semicolon ,token ,token ,token
        :semicolon ,token)
       ((,indentation ,token)
        (,indentation ,token ,token ,token)
        (,indentation ,token)))
      ((:semicolon :semicolon ,indentation ,token
        :semicolon ,token ,token ,token
        :newline ,token ,token ,token
        :semicolon ,token
        :newline ,indentation ,token ,token
        :semicolon ,token)
       ((,indentation ,token) ; leading semicolons discarded, new indentation
        (,indentation ,token ,token ,token) ; previous indentation carried over
        (,token ,token ,token)              ; previous indentation discarded
        (,token)                            ; no indentation
        (,indentation ,token ,token)        ; new indentation
        (,indentation ,token))))))          ; indentation carried over

(deftest test-split-lines (&optional verbose)
  (loop :for (tokens expect) :in *test-cases-for-split-lines*
        :as actual  ; split-lines mutates list structure, so copy list
          := (cl-quil/frontend::split-lines (copy-list tokens))
        :as i :from 1
        :do (when verbose
              (let ((*print-circle* nil)) ; don't print #1#, say
                (format t "~%[~d:] parse-lines:~%  ~s" i tokens)
                (cond
                  ((equal actual expect)
                   (format t "~%  Actual/expect:~%  ~s~%" actual))
                  (t
                   (format t "~%  ** TEST FAIL **~%")
                   (format t "~%  Actual:~%  ~s" actual)
                   (format t "~%  Expect:~%  ~s~%" expect)))))
            (is (equal actual expect))))
