;;;; typed-memory-tests.lisp
;;;;
;;;; Authors: Eric Peterson
;;;;          Robert Smith
;;;;
;;;; Tests for type-safety.lisp and classical-memory.lisp

(in-package #:cl-quil-tests)

(deftest test-typed-circuit-expansion ()
  (let ((pp (cl-quil::parse-quil "
DECLARE a BIT

DEFCIRCUIT CLEAR q scratch-bit:
    MEASURE q scratch-bit
    JUMP-UNLESS @end scratch-bit
    X q
    LABEL @end

CLEAR 0 a")))
    (let ((instr (aref (cl-quil::parsed-program-executable-code pp) 0)))
      (is (typep instr 'cl-quil::measure))
      (is (= (cl-quil::qubit-index (cl-quil::measurement-qubit instr)) 0))
      (is (equalp (cl-quil::measure-address instr)
                  (cl-quil::mref "a" 0 (first (cl-quil::parsed-program-memory-definitions pp))))))))

(deftest test-constant-coercion ()
  (let ((pp (cl-quil::parse-quil "
DECLARE stats INTEGER
DECLARE angle REAL

# Initialize
MOVE stats 0
MOVE angle 0

RX(-2.0*angle) 0")))
    (let ((code (parsed-program-executable-code pp)))
      (is (equalp cl-quil::quil-integer (cl-quil::constant-value-type
                                      (cl-quil::classical-right-operand (aref code 0)))))
      (is (equalp cl-quil::quil-real (cl-quil::constant-value-type
                                   (cl-quil::classical-right-operand (aref code 1)))))
      (let ((param (first (application-parameters (aref code 2)))))
        (is (typep param 'cl-quil::delayed-expression))))))

(deftest test-compression-with-classical-angles ()
  (let ((pp (cl-quil::parse-quil "
DECLARE val REAL[8]
DECLARE ro BIT
DECLARE int INTEGER

RZ(val[2]) 0
RZ(val[0]+val[1]) 0
RZ(val[1]) 0
RZ(val[3]) 1")))
    (let ((cpp (cl-quil::compiler-hook pp (cl-quil::build-8Q-chip))))
      (is (= 3 (length (cl-quil::parsed-program-executable-code cpp)))))))

(deftest test-compression-with-classical-angles-+-resource-usage ()
  (let ((pp (cl-quil::parse-quil "
DECLARE val REAL[8]
DECLARE ro BIT
DECLARE int INTEGER

RZ(val[2]) 0
STORE val int[0] val[1]
RZ(val[0]+val[1]) 0
RZ(val[1]) 0
RZ(val[3]) 1")))
    (let ((cpp (cl-quil::compiler-hook pp (cl-quil::build-8Q-chip))))
      (is (= 5 (length (cl-quil::parsed-program-executable-code cpp)))))))

(deftest test-paltry-type-conversions ()
  "Test that we convert classical base classes into specialized classes correctly."
  (let ((code (cl-quil:parsed-program-executable-code
               (cl-quil:parse-quil "
DECLARE w BIT
DECLARE x OCTET
DECLARE y INTEGER
DECLARE z REAL
NOT w
ADD z z
ADD z 1.0
CONVERT z w
XOR x x
LOAD z z y
STORE w y w
"))))
    (is (every #'typep code '(cl-quil:classical-not-bit
                              cl-quil:classical-addition-real/real
                              cl-quil:classical-addition-real/immediate
                              cl-quil:classical-convert-real/bit
                              cl-quil:classical-exclusive-or-octet/octet
                              cl-quil:classical-load-real/real*/integer
                              cl-quil:classical-store-bit*/integer/bit)))))

(deftest test-classical-equality-immediate-types ()
  (signals quil-type-error (parse-quil "DECLARE r BIT; DECLARE a BIT; EQ r a 1.0"))
  (signals quil-type-error (parse-quil "DECLARE r BIT; DECLARE a OCTET; EQ r a 1.0"))
  (signals quil-type-error (parse-quil "DECLARE r BIT; DECLARE a INTEGER; EQ r a 1.0"))
  (signals quil-type-error (parse-quil "DECLARE r BIT; DECLARE a REAL; EQ r a 1"))

  (not-signals quil-type-error (parse-quil "DECLARE r BIT; DECLARE a BIT; DECLARE b BIT; EQ r a b"))
  (not-signals quil-type-error (parse-quil "DECLARE r BIT; DECLARE a BIT; EQ r a 1"))

  (not-signals quil-type-error (parse-quil "DECLARE r BIT; DECLARE a OCTET; DECLARE b OCTET; EQ r a b"))
  (not-signals quil-type-error (parse-quil "DECLARE r BIT; DECLARE a OCTET; EQ r a 1"))

  (not-signals quil-type-error (parse-quil "DECLARE r BIT; DECLARE a INTEGER; DECLARE b INTEGER; EQ r a b"))
  (not-signals quil-type-error (parse-quil "DECLARE r BIT; DECLARE a INTEGER; EQ r a 1"))

  (not-signals quil-type-error (parse-quil "DECLARE r BIT; DECLARE a REAL; DECLARE b REAL; EQ r a b"))
  (not-signals quil-type-error (parse-quil "DECLARE r BIT; DECLARE a REAL; EQ r a 1.0")))

(deftest test-classical-store-types ()
  (not-signals quil-type-error (parse-quil "DECLARE ro BIT; DECLARE v BIT; DECLARE index INTEGER; STORE ro index v;"))
  (not-signals quil-type-error (parse-quil "DECLARE ro BIT; DECLARE index INTEGER; STORE ro index 1;"))
  (not-signals quil-type-error (parse-quil "DECLARE ro OCTET; DECLARE v OCTET; DECLARE index INTEGER; STORE ro index v;"))
  (not-signals quil-type-error (parse-quil "DECLARE ro OCTET; DECLARE index INTEGER; STORE ro index 1;"))
  (not-signals quil-type-error (parse-quil "DECLARE ro INTEGER; DECLARE v INTEGER; DECLARE index INTEGER; STORE ro index v;"))
  (not-signals quil-type-error (parse-quil "DECLARE ro INTEGER; DECLARE index INTEGER; STORE ro index 1;"))
  (not-signals quil-type-error (parse-quil "DECLARE ro REAL; DECLARE v REAL; DECLARE index INTEGER; STORE ro index v;"))
  (not-signals quil-type-error (parse-quil "DECLARE ro REAL; DECLARE index INTEGER; STORE ro index 1;"))

  ;; Storing REAL into BIT
  (signals quil-type-error (parse-quil "DECLARE ro BIT; DECLARE v REAL; DECLARE index INTEGER; STORE ro index v;"))
  (signals quil-type-error (parse-quil "DECLARE ro BIT; DECLARE index INTEGER; STORE ro index 1.0;"))
  ;; Storing REAL into OCTET
  (signals quil-type-error (parse-quil "DECLARE ro OCTET; DECLARE v REAL; DECLARE index INTEGER; STORE ro index v;"))
  (signals quil-type-error (parse-quil "DECLARE ro OCTET; DECLARE index INTEGER; STORE ro index 1.0;"))
  ;; Storing REAL into INTEGER
  (signals quil-type-error (parse-quil "DECLARE ro INTEGER; DECLARE v REAL; DECLARE index INTEGER; STORE ro index v;"))
  (signals quil-type-error (parse-quil "DECLARE ro INTEGER; DECLARE index INTEGER; STORE ro index 1.0;")))
