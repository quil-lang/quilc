;;;; hasse-diagram-tests.lisp
;;;;
;;;; Author: Mark David

(in-package #:cl-quil/tools-tests)


;;; For now, this tests module provides both an demo/example as well
;;; as an automated test, both driven off parameter
;;; *hasse-example-quil-programs*, below.
;;;
;;; The function hasse-example is a standalone hand-runnable
;;; example/demo, not really a test. It seems useful to have around,
;;; so sticking it in here as there seems no better place for it.
;;;
;;; The automatic test hasse-diagram-test covers just the first couple
;;; of the entries in *hasse-example-quil-programs*, which should very
;;; well ensure against regressions.


(defparameter *hasse-example-quil-programs*
  ;; each entry should either be a string or a list of strings
  `(
    ;; From logical-scheduler doc:
    "X 0; H 3; CNOT 0 1; X 3; CNOT 1 3"

    ;; From qvm repo: tests/measurement-tests.lisp:
    ("DECLARE ro BIT[3]"
     "X 0"
     "X 2"
     "MEASURE 0 ro[0]"
     "MEASURE 1 ro[1]"
     "MEASURE 2 ro[2]")
    
    ("DECLARE ro BIT[3]"
     "X 0"
     "X 2"
     "X 3"
     "MEASURE 0 ro[0]"
     "MEASURE 1 ro[1]"
     "MEASURE 2 ro[2]")
    ("DECLARE ro BIT[3]"
     "X 0"
     "X 2"
     "SWAP 0 1"
     "SWAP 0 2"
     "MEASURE 0 ro[0]"
     "MEASURE 1 ro[1]"
     "MEASURE 2 ro[2]")
    ("DECLARE ro BIT[3]"
     "X 0"
     "X 2"
     "X 3"
     "SWAP 0 1"
     "SWAP 0 2"
     "MEASURE 0 ro[0]"
     "MEASURE 1 ro[1]"
     "MEASURE 2 ro[2]")
    ("DECLARE ro BIT"
     "I 1"
     "H 0"
     "MEASURE 0 ro[0]"
     "RESET")

    ;; From qvm repo: tests/classical-memory-tests.lisp:
    ;;   test-load-out-of-range
    "
DECLARE idx INTEGER
DECLARE z REAL[4]
DECLARE target REAL

MOVE idx 0
LOAD target z idx   # OK!

MOVE idx 5
LOAD target z idx   # NOT OK!
"
    ;;   test-measure-out-of-range
    "
DECLARE ro BIT[1]
X 0
X 1
MEASURE 0 ro[0]
MEASURE 1 ro[1]
"
    ;;   test-bit-offsetting-bit
    "
DECLARE mem BIT[1024]     # 128 octets
DECLARE b0 BIT SHARING mem
DECLARE b1 BIT SHARING mem OFFSET 1 BIT
DECLARE b2 BIT SHARING mem OFFSET 2 BIT
DECLARE b3 BIT SHARING mem OFFSET 3 BIT
DECLARE b4 BIT SHARING mem OFFSET 4 BIT
DECLARE b5 BIT SHARING mem OFFSET 5 BIT
DECLARE b6 BIT SHARING mem OFFSET 6 BIT
DECLARE b7 BIT SHARING mem OFFSET 7 BIT
DECLARE b8 BIT SHARING mem OFFSET 8 BIT

DECLARE b80 BIT SHARING mem OFFSET 10 OCTET 1 BIT

MOVE b0 1
MOVE b1 1
MOVE b2 1
MOVE b3 1
MOVE b4 1
MOVE b5 1
MOVE b6 1
MOVE b7 1
MOVE b8 1

MOVE b80 1
"
    ;;   test-indirect-comparators
    "
DECLARE bx BIT[2]
DECLARE rx REAL[2]
DECLARE ix INTEGER[2]
DECLARE ox OCTET[2]

DECLARE lt BIT[4]
DECLARE le BIT[4]
DECLARE eq BIT[4]
DECLARE ge BIT[4]
DECLARE gt BIT[4]

LT lt[0] bx[0] bx[1]
LT lt[1] rx[0] rx[1]
LT lt[2] ix[0] ix[1]
LT lt[3] ox[0] ox[1]

LE le[0] bx[0] bx[1]
LE le[1] rx[0] rx[1]
LE le[2] ix[0] ix[1]
LE le[3] ox[0] ox[1]

EQ eq[0] bx[0] bx[1]
EQ eq[1] rx[0] rx[1]
EQ eq[2] ix[0] ix[1]
EQ eq[3] ox[0] ox[1]

GE ge[0] bx[0] bx[1]
GE ge[1] rx[0] rx[1]
GE ge[2] ix[0] ix[1]
GE ge[3] ox[0] ox[1]

GT gt[0] bx[0] bx[1]
GT gt[1] rx[0] rx[1]
GT gt[2] ix[0] ix[1]
GT gt[3] ox[0] ox[1]
"
    ;;   test-exhange
    "
DECLARE bx BIT[2]
DECLARE rx REAL[2]
DECLARE ix INTEGER[2]
DECLARE ox OCTET[2]

MOVE bx[0] 0
MOVE bx[1] 1

MOVE rx[0] 0.0
MOVE rx[1] 55.0

MOVE ix[0] 0
MOVE ix[1] 123

MOVE ox[0] 0
MOVE ox[1] 255

EXCHANGE bx[0] bx[1]
EXCHANGE rx[0] rx[1]
EXCHANGE ix[0] ix[1]
EXCHANGE ox[0] ox[1]
"
    ;;   test-memory-name-closure-bug
    "
DECLARE ro BIT[4]
DECLARE theta REAL[1]
MOVE theta[0] 0.0
PRAGMA EXPECTED_REWIRING \"#(0 1 2 5 3 4)\"
RX(-pi/2) 0
CZ 1 0
RX(pi) 5
RZ(pi) 0
RZ(-pi/2) 1
RX(-pi/2) 1
CZ 2 1
RZ(-pi/2) 2
RX(-pi/2) 2
CZ 5 2
RZ(pi/2) 2
RZ(-pi/2) 5
RX(-pi/2) 5
RZ(pi/2 + theta[0] + -pi/2) 5
RX(pi/2) 5
CZ 5 2
RZ(pi/2) 2
RX(pi/2) 2
CZ 2 1
RX(pi/2) 1
CZ 0 1
RX(-1.5707963267948968) 0
RZ(1.5707963267948974) 1
RZ(pi/2) 2
RX(pi) 2
RZ(pi/2) 5
RX(pi) 5
PRAGMA CURRENT_REWIRING \"#(0 1 2 5 3 4)\"
PRAGMA EXPECTED_REWIRING \"#(0 1 2 5 3 4)\"
MEASURE 5 ro[3]
MEASURE 2 ro[2]
MEASURE 1 ro[1]
MEASURE 0 ro[0]
PRAGMA CURRENT_REWIRING \"#(0 1 2 5 3 4)\"
"))



(defun hasse-example (&optional pathname-defaults)
  "Generate example Hasse diagrams as Graphviz dot files.
  The files go in the directory given by PATHNAME-DEFAULTS, if
  non-nil, or otherwise by *default-pathname-defaults*. File names are
  of the form \"example-1.gv\", \"example-2.gv\", etc., corresponding
  in order to the programs in *hasse-example-quil-programs*. See
  above."

  (loop :with base-name := "hasse-example"
        :with file-type := "gv"
        :with defaults := (or pathname-defaults *default-pathname-defaults*)
        :for i :from 1
        :as program :in *hasse-example-quil-programs*
        :as file-name := (format nil "~a-~d.~a" base-name i file-type)
        :as pathname := (merge-pathnames file-name defaults)
        :collect (tools:write-hasse-for-quil program :output-file pathname)))



(defun test-hasse-diagram (program expected-string)
  (flet ((trim (string)
           (string-trim
            '(#\space #\tab #\return #\linefeed)
            string)))
    (let* ((dot-output-pathname
             (uiop:with-temporary-file (:stream stream :keep t)
               (tools:write-hasse-for-quil program :stream stream)
               (truename stream)))
           (actual-string
             (trim
              (with-open-file (in dot-output-pathname)
                (with-output-to-string (out)
                  (loop for line = (read-line in nil nil)
                        while line
                        do (write-line line out))))))
           (expected-string
             (trim expected-string)))
      (is (string= expected-string actual-string)))))





(defparameter *hasse-diagram-test-1-expected-output* "
digraph G {
    rankdir=BT;
    \"H 3 [5]\" [label = \"H 3\"]
    \"X 3 [4]\" [label = \"X 3\"]
    \"CNOT 1 3 [3]\" [label = \"CNOT 1 3\"]
    \"X 0 [2]\" [label = \"X 0\"]
    \"CNOT 0 1 [1]\" [label = \"CNOT 0 1\"]
    \"X 3 [4]\" -> \"H 3 [5]\"
    \"CNOT 1 3 [3]\" -> \"X 3 [4]\"
    \"CNOT 1 3 [3]\" -> \"CNOT 0 1 [1]\"
    \"CNOT 0 1 [1]\" -> \"X 0 [2]\"
}

# X 0; H 3; CNOT 0 1; X 3; CNOT 1 3")

(defparameter *hasse-diagram-test-2-expected-output* "
digraph G {
    rankdir=BT;
    \"X 2 [5]\" [label = \"X 2\"]
    \"MEASURE 2 ro[2] [4]\" [label = \"MEASURE 2 ro[2]\"]
    \"X 0 [3]\" [label = \"X 0\"]
    \"MEASURE 0 ro[0] [2]\" [label = \"MEASURE 0 ro[0]\"]
    \"MEASURE 1 ro[1] [1]\" [label = \"MEASURE 1 ro[1]\"]
    \"MEASURE 2 ro[2] [4]\" -> \"X 2 [5]\"
    \"MEASURE 0 ro[0] [2]\" -> \"X 0 [3]\"
    \"MEASURE 1 ro[1] [1]\"
}

# DECLARE ro BIT[3]
# X 0
# X 2
# MEASURE 0 ro[0]
# MEASURE 1 ro[1]
# MEASURE 2 ro[2]")



(deftest hasse-diagram-test ()
  (test-hasse-diagram
   (first *hasse-example-quil-programs*)
   *hasse-diagram-test-1-expected-output*)
  (test-hasse-diagram
   (second *hasse-example-quil-programs*)
   *hasse-diagram-test-2-expected-output*))


