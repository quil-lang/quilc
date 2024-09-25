;;;; foust-benchmarking.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil-benchmarking.foust
  (:use
   #:coalton
   #:coalton-prelude)
  (:use
   #:coalton-quil
   #:cl-quil.foust-quil)
  (:import-from #:coalton-library/math/complex #:square-magnitude)
  (:local-nicknames
   (#:bits #:coalton-library/bits)
   (#:cell #:coalton-library/cell)
   (#:file #:coalton-library/file)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:map #:coalton-library/ord-map)
   (#:string #:coalton-library/string))
  (:export
   #:foust-benchmark-qasm-suite
   #:cl-foust-benchmark-qasm-suite))

(in-package #:cl-quil-benchmarking.foust)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare qasm-prefix file:Pathname)
  (define qasm-prefix
    "The pathname of the directory in which the QASM benchmarking files are stored."
    (unwrap (file:system-relative-pathname "cl-quil" "benchmarking/ibm_qx_mapping/examples/")))

  (declare qasm-test-files (Unit -> (List file:Pathname)))
  (define (qasm-test-files)
    "A list of pathnames associated with the QASM benchmarking files."
    (filter (compose (string:substring? ".qasm") into) (unwrap (file:directory-files qasm-prefix))))

  (declare parsed-program-multi-qubit-depth (QuilParsedProgram -> UFix))
  (define (parsed-program-multi-qubit-depth parsed-program-p)
    "The number of gates applied to two or more qubits in a parsed program."
    (match (get-parsed-program-executable-code parsed-program-p)
      ((QuilExecutableCode instructions)
       (list:countby (fn (instruction)
                       (match instruction
                         ((QuilGateApplication gate-application-g)
                          (<= 2 (length (get-quil-gate-application-qubits gate-application-g))))
                         (_ False)))
                     instructions)))))

(coalton-toplevel

  (declare remap-ufix (QuilRewiring -> UFix -> UFix))
  (define (remap-ufix rewiring n)
    "Transform a number by a bitwise rewiring."
    (iter:fold! (fn (n-prime (Tuple new-m old-m))
                  (if (< 0 (bits:and n (bits:shift (as Integer old-m) 1)))
                      (+ n-prime (bits:shift (as Integer new-m) 1))
                      n-prime))
                0
                (iter:enumerate!
                 (iter:into-iter (get-quil-rewiring-p2l rewiring)))))

  (declare parsed-program-amplitudes (QuilParsedProgram -> (map:Map UFix (Complex Double-Float))))
  (define (parsed-program-amplitudes parsed-program-p)
    "Simulate a parsed program with the initial state |00...0⟩ and return the resulting amplitudes."
    (let ((raw-amplitudes (lisp (List (Complex Double-Float)) (parsed-program-p)
                            (cl:coerce (qvm::amplitudes (qvm:run-program 16 parsed-program-p)) 'cl:list))))
      (pipe (match (find (compose (< 0.0000001d0) square-magnitude) raw-amplitudes)
              ((Some first-amp) (let ((phase (/ first-amp (into (sqrt (square-magnitude first-amp))))))
                                  (map (flip / phase) raw-amplitudes)))
              ((None) raw-amplitudes))
            iter:into-iter
            iter:enumerate!
            map:collect!)))

  (declare complex== ((Complex Double-Float) -> (Complex Double-Float) -> Boolean))
  (define (complex== a b)
    "Are `a` and `b` approximately equal?"
    (> 0.000001d0 (square-magnitude (- a b))))

  (declare amplitudes==at (QuilRewiring -> QuilRewiring
                           -> (map:Map UFix (Complex Double-Float)) -> (map:Map UFix (Complex Double-Float))
                           -> UFix -> Boolean))
  (define (amplitudes==at rewiring-one rewiring-two amplitudes-one amplitudes-two n)
    "Given a pair of rewirings, are two sets of amplitudes equal by `complex==` for an index `n`."
    (complex== (unwrap (map:lookup amplitudes-one (remap-ufix rewiring-one n)))
               (unwrap (map:lookup amplitudes-two (remap-ufix rewiring-two n))))))

(coalton-toplevel

  (declare ==by-qvm (QuilParsedProgram -> QuilParsedProgram -> Boolean))
  (define (==by-qvm parsed-program-one parsed-program-two)
    "Do two parsed programs produce the same amplitudes when simulated on the initial state |00...0⟩"
    (let ((rewiring-one (get-parsed-program-final-rewiring parsed-program-one))
          (rewiring-two (get-parsed-program-final-rewiring parsed-program-two))
          (amplitudes-one (parsed-program-amplitudes parsed-program-one))
          (amplitudes-two (parsed-program-amplitudes parsed-program-two)))
      (all (amplitudes==at rewiring-one rewiring-two amplitudes-one amplitudes-two)
           (range 0 (1- (bits:shift 16 1)))))))

(coalton-toplevel


  (declare nstring (UFix -> String -> String))
  (define (nstring n str)
    "Repeat the String `str` `n` times and concatenate."
    (iter:fold! <> mempty (iter:take! n (iter:repeat str))))

  (declare num->stringm ((Num :a) (Into :a String) => UFix -> :a -> String))
  (define (num->stringm m n)
    "Produce a String of length `m` from the String representation of the number `n`."
    (let ((str (string:substring (into n) 0 m))
          (len (string:length str)))
      (string:concat (nstring (- m len) " ") str)))

  (declare stringm (UFix -> String -> String))
  (define (stringm m str)
    "Produce a String of length `m` from `str`, adding trailing spaces or trimming as needed."
    (let ((str2 (string:substring str 0 16)))
      (string:concat str2 (nstring (- m (string:length str2)) " ")))))

;; This macro is copied directly from
;; quilc/benchmarking/rewiring-analysis.lisp
;; written by Robert Smith.
(cl:defmacro with-stopwatch (elapsed-var cl:&body body)
  (cl:let ((start-time (cl:gensym)))
    `(cl:let ((,start-time (cl:get-internal-real-time)))
       (cl:symbol-macrolet ((,elapsed-var (cl:- (cl:get-internal-real-time) ,start-time)))
         ,@body))))

;; The code in the following block is adapted from
;; the function benchmark-qasm-suite in the file
;; quilc/benchmarking/qasm-benchmarking.lisp
;; for use with Foust in Coalton.
(coalton-toplevel

  (declare foust-benchmark-qasm-suite (UFix -> Unit))
  (define (foust-benchmark-qasm-suite timeout)
    "Benchmark Foust by compiling a suite of QASM files to the chip `ibm-qx-5` with the Quil compiler, with and without using Foust in its `preserve` mode."
    (print "┌─────────────────┬───────────────────────────┬───────────────────────────┬───────────────────────────┬──────────────────────┐")
    (print "│                 │       WITHOUT FOUST       │     WITH NAIVE FOUST      │   WITH CHIP-AWARE FOUST   │      VALIDATION      │")
    (print "├─────────────────┼───────────────────────────┼───────────────────────────┼───────────────────────────┼──────────────────────┤")
    (print "│       NAME      │ TIME (s)  SWAPS  2Q DEPTH │ TIME (s)  SWAPS  2Q DEPTH │ TIME (s)  SWAPS  2Q DEPTH │ Matrix?  Amplitudes? │")
    (print "├─────────────────┼───────────────────────────┼───────────────────────────┼───────────────────────────┼──────────────────────┤")
    (for file-f in (qasm-test-files)
      (let ((pp (parse-file file-f))
            (unfousted-cpp (cell:new (the (Optional QuilParsedProgram) None)))
            (fousted-cpp (cell:new (the (Optional QuilParsedProgram) None))))
        (print
         (mconcat
          (make-list
           "│ " (stringm 16 (unwrap (string:strip-suffix ".qasm" (unwrap (string:strip-prefix (into qasm-prefix) (into file-f))))))
           (match (lisp (Tuple (Optional QuilParsedProgram) String) (timeout pp)
                    (trivial-garbage:gc :full cl:t)
                    (cl:handler-case
                        (bordeaux-threads:with-timeout (timeout)
                          (with-stopwatch elapsed-time
                            (coalton
                             (match (compiler-hook (lisp QuilParsedProgram () pp) (build-IBM-Qx5) True False)
                               ((Tuple3 cpp swaps _)
                                (Tuple (Some cpp)
                                       (mconcat
                                        (make-list
                                         "│ " (num->stringm 8 (lisp Double-Float () (cl:coerce (cl:/ elapsed-time 1000000) 'cl:double-float))) " "
                                         " " (num->stringm 5 swaps) " "
                                         " " (num->stringm 8 (parsed-program-multi-qubit-depth cpp)) " "))))))))
                      (bordeaux-threads:timeout ()
                        (coalton (Tuple None "│ TIMEOUT!  ?????  ???????? ")))))
             ((Tuple cpp str)
              (progn (cell:write! unfousted-cpp cpp) str)))
           (match (lisp (Tuple (Optional QuilParsedProgram) String) (timeout pp)
                    (trivial-garbage:gc :full cl:t)
                    (cl:handler-case
                        (bordeaux-threads:with-timeout (timeout)
                          (with-stopwatch elapsed-time
                            (coalton
                             (match (compiler-hook
                                     (foust-parsed-program (lisp QuilParsedProgram () pp) None True False)
                                     (build-IBM-Qx5) True False)
                               ((Tuple3 cpp swaps _)
                                (Tuple (Some cpp)
                                       (mconcat
                                        (make-list
                                         "│ " (num->stringm 8 (lisp Double-Float () (cl:coerce (cl:/ elapsed-time 1000000) 'cl:double-float))) " "
                                         " " (num->stringm 5 swaps) " "
                                         " " (num->stringm 8 (parsed-program-multi-qubit-depth cpp)) " "))))))))
                      (bordeaux-threads:timeout ()
                        (coalton (Tuple None "│ TIMEOUT!  ?????  ???????? ")))))
             ((Tuple _ str) str))
           (match (lisp (Tuple (Optional QuilParsedProgram) String) (timeout pp)
                    ;;
                    (trivial-garbage:gc :full cl:t)
                    (cl:handler-case
                        (bordeaux-threads:with-timeout (timeout)
                          (with-stopwatch elapsed-time
                            (coalton
                             (match (compiler-hook
                                     (foust-parsed-program (lisp QuilParsedProgram () pp) (Some (build-IBM-Qx5)) True False)
                                     (build-IBM-Qx5) True False)
                               ((Tuple3 cpp swaps _)
                                (Tuple (Some cpp)
                                       (mconcat
                                        (make-list
                                         "│ " (num->stringm 8 (lisp Double-Float () (cl:coerce (cl:/ elapsed-time 1000000) 'cl:double-float))) " "
                                         " " (num->stringm 5 swaps) " "
                                         " " (num->stringm 8 (parsed-program-multi-qubit-depth cpp)) " "))))))))
                      (bordeaux-threads:timeout ()
                        (coalton (Tuple None "│ TIMEOUT!  ?????  ???????? ")))))
             ((Tuple cpp str)
              (progn (cell:write! fousted-cpp cpp) str)))
           (match (Tuple (cell:read unfousted-cpp) (cell:read fousted-cpp))
             ((Tuple (Some unwrapped-unfousted-cpp) (Some unwrapped-fousted-cpp))
              (lisp String (timeout unwrapped-unfousted-cpp unwrapped-fousted-cpp)
                (cl:concatenate
                 'cl:string
                 (cl:handler-case
                     (bordeaux-threads:with-timeout (timeout)
                       (coalton
                        (if (== (lisp QuilParsedProgram () unwrapped-unfousted-cpp)
                                (lisp QuilParsedProgram () unwrapped-fousted-cpp))
                            "│   YES   " "│   NO    ")))
                   (sb-kernel::heap-exhausted-error  () "│ ??????? ")
                   (bordeaux-threads:timeout () "│ ??????? "))
                 (cl:handler-case
                     (bordeaux-threads:with-timeout (timeout)
                       (coalton
                        (if (==by-qvm (lisp QuilParsedProgram () unwrapped-unfousted-cpp)
                                      (lisp QuilParsedProgram () unwrapped-fousted-cpp))
                            "     YES     │" "     NO      │")))
                   (bordeaux-threads:timeout () " ????????? │")))))
             (_ "│ ???????  ??????????? │")))))))
    (print "└─────────────────┴───────────────────────────┴───────────────────────────┴───────────────────────────┴──────────────────────┘")))

(cl:defun cl-foust-benchmark-qasm-suite (cl:&key (timeout 30))
  (coalton (foust-benchmark-qasm-suite (lisp UFix () timeout))))
