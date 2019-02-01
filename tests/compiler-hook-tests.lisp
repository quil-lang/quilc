;;;; compiler-hook-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-tests)

(defparameter *compiler-hook-test-file-directory*
  (asdf:system-relative-pathname
   ':cl-quil-tests
   "tests/compiler-hook-test-files/"))

(deftest test-gate-applications-to-logical-matrix-cnot-rewiring ()
  "Test whether quil::gate-applications-to-logical-matrix converts equivalent
programs (modulo rewiring) to equivalent matrices."
  (let ((pp (quil::parse-quil-string "
CNOT 1 2
CNOT 1 0")) 
        (pp-rewired (quil::parse-quil-string "
PRAGMA EXPECTED_REWIRING \"#(2 0 1)\"
CNOT 0 1
CNOT 0 2
PRAGMA CURRENT_REWIRING \"#(2 0 1)\"
")))
    (is (operator= (quil::gate-applications-to-logical-matrix pp)
                   (quil::gate-applications-to-logical-matrix pp-rewired)))))

(deftest test-gate-applications-to-logical-matrix-swap-rewiring ()
  "Test whether quil::gate-applications-to-logical-matrix converts equivalent
programs (modulo rewiring) to equivalent matrices."
  (let ((pp (quil::parse-quil-string "
CNOT 0 1
SWAP 0 1"))
        (pp-rewired (quil::parse-quil-string "
PRAGMA EXPECTED_REWIRING \"#(0 1)\"
CNOT 0 1
PRAGMA CURRENT_REWIRING \"#(1 0)\"")))
    (is (operator= (quil::gate-applications-to-logical-matrix pp)
                   (quil::gate-applications-to-logical-matrix pp-rewired)))))


(deftest test-rewiring-modes ()
  "Iterates over the rewiring modes and tests that the addresser is well-behaved on each of them."
  ;; first, the straight-line rewiring methods
  (dolist (quil::*initial-rewiring-default-type* '(:naive :random :partial :greedy))
    (format t "    Testing rewiring type ~A~%" quil::*initial-rewiring-default-type*)
    (finish-output)
    (let* ((pstring "
CNOT 0 3
CNOT 1 4")
           (pp (quil::parse-quil-string pstring))
           (cpp (quil::compiler-hook (quil::parse-quil-string pstring)
                                     (quil::build-8Q-chip)
                                     :protoquil t)))
      (is (operator= (quil::gate-applications-to-logical-matrix pp)
                     (quil::gate-applications-to-logical-matrix cpp)))))
  ;; then, the block-to-block rewiring methods.
  ;; i'm too lazy to check correctness, but we're at least exercising the pathway.
  (dolist (quil::*addresser-move-to-rewiring-swap-search-type* '(:greedy-path :greedy-qubit :a*))
    (format t "    Testing addresser move type ~A~%" quil::*addresser-move-to-rewiring-swap-search-type*)
    (finish-output)
    (let* ((pp (quil::parse-quil "
LABEL @a
CNOT 0 1
CNOT 1 2
CNOT 0 2
JUMP @a")))
      (quil::compiler-hook pp (quil::build-8Q-chip))
      (is t))))


(deftest test-compiler-hook ()
  "Test whether the compiler hook preserves semantic equivalence for
some test programs."
  (labels ((compare-compiled (file architecture)
             ;; note: we compress qubits twice. the first is a compression of
             ;; logical qubits in the original, uncompiled program. for the
             ;; compiled program, we also compress qubits when creating the
             ;; logical matrix, since the compiler may use a larger range of
             ;; physical qubits
             (let* ((orig-prog (quil::transform 'quil::compress-qubits
                                                (cl-quil::read-quil-file file)))
                    (proc-prog
                      (quil::compiler-hook (quil::transform 'quil::compress-qubits
                                                            (cl-quil::read-quil-file file))
                                           (quil::build-nQ-linear-chip 5 :architecture architecture))))
               (is (matrix-equals-dwim (quil::gate-applications-to-logical-matrix orig-prog)
                                       (quil::gate-applications-to-logical-matrix proc-prog :compress-qubits t))))))
    (finish-output *debug-io*)
    (dolist (state-prep '(nil t))
      (let ((quil::*enable-state-prep-compression* state-prep))
        (format *debug-io* "    With *ENABLE-STATE-PREP-COMPRESSION* ~a~%" quil::*enable-state-prep-compression*)
        (dolist (file (uiop:directory-files *compiler-hook-test-file-directory* #P"*.quil"))
          (format *debug-io* "      Testing file ~a:" (pathname-name file))
          (dolist (architecture (list ':cz ':iswap ':cphase ':piswap ':cnot))
            (format *debug-io* " ~a" architecture)
            (compare-compiled file architecture))
          (terpri *debug-io*))))))

(deftest test-compression-bug-QUILC-152 ()
  "QUILC-152: A bug in state compression caused a failed assertion."
  (let ((quil::*enable-state-prep-compression* t)
        (quil::*compress-carefully* t)
        (instructions (quil:parsed-program-executable-code
                       (quil:parse-quil-string "
RZ(0.9800157744729435) 2
RX(pi/2) 2
RZ(1.2991200310990418) 2
RX(-pi/2) 2
RZ(-0.5506190918318916) 3
RX(pi/2) 3
RZ(-1.831598072271041) 3
RX(-pi/2) 3
CPHASE(pi) 3 2
RZ(-1.944647413382994) 3
RX(pi/2) 3
RZ(2.0374814290080794) 3
RX(-pi/2) 3
RZ(-0.8749718863654405) 2
RX(pi/2) 2
RZ(1.8500104155983104) 2
RX(-pi/2) 2
RZ(0.9293531094939129) 2
RX(pi) 2
"))))
    (CL-QUIL::COMPRESS-INSTRUCTIONS-IN-CONTEXT
     (coerce instructions 'list)
     (quil::build-nQ-linear-chip 4 :architecture ':cphase)
     (quil::set-up-compressor-context :qubit-count 4 :simulate t))))

(defun shuffle-list (l &optional (k nil))
        (let* ((elt (nth (random (length l)) l))
               (l (remove elt l))
               (k (cons elt k)))
          (if (zerop (length l))
              k
              (shuffle-list l k))))

(deftest test-compiler-hook-random-4Q ()
  (finish-output *debug-io*)
  (dolist (state-prep '(nil t))
    (let ((quil::*enable-state-prep-compression* state-prep))
      (format *debug-io* "    With *ENABLE-STATE-PREP-COMPRESSION* ~a~%" quil::*enable-state-prep-compression*)
      (dolist (architecture '(:cz :iswap :cphase :piswap :cnot))
        (format *debug-io* "      Working on architecture ~a.~%" architecture)
        (let* ((num-qubits 4)
               (v (quil::random-special-unitary (expt 2 num-qubits)))
               (args (shuffle-list (alexandria:iota num-qubits :start (1- num-qubits) :step -1)))
               (parsed-prog (make-instance
                             'quil::parsed-program
                             :executable-code (make-array 1
                                                          :initial-element (make-instance
                                                                            'quil::gate-application
                                                                            :operator (named-operator "RANDO-GATE")
                                                                            :gate v
                                                                            :arguments (mapcar #'qubit args)))))
               (processed-program
                 (quil::compiler-hook parsed-prog (quil::build-nQ-linear-chip num-qubits
                                                                              :architecture architecture))))
          (is (matrix-equals-dwim (quil::kq-gate-on-lines v num-qubits args)
                                  (quil::gate-applications-to-logical-matrix processed-program))))))))



(deftest test-compiler-hook-reset-naive-rewiring ()
  ;; Note this numbering depends on the fact that the CZ gates are
  ;; native on the 8Q chip.
  (let* ((pp (quil::parse-quil "
PRAGMA INITIAL_REWIRING \"NAIVE\"
CZ 1 2
CZ 3 4
RESET
CZ 5 6
"))
         (processed-program (quil::compiler-hook pp (quil::build-8Q-chip))))
    (is (= 3
           (loop :for instr :across (quil::parsed-program-executable-code processed-program)
                 :count (and (typep instr 'quil::gate-application)
                             (adt:match quil:operator-description (application-operator instr)
                               ((named-operator name) (string= "CZ" name))
                               (_ nil))))))))

(deftest test-compiler-hook-reset-partial-rewiring ()
  (let* ((pp (quil::parse-quil "
PRAGMA INITIAL_REWIRING \"PARTIAL\"
CZ 1 2
CZ 7 6
RESET
CZ 2 7
"))
         (processed-program (quil::compiler-hook pp (quil::build-8Q-chip))))
    (is (= 3
           (loop :for instr :across (quil::parsed-program-executable-code processed-program)
                 :count (and (typep instr 'quil::gate-application)
                             (adt:match quil:operator-description (application-operator instr)
                               ((named-operator name) (string= "CZ" name))
                               (_ nil))))))))

(deftest test-compiling-empty-program ()
  "Test that an empty program goes through the pipes correctly."
  (let* ((pp (quil::parse-quil ""))
         (processed-program (quil::compiler-hook pp (quil::build-8Q-chip))))
    (is (every (lambda (isn)
                 (or (typep isn 'quil:halt)
                     (typep isn 'quil:pragma)
                     (typep isn 'quil:no-operation)))
               (parsed-program-executable-code processed-program)))))

(defun parametric-compiler-test (program-string segment-table)
  (flet ((substitute-params (pp segment-table)
           (loop :for instr :across (parsed-program-executable-code pp)
                 :when (typep instr 'application)
                   :do (map-into (application-parameters instr)
                                 (lambda (p)
                                   (etypecase p
                                     (quil::constant p)
                                     (quil::delayed-expression
                                      (quil::evaluate-delayed-expression
                                       p
                                       (lambda (term)
                                         (let ((lookup (assoc term segment-table :test #'equalp)))
                                           (if lookup
                                               (cdr lookup)
                                               p)))))))
                                 (application-parameters instr))))

         (make-pp ()
           (quil::transform 'quil::resolve-applications
                            (quil::parse-quil program-string))))
    (let* ((chip (quil::build-8Q-chip :architecture ':cz))
             (processed-pp (compiler-hook (make-pp) chip))
             (orig-pp (make-pp)))
        (substitute-params orig-pp segment-table)
        (substitute-params processed-pp segment-table)
        (is (matrix-equals-dwim (quil::gate-applications-to-logical-matrix orig-pp :compress-qubits t)
                                (quil::gate-applications-to-logical-matrix processed-pp :compress-qubits t))))))

(deftest test-parametric-compiler-cphase ()
  (dolist (quil::*enable-state-prep-compression* '(nil t))
    (parametric-compiler-test "
DECLARE angle REAL

CPHASE(angle) 0 1
"
                              (list (cons (mref "angle" 0) (random 1d0))))))

(deftest test-parametric-compiler-extended ()
  (dolist (quil::*enable-state-prep-compression* '(nil t))
    (parametric-compiler-test "
DECLARE angle REAL

RY(pi/4) 0
RX(pi/5) 0
RZ(pi/6) 0
RX(pi/3) 0
CPHASE(0.9) 0 1
CPHASE(angle) 0 1
RY(pi/4) 0
RX(pi/5) 0
RZ(pi/6) 0
RX(pi/3) 0
"
                              (list (cons (mref "angle" 0) (random 1d0))))))

(deftest test-gapped-qpu ()
  (dolist (state-prep '(nil)) ; TODO XXX compression disabled until QUILC-119 is resolved
    (let ((quil::*enable-state-prep-compression* state-prep))
      (let* ((chip-spec (quil::qpu-hash-table-to-chip-specification (yason:parse "{\"isa\": {\"1Q\": {\"0\": {\"dead\": true},
  \"1\": {},
  \"2\": {}},
 \"2Q\": {\"1-2\": {}}}}")))
             (pp (quil::parse-quil-string "
H 1
CNOT 1 2"))
             (old-matrix (quil::gate-applications-to-logical-matrix pp))
             (cpp (quil::compiler-hook pp chip-spec :protoquil t))
             (new-matrix (quil::gate-applications-to-logical-matrix cpp)))
        (is (matrix-equals-dwim old-matrix new-matrix))))))

(deftest test-rewiring-backfilling ()
  (let ((pp (quil::parse-quil "
DECLARE beta REAL[1]
DECLARE gamma REAL[1]
DECLARE ro BIT[3]
H 0
H 1
H 2
PRAGMA COMMUTING_BLOCKS
PRAGMA BLOCK
CNOT 0 1
RZ(-1.507047558941458*gamma) 1
CNOT 0 1
PRAGMA END_BLOCK
PRAGMA BLOCK
CNOT 0 2
RZ(-0.4401737330167735*gamma) 2
CNOT 0 2
PRAGMA END_BLOCK
PRAGMA BLOCK
CNOT 1 2
RZ(1.2389732603453032*gamma) 2
CNOT 1 2
PRAGMA END_BLOCK
PRAGMA END_COMMUTING_BLOCKS
H 0
RZ(2.0*beta) 0
H 0
H 1
RZ(2.0*beta) 1
H 1
H 2
RZ(2.0*beta) 2
H 2
MEASURE 0 ro[0]
MEASURE 1 ro[1]
MEASURE 2 ro[2]
")))
    (quil::transform 'quil::resolve-applications pp)
    (multiple-value-bind (initial code final)
        (quil::do-greedy-temporal-addressing
            (coerce (parsed-program-executable-code pp) 'list)
          (quil::qpu-hash-table-to-chip-specification
           (yason:parse "
{\"isa\":
{\"1Q\": {\"0\": {}, \"1\": {}, \"2\": {}, \"3\": {}, \"4\": {}, \"5\": {}, \"6\": {}, \"7\": {}, \"8\": {}}, \"2Q\": {\"0-3\": {}, \"0-1\": {}, \"1-4\": {}, \"1-2\": {}, \"2-5\": {}, \"3-6\": {}, \"3-4\": {}, \"4-7\": {}, \"4-5\": {}, \"5-8\": {}, \"6-7\": {}, \"7-8\": {}}}}"))
          :use-free-swaps t)
      (declare (ignore code))
      (is (every #'identity (quil::rewiring-l2p initial)))
      (is (every #'identity (quil::rewiring-p2l initial)))
      (is (every #'identity (quil::rewiring-l2p final)))
      (is (every #'identity (quil::rewiring-p2l final))))))

(deftest test-pragma-preserve-block ()
  (let* ((pp (parse-quil-string "
H 0
PRAGMA PRESERVE_BLOCK
H 0
PRAGMA END_PRESERVE_BLOCK
"))
         ;; Strip out pragmas and halt
         (cp (remove-if (lambda (instr) (or (typep instr 'pragma)
                                       (typep instr 'halt)))
                        (parsed-program-executable-code (compiler-hook pp (build-8q-chip)))))
         (ph (parse-quil-string "H 0"))
         (ch (remove-if (lambda (instr) (or (typep instr 'pragma)
                                       (typep instr 'halt)))
                        (parsed-program-executable-code (compiler-hook ph (build-8q-chip))))))
    ;; Check first H 0 *has not* been preseved
    (is (every #'string=
               (map 'vector (lambda (instr) (print-instruction instr nil))
                    (subseq cp 0 (length ch)))
               (map 'vector (lambda (instr) (print-instruction instr nil))
                    ch)))
    ;; Check second H 0 *has* been preserved
    (is (string= (print-instruction (elt cp (length ch)) nil)
                 (print-instruction (elt (parsed-program-executable-code ph) 0) nil)))))

(deftest test-check-protoquil-program ()
  (let* ((valid-pp (parse-quil-string "
PRAGMA PRESERVE_BLOCK
RESET
PRAGMA END_PRESERVE_BLOCK
H 0
CNOT 0 1
PRAGMA PRESERVE_BLOCK
MEASURE 0
PRAGMA END_PRESERVE_BLOCK
MEASURE 1
"))
         (invalid-pp-1in2 (parse-quil-string "
PRAGMA PRESERVE_BLOCK
RESET
PRAGMA END_PRESERVE_BLOCK
H 0
CNOT 0 1
RESET
PRAGMA PRESERVE_BLOCK
MEASURE 0
PRAGMA END_PRESERVE_BLOCK
MEASURE 1
"))
         (invalid-pp-1in3 (parse-quil-string "
PRAGMA PRESERVE_BLOCK
RESET
PRAGMA END_PRESERVE_BLOCK
H 0
CNOT 0 1
PRAGMA PRESERVE_BLOCK
MEASURE 0
RESET
PRAGMA END_PRESERVE_BLOCK
MEASURE 1
"))
         (invalid-pp-2in3 (parse-quil-string "
PRAGMA PRESERVE_BLOCK
RESET
PRAGMA END_PRESERVE_BLOCK
H 0
CNOT 0 1
PRAGMA PRESERVE_BLOCK
MEASURE 0
H 0
PRAGMA END_PRESERVE_BLOCK
MEASURE 1
")))
    ;; TODO Maybe there should be a (define-condition invalid-protoquil ...) ?
    (not-signals simple-error (cl-quil::check-protoquil-program valid-pp))
    (signals simple-error (cl-quil::check-protoquil-program invalid-pp-1in2))
    (signals simple-error (cl-quil::check-protoquil-program invalid-pp-1in3))
    (signals simple-error (cl-quil::check-protoquil-program invalid-pp-2in3))))

(deftest test-global-pragma-survives-compilation ()
  "Test that a global pragma survives compilation."
  (let* ((p (with-output-to-quil
              "PRAGMA INITIAL_REWIRING \"GREEDY\""
              "PRAGMA READOUT-POVM 3 \"(0.9 0.2 0.1 0.8)\""
              "PRAGMA ADD-KRAUS X 2 \"(1.0 0.0 0.0 1.0)\""
              "PRAGMA ADD-KRAUS X 2 \"(1.0 0.0 0.0 -i)\""
              "X 0"
              "X 1"
              "X 2"
              "X 3"))
         (cp (quil:parsed-program-executable-code
              (cl-quil::compiler-hook p (quil::build-8q-chip)))))
    (is (= 1 (count-if (lambda (x) (typep x 'quil::pragma-initial-rewiring)) cp)))
    (is (= 1 (count-if (lambda (x) (typep x 'quil::pragma-readout-povm)) cp)))
    (is (= 2 (count-if (lambda (x) (typep x 'quil::pragma-add-kraus)) cp)))))
