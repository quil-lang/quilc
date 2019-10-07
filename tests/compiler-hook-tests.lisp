;;;; compiler-hook-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-tests)

(defun attach-rewirings-to-program (pp in-rewiring-vector out-rewiring-vector)
  (check-type in-rewiring-vector (or null quil::integer-vector))
  (check-type out-rewiring-vector (or null quil::integer-vector))

  (unless (and (null in-rewiring-vector) (null out-rewiring-vector))
    (let ((code (quil::parsed-program-executable-code pp)))
      (cond
        ((< (length code) 1)
         (error "Cannot attach rewirings to program with no instructions"))
        ((= (length code) 1)
         (setf (quil::comment (aref code 0))
               (quil::make-rewiring-comment :entering in-rewiring-vector
                                            :exiting out-rewiring-vector)))
        (t
         (when (not (null in-rewiring-vector))
           (setf (quil::comment (aref code 0))
                 (quil::make-rewiring-comment :entering in-rewiring-vector)))
         (when (not (null out-rewiring-vector))
           (setf (quil::comment (aref code (1- (length code))))
                 (quil::make-rewiring-comment :exiting out-rewiring-vector)))))))
  pp)

(defun %parsed-program-to-logical-matrix-rewiring-test (pp-a pp-b)
  (dolist (compress-qubits '(nil t))
    (is (quil::operator=
         (quil::parsed-program-to-logical-matrix pp-a)
         (quil::parsed-program-to-logical-matrix pp-b :compress-qubits compress-qubits)))))

(deftest test-parsed-program-to-logical-matrix-cnot-rewiring ()
  "Test whether quil::parsed-program-to-logical-matrix converts equivalent
programs (modulo rewiring) to equivalent matrices."
  (let ((pp (quil::parse-quil "
CNOT 1 2
CNOT 1 0"))
        (pp-rewired (attach-rewirings-to-program (quil::parse-quil "
CNOT 0 1
CNOT 0 2
")
                                                 #(2 0 1) #(2 0 1))))
    (%parsed-program-to-logical-matrix-rewiring-test pp pp-rewired)))

(deftest test-parsed-program-to-logical-matrix-swap-rewiring ()
  "Test whether quil::parsed-program-to-logical-matrix converts equivalent
programs (modulo rewiring) to equivalent matrices."
  (let ((pp (quil::parse-quil "
CNOT 0 1
Z 0
SWAP 0 1"))
        (pp-rewired (attach-rewirings-to-program (quil::parse-quil "
CNOT 0 1
Z 0")
                                                 #(0 1) #(1 0))))
    (%parsed-program-to-logical-matrix-rewiring-test pp pp-rewired)))

(deftest test-parsed-program-to-logical-matrix-entering-exiting-rewiring ()
  "Test whether quil::parsed-program-to-logical-matrix handles single-instruction entering/exiting
rewirings correctly."
  (let ((pp (quil::parse-quil "
CNOT 0 1
SWAP 0 1"))
        (pp-rewired (attach-rewirings-to-program (quil::parse-quil "
CNOT 0 1")
                                                 #(0 1) #(1 0))))
    (%parsed-program-to-logical-matrix-rewiring-test pp pp-rewired)))

(deftest test-rewiring-modes ()
  "Iterates over the rewiring modes and tests that the addresser is well-behaved on each of them."
  ;; first, the straight-line rewiring methods
  (dolist (quil::*initial-rewiring-default-type* '(:naive :random :partial :greedy))
    (format t "~&    Testing rewiring type ~A~%" quil::*initial-rewiring-default-type*)
    (dolist (pstring (list "CNOT 2 0" (format nil "CNOT 0 2~%CNOT 1 3")))
      (let* ((pp (quil::parse-quil pstring))
             (cpp (quil::compiler-hook (quil::parse-quil pstring)
                                       (quil::build-nQ-linear-chip (quil:qubits-needed pp))
                                       :protoquil t)))
        (format t "~&        Testing program ~A~%" (parsed-program-executable-code pp))
        (finish-output)
        (%parsed-program-to-logical-matrix-rewiring-test pp cpp))))
  ;; then, the block-to-block rewiring methods.
  ;; i'm too lazy to check correctness, but we're at least exercising the pathway.
  (dolist (quil::*addresser-move-to-rewiring-swap-search-type* '(:greedy-path :greedy-qubit :a*))
    (format t "~&    Testing addresser move type ~A~%" quil::*addresser-move-to-rewiring-swap-search-type*)
    (finish-output)
    (let* ((pp (quil::parse-quil "
LABEL @a
CNOT 0 1
CNOT 1 2
CNOT 0 2
JUMP @a")))
      (not-signals error (quil::compiler-hook pp (quil::build-8Q-chip))))))

(defun compare-compiled (file architecture)
  "Compares the matrix representation of the (uncompiled) program in FILE against the compiled version. Compilation uses a linear chip topology with architecture ARCHITECTURE."
  (let* ((orig-prog (quil::transform 'quil::compress-qubits
                                     (cl-quil::read-quil-file file)))
         (proc-prog
           (quil::compiler-hook (quil::transform 'quil::compress-qubits
                                                 (cl-quil::read-quil-file file))
                                (quil::build-nQ-linear-chip 5 :architecture architecture)
                                :protoquil t)))
    (values
     (quil::matrix-equals-dwim (quil::parsed-program-to-logical-matrix orig-prog)
                               (quil::parsed-program-to-logical-matrix proc-prog))
     (quil::calculate-instructions-2q-depth (coerce (quil::parsed-program-executable-code proc-prog)
                                                    'list)))))

(defparameter *compiler-hook-test-file-directory*
  (asdf:system-relative-pathname
   ':cl-quil-tests
   "tests/compiler-hook-test-files/"))

(deftest test-compiler-hook ()
  (let ((num-tasks 0)
        (tasks-lock (bt:make-lock)))
    (flet ((create-task (state-prep file arch)
             ;; Better safe than sorry.
             (bt:with-lock-held (tasks-lock)
               (incf num-tasks))
             (lambda ()
               (let ((quil::*enable-state-prep-compression* state-prep))
                 (format fiasco::*test-run-standard-output*
                         "    Testing file ~a (~a, with~:[out~;~] state prep)~%"
                         (pathname-name file) arch state-prep)
                 (compare-compiled file arch)))))
      (let ((ch (lparallel:make-channel)))
        (dolist (state-prep (list nil t))
          (dolist (file (uiop:directory-files *compiler-hook-test-file-directory* #P"*.quil"))
            (dolist (arch (list ':cz ':iswap ':cphase ':piswap ':cnot))
              (lparallel:submit-task ch (create-task state-prep file arch)))))
        (loop :repeat num-tasks :do
          (let ((fiasco::*print-test-run-progress* nil))
            (is (lparallel:receive-result ch))))))))

(deftest test-compression-bug-QUILC-152 ()
  "QUILC-152: A bug in state compression caused a failed assertion."
  (let ((quil::*enable-state-prep-compression* t)
        (quil::*compress-carefully* t)
        (instructions (quil:parsed-program-executable-code
                       (quil:parse-quil "
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
     (quil::set-up-compilation-context :qubit-count 4 :simulate t))))

(defun shuffle-list (l &optional (k nil))
        (let* ((elt (nth (random (length l)) l))
               (l (remove elt l))
               (k (cons elt k)))
          (if (zerop (length l))
              k
              (shuffle-list l k))))

(deftest test-compiler-hook-random-4Q ()
  (finish-output fiasco::*test-run-standard-output*)
  (let ((num-tasks 0)
        (tasks-lock (bt:make-lock)))
    (flet ((create-task (state-prep arch)
             ;; Better safe than sorry.
             (bt:with-lock-held (tasks-lock)
               (incf num-tasks))
             (lambda ()
               (let ((quil::*enable-state-prep-compression* state-prep))
                 (format fiasco::*test-run-standard-output*
                         "    Testing architecture ~a, with~:[out~;~] state prep~%"
                         arch state-prep)
                 (let* ((num-qubits 4)
                        (v (quil::random-special-unitary (expt 2 num-qubits)))
                        (args (shuffle-list (a:iota num-qubits :start (1- num-qubits) :step -1)))
                        (parsed-prog
                          (make-instance
                           'quil::parsed-program
                           :executable-code (make-array 1
                                                        :initial-element (make-instance
                                                                          'quil::gate-application
                                                                          :operator (named-operator "RANDO-GATE")
                                                                          :gate v
                                                                          :arguments (mapcar #'qubit args)))))
                        (processed-program
                          (quil::compiler-hook parsed-prog (quil::build-nQ-linear-chip num-qubits
                                                                                       :architecture arch))))
                   (quil::matrix-equals-dwim (quil::kq-gate-on-lines v num-qubits args)
                                             (quil::parsed-program-to-logical-matrix processed-program)))))))
      (let ((ch (lparallel:make-channel)))
        (dolist (state-prep (list nil t))
          (dolist (arch (list ':cz ':iswap ':cphase ':piswap ':cnot))
            (lparallel:submit-task ch (create-task state-prep arch))))
        (loop :repeat num-tasks :do
          (let ((fiasco::*print-test-run-progress* nil))
            (is (lparallel:receive-result ch))))))))

(deftest test-compiler-hook-preserves-RESETs ()
  (let* ((pp (quil::parse-quil "
PRAGMA INITIAL_REWIRING \"PARTIAL\"
RESET
DECLARE ro BIT[2]
RY(pi/3) 0
RY(pi/3) 1
RY(pi/3) 2
RY(pi/3) 3
H 0
MEASURE 0 ro[1]"))
         (cpp (quil::compiler-hook pp (quil::build-8Q-chip))))
    (loop :for instr :across (quil::parsed-program-executable-code cpp)
          :count (typep instr 'quil::reset) :into reset-count
          :count (not (typep instr 'quil::gate-application)) :into non-application-count
          :finally (progn
                     (is (= 1 reset-count))
                     (is (= 4 non-application-count))))))


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

         (make-pp () (quil::parse-quil program-string :transforms nil)))
    (let* ((chip (quil::build-8Q-chip :architecture ':cz))
           (processed-pp (compiler-hook (make-pp) chip))
           (orig-pp (make-pp)))
      (substitute-params orig-pp segment-table)
      (substitute-params processed-pp segment-table)
      (is (quil::matrix-equals-dwim (quil::parsed-program-to-logical-matrix orig-pp :compress-qubits t)
                                    (quil::parsed-program-to-logical-matrix processed-pp :compress-qubits t))))))

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
             (pp (quil::parse-quil "
H 1
CNOT 1 2"))
             (old-matrix (quil::parsed-program-to-logical-matrix pp))
             (cpp (quil::compiler-hook pp chip-spec :protoquil t))
             (new-matrix (quil::parsed-program-to-logical-matrix cpp)))
        (is (quil::matrix-equals-dwim old-matrix new-matrix))))))

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
  (let* ((pp (parse-quil "
H 0
PRAGMA PRESERVE_BLOCK
H 0
PRAGMA END_PRESERVE_BLOCK
"))
         ;; Strip out pragmas and halt
         (cp (remove-if (lambda (instr) (or (typep instr 'pragma)
                                       (typep instr 'halt)))
                        (parsed-program-executable-code (compiler-hook pp (build-8q-chip)))))
         (ph (parse-quil "H 0"))
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
  (let* ((valid-pp (parse-quil "
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
         (invalid-pp-1in2 (parse-quil "
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
         (invalid-pp-1in3 (parse-quil "
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
         (invalid-pp-2in3 (parse-quil "
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
    (not-signals quil:not-protoquil (quil:check-protoquil-program valid-pp))
    (signals quil:not-protoquil (quil:check-protoquil-program invalid-pp-1in2))
    (signals quil:not-protoquil (quil:check-protoquil-program invalid-pp-1in3))
    (signals quil:not-protoquil (quil:check-protoquil-program invalid-pp-2in3))))

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

(deftest test-clever-CCNOT-depth-reduction ()
  "Test that the ':GREEDY-QUBIT swap selection strategy brings CZ depth down to optimal for CCNOT."
  (let ((p (quil::compiler-hook (quil::parse-quil "
PRAGMA INITIAL_REWIRING \"GREEDY\"
CCNOT 0 1 2")
                                (quil::build-8Q-chip)))
        (ls (quil::make-lscheduler)))
    (quil::append-instructions-to-lschedule ls (coerce (quil::parsed-program-executable-code p)
                                                       'list))
    (flet
        ((value-bumper (instr value)
           (cond
             ((not (typep instr 'gate-application))
              value)
             ((adt:with-data (named-operator name) (application-operator instr)
                (string= "CZ" name))
              (1+ value))
             (t value))))
      (let ((CZ-depth (quil::lscheduler-walk-graph ls :bump-value #'value-bumper)))
        (is (>= 8 CZ-depth))))))

(deftest test-resource-carving-basic ()
  (let* ((chip (build-8Q-chip))
         (sched (quil::make-chip-schedule chip)))
    (map nil (lambda (instr) (quil::chip-schedule-append sched instr))
         (list (quil::build-gate "CZ" () 0 1)
               (quil::build-gate "CZ" () 2 3)
               (quil::build-gate "CZ" () 1 2)
               (quil::build-gate "CZ" () 0 3)
               (quil::build-gate "RX" '(#.(/ pi 2)) 1)))
    (multiple-value-bind (order index obj)
        (quil::lookup-hardware-address-by-qubits chip (list 1 2))
      (declare (ignore order index))
      (is (= (quil::hardware-object-native-instruction-p obj (quil::build-gate "CZ" () 1 2))
             (quil::chip-schedule-resource-carving-point sched (quil::make-qubit-resource 1 2)))))))
