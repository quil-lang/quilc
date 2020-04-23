(in-package :cl-quil-benchmarking)

(defun initial-rewiring (program chip)
  (quil::prog-initial-rewiring
   program
   chip
   :type (quil::prog-initial-rewiring-heuristic program chip)))

(defun make-addresser-state (program chip)
  "Build an addresser state object of type *DEFAULT-ADDRESSER-STATE-CLASS* for the given CHIP specification and PROGRAM."
  (make-instance
   quil::*default-addresser-state-class*
   :chip-spec chip
   :initial-rewiring (initial-rewiring program chip)))

(defun do-addresser-benchmark-qft (qubits chip &key (runs 1))
  "Run a QFT program on the given QUBITS and CHIP, repeated RUNS times. Returns the average time spent in the addresser."
  (let* ((program (qvm-examples::qft-circuit qubits))
         (state (make-addresser-state program chip)))
    (with-timing (runs)
      (quil::do-greedy-addressing
        state
        (coerce (quil:parsed-program-executable-code program) 'list)
        :initial-rewiring (initial-rewiring program chip)
        :use-free-swaps t))))

(defun run-addresser-benchmarks-qft (chip max-qubits &key (min-qubits 1) (step 1) (runs 1)
                                                       setup-fn
                                                       completion-fn)
  "Run the QFT benchmarks against the addresser using CHIP, running from MIN-QUBITS to MAX-QUBITS in steps of size STEP. SETUP-FN is called before any benchmarks run. COMPLETION-FN runs after every benchmark (useful for streaming results to a file)."
  (when setup-fn
    (funcall setup-fn))
  (loop :for i :from min-qubits :upto max-qubits :by step
        :for (avg stddev) := (multiple-value-list (do-addresser-benchmark-qft (alexandria:iota i) chip :runs runs))
        :do (format t "~a of ~a~%" i max-qubits)
        :when completion-fn :do
          (funcall completion-fn i avg stddev)
        :collect (cons i avg)))
