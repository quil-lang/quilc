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

(defun do-addresser-benchmark-qft (qubits chip)
  "Run a QFT program on the given QUBITS and CHIP. Returns the time spent in the addresser."
  (let* ((program (qvm-examples::qft-circuit qubits))
         (state (make-addresser-state program chip)))
    (with-timing (1)
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
        :append (loop :repeat runs
                      :for avg := (do-addresser-benchmark-qft (a:iota i) chip)
                      :when completion-fn :do
                        (funcall completion-fn i avg)
                      :collect (cons i avg))
        :do (format t "~a of ~a~%" i max-qubits)))

;; These separate existed to determine if the addresser was sensitive
;; to the size of the chip, rather than just the size of the program.
(defun addresser-benchmark-qft-wilson ()
  (let ((wilson (build-tiled-octagon 4 4))
        (output (merge-pathnames *benchmarks-results-directory* "/addresser-wilson.txt")))
    (run-addresser-benchmarks-qft wilson 32
                                  :min-qubits 2
                                  :runs 10
                                  :setup-fn (lambda () (confirm-clear-file output))
                                  :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun addresser-benchmark-qft-1x5 ()
  (let ((1x5 (build-tiled-octagon 5 5))
        (output (merge-pathnames *benchmarks-results-directory* "/addresser-1x5.txt")))
    (run-addresser-benchmarks-qft 1x5 (* 5 8)
                                  :min-qubits 2
                                  :runs 10
                                  :setup-fn (lambda () (confirm-clear-file output))
                                  :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun addresser-benchmark-qft-2x5 ()
  (let ((1x5 (build-tiled-octagon 10 5))
        (output (merge-pathnames *benchmarks-results-directory* "/addresser-2x5.txt")))
    (run-addresser-benchmarks-qft 1x5 (* 10 8)
                                  :min-qubits 2
                                  :runs 10
                                  :setup-fn (lambda () (confirm-clear-file output))
                                  :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun addresser-benchmark-qft-denali ()
  (let ((1x5 (build-tiled-octagon 20 5))
        (output (merge-pathnames *benchmarks-results-directory* "/addresser-denali.txt")))
    (run-addresser-benchmarks-qft 1x5 (* 5 20)
                                  :min-qubits 2
                                  :runs 1
                                  :setup-fn (lambda () (confirm-clear-file output))
                                  :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun bit-reversal-program (qubits)
  (let ((program (make-instance 'quil:parsed-program)))
    (setf (quil:parsed-program-executable-code program)
          (coerce (qvm-examples:bit-reversal-circuit qubits) 'vector))
    program))

(defun do-addresser-benchmark-bit-reversal (qubits chip)
  "Run a BIT-REVERSAL program on the given QUBITS and CHIP. Returns the time spent in the addresser."
  (let* ((program (bit-reversal-program qubits))
         (state (make-addresser-state program chip)))
    (with-timing (1)
      (quil::do-greedy-addressing
        state
        (coerce (quil:parsed-program-executable-code program) 'list)
        :initial-rewiring (initial-rewiring program chip)
        :use-free-swaps t))))

(defun run-addresser-benchmarks-bit-reversal (chip max-qubits &key (min-qubits 1) (step 1) (runs 1)
                                                                setup-fn
                                                                completion-fn)
  "Run the BIT-REVERSAL benchmarks against the addresser using CHIP, running from MIN-QUBITS to MAX-QUBITS in steps of size STEP. SETUP-FN is called before any benchmarks run. COMPLETION-FN runs after every benchmark (useful for streaming results to a file)."
  (when setup-fn
    (funcall setup-fn))
  (loop :for i :from min-qubits :upto max-qubits :by step
        :append (loop :repeat runs
                      :for avg := (do-addresser-benchmark-bit-reversal (a:iota i) chip)
                      :when completion-fn :do
                        (funcall completion-fn i avg)
                      :collect (cons i avg))
        :do (format t "~a of ~a~%" i max-qubits)))

;; These separate existed to determine if the addresser was sensitive
;; to the size of the chip, rather than just the size of the program.
(defun addresser-benchmark-bit-reversal-wilson ()
  (let ((wilson (build-tiled-octagon 4 4))
        (output (merge-pathnames *benchmarks-results-directory* "/addresser-bit-reversal-wilson.txt")))
    (run-addresser-benchmarks-bit-reversal wilson 32
                                           :min-qubits 2
                                           :runs 10
                                           :setup-fn (lambda () (confirm-clear-file output))
                                           :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun addresser-benchmark-bit-reversal-1x5 ()
  (let ((1x5 (build-tiled-octagon 5 5))
        (output (merge-pathnames *benchmarks-results-directory* "/addresser-bit-reversal-1x5.txt")))
    (run-addresser-benchmarks-bit-reversal 1x5 (* 5 8)
                                           :min-qubits 2
                                           :runs 10
                                           :setup-fn (lambda () (confirm-clear-file output))
                                           :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun addresser-benchmark-bit-reversal-6oct-5wid ()
  (let ((2x5 (build-tiled-octagon 6 5))
        (output (merge-pathnames *benchmarks-results-directory* "/addresser-bit-reversal-6oct-5wid.txt")))
    (run-addresser-benchmarks-bit-reversal 2x5 (* 6 8)
                                           :min-qubits 2
                                           :runs 10
                                           :setup-fn (lambda () (confirm-clear-file output))
                                           :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun addresser-benchmark-bit-reversal-2x5 ()
  (let ((2x5 (build-tiled-octagon 10 5))
        (output (merge-pathnames *benchmarks-results-directory* "/addresser-bit-reversal-2x5.txt")))
    (run-addresser-benchmarks-bit-reversal 2x5 (* 10 8)
                                           :min-qubits 2
                                           :runs 2
                                           :setup-fn (lambda () (confirm-clear-file output))
                                           :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun addresser-benchmark-bit-reversal-denali ()
  (let ((denali (build-tiled-octagon 20 5))
        (output (merge-pathnames *benchmarks-results-directory* "/addresser-bit-reversal-denali.txt")))
    (run-addresser-benchmarks-bit-reversal denali (* 5 20)
                                           :min-qubits 2
                                           :runs 1
                                           :setup-fn (lambda () (confirm-clear-file output))
                                           :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))



(defun do-addresser-benchmark-xeb (layers chip)
  "Run a XEB program on the given QUBITS and CHIP. Returns the time spent in the addresser."
  (let* ((program (xeb-program layers chip))
         (state (make-addresser-state program chip)))
    (with-timing (1)
      (quil::do-greedy-addressing
        state
        (coerce (quil:parsed-program-executable-code program) 'list)
        :initial-rewiring (initial-rewiring program chip)
        :use-free-swaps t))))

(defun run-addresser-benchmarks-xeb (chip max-layers &key (min-layers 1) (step 1) (runs 1) setup-fn completion-fn)
  "Run the XEB benchmarks against the addresser using CHIP, running from MIN-QUBITS to MAX-QUBITS in steps of size STEP. SETUP-FN is called before any benchmarks run. COMPLETION-FN runs after every benchmark (useful for streaming results to a file)."
  (when setup-fn
    (funcall setup-fn))
  (loop :for i :from min-layers :upto max-layers :by step
        :append (loop :repeat runs
                      :for avg := (do-addresser-benchmark-xeb i chip)
                      :when completion-fn :do
                        (funcall completion-fn i avg)
                      :collect (cons i avg))
        :do (format t "~a of ~a~%" i max-layers)))

(defun addresser-benchmark-xeb-wilson ()
  (let ((denali (build-tiled-octagon 4 4))
        (output (merge-pathnames *benchmarks-results-directory* "/addresser-xeb-wilson.txt")))
    (run-addresser-benchmarks-xeb denali (* 4 8)
                                  :min-layers 2
                                  :runs 5
                                  :setup-fn (lambda () (confirm-clear-file output))
                                  :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun addresser-benchmark-xeb-1x5 ()
  (let ((denali (build-tiled-octagon 5 5))
        (output (merge-pathnames *benchmarks-results-directory* "/addresser-xeb-1x5.txt")))
    (run-addresser-benchmarks-xeb denali (* 5 8)
                                  :min-qubits 2
                                  :runs 5
                                  :setup-fn (lambda () (confirm-clear-file output))
                                  :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun addresser-benchmark-xeb-6oct-5wid ()
  (let ((denali (build-tiled-octagon 6 5))
        (output (merge-pathnames *benchmarks-results-directory* "/addresser-xeb-6oct-5wid.txt")))
    (run-addresser-benchmarks-xeb denali (* 6 8)
                                  :min-qubits 2
                                  :runs 5
                                  :setup-fn (lambda () (confirm-clear-file output))
                                  :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun addresser-benchmark-xeb-2x5 ()
  (let ((denali (build-tiled-octagon 10 5))
        (output (merge-pathnames *benchmarks-results-directory* "/addresser-xeb-2x5.txt")))
    (run-addresser-benchmarks-xeb denali (* 10 8)
                                  :min-qubits 2
                                  :runs 5
                                  :setup-fn (lambda () (confirm-clear-file output))
                                  :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun addresser-benchmark-xeb-denali ()
  (let ((denali (build-tiled-octagon 20 5))
        (output (merge-pathnames *benchmarks-results-directory* "/addresser-xeb-denali.txt")))
    (run-addresser-benchmarks-xeb denali (+ (* 5 8 4) 10)
                                  :min-qubits 2
                                  :runs 5
                                  :setup-fn (lambda () (confirm-clear-file output))
                                  :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))
