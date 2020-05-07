(in-package :cl-quil-benchmarking)

;; (defun do-compression-experiment-native-rz-only ()
;;   (loop :with chip := (build-nq-linear-chip 1)
;;         :for i :from 1e3 :upto 1e4 :by 1e2
;;         :for program := (parsed-program-executable-code
;;                          (random-1q-program 0 i :instruction-generators (list #'native-rz)))
;;         :collect (let ((time (get-internal-real-time)))
;;                    (format t "~4F%~%" (* 100d0 (/ i 1e4)))
;;                    (compress-instructions (coerce program 'list) chip)
;;                    (compress-instructions (coerce program 'list) chip)
;;                    (compress-instructions (coerce program 'list) chip)
;;                    (cons i
;;                          (/ (- (get-internal-real-time) time)
;;                             internal-time-units-per-second 3d0)))))

;; (defun do-compression-experiment-native-rz-rx-only ()
;;   (loop :with chip := (build-nq-linear-chip 1)
;;         :for i :from 1e3 :upto 1e5  :by 1e3
;;         :for program := (parsed-program-executable-code
;;                          (random-1q-program
;;                           0 i :instruction-generators (list #'native-rz #'native-rx)))
;;         :collect (let ((time (get-internal-real-time)))
;;                    (format t "~4F%~%" (* 100d0 (/ i 1e5)))
;;                    (compress-instructions (coerce program 'list) chip)
;;                    (compress-instructions (coerce program 'list) chip)
;;                    (compress-instructions (coerce program 'list) chip)
;;                    (cons i
;;                          (/ (- (get-internal-real-time) time)
;;                             internal-time-units-per-second 3d0)))))

(defun do-compressor-benchmark-1q-program (length chip)
  (let ((program (random-1q-program 0 length)))
    (with-timing (5)
      (quil::compress-instructions
       (coerce (quil:parsed-program-executable-code program) 'list)
       chip))))

(defun run-compressor-benchmarks-1q-program (chip max-length
                                             &key (min-length 0) (step 1) (runs 1)
                                               setup-fn
                                               completion-fn)
  (when setup-fn
    (funcall setup-fn))

  (loop :for i :from min-length :upto max-length :by step
        :append (loop :repeat runs
                      :for avg := (do-compressor-benchmark-1q-program i chip)
                      :when completion-fn :do
                        (funcall completion-fn i avg)
                      :collect (cons i avg))
        :do (format t "~a of ~a~%" i max-length)))

(defun compressor-benchmark-1q-program-wilson ()
  (let ((wilson (build-tiled-octagon 4 4))
        (output (merge-pathnames *benchmarks-results-directory* "/compressor-1q-wilson.txt")))
    (run-compressor-benchmarks-1q-program wilson 10000
                                          :step 100
                                          :setup-fn (lambda () (confirm-clear-file output))
                                          :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun compressor-benchmark-1q-program-1x5 ()
  (let ((1x5 (build-tiled-octagon 5 5))
        (output (merge-pathnames *benchmarks-results-directory* "/compressor-1q-1x5.txt")))
    (run-compressor-benchmarks-1q-program 1x5 10000
                                          :step 100
                                          :setup-fn (lambda () (confirm-clear-file output))
                                          :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun compressor-benchmark-1q-program-6oct-5wid ()
  (let ((6oct-5wid (build-tiled-octagon 6 5))
        (output (merge-pathnames *benchmarks-results-directory* "/compressor-1q-6oct-5wid.txt")))
    (run-compressor-benchmarks-1q-program 6oct-5wid 10000
                                          :step 100
                                          :setup-fn (lambda () (confirm-clear-file output))
                                          :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun compressor-benchmark-1q-program-2x5 ()
  (let ((2x5 (build-tiled-octagon 10 5))
        (output (merge-pathnames *benchmarks-results-directory* "/compressor-1q-2x5.txt")))
    (run-compressor-benchmarks-1q-program 2x5 10000
                                          :step 100
                                          :setup-fn (lambda () (confirm-clear-file output))
                                          :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun compressor-benchmark-1q-program-denali ()
  (let ((denali (build-tiled-octagon 20 5))
        (output (merge-pathnames *benchmarks-results-directory* "/compressor-1q-denali.txt")))
    (run-compressor-benchmarks-1q-program denali 10000
                                          :step 100
                                          :setup-fn (lambda () (confirm-clear-file output))
                                          :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

;; (defun do-compressor-benchmark-nq-program (n-qubits length chip)
;;   (let ((program (random-nq-program (a:iota n-qubits) length)))
;;     (with-timing (1)
;;       (quil::compress-instructions
;;        (coerce (quil:parsed-program-executable-code program) 'list)
;;        chip))))

;; (defun run-compressor-benchmarks-nq-program (chip num-instructions max-qubits
;;                                              &key (min-qubits 1) (step 1) (runs 1)
;;                                                setup-fn
;;                                                completion-fn)
;;   (when setup-fn
;;     (funcall setup-fn))
;;   (loop :for i :from min-qubits :upto max-qubits :by step
;;         :append (loop :repeat runs
;;                       :for avg := (do-compressor-benchmark-nq-program i num-instructions chip)
;;                       :when completion-fn :do
;;                         (funcall completion-fn i avg)
;;                       :collect (cons i avg))
;;         :do (format t "~a of ~a~%" i max-qubits)))

;; (defun compressor-benchmark-nq-program-wilson-10000 ()
;;   (let ((wilson (build-tiled-octagon 4 4))
;;         (output (merge-pathnames *benchmarks-results-directory* "/compressor-nq-wilson-10000.txt")))
;;     (run-compressor-benchmarks-nq-program wilson 10000 32
;;                                           :runs 5
;;                                           :setup-fn (lambda () (confirm-clear-file output))
;;                                           :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))
































(defun do-compression-experiment-native-rz-rx-only ()
  (let ((count 0)
        (lock (bt:make-lock))
        (num-exps 100)
        (start 4e4)
        (stop 100000)
        (chip (quil::build-nq-linear-chip 1))
        (lparallel:*kernel* (lparallel:make-kernel 4)))
    (flet ((experiment (length)
             (let* ((program
                      (parsed-program-executable-code
                       (random-1q-program
                        0 length :instruction-generators (list #'native-rz
                                                               #'native-rx))))
                    (time (get-internal-real-time)))
               (quil::compress-instructions (coerce program 'list) chip)
               (quil::compress-instructions (coerce program 'list) chip)
               (prog1
                   (cons length
                         (/ (- (get-internal-real-time) time)
                            internal-time-units-per-second 2d0))
                 (bt:with-lock-held (lock)
                   (incf count)
                   (format t "~4F%~%" (* 100 (/ count num-exps))))))))
      (lparallel:pmapcar #'experiment
                         (alexandria:iota num-exps
                                          :start start
                                          :step (/ stop num-exps))))))

(defun do-compression-experiment-native-rz-only ()
  (let ((count 0)
        (lock (bt:make-lock))
        (num-exps 100)
        (start 4e4)
        (stop 100000)
        (chip (build-nq-linear-chip 1))
        (lparallel:*kernel* (lparallel:make-kernel 4)))
    (flet ((experiment (length)
             (let* ((program
                     (parsed-program-executable-code
                      (random-1q-program
                       0 length :instruction-generators (list #'native-rz))))
                    (time (get-internal-real-time)))
               (compress-instructions (coerce program 'list) chip)
               (compress-instructions (coerce program 'list) chip)
               (prog1
                   (cons length
                         (/ (- (get-internal-real-time) time)
                            internal-time-units-per-second 2d0))
                 (bt:with-lock-held (lock)
                   (incf count)
                   (format t "~4F%~%" (* 100 (/ count num-exps))))))))
      (lparallel:pmapcar #'experiment
                         (alexandria:iota num-exps
                                 :start start
                                 :step (/ stop num-exps))))))

(defun save-compression-experiment (results name)
  (time
   (with-open-file (s (format nil "results/compression-~A.txt" name)
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
     (dolist (result results)
       (format s "~F ~F~%" (car result) (cdr result))))))


(defun do-compressor-benchmark-xeb (qubits chip)
  "Run a XEB program on the given QUBITS and CHIP. Returns the time spent in the addresser."
  (let* ((program (xeb-program qubits chip))
         (instructions (coerce (parsed-program-executable-code program) 'list)))
    (with-timing (1)
      (quil::compress-instructions instructions chip))))

(defun run-compressor-benchmarks-xeb
    (chip max-qubits &key (min-qubits 1) (step 1) (runs 1) setup-fn completion-fn)
  "Run the XEB benchmarks against the compressor using CHIP, running from MIN-QUBITS to MAX-QUBITS in steps of size STEP. SETUP-FN is called before any benchmarks run. COMPLETION-FN runs after every benchmark (useful for streaming results to a file)."
  (when setup-fn
    (funcall setup-fn))
  (loop :for i :from min-qubits :upto max-qubits :by step
        :append (loop :repeat runs
                      :for avg := (do-compressor-benchmark-xeb (a:iota i) chip)
                      :when completion-fn :do
                        (funcall completion-fn i avg)
                      :collect (cons i avg))
        :do (format t "~a of ~a~%" i max-qubits)))

(defun compressor-benchmark-xeb-wilson ()
  (let ((denali (build-tiled-octagon 4 4))
        (output (merge-pathnames *benchmarks-results-directory* "/compressor-xeb-wilson.txt")))
    (run-compressor-benchmarks-xeb denali (* 4 8)
                                   :min-qubits 2
                                   :runs 5
                                   :setup-fn (lambda () (confirm-clear-file output))
                                   :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun compressor-benchmark-xeb-1x5 ()
  (let ((denali (build-tiled-octagon 5 5))
        (output (merge-pathnames *benchmarks-results-directory* "/compressor-xeb-1x5.txt")))
    (run-compressor-benchmarks-xeb denali (* 5 8)
                                   :min-qubits 2
                                   :runs 5
                                   :setup-fn (lambda () (confirm-clear-file output))
                                   :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun compressor-benchmark-xeb-6oct-5wid ()
  (let ((denali (build-tiled-octagon 6 5))
        (output (merge-pathnames *benchmarks-results-directory* "/compressor-xeb-6oct-5wid.txt")))
    (run-compressor-benchmarks-xeb denali (* 6 8)
                                   :min-qubits 2
                                   :runs 5
                                   :setup-fn (lambda () (confirm-clear-file output))
                                   :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun compressor-benchmark-xeb-2x5 ()
  (let ((denali (build-tiled-octagon 10 5))
        (output (merge-pathnames *benchmarks-results-directory* "/compressor-xeb-2x5.txt")))
    (run-compressor-benchmarks-xeb denali (* 10 8)
                                   :min-qubits 2
                                   :runs 5
                                   :setup-fn (lambda () (confirm-clear-file output))
                                   :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))

(defun compressor-benchmark-xeb-denali ()
  (let ((denali (build-tiled-octagon 20 5))
        (output (merge-pathnames *benchmarks-results-directory* "/compressor-xeb-denali.txt")))
    (run-compressor-benchmarks-xeb denali (+ (* 5 8 4) 10)
                                   :min-qubits 2
                                   :runs 5
                                   :setup-fn (lambda () (confirm-clear-file output))
                                   :completion-fn (lambda (i avg) (file>> output "~D ~F~%" i avg)))))
