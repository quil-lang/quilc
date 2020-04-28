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

(defun do-compression-experiment-native-rz-rx-only ()
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
                       0 length :instruction-generators (list #'native-rz
                                                              #'native-rx))))
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

