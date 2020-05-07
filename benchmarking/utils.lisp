(in-package :cl-quil-benchmarking)

(defmacro with-timing ((n) &body body)
  "Evaluate BODY N times, returning the average run-time."
  (alexandria:with-gensyms (time-start)
    `(loop :repeat ,n
           :for ,time-start := (get-internal-real-time)
           :do (progn ,@body)
           :collect (/ (- (get-internal-real-time) ,time-start)
                       internal-time-units-per-second)
             :into timings
           :finally (return (values (alexandria:mean timings)
                                    (alexandria:standard-deviation timings))))))

(defun chip-spec-offset-qubits (chip offset))

(defmacro with-output-appending-file ((stream filespec) &body body)
  `(with-open-file (,stream
                    ,filespec
                    :direction :output
                    :if-does-not-exist :create
                    :if-exists :append)
     ,@body))

(defun tiled-octagon-graph (number-of-octagons max-width)
  (labels
      ((col-1- (n) (unless (zerop (mod n max-width)) (1- n)))
       (row-1- (n) (and (>= n max-width) (- n max-width)))
       (qubit (index octagon-index) (+ (* octagon-index 10) index))
       (make-octagon (&optional (octagon-index 0))
         (loop :for i :from 0 :below 8 :collect (list (qubit i octagon-index)
                                                      (qubit (mod (1+ i) 8)
                                                             octagon-index))))
       (link-left-octagon (octagon-index)
         (let* ((left-octagon-index (col-1- octagon-index))
                (left-qubits (mapcar (alexandria:rcurry #'qubit octagon-index) '(6 5)))
                (right-qubits (mapcar (alexandria:rcurry #'qubit left-octagon-index) '(1 2))))
           (mapcar #'list right-qubits left-qubits)))
       (link-below-octagon (octagon-index)
         (let* ((below-octagon-index (row-1- octagon-index))
                (bottom-qubits (mapcar (alexandria:rcurry #'qubit octagon-index) '(0 7)))
                (top-qubits (mapcar (alexandria:rcurry #'qubit below-octagon-index) '(3 4))))
           (mapcar #'list top-qubits bottom-qubits))))
    (loop :for octagon-index :from 0 :below number-of-octagons
          :for left-octagon-index := (col-1- octagon-index)
          :for below-octagon-index := (row-1- octagon-index)
          :append (make-octagon octagon-index)
          :when left-octagon-index
            :append (link-left-octagon octagon-index)
          :when below-octagon-index
            :append (link-below-octagon octagon-index))))

(defun build-tiled-octagon (number-of-octagons max-width)
  (let ((chip (quil::build-chip-from-digraph (tiled-octagon-graph number-of-octagons max-width))))
    (loop :for i :below (1- number-of-octagons) :do
      (setf (gethash "dead" (quil::hardware-object-misc-data (quil::chip-spec-nth-qubit chip (+ (* 10 i) 8)))) t)
      (setf (gethash "dead" (quil::hardware-object-misc-data (quil::chip-spec-nth-qubit chip (+ (* 10 i) 9)))) t))
    chip))

(defun file> (filepath fmt &rest args)
  (check-type filepath pathname)
  (check-type fmt string)
  (with-open-file (s filepath :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string (apply #'format nil fmt args) s)))

(defun file>> (filepath fmt &rest args)
  (check-type filepath pathname)
  (check-type fmt string)
  (with-open-file (s filepath :direction :output :if-exists :append :if-does-not-exist :create)
    (write-string (apply #'format nil fmt args) s)))

(defun clear-file (filepath)
  "If a file at FILEPATH exists, delete and recreate it, otherwise create it."
  (check-type filepath pathname)
  (file> filepath ""))

(defparameter *benchmarks-results-directory*
  (asdf:system-relative-pathname
   ':cl-quil-benchmarking
   "benchmarking/results/"))

(defun native-rz (&optional (qubit (random 10)))
  (quil:build-gate "RZ" (list (random 2pi)) qubit))
(defun native-rx (&optional (qubit (random 10)))
  (quil:build-gate "RX" (list (a:random-elt (list 0d0 -pi -pi/2 pi pi/2))) qubit))
(defun native-cz (&optional ctrl tgt)
  (let* ((possible-qubits (a:iota 10))
         (ctrl (or ctrl (a:random-elt possible-qubits)))
         (tgt (or tgt (a:random-elt (remove ctrl possible-qubits)))))
    (quil:build-gate "CZ" nil ctrl tgt)))

(defvar *1q-program-generators*
  (list #'native-rz #'native-rx))

(defvar *2q-program-generators*
  (append *1q-program-generators* (list #'native-cz)))

(defun parsed-program-from-straight-line-quil (instructions)
  (make-instance 'quil:parsed-program
                 :executable-code (coerce instructions 'vector)))

(defun random-1q-program (qubit length &key (instruction-generators *1q-program-generators*))
  "Make a program with LENGTH many randomly chosen RZ or RX gates on QUBIT."
  (parsed-program-from-straight-line-quil
   (loop :repeat length
         :for generator := (a:random-elt instruction-generators)
         :collect (funcall generator qubit))))

(defun random-nq-program (length &key (instruction-generators *2q-program-generators*))
  "Make a program with LENGTH many randomly chosen RZ or RX gates each on a random qubit chosen from QUBITS."
  (parsed-program-from-straight-line-quil
   (loop :repeat length
         :for generator := (a:random-elt instruction-generators)
         :collect (funcall generator))))

(defun xeb-program (layers chip-spec &key (use-1q-layers t))
  (let ((2q-layers
          (loop :repeat layers
                :collect
                (let ((qubits (quil::chip-spec-live-qubits chip-spec)))
                  (flet ((pop-random ()
                           (let ((elt (a:random-elt qubits)))
                             (setf qubits (remove elt qubits))
                             elt))
                         (pop-random-neighbor (q)
                           (a:when-let* ((n (intersection (quil::chip-spec-adj-qubits chip-spec q) qubits))
                                         (elt (a:random-elt n)))
                             (setf qubits (remove elt qubits))
                             elt)))
                    (loop :for q := (pop-random)
                          :for qn := (pop-random-neighbor q)
                          :repeat 4
                          :when qn
                            :collect (quil:build-gate "CZ" nil q qn) :into czs
                          :finally
                             (return czs)))))))
    (flet ((random-1q (q)
             (a:random-elt (list (quil:build-gate "RZ" `(,pi/2) q)
                                 (quil:build-gate "RZ" (list (/ pi 3)) q)
                                 (quil:build-gate "RX" `(,pi/2) q)))))
      (let ((circuit (a:flatten
                      (if use-1q-layers
                          (mapcar (lambda (2q-layer)
                                    (append (list (quil:make-pragma '("LATEX_GATE_GROUP")))
                                            (mapcar #'random-1q (quil::chip-spec-live-qubits chip-spec))
                                            2q-layer
                                            (list (quil:make-pragma '("END_LATEX_GATE_GROUP")))))
                                  2q-layers)
                          2q-layers))))
        (parsed-program-from-straight-line-quil circuit)))))

(defmacro confirm ((message &rest format-args) &body body)
  `(when (y-or-n-p ,message ,@format-args)
     ,@body))

(defun confirm-clear-file (file)
  (confirm ("clear file ~S?" file)
    (clear-file file)))
