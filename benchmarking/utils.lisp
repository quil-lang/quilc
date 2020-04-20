(in-package :cl-quil-benchmarking)

(defmacro with-timing ((n) &body body)
  "Evaluate BODY N times, returning the average run-time."
  (alexandria:with-gensyms (time-start)
    `(loop :repeat ,n
           :for ,time-start := (get-internal-real-time)
           :do (progn ,@body)
           :summing (/ (- (get-internal-real-time) ,time-start)
                       internal-time-units-per-second)
             :into timings
           :finally (return (/ timings ,n)))))

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
  (quil::build-chip-from-digraph (tiled-octagon-graph number-of-octagons max-width)))
