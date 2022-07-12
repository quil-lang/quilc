;;;; god-table-utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil/clifford)

(defun write-gt-stream (gt f)
  "Write out the God table GT to the stream F."
  (check-type gt god-table)
  (check-type f stream)
  (let ((size (hash-table-count (mapping gt)))
        (processed 0))
    (format f "~Dq Cliffords: ~D~2%"
            (num-qubits gt)
            (hash-table-count (mapping gt)))
    (format f "GENERATORS:~2%")
    (loop :for i :from 0
          :for g :in (cliffords (gateset gt))
          :do (format f "~D~%" i)
              (prin1 g f)
              (terpri f))
    (terpri f)
    (terpri f)
    (format f "GOD TABLE:~2%")
    (maphash (lambda (k v)
               (prin1 v f)
               (terpri f)
               (prin1 k f)
               (terpri f)
               (terpri f)
               (incf processed)
               (when (zerop (mod processed 100000))
                 (format t "~3,2F%~%" (* 100 (/ processed size)))))
             (mapping gt)))
  nil)

(defun write-gt (gt filename)
  "Write a compressed God table GT to the file FILENAME."
  (let ((octets (babel:string-to-octets
                 (with-output-to-string (s)
                   (write-gt-stream gt s))
                 :encoding :utf-8)))
    (with-open-file (raw-f filename :direction ':output
                                    :if-exists ':supersede
                                    :if-does-not-exist ':create
                                    :element-type '(unsigned-byte 8))
      (map nil (lambda (byte)
                 (write-byte byte raw-f))
           (salza2:compress-data octets 'salza2:gzip-compressor)))
    'done))

(defun distance-table (gt)
  (let ((a (make-array 50 :initial-element 0))
        (n 0)
        (ct (hash-table-count (mapping gt))))
    (maphash (lambda (k v)
               (declare (ignore v))
               (incf n)
               (when (zerop (mod n 100000))
                 (format t "~3,1F%~%" (* 100 (/ n ct))))
               (incf (aref a (length (reconstruct k gt)))))
             (mapping gt))
    a))

