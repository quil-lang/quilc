;;;; chip-cache.lisp
;;;;
;;;; Chip caching for faster compilation requests
;;;;
;;;; Author: Mark Skilbeck
;;;;

(in-package #:quilc)

(defclass cached-chip ()
  ((last-accessed
    :initarg :last-accessed
    :type real
    :accessor cached-chip-last-accessed)
   (chip
    :initarg :chip
    :type chip-specification
    :accessor cached-chip-chip)
   (addresser-state
    :initarg :addresser-state
    :type cl-quil::addresser-state
    :accessor cached-chip-addresser-state))
  (:documentation "Represents chip and addresser state cached between server requests."))

(defvar *chip-cache*
  (cons (make-hash-table :test #'equalp)
        (bt:make-lock "chip cache lock"))
  "Cached chip specifications. Pair of values (hash-table . lock). Large chips have a significant construction overhead, and caching chips between requests reduces or eliminates that overhead. The hash value for a given key is a pair (last-access . chip-spec). Note: use the lock when accessing.")

(defvar *chip-cache-max-size* 10
  "The maximum number of entries in the cache.")

(defun get-internal-real-time-seconds ()
  (/ (get-internal-real-time)
     internal-time-units-per-second))

(defun chip-cache-purge ()
  "Purge the least recently used entries in *CHIP-CACHE* according to *CHIP-CACHE-MAX-SIZE*.

After calling this function, *CHIP-CACHE* has at most *CHIP-CACHE-MAX-SIZE* entries."
  (let ((cache (car *chip-cache*)))
    (when (>= (hash-table-count cache) *chip-cache-max-size*)
      (let* ((n (- (hash-table-count cache) *chip-cache-max-size*))
             (lru (subseq (sort (a:hash-table-alist cache)
                                #'<
                                :key (a:compose #'cached-chip-last-accessed #'cdr))
                          0 n)))
        (dolist (cached lru)
          (remhash (car cached) cache))))))

(defun chip-cache-or-create (qpu-hash)
  "Look up the chip described by QPU-HASH in the chip spec cache if it exists, otherwise create and cache it.

This function has the added side-effect that it will purge old chips according to *CHIP-CACHE-MAX-SIZE*"
  (let* ((cache (car *chip-cache*))
         (cached-chip (gethash qpu-hash cache)))
    (cond
      (cached-chip
       (setf (cached-chip-last-accessed cached-chip)
             (get-internal-real-time-seconds))
       cached-chip)
      (t
       (let* ((chip (cl-quil::qpu-hash-table-to-chip-specification qpu-hash))
              (cached-chip (make-instance 'cached-chip
                                          :last-accessed (get-internal-real-time-seconds)
                                          :chip chip)))
         (setf (gethash qpu-hash cache)
               cached-chip)
         (chip-cache-purge)
         cached-chip)))))
