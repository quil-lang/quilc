;;;; astar-rewiring-search.lisp
;;;;
;;;; Author: Corwin de Boor
;;;;
;;;; An implementation of A* search for finding the next rewiring to use. These
;;;; functions are used by the temporal addresser for searching for rewirings.
;;;;
;;;; Individual states are kept in two forms: inflated and deflated. The
;;;; deflated form allows much more efficient memory storage, while the inflated
;;;; form allows much faster datastructure access. We store the deflated form in
;;;; the queue and inflate each item that is pulled off of the queue.
;;;;
;;;; Going along with this inflation idea, we do not store all of the rewirings
;;;; in the search space. Instead, we store an immutable list of swaps that
;;;; makes use of structural sharing to minimize the storage. To pull this off,
;;;; we pre-hash the rewirings into a 64-bit space. This may have some
;;;; unrecoverable collisions, but the collisions are extremely unlikely. Then,
;;;; we only need to store on 64-bit number for each state rather than the
;;;; entire inflated rewiring.

(in-package #:cl-quil)


(defparameter *addresser-a*-swap-search-heuristic-scale* 1d0
  "Scale the swap-search A* heuristic by this value. When > 1, this causes the
search to perform more greedily.")

(defparameter *addresser-a*-swap-search-max-iterations* 50000
  "Limit the number of iterations of A* in the swap search. Upon reaching the
end, choose the node of lowest cost seen so far.")

(defparameter *addresser-a*-distance-metric* :sum
  "Change what the A* search uses as its distance metric

DEPTH: The total runtime of the circuit if entirely parallelized
SIZE: The total runtime of the circuit if entirely sequential
SUM: The sum of these two values
")

(defparameter *addresser-a*-search-state-rebase-threshold* 5000
  "When should the A* search create a new base-search-state from a derived-search-state.")


(defun a*-distance-metric (size depth)
  (ecase *addresser-a*-distance-metric*
    (:depth depth)
    (:size size)
    (:sum (+ depth size))))

;;; See http://www.isthe.com/chongo/tech/comp/fnv/
(defconstant +fnv-1a-64bit-offset-basis+ 14695981039346656037)
(defconstant +fnv-1a-64bit-prime+ 1099511628211)

(defun rewiring-hash (rewiring)
  "FNV-1A hash with a 64 bit modulus. Slightly modified to xor one rewiring
index at a time, rather than one octet at a time."
  (let ((result +fnv-1a-64bit-offset-basis+)
        (mod (expt 2 64))
        (length (rewiring-length rewiring)))
    (loop
      :for i :below length
      :for value := (apply-rewiring-p2l rewiring i)
      :do (setf result (mod (* (logxor result (if value (1+ value) 0))
                               +fnv-1a-64bit-prime+)
                            mod)))
    result))


(defstruct next-swap link link-index)

(defun next-swap-info (swap)
  (with-slots (link) swap
    (destructuring-bind (q0 q1) (coerce (vnth 0 (hardware-object-cxns link)) 'list)
      ;; TODO: Eventually this should look up swap in the list. (See cost-function.lisp)
      (values
       (permutation-record-duration (vnth 0 (hardware-object-permutation-gates link)))
       q0 q1))))

(defclass search-state ()
  ((count :initform 0 :reader search-state-count)
   (length :initform 0 :reader search-state-length)
   (value :initarg :value :reader search-state-value)
   (size :initarg :size)
   (depth :initarg :depth)))

(defclass base-search-state (search-state)
  ((rewiring :initarg :rewiring) (times :initarg :times) (swaps :initarg :swaps)))

(defclass derived-search-state (search-state)
  ((swap :initarg :swap) (prev :initarg :prev)))

(defmethod initialize-instance :after ((state derived-search-state) &key &allow-other-keys)
  (with-slots (length prev) state
    (setf length (1+ (slot-value prev 'length)))))

(defun search-state< (s1 s2)
  (< (search-state-value s1) (search-state-value s2)))

(defun search-state-heuristic (s)
  (with-slots (value size depth) s
    (/ (- value (a*-distance-metric size depth))
       *addresser-a*-swap-search-heuristic-scale*)))

(defgeneric search-state-swaps (state &optional rest)
  (:documentation "Get the swaps that formed this state.")
  (:method ((state base-search-state) &optional rest)
    (with-slots (swaps) state
      (nconc (nreverse (map 'list #'next-swap-link-index swaps)) rest)))
  (:method ((state derived-search-state) &optional rest)
    (with-slots (swap prev) state
      (search-state-swaps prev (list* (next-swap-link-index swap) rest)))))

(defgeneric search-state-base-swaps (state)
  (:documentation "Get the base swaps that formed this state.")
  (:method ((state base-search-state))
    (with-slots (swaps) state swaps))
  (:method ((state derived-search-state))
    (with-slots (swap prev) state
      (list* swap (search-state-base-swaps prev)))))

(defgeneric search-state-data (state)
  (:documentation "Get the rewiring and times for this search state.")
  (:method ((state base-search-state))
    (with-slots (count rewiring times) state
      (incf count)
      (values (copy-rewiring rewiring) (copy-seq times) 0)))
  (:method ((state derived-search-state))
    (with-slots (count prev swap) state
      (incf count)
      (multiple-value-bind (rewiring times length) (search-state-data prev)
        (multiple-value-bind (cost q0 q1) (next-swap-info swap)
          (let ((gate-end (+ cost (max (aref times q0) (aref times q1)))))
            (setf (aref times q0) gate-end
                  (aref times q1) gate-end)
            (update-rewiring rewiring q0 q1)
            (when (> (* length count) *addresser-a*-search-state-rebase-threshold*)
              (change-class state 'base-search-state
                            :swaps (search-state-base-swaps state)
                            :rewiring (copy-rewiring rewiring)
                            :times (copy-seq times))
              (setf length 0))
            (setf (slot-value state 'length) length)
            (values rewiring times (1+ length))))))))

(defun make-initial-search-state (rewiring qubit-times)
  (make-instance 'base-search-state
   :value most-positive-double-float
   :rewiring rewiring
   :times qubit-times
   :swaps nil
   :size 0
   :depth (reduce #'max qubit-times)))

(defstruct active-state
  base
  rewiring
  times
  heuristic)

(defun activate-search-state (state)
  "Bring up a search state to be ready for expansion."
  (with-slots (size depth value) state
    (multiple-value-bind (rewiring times) (search-state-data state)
      (make-active-state
       :base state
       :rewiring rewiring
       :times times
       ;; find the heuristic removing the distance from the value
       :heuristic (search-state-heuristic state)))))

(defun active-state< (s1 s2)
  (< (active-state-heuristic s1) (active-state-heuristic s2)))

(defun active-state-swaps (state)
  (search-state-swaps (active-state-base state)))

(defun active-state-hash (state)
  (rewiring-hash (active-state-rewiring state)))

(defun active-state-apply (state heuristic-fn swap)
  "Derive a new search state after applying a swap. Return the new search state
and its hash."
  (multiple-value-bind (cost q0 q1) (next-swap-info swap)
    (with-slots (base rewiring times) state
      (with-slots (size depth next-swaps) base
        ;; move to the new rewiring
        (with-update-rewiring rewiring q0 q1
          (let ((hash (rewiring-hash rewiring))
                (heuristic (funcall heuristic-fn rewiring))
                (size (+ cost size))
                (depth (max depth (+ cost (max (aref times q0) (aref times q1))))))
            (values (make-instance 'derived-search-state
                                   :value (+ (* *addresser-a*-swap-search-heuristic-scale* heuristic)
                                             (a*-distance-metric size depth))
                                   :prev base
                                   :swap swap
                                   :size size
                                   :depth depth)
                    hash)))))))

(defun active-state-done (active-state done-fn)
  (funcall done-fn (active-state-rewiring active-state)))

(defun make-histogram (seq &key key (test 'eql))
  "Given a sequence, make an alist counting the number of occurrences of key(el)
for each el in seq."
  (let ((hist (make-hash-table :test test)))
    (map nil (lambda (el &aux (value (funcall key el)))
               (setf (gethash value hist) (1+ (or (gethash value hist) 0))))
         seq)
    (sort (a:hash-table-alist hist) #'> :key #'car)))

(defun search-rewiring (chip-spec initial-rewiring initial-qubit-times heuristic-fn done-fn
                        &key (max-iterations most-positive-fixnum))
  "Perform A*-like search to find a path to a rewiring. The heuristic-fn should
measure the cost at the current state. The done-fn should return t if the search
should stop with the given rewiring and false otherwise."
  (loop
    ;; seen table: (distance (or queue-node nil))
    :with seen := (make-hash-table :test 'eql)
    ;; for early termination with max-iterations
    :with best-state := nil
    ;; open queue
    :with queue := (queues:make-queue :priority-queue :compare #'search-state<)
    ;; TODO XXX: If you change the below best-value-so-far into 0, then
    ;; in the early termination nothing will be returned (as the nil
    ;; swaps are the best). Because of the randomization of the
    ;; heuristic, that should cause this function to be called
    ;; repeatedly until it finds a valid swap sequence before hitting
    ;; the max-iterations limit. That should have no side effects in
    ;; other parts of the program. For some reason, this causes the
    ;; search to blow heap occasionally. This makes me think there must
    ;; be a memory leak somewhere.
      :initially (queues:qpush queue (make-initial-search-state initial-rewiring initial-qubit-times))

    :while (plusp (queues:qsize queue))
    :for state := (queues:qpop queue)
    :for active-state := (activate-search-state state)

    :count t :into iterations

        ;; the successful termination point: did we find a good-enough rewiring?
    :when (active-state-done active-state done-fn)
      :do (format-noise "SEARCH-REWIRING: Iterations ~A." iterations)
          (return (values (active-state-swaps active-state) t))

    :when (or (not best-state) (active-state< active-state best-state))
      :do (setf best-state active-state)

          ;; did we run out of time?
    :when (>= iterations max-iterations)
      :do (format-noise "SEARCH-REWIRING: Ran out of iterations.")
          (return (values (active-state-swaps best-state) nil))

          ;; update that we've visited the state
    :do (setf (gethash (active-state-hash active-state) seen) (list state))

    :do (loop
          :for link-index :below (chip-spec-n-links chip-spec)
          :for link := (chip-spec-nth-link chip-spec link-index)

          :for (next-state hash) := (multiple-value-list
                                     (active-state-apply active-state heuristic-fn
                                                         (make-next-swap :link-index link-index :link link)))
          :for ((other-state node) seen-before) := (multiple-value-list (gethash hash seen))

          :unless seen-before
            :do (setf (gethash hash seen)
                      (cons next-state (nth-value 2 (queues:qpush queue next-state))))

          :when (and node (search-state< next-state other-state))
            :do (setf (gethash hash seen)
                      (cons next-state (queues:queue-change queue node next-state))))))

