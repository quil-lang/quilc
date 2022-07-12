;;;; src/clifford/god-table.lisp
;;;;
;;;; Author: Nik Tezak
;;;;         Robert Smith

(in-package #:cl-quil/clifford)

;;; This file contains an implementation of a Clifford "God table", a
;;; lookup table used to reconstruct Clifford elements from some
;;; generating set.
;;;
;;; A sample generating set consisting of H, PHASE, and CNOT is
;;; provided.

(defun hadamard (n j)
  "Generate a Hadamard gate represented on N qubits acting on the J-th qubit."
  (embed (clifford-element
           X -> Z
           Z -> X)
         n (list j)))

(defun phase-gate (n j)
  "Generate a phase gate represented on N qubits acting on the J-th qubit."
  (embed (clifford-element
           X -> Y)
         n (list j)))

(defun cnot (n i j)
  "Generate a CNOT gate represented on N qubits acting on the I-th (control)
and J-th (target) qubit."
  (embed (clifford-element
           XI -> XX
           IZ -> ZZ)
         n (list i j)))

(defun swap (n permutation)
  "Generate the n qubit SWAP gate that permutes indices as described by the PERM object PERMUTATION. "
  (let ((cliff (clifford-identity n)))
    (map-pauli-basis n (lambda (idx basis-vector)
                         (setf (aref (basis-map cliff) idx)
                               (make-pauli (permute permutation (base4-list basis-vector)) (phase-factor basis-vector)))))
    cliff))

(defclass gateset ()
  ((cliffords :initarg :cliffords :reader cliffords)
   (names :initarg :names :reader names)
   (inverse-names :initarg :inverse-names :reader inverse-names))
  (:documentation "Store a gateset definition."))

(defun gateset= (g1 g2)
  ;; The names and other things don't matter.
  (and (subsetp (cliffords g1) (cliffords g2) :test #'clifford=)
       (subsetp (cliffords g2) (cliffords g1) :test #'clifford=)))

(defun gateset-hash (g)
  ;; The hash function is specifically such that permutation doesn't
  ;; matter.
  (loop :with hash := 0
        :for c :in (cliffords g)
        :for hash-c := (clifford-hash c)
        :do (setf hash (logxor hash hash-c))
        :finally (return hash)))

(defun make-gateset-hash-table ()
  "Make a thread-safe hash table whose keys are GATESET objects."
  (make-hash-table :test 'gateset=
                   :hash-function 'gateset-hash
                   ;; We need to synchronize because this table will
                   ;; be used as a cache from the server. LW and CCL
                   ;; support that by default.
                   #+(or sbcl ecl) :synchronized #+(or sbcl ecl) t))

(defclass god-table ()
  ((mapping :initarg :mapping :reader mapping)
   (gateset :initarg :gateset :reader gateset))
  (:documentation
   "A God-Table is specific to a gateset and for each given clifford
allows to compute a minimal length sequence of generators to invert it."))

(defmethod num-qubits ((g gateset))
  (num-qubits (first (cliffords g))))

(defmethod num-qubits ((gt god-table))
  (num-qubits (gateset gt)))

(defun make-god-table (gateset)
  "Compute the God table for the closure of some specific generators."
  (let* ((q (cl-quil:make-queue))
         (n (num-qubits gateset))
         #+#:debug(num-cliffords (count-clifford n))
         (generators (cliffords gateset))
         (gt (make-clifford-hash-table :pre-allocate n
                                       :synchronized nil)))

    ;; seed the god table mapping with the identity and the generators
    ;; which are by definition one step away from the identity
    (let ((i (clifford-identity n)))
      (setf (gethash i gt) -1)
      (loop :for g-idx :from 0
            :for g :in generators
            :do (setf (gethash g gt)
                      ;; for a hash key denoting the image we store
                      ;; the index of the generator applied and the
                      ;; pre-image clifford which in this case is the
                      ;; identity
                      g-idx)
                ;; enqueue the generator to recursively generate more
                ;; cliffords below
                (cl-quil:enqueue q g)))

    ;; At this point we know all cliffords that are 0 and 1 generator
    ;; applications removed from the identity.

    ;; Loop over queue containing potential pre image cliffords from
    ;; which we generate more cliffords by the composition with a
    ;; single generator. Usage of a FIFO queue in conjunction with the
    ;; loops below ensures that we will first find all cliffords that
    ;; are 2 generator applications away from the identity, then 3,
    ;; etc. until we have reached all cliffords.
    (let ((explored 0)
          #+#:debug(time (get-internal-real-time)))
      (loop :until (cl-quil:queue-empty-p q)
            :for next := (cl-quil:dequeue q)
            :do
               ;; For each clifford one generator removed from NEXT
               ;; check if it has been seen before in which case we have
               ;; generated it in at least as few steps. Otherwise store
               ;; it in the god table mapping and enqueue it as a future
               ;; pre image.
               (loop :for s-idx :from 0
                     :for s :in generators
                     :for next-s := (group-mul next s)
                     :do
                        (incf explored)
                        (unless (gethash next-s gt)
                          #+#:debug
                          (when (zerop (mod explored 10000))
                            (format t "Explored: ~A, Found: ~A (~3,1F%); Waste: ~3,1F%;  dt = ~A ms~%"
                                    explored
                                    (hash-table-count gt)
                                    (* 100 (/ (hash-table-count gt)
                                              num-cliffords))
                                    (* 100 (/ (- explored (hash-table-count gt))
                                              explored))
                                    (round (* 1000 (- (get-internal-real-time) time))
                                           internal-time-units-per-second))
                            (setf time (get-internal-real-time)))
                          (setf (gethash next-s gt) s-idx)
                          (cl-quil:enqueue q next-s)))))
    (make-instance 'god-table
                   :gateset gateset
                   :mapping gt)))

#+#:ignore
(defun make-god-table-iddfs (gateset)
  "Compute the god-table for the closure of some specific generators."
  (let* ((n (num-qubits gateset))
         (generators (cliffords gateset))

         (cliffords-left (count-clifford n))
         (gt (make-clifford-hash-table :pre-allocate n))
         (root (clifford-identity n))
         ;; For printing
         (num-cliffords cliffords-left)
         (num-digits    (length (prin1-to-string num-cliffords))))


    (setf (gethash root gt) (list () () 0))
    (decf cliffords-left (hash-table-count gt))

    (loop :for max-depth :from 1
          :while (plusp cliffords-left)
          :do
             (labels ((iddfs (depth next)
                        (if (> depth max-depth)
                            nil
                            (loop :for s-idx :from 0
                                  :for s :in generators
                                  :for next-s := (group-mul next s)
                                  :do
                                     (when (zerop cliffords-left)
                                       (return))
                                     (multiple-value-bind (entry exists?)
                                         (gethash next-s gt)
                                       (cond
                                         (exists?
                                          ;; Prune Cliffords we've
                                          ;; seen at an earlier depth.
                                          (when (not (< (third entry) depth))
                                            (iddfs (1+ depth) next-s)))
                                         (t
                                          (setf (gethash next-s gt) (list s-idx next depth))
                                          (decf cliffords-left)
                                          (iddfs (1+ depth) next-s)))
                                       ;; Prune!
                                       (when (and exists?
                                                  (not (< (third entry) depth)))
                                         ))))))
               (iddfs 1 root)
               (format t "Depth ~2D: ~V,' D (~3,1F%)~%"
                       max-depth
                       num-digits
                       (hash-table-count gt)
                       (float (* 100 (/ (hash-table-count gt) num-cliffords))))))
    (make-instance 'god-table
                   :gateset gateset
                   :mapping gt)))

#+#:ignore
(defun search-iddfs (g gateset)
  "Compute the god-table for the closure of some specific generators."
  (let ((generators (cliffords gateset))
        (root (clifford-identity (num-qubits gateset)))
        (explored 0)
        (num-digits (+ 6 (length (prin1-to-string (count-clifford (num-qubits gateset)))))))

    (block iterative-deepening
      (loop :for max-depth :from 1
            :do
               (labels ((iddfs (depth next collected)
                          (cond
                            ((> depth max-depth)
                             nil)

                            (t
                             (when (and (= depth max-depth)
                                        (clifford= g next))
                               (return-from iterative-deepening collected))
                             (loop :for s-idx :of-type fixnum :from 0
                                   :for s :in generators
                                   :for next-s := (group-mul next s)
                                   :do (incf explored)
                                       (iddfs (1+ depth) next-s (cons s-idx collected)))))))
                 (setf explored 0)
                 (iddfs 1 root nil)
                 (format t "Depth ~2D: ~V,' D~%"
                         max-depth
                         num-digits
                         explored))))))

(defmethod reconstruct ((c clifford) (g god-table) &optional (g-accessor #'cliffords))
  "Using a god-table G compute a sequence of Clifford generators that when
multiplied reproduce a given clifford C.

The optional G-ACCESSOR argument must be function that takes a single
argument of type gateset, e.g., a gateset slot accessor function.

When this equals #'CLIFFORDS (by default) this function returns an
actual sequence of clifford objects. In this case the following
identity holds for any clifford A and any complete god-table G:

  > (clifford= a (reduce #'group-mul (reconstruct a g))
    T

Set it to #'NAMES to return a sequence of generator names.

Set it to #'INVERSE-NAMES to return a sequence of cliffords that
invert C when applied in the correct (reversed) order.
"
  (let ((seq ())
        (current-clifford c)
        (i (clifford-identity (num-qubits c))))
    (loop :until (clifford= current-clifford i)
          :do (let* ((idx (gethash current-clifford (mapping g)))
                     (gs (gateset g))
                     (gen (nth idx (cliffords gs))))
                (push  (nth idx (funcall g-accessor gs)) seq)
                (setq current-clifford (group-mul current-clifford (group-inv gen)))))
    seq))

(defun sample-from (lst n)
  "Uniformly sample N elements (with replacement) from a list LST."
  (loop :with l := (length lst)
        :repeat n
        :collect (elt lst (random l))))

(defun sample (n g)
  "Sample N Cliffords uniformly via the god-table G. Returns an LIST of CLIFFORDs."
  (let ((sample-idxs (make-hash-table))
        (results (make-array n :initial-element 0 :fill-pointer 0 :adjustable t))
        (table-size (hash-table-count (mapping g))))
    (loop :for idx :below n :do
      (incf (gethash (random table-size) sample-idxs 0)))
    (loop :for hash-key :being :the hash-keys :of (mapping g)
          :for i :from 0
          :do (dotimes (jdx (gethash i sample-idxs 0))
                (vector-push hash-key results)))
    (coerce (a:shuffle results) 'list)))

(defun default-gateset (num-qubits &optional (cnot-edges :complete))
  "Generate a list of generators of single qubit Hadamard and Phase
gates as well as two-qubit CNOT gates on pairs as specified by the
optional argument cnot-edges. If omitted, default to a complete graph,
i.e., all pairs of CNOTs."
  (let ((generators ())
        (names ())
        (inverse-names ()))
    (when (eq cnot-edges :complete)
      (setq cnot-edges
            (apply 'append
                   (loop :for idx :from 0 :below num-qubits
                         :collect (loop
                                    :for jdx :from 0 :below num-qubits
                                    :unless (= idx jdx)
                                      :collect (list idx jdx))))))
    (loop :for idx :from 0 :below num-qubits
          :do
             (setq generators
                   (cons (hadamard num-qubits idx) generators))
             (setq names
                   (cons (format () "H(~S)" idx) names))
             (setq inverse-names
                   (cons (format () "H(~S)" idx) inverse-names))
             (setq generators
                   (cons (phase-gate num-qubits idx) generators))
             (setq names
                   (cons (format () "PHASE(~S)" idx) names))
             (setq inverse-names
                   (cons (format () "INV-PHASE(~S)" idx) inverse-names)))

    (loop :for pair :in cnot-edges
          :do
             (setq generators
                   (cons (cnot num-qubits (car pair) (cadr pair)) generators))
             (setq names
                   (cons (format () "CNOT(~S,~S)" (car pair) (cadr pair)) names))
             (setq inverse-names
                   (cons (format () "CNOT(~S,~S)" (car pair) (cadr pair)) inverse-names))
          )
    (make-instance 'gateset
                   :cliffords generators
                   :names names
                   :inverse-names inverse-names)))
