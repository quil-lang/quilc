;;;; wavefunctions.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; This file supports compressor.lisp by providing mechanisms for simulating
;;;; partial quantum states, used for the state preparation routines.

(in-package #:cl-quil)

;;; first, some utilities

;; NOTE: these break some MAGICL abstractions and really belongs in that package
(defun nondestructively-apply-matrix-to-vector (matrix vector)
  (let* ((vector-as-matrix (magicl:make-matrix :rows (array-total-size vector)
                                               :cols 1
                                               :data (copy-seq vector)))
         (new-vector (make-array (array-total-size vector)
                                 :element-type '(complex double-float))))
    (let ((output-matrix (magicl:multiply-complex-matrices matrix vector-as-matrix)))
      (dotimes (j (array-total-size vector) new-vector)
        (setf (aref new-vector j)
              (magicl:ref output-matrix j 0))))))

(defun destructively-normalize-vector (vect)
  "Scales a vector VECT to be of unit norm."
  (let ((norm (sqrt (loop :for v :across vect
                          :sum (abs (* v v))))))
    (dotimes (j (length vect) vect)
      (setf (aref vect j)
            (/ (aref vect j) norm)))))

(defun generate-sublists (list size)
  "Returns a list of all (ordered) sublists of a particular SIZE from LIST."
  (cond
    ((= 0 size)
     (return-from generate-sublists (list nil)))
    ((> size (length list))
     (return-from generate-sublists nil))
    (t
     (nconc (generate-sublists (rest list) size)
            (mapcar (lambda (l) (cons (first list) l))
                    (generate-sublists (rest list) (1- size)))))))


;;; the antisocial qvm data type
;;;
;;; XXX: go through and make sure the WF arrays have element type '(complex double-float)

(defparameter *aqvm-correlation-threshold*
  *global-queue-tolerance-threshold*
  "Threshold beyond which an impure collection of qubits will be marked as unsalvageable.")

(defstruct antisocial-qvm
  "ANTISOCIAL-QVM instances represent partially-tracked low-correlation wavefunctions.

WFS is a vector. The jth entry points to the wavefunction component in which the jth external qubit lies.

INTERNAL-INDICES is a vector. The jth entry points to the index of the wavefunction component corresponding to the jth external qubit.

Both arrays may be populated instead by the keyword :NOT-SIMULATED, which indicates that the AQVM has discarded all information about this qubit (and this qubit is not correlated with any wavefunction component that the AQVM is still tracking)."
  (wfs #())
  (internal-indices #()))

(defun build-ground-state (qubit-count)
  "Initializes a wavefunction encoding the ground state over QUBIT-COUNT many qubits."
  (let ((fresh-array (make-array (expt 2 qubit-count)
                                 :element-type '(complex double-float)
                                 :initial-element #C(0d0 0d0))))
    (setf (aref fresh-array 0) #C(1d0 0d0))
    fresh-array))

(defun qubit-count (wf)
  "For a wavefunction WF expressed as an array of components, return the number of qubits involved."
  (1- (integer-length (length wf))))

(defun wavefunction-size (qubit-indices)
  "For a collection of qubit indices, calculate how large of an array is needed to store a wavefunction on these qubits."
  (expt 2 (length qubit-indices)))

(defun wavefunction-indices (aqvm wf)
  "Returns an in-order list of those qubit indices associated to a wavefunction WF in AQVM."
  (cond
    ((eql wf ':not-simulated)
     (loop :for index :from 0
           :for aqvm-wf :across (antisocial-qvm-wfs aqvm)
           :when (eql aqvm-wf ':not-simulated)
             :collect index))
    (t
     (let ((array (make-array (qubit-count wf))))
       (loop :for internal-index :across (antisocial-qvm-internal-indices aqvm)
             :for aqvm-wf :across (antisocial-qvm-wfs aqvm)
             :for index :from 0
             :when (eql wf aqvm-wf)
               :do (setf (aref array internal-index)
                         index))
       (coerce array 'list)))))

(defun build-aqvm (qubit-count &key (simulate t))
  "Initializes an antisocial-qvm instance with QUBIT-COUNT many qubits. If SIMULATE is T, they are initialized in the ground state; if SIMULATE is NIL, then they are instead initialized as :NOT-SIMULATED."
  (let ((wfs (make-array qubit-count :initial-element ':not-simulated))
        (internal-indices (make-array qubit-count :initial-element ':not-simulated)))
    (when simulate
      (fill internal-indices 0)
      (map-into wfs (lambda () (build-ground-state 1))))
    (make-antisocial-qvm :wfs wfs :internal-indices internal-indices)))

(defun aqvm-extract-single-wf-component (aqvm q)
  "Same behavior as AQVM-EXTRACT-STATE, but Q is required to be a single qubit index."
  ;; look up the first bit of internal data
  (let ((wf (aref (antisocial-qvm-wfs aqvm) q)))
    (list wf (wavefunction-indices aqvm wf))))

(defun aqvm-extract-state (aqvm qubit-complex &key (destructive-update nil))
  "AQVM associates a wavefunction to QUBIT-COMPLEX, which may have more correlated components than visible by QUBIT-COMPLEX alone. Returns a list (WF UPDATED-QC) consisting of a vector wavefunction and the full list of correlated qubit indices.

If DESTRUCTIVE-UPDATE is T, we will update AQVM's internal structure to correlate all the qubits in QUBIT-COMPLEX."
  ;; before we do anything, see if we touch any dead zones
  (when (some (lambda (q) (eql ':not-simulated (aref (antisocial-qvm-wfs aqvm) q)))
              qubit-complex)
    ;; we hit a dead zone. if we're destructive, this means we're going to
    ;; mark all the qubits in qubit-complex as dead too.
    (when destructive-update
      (dolist (q qubit-complex)
        (aqvm-stop-simulating aqvm q)))
    ;; one way or another, report it back
    (return-from aqvm-extract-state (list ':not-simulated nil)))
    
  ;; no dead zones!
  (let ((wf #())
        (updated-qc nil))
    ;; for each qubit in qubit-complex
    (dolist (q qubit-complex)
      ;; check to see if we already have this qubit in updated-qc
      (unless (member q updated-qc)
        ;; grab the associated wf component and all the qubits that point to it
        (destructuring-bind (incoming-wf incoming-qc)
            (aqvm-extract-single-wf-component aqvm q)
          ;; join this new qubit complex to UPDATED-QC by appending it
          (setf updated-qc
                (append updated-qc incoming-qc))
          ;; hadamard the WF representations together too (if needed)
          (cond
            ((zerop (array-total-size wf))
             (setf wf incoming-wf))
            (t
             (let ((new-wf (make-array (* (array-total-size wf)
                                          (array-total-size incoming-wf))
                                       :element-type '(complex double-float)
                                       :initial-element #C(0d0 0d0))))
               (dotimes (i (array-total-size wf))
                 (dotimes (j (array-total-size incoming-wf))
                   (setf (aref new-wf (+ (* i (array-total-size incoming-wf))
                                         j))
                         (* (aref wf i)
                            (aref incoming-wf j)))))
               (setf wf new-wf))))
          ;; if we're supposed to destructively update the qvm with this correlated state...
          (when destructive-update
            ;; for each qubit in updated-qc...
            (loop :for internal-index :from 0
                  :for p :in updated-qc
                  :do
                     ;; poke this wf into all the positions in updated-qc
                     (setf (aref (antisocial-qvm-wfs aqvm) p) wf)
                     ;; poke the associated index into internal-indices
                     (setf (aref (antisocial-qvm-internal-indices aqvm) p) internal-index))))))
    (list wf updated-qc)))

(defun nondestructively-apply-instr-to-wf (instr wf qc &optional environs)
  (declare (ignore environs))
  (unless (eq wf ':not-simulated)
    (handler-case 
        (let* ((qubit-indices (mapcar #'qubit-index (application-arguments instr)))
               (rewiring (mapcar (lambda (q) (- (length qc) 1 (position q qc)))
                                 qubit-indices))
               (matrix (gate-matrix instr)))
          (nondestructively-apply-matrix-to-vector
           (kq-gate-on-lines matrix (length qc) rewiring)
           wf))
      (unknown-gate-parameter () ':not-simulated))))

(defun nondestructively-apply-instrs-to-wf (instrs wf qc &optional environs)
  (alexandria:when-let ((wf (copy-seq wf)))
    (assert (= (length wf) (expt 2 (length qc))))
    (dolist (instr instrs wf)
      (let ((new-wf (nondestructively-apply-instr-to-wf instr wf qc environs)))
        (when (eq new-wf ':not-simulated)
          (return ':not-simulated))
        (dotimes (j (array-total-size wf))
          (setf (aref wf j)
                (aref new-wf j)))))))

(defun aqvm-apply-instruction (aqvm instr &optional environs)
  "Applies INSTR to the wavefunction housed in AQVM, correlating components as needed."
  (check-type instr gate-application)
  ;; get the state out of AQVM in a way that it's ready to receive an update
  (let ((qubit-indices (mapcar #'qubit-index (application-arguments instr))))
    (destructuring-bind (wf updated-qc)
        (aqvm-extract-state aqvm qubit-indices :destructive-update t)
      ;; if we're in a dead zone, we can just quit early
      (unless (eql wf ':not-simulated)
        ;; get the associated matrix (& information for how to use it)
        (let* ((updated-wf (nondestructively-apply-instr-to-wf instr wf updated-qc environs)))
          ;; destructively store the result back into wf
          (cond ((eq updated-wf ':not-simulated)
                 (dolist (q updated-qc)
                   (aqvm-stop-simulating aqvm q)))
                (t
                 (dotimes (j (array-total-size wf))
                   (setf (aref wf j)
                         (aref updated-wf j))))))))))

;; NOTE: there are a lot of degrees of thoroughness that you could write into
;; this function. here are three strategies, from simple to difficult:
;;  (0) do nothing.
;;  (1) if a wf component has grown too large, just call aqvm-stop-simulating on
;;      it no matter what.
;;  (2) if a wf component has grown too large, look to see if its active fields
;;      all share some common bit bj. if that's the case, this wf is actually
;;      of the form wf = wf' (x) |bj>, and so that bit component can be pulled
;;      out from the others.  if no such component exists, give up and disable.
;;  (3) if a wf component has grown too large, look to see if it is in the image
;;      of any Segre embedding by doing a bunch of determinant calculations.
;;      if any of these return true, then calculate the Segre components and
;;      use them to dissect the wf into smaller components.
(defun aqvm-unlink (aqvm)
  "Attempts to write the correlated components of ANTISOCIAL-QVM into smaller components, else discards them."
  ;; buckets is a list of lists, indexed descending by the size of the string we're going to attempt to unlink
  ;; each bucket component is a list of pairs: (list wf qubit-indices)
  (let ((buckets (list nil)))
    ;; for each wf in the aqvm, put wf into the bucket indexed as |_ size(wf) / 2 _|
    (dolist (wf (remove-duplicates (coerce (antisocial-qvm-wfs aqvm) 'list)))
      (let* ((index-list (wavefunction-indices aqvm wf))
             (last-bucket-index (1- (length buckets)))
             (insert-index (if (eql wf ':not-simulated)
                               last-bucket-index
                               (- last-bucket-index (floor (qubit-count wf) 2)))))
        ;; ensure there are enough buckets
        (when (minusp insert-index)
          (dotimes (i (abs insert-index))
            (push nil buckets))
          (setf insert-index 0))
        (push (list wf index-list)
              (nth insert-index buckets))))
    (labels (;; on the input (p0 p1 ... pk), |bm b(m-1) ... b0>
             ;; this gives |bpm bp(m-1) ... bp1 bp0>.
             ;;
             ;; NOTE: we really intend to form our output using logior/dpb, and
             ;;       the :sum below is just to have the convenience of loop.
             (bit-scatter (bit-positions word)
               (loop :for k :below (length bit-positions)
                     :for posn :in bit-positions
                     :sum (ash (ldb (byte 1 k) word) posn)))
             
             ;; the opposite of bit-scatter
             (bit-gather (bit-positions word)
               (loop :for k :below (length bit-positions)
                     :for posn :in bit-positions
                     :sum (ash (ldb (byte 1 posn) word) k)))
             
             ;; bucket-descend will be called recursively until all wfs are in the 0th bucket
             (bucket-descend ()
               (cond
                 ;; if everything is in one bucket, we're done.
                 ((endp (rest buckets))
                   (return-from bucket-descend))
                 ;; if our top bucket is empty, discard it
                 ((endp (first buckets))
                   (setf buckets (rest buckets))
                   (return-from bucket-descend
                     (bucket-descend)))
                 ;; select a wf in the largest bucket
                 (t
                  (destructuring-bind (wf wf-indices) (first (first buckets))
                    ;; for each subset of the wf indices of size bucket-size
                    (return-from bucket-descend
                      (try-next-wf-dissection wf wf-indices
                                              (generate-sublists wf-indices
                                                                 (1- (length buckets)))))))))
             
             ;; try-next-wf-dissection iterates over different ways to try to
             ;; factorize a given wf into a product state.
             (try-next-wf-dissection (wf wf-indices sublists)
               (when (endp sublists)
                 ;; we're out of different ways we could split up this wf.
                 ;; push it down by one bucket.
                 (pop (first buckets))
                 (push (list wf wf-indices) (second buckets))
                 ;; and recurse onto the next possible wf
                 (return-from try-next-wf-dissection
                   (bucket-descend)))
               ;; our goal is to come up with candidate a-vect and b-vect values
               ;; this means finding a bit setting where the wf values are not all zero
               (let* ((a-indices (first sublists))
                      (b-indices (set-difference wf-indices a-indices))
                      (a-vect (make-array (wavefunction-size a-indices)
                                          :initial-element #C(0d0 0d0)
                                          :element-type '(complex double-float)))
                      (b-vect (make-array (wavefunction-size b-indices)
                                          :initial-element #C(0d0 0d0)
                                          :element-type '(complex double-float)))
                      (a-bit-positions (mapcar (lambda (index) (- (length wf-indices) 1 (position index wf-indices)))
                                               a-indices))
                      (b-bit-positions (mapcar (lambda (index) (- (length wf-indices) 1 (position index wf-indices)))
                                               b-indices))
                      (good-a-vect-p nil))
                 (dotimes (j (length b-vect))
                   (let ((b-offset (bit-scatter b-bit-positions j)))
                     (dotimes (i (length a-vect))
                       (let* ((a-offset (bit-scatter a-bit-positions i))
                              (wf-entry (aref wf (+ b-offset a-offset))))
                         (setf (aref a-vect i) wf-entry)
                         (unless (double= 0d0 wf-entry)
                           ;; this is a good location.
                           ;; if we haven't found a good location before, use this to calculate b-vect
                           (unless good-a-vect-p
                             ;; record that we found a good value so that we don't do this twice
                             (setf good-a-vect-p t)
                             ;; note: we're recalculating j and b-offset
                             (dotimes (jj (length b-vect))
                               (setf (aref b-vect jj)
                                     (/ (aref wf (+ a-offset
                                                    (bit-scatter b-bit-positions jj)))
                                        wf-entry))))))))
                   ;; if we found a point where we could make a splitting...
                   (when (and good-a-vect-p
                              ;; check whether wf agrees with a-vect (x) b-vect
                              (loop :for k :below (wavefunction-size wf-indices)
                                    :always (let ((a-index (bit-gather a-bit-positions k))
                                                  (b-index (bit-gather b-bit-positions k)))
                                              (double= (aref wf k)
                                                       (* (aref a-vect a-index)
                                                          (aref b-vect b-index))))))
                     (let ((a-pair (list a-vect (reverse a-indices)))
                           (b-pair (list b-vect (reverse b-indices))))
                       ;; pop wf from the top bucket
                       (pop (first buckets))
                       ;; add a-vect and b-vect to their appropriate buckets
                       (push a-pair
                             (nth (max 0 (- (length buckets) 1 (floor (length a-indices) 2)))
                                  buckets))
                       (push b-pair
                             (nth (max 0 (- (length buckets) 1 (floor (length b-indices) 2)))
                                  buckets))
                       ;; recurse
                       (return-from try-next-wf-dissection
                         (bucket-descend)))))
                 ;; it doesn't, so try the next sublist
                 (try-next-wf-dissection wf wf-indices (rest sublists)))))
      ;; start the iteration
      (bucket-descend)
      ;; reconstitute the aqvm, killing wfs that are over the size limit
      (dolist (pair (first buckets))
        (destructuring-bind (wf wf-indices) pair
          (let ((too-big-p (or (eql wf ':not-simulated)
                               (< *aqvm-correlation-threshold* (length wf-indices))))
                (wf (if (eql wf ':not-simulated)
                        ':not-simulated
                        (destructively-normalize-vector wf))))
            (loop :for j :from 0
                  :for qubit-index :in wf-indices
                  :do
                     (setf (aref (antisocial-qvm-wfs aqvm) qubit-index)
                           (if too-big-p ':not-simulated wf))
                     (setf (aref (antisocial-qvm-internal-indices aqvm) qubit-index)
                           (if too-big-p ':not-simulated j)))))))))

(defun aqvm-stop-simulating (aqvm q)
  "Mark qubit Q (as well as any qubits entangled with it) as unrecoverably correlated."
  (let ((target (aref (antisocial-qvm-wfs aqvm) q)))
    (dotimes (j (array-total-size (antisocial-qvm-wfs aqvm)))
      (when (eql target (aref (antisocial-qvm-wfs aqvm) j))
        (setf (aref (antisocial-qvm-wfs aqvm) j) ':not-simulated)
        (setf (aref (antisocial-qvm-internal-indices aqvm) j) ':not-simulated)))))

(defun aqvm-copy (aqvm)
  (let ((new-aqvm (make-antisocial-qvm)))
    (setf (antisocial-qvm-internal-indices new-aqvm)
          (make-array (length (antisocial-qvm-internal-indices aqvm))
                      :initial-element nil))
    (setf (antisocial-qvm-wfs new-aqvm)
          (make-array (length (antisocial-qvm-wfs aqvm))
                      :initial-element nil))
    (loop :for internal-index :across (antisocial-qvm-internal-indices aqvm)
          :for count :from 0
          :do (setf (aref (antisocial-qvm-internal-indices new-aqvm) count)
                    internal-index))
    (dotimes (index (length (antisocial-qvm-wfs new-aqvm)))
      (cond
        ;; if new-aqvm at index has a thing, skip ahead
        ((aref (antisocial-qvm-wfs new-aqvm) index)
         nil)
        ;; if aqvm at index has :not-simulated, copy that
        ((eql ':not-simulated (aref (antisocial-qvm-wfs aqvm) index))
         (setf (aref (antisocial-qvm-wfs new-aqvm) index)
               ':not-simulated))
        ;; otherwise, we're seeing a fresh wf
        (t
         (let* ((wf (aref (antisocial-qvm-wfs aqvm) index))
                (copied-wf (copy-seq wf))
                (wf-indices (wavefunction-indices aqvm wf)))
           (dolist (index wf-indices)
             (setf (aref (antisocial-qvm-wfs new-aqvm) index)
                   copied-wf))))))
    new-aqvm))

(defun pprint-aqvm (stream aqvm)
  "Prints a pretty representation of AQVM to STREAM."
  (let ((aqvm-hash (make-hash-table)))
    (loop :for q :from 0
          :for wf :across (antisocial-qvm-wfs aqvm)
          :for internal-index :across (antisocial-qvm-internal-indices aqvm)
          :when (and (null (gethash wf aqvm-hash))
                     (not (eql ':not-simulated wf)))
            :do (setf (gethash wf aqvm-hash)
                      (make-list (qubit-count wf)))
          :do
             (if (eql ':not-simulated wf)
                 (push q (gethash wf aqvm-hash))
                 (setf (nth internal-index (gethash wf aqvm-hash))
                       q)))
    (pprint-logical-block (stream nil)
      (print-unreadable-object (aqvm stream :type t)
        (format stream "(~D wfs on ~D qubits):"
                (hash-table-count aqvm-hash)
                (array-total-size (antisocial-qvm-wfs aqvm)))
        (dohash ((wf qubit-list) aqvm-hash)
          (format stream "~%Wavefunction on ~a:" (reverse qubit-list))
          (cond
            ((eql ':not-simulated wf)
             (format stream "~%  NOT SIMULATED"))
            (t
             (loop :for j :from 0
                   :for wf-entry :across wf
                   :do (format stream "~%  |~v,'0B>: "
                               (length qubit-list)
                               j)
                       (format stream "~6,3f ~:[+~;-~]~6,3fj"
                               (realpart wf-entry)
                               (minusp (imagpart wf-entry))
                               (abs (imagpart wf-entry)))))))))))

(set-pprint-dispatch 'antisocial-qvm 'pprint-aqvm)
