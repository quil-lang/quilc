;;;; initial-rewiring.lisp
;;;;
;;;; Author: Corwin de Boor
;;;;
;;;; We attempt to make an intelligent guess for what the initial rewiring
;;;; should be for the quil program. This will either check that the used qubits
;;;; all appear in a single connected component (do nothing if so, and move them
;;;; naively if not), or greedily reassign the qubits close to where they will
;;;; be needed first.

(in-package #:cl-quil)

(defparameter *initial-rewiring-default-type* ':partial
  "Determines the default initial rewiring type when not provided and PROG-INITIAL-REWIRING-HEURISTIC cannot determine that a NAIVE rewiring is \"safe\".

PARTIAL: Start with a completely empty rewiring.
GREEDY: Attempt to rewire greedily based on distance of 2-qubit gates.
RANDOM: Start with a random rewiring.
NAIVE: Start with the identity rewiring. This will fail if any 2-qubit gates are
impossible using that rewiring.")


;;; a minimalist queue implementation ;;;
(defun make-q (&rest items)
  (let ((dummy (list nil)))
    (list* (nconc items dummy) dummy)))
(defun q-empty (q)
  (eql (car q) (cdr q)))
(defun q-enq (el q)
  (setf (cadr q) el
        (cddr q) (list nil)
        (cdr q) (cddr q))
  q)
(defun q-deq (q)
  (if (q-empty q) nil (pop (car q))))


(defun chip-spec-live-qubit-bfs (chip-spec qubit-index &optional seen)
  "From a given initial qubit, find the distance to every other qubit in the connected component. SEEN is an array of T/NIL for each qubit, indicating whether that qubit has been visited yet."
  (assert (not (chip-spec-qubit-dead? chip-spec qubit-index)) (qubit-index)
          "Cannot BFS from dead qubit ~A."
          qubit-index)
  (loop
    :with level := 0
    :and seen := (or seen (make-array (chip-spec-n-qubits chip-spec) :initial-element nil))

    ;; this uses a trick where a special nil token signifies we have reached the
    ;; next level of the bfs
    :and queue := (make-q qubit-index nil)
           :initially (setf (aref seen qubit-index) t)

    :for cur := (q-deq queue)
    :until (q-empty queue) ; if empty, only special token was left

    :if cur
      ;; process a node
      :do (dolist (adj (chip-spec-adj-qubits chip-spec cur))
            (unless (or (chip-spec-qubit-dead? chip-spec adj) (aref seen adj))
              (q-enq adj queue)
              (setf (aref seen adj) t)))
      :and :collect (cons cur level) :into distances
    :else
      ;; we have reached the end of the level
      :do (incf level)
      :and :do (q-enq nil queue)

    :finally (return (values distances seen))))

(defun chip-spec-live-qubit-cc (chip-spec)
  "Get a list of lists of live qubits that are in the same connected component on the chip."
  (loop
    :with n-qubits := (chip-spec-n-qubits chip-spec)
    :with seen := (make-array n-qubits :initial-element nil)

    :for qubit :below n-qubits
    :unless (or (chip-spec-qubit-dead? chip-spec qubit) (aref seen qubit))
      :collect (mapcar #'car (chip-spec-live-qubit-bfs chip-spec qubit seen))))

(defun prog-used-qubits (parsed-prog)
  "Get a ordered set of qubits used by this program in program order."
  (let ((qubit-seen (make-hash-table)))
    (loop
      :for inst :across (parsed-program-executable-code parsed-prog)
      :for idx := (when (typep inst 'measurement) (qubit-index (measurement-qubit inst)))
      :when (typep inst 'application)
        :nconc (loop
                 :for q :in (application-arguments inst)
                 :for idx := (qubit-index q)
                 :unless (gethash idx qubit-seen)
                   :do (setf (gethash idx qubit-seen) t)
                   :and :collect idx)
      :when idx
        :unless (gethash idx qubit-seen)
          :do (setf (gethash idx qubit-seen) t)
          :and :collect idx)))

(defun containing-indices (list-of-lists)
  "Given a list of lists, return a table mapping elements to the index of the last list in which they appear."
  (let ((tbl (make-hash-table)))
    (loop
      :for i :from 0
      :for cc :in list-of-lists
      :do (dolist (el cc) (setf (gethash el tbl) i)))
    tbl))

(defun assign-sequentially (mapping)
  "Fill in the rest of mapping with the unused indices in the range sequentially."
  (let* ((size (length mapping))
        (used (make-array size :initial-element nil)))

    (loop
      :for dst :across mapping
      :when dst :do (setf (aref used dst) t))

    (loop
      :with physical := 0
      :for idx :from 0 :to (1- size)
      :when (null (aref mapping idx))
        :do (setf physical (position nil used :start physical)
                  (aref mapping idx) physical
                  (aref used physical) t))

    mapping))

(defun assign-arbitrarily (source dest mapping)
  "Assign all elements of the set source to elements of the set dest in the mapping."
  (loop
    :for logical :in source
    :for physical :in dest
    :do (setf (aref mapping logical) physical))
  mapping)

(defparameter *rewiring-adjacency-weight-decay* 2.0
  "Rate of decay on the weight of instruction value for closeness.")

(defparameter *rewiring-distance-offset* 0.0
  "How much we should shift the distances before weighting. High values means small differences in distance have smaller effect.")

(defun compute-adjacency-weights (n-qubits order)
  "Given the order in which to expect qubits, compute the relative benefit of
being close to each of the n qubits."
  (loop
    :with weights-by-qubit := (make-array n-qubits :initial-element 0d0)
    :for adj :in order
    :for weight := 1d0 :then (/ weight *rewiring-adjacency-weight-decay*)
    :do (incf (aref weights-by-qubit adj) weight)
    :finally (return weights-by-qubit)))

(defun prog-rewiring-pragma (parsed-prog)
  "Finds a rewiring pragma in the parsed program. This pragma needs to occur
before any non-pragma instructions.

If no

    PRAGMA INITIAL_REWIRING \"...\"

is found, then return NIL."
  (loop :for inst :across (parsed-program-executable-code parsed-prog) :do
    (cond
      ((typep inst 'pragma)
       (when (typep inst 'pragma-initial-rewiring)
         (return-from prog-rewiring-pragma (pragma-rewiring-type inst))))
      (t
       (return-from prog-rewiring-pragma nil)))))

(defun %naively-applicable-p (instr chip-spec
                              &aux (qubit-indices (qubits-used instr)))
  "Return true if the given INSTR does not consume qubit arguments or if the qubit arguments correspond to a valid HARDWARE-OBJECT in CHIP-SPEC."
  (or (null qubit-indices)
      (and (not (null (lookup-hardware-object-by-qubits chip-spec qubit-indices)))
           (or (/= 1 (length qubit-indices))
               (not (chip-spec-qubit-dead? chip-spec (first qubit-indices)))))))

(defun prog-initial-rewiring-heuristic (parsed-prog chip-spec)
  "Return a resonable guess at the initial rewiring for PARSED-PROG that is compatible with CHIP-SPEC.

If PARSED-PROG contains an explicit PRAGMA INITIAL_REWIRING, respect it.
Otherwise, if every GATE-APPLICATION in PARSED-PROG can use a NAIVE rewiring, return ':NAIVE.
Otherwise, return *INITIAL-REWIRING-DEFAULT-TYPE*."
  (or (prog-rewiring-pragma parsed-prog)
      (and (every (a:rcurry #'%naively-applicable-p chip-spec)
                  (parsed-program-executable-code parsed-prog))
           ':naive)
      *initial-rewiring-default-type*))

;;; We need to find some rewiring that makes sure that connected qubits all
;;; appear in the same location on the QPU. We make the assumption that
;;; reasonable programs will have a single connected component of qubits, so all
;;; of the qubits that are used need to appear in the same connected component
;;; of the QPU. Note that this effectively limits the size of the QPU to the
;;; size of the largest connected component on it.

;;; In the future, we should definitely try to parallelize across the connected
;;; components.

(defun rewire-non-cc-qubits-on-chip-spec (rewiring chip-spec needed cc)
  "Rewires physical qubits not in CC to logical qubits that are not in
NEEDED."
  (loop :for qi-non-cc
          :in (set-difference (a:iota (chip-spec-n-qubits chip-spec))
                              cc)
        :for qi-non-needed
          :in (set-difference (a:iota (chip-spec-n-qubits chip-spec))
                              needed) :do
          (setf (aref (rewiring-p2l rewiring) qi-non-cc) qi-non-needed
                (aref (rewiring-l2p rewiring) qi-non-needed) qi-non-cc))
  rewiring)

(defun prog-initial-rewiring (parsed-prog chip-spec &key (type *initial-rewiring-default-type*))
  "Find an initial rewiring for a program that ensures that all used qubits
appear in the same connected component of the qpu."
  (let* ((n-qubits (chip-spec-n-qubits chip-spec))
         (connected-components (chip-spec-live-qubit-cc chip-spec))
         (indices (containing-indices connected-components))
         (cc (a:extremum connected-components #'> :key #'length))
         (needed (prog-used-qubits parsed-prog)))
    (assert (or (endp needed)
                (<= (apply #'max needed) n-qubits))
            ()
            "User program incompatible with chip: qubit index ~A used and ~A available."
            (apply #'max needed) n-qubits)

    (when (eql type ':naive)
      (unless (loop
                :with component
                :for qubit :in needed
                :for index := (gethash qubit indices)
                :when (not component)
                  :do (setf component index)
                :always (and index (= index component)))
        (error "User program incompatible with chip: naive rewiring crosses chip component boundaries."))
      (return-from prog-initial-rewiring (make-rewiring n-qubits)))

    (assert (<= (length needed) (length cc)) ()
            "User program used too many qubits: ~A used and ~A available in the largest connected component."
            (length needed) (length cc))

    (when (eql type ':partial)
      (return-from prog-initial-rewiring
        (rewire-non-cc-qubits-on-chip-spec (make-partial-rewiring n-qubits) chip-spec needed cc)))

    (when (eql type ':random)
      (return-from prog-initial-rewiring (generate-random-rewiring n-qubits)))

    (assert (eql type ':greedy) (type)
            "Unexpected rewiring type: ~A." type)

    ;; TODO: this assumes that the program is sequential
    (let* ((per-qubit-ins (prog-qubit-pair-order n-qubits parsed-prog))

           ;; compute for each qubit pair (q1 q2) the benefit of putting q2 close to q1
           (l2l-multiplier (map 'vector
                                (lambda (order) (compute-adjacency-weights n-qubits order))
                                per-qubit-ins))

           ;; physical qubit -> logical qubit -> distance
           (p2l-distances (make-array n-qubits :initial-element nil))
           (rewiring (rewire-non-cc-qubits-on-chip-spec (make-partial-rewiring n-qubits) chip-spec needed cc)))

      (labels
          ((qubit-best-location (qubit)
             (loop
               :with l-multiplier := (aref l2l-multiplier qubit)
               :with best := nil
               :with best-cost := most-positive-double-float

               :for physical-target :in cc
               :for cost
                 := (loop
                      :for (placed . distance) :in (aref p2l-distances physical-target)
                      :sum (* (aref l-multiplier placed)
                              (+ *rewiring-distance-offset* distance)))

               :unless (apply-rewiring-p2l rewiring physical-target)
                 :when (< cost best-cost)
                   :do (setf best physical-target
                             best-cost cost)
               :finally (return best))))

        ;; assign all of the needed qubits
        (dolist (qubit needed)
          (let ((location (qubit-best-location qubit)))
            ;; assign the qubit to the best location
            (rewiring-assign rewiring qubit location)

            ;; update the distances
            (loop
              :for (other-location . distance) :in (chip-spec-live-qubit-bfs chip-spec location)
              :do (push (cons qubit distance) (aref p2l-distances other-location)))))

        rewiring))))

(defun prog-qubit-pair-order (n-qubits parsed-prog)
  "For each qubit, find a list of the other qubits it communicates with in order
of use."
  (loop
    :with per-qubit-ins := (make-array n-qubits :initial-element nil)
    :for inst :across (parsed-program-executable-code parsed-prog)
    :when (and (typep inst 'application)
               (= (length (application-arguments inst)) 2))
      :do (destructuring-bind (q1 q2) (application-arguments inst)
            (let ((q1 (qubit-index q1)) (q2 (qubit-index q2)))
              (push q2 (aref per-qubit-ins q1))
              (push q1 (aref per-qubit-ins q2))))
    :finally (return (map-into per-qubit-ins #'nreverse per-qubit-ins))))
