;;;; rewiring.lisp
;;;;
;;;; Author: Eric Peterson, Corwin de Boor

(in-package #:cl-quil)

(defun inverse-matches-forward-p (forward inverse)
  "Checks that each non-NIL mapping in FORWARD has a corresponding inverse mapping in
INVERSE."
  (loop
    :for src :from 0
    :for dst :across forward
    :always (or (not dst)
                (and (<= 0 dst (1- (length inverse)))
                     (= src (aref inverse dst))))))

(defun bijective-rewiring-p (rewiring)
  (and (= (length (rewiring-l2p rewiring)) (length (rewiring-p2l rewiring)))
       (inverse-matches-forward-p (rewiring-l2p rewiring) (rewiring-p2l rewiring))
       (inverse-matches-forward-p (rewiring-p2l rewiring) (rewiring-l2p rewiring))))

(defun full-rewiring-p (rewiring)
  (and (every #'integerp (rewiring-l2p rewiring))))

(deftype bijective-rewiring () `(and rewiring (satisfies bijective-rewiring-p)))
(deftype full-rewiring () `(and bijective-rewiring (satisfies full-rewiring-p)))

(defun make-partial-rewiring (n)
  (init-rewiring :l2p (make-array n :initial-element nil)
                 :p2l (make-array n :initial-element nil)))

(defun make-rewiring (n)
  "Initialize a rewiring of length N."
  (let ((id
          (coerce
           (loop :for j :below n :collect j)
           'vector)))
    ;; Store two copies of the identity mapping to start
    (init-rewiring :l2p id :p2l (copy-seq id))))

(defun generate-random-rewiring (n l2p-components)
  "Generate a random rewiring of length N constrained by the component mapping L2P-COMPONENTS."
  (loop
    :with rewiring := (make-rewiring n)
    :for end :downfrom n :above 1
    ;; when a qubit is unused, do not permute it.
    :when (gethash n l2p-components)
      :do (update-rewiring rewiring (1- end) (a:random-elt (find-physical-component l2p-components (1- end))))
            
    :finally (return rewiring)))

(defun copy-rewiring (rewiring)
  (init-rewiring
   :l2p (copy-seq (rewiring-l2p rewiring))
   :p2l (copy-seq (rewiring-p2l rewiring))))

(defun rewiring-length (rewiring)
  (length (rewiring-l2p rewiring)))

(defun update-rewiring (rewiring n m)
  "Swaps the physical wires n and m."
  (let* ((ln (aref (rewiring-p2l rewiring) n))
         (lm (aref (rewiring-p2l rewiring) m)))
    ;; We need to swap both the l2p and p2l mappings for this wire swap
    (setf (aref (rewiring-p2l rewiring) m) ln
          (aref (rewiring-p2l rewiring) n) lm)
    (when lm (setf (aref (rewiring-l2p rewiring) lm) n))
    (when ln (setf (aref (rewiring-l2p rewiring) ln) m))
    rewiring))

(defun rewiring-assign (rewiring logical physical)
  "Assign a logical qubit to a physical qubit in the rewiring. The physical qubit must be unoccupied or already assigned to that logical qubit."
  (let ((old-physical (aref (rewiring-l2p rewiring) logical))
        (old-logical (aref (rewiring-p2l rewiring) physical)))
    (assert (or (not old-physical) (= old-physical physical)) ()
            "Cannot re-assign logical qubit ~A to different physical qubit ~A."
            logical physical)
    (assert (or (not old-logical) (= old-logical logical)) ()
            "Cannot assign logical qubit ~A to occupied physical qubit ~A."
            logical physical)
    (setf (aref (rewiring-l2p rewiring) logical) physical
          (aref (rewiring-p2l rewiring) physical) logical)
    rewiring))

(defun rewiring-unassign (rewiring logical)
  "Remove an assignment of the logical qubit in the rewiring."
  (assert (aref (rewiring-l2p rewiring) logical) (logical)
          "Logical qubit ~A not assigned." logical)
  (let ((physical (aref (rewiring-l2p rewiring) logical)))
    (setf (aref (rewiring-p2l rewiring) physical) nil
          (aref (rewiring-l2p rewiring) logical) nil)
    rewiring))

(define-condition missing-rewiring-assignment (error)
  ((rewiring :initarg :rewiring
             :reader missing-rewiring-assignment-rewiring
             :documentation "The rewiring responsible for the error.")
   (wire :initarg :wire
         :reader missing-rewiring-assignment-wire
         :documentation "The wire whose assignment doesn't exist."))
  (:report (lambda (condition stream)
             (let ((*print-pretty* nil))
               (format stream "Rewiring missing assignment for qubit ~A in rewiring ~A."
                       (missing-rewiring-assignment-wire condition)
                       (missing-rewiring-assignment-rewiring condition)))))
  (:documentation "An error signaled if a rewiring is being applied but an assignment is missing."))

(defun apply-rewiring-l2p (rewiring n &key assert-wired)
  "Uses REWIRING to get a physical qubit address from the logical qubit address
N. When ASSERT-WIRED is T, ensures that the mapping exists."
  (let ((image (aref (rewiring-l2p rewiring) n)))
    (when (and assert-wired (null image))
      (error 'missing-rewiring-assignment :wire n :rewiring rewiring))
    image))

(defun apply-rewiring-p2l (rewiring n &key assert-wired)
  "Uses REWIRING to get a logical qubit address from the physical qubit address
N. When ASSERT-WIRED is T, ensures that the mapping exists."
  (let ((image (aref (rewiring-p2l rewiring) n)))
    (when (and assert-wired (null image))
      (error 'missing-rewiring-assignment :wire n :rewiring rewiring))
    image))

(defun rewiring-to-permutation-matrix-l2p (rewiring)
  (check-type rewiring full-rewiring)
  (let* ((size (expt 2 (rewiring-length rewiring)))
         (m (zeros (list size size))))
    (dotimes (i size)
      (let ((shuffled-i (loop :for j :below (rewiring-length rewiring)
                              :sum (if (logbitp j i)
                                       (ash 1 (apply-rewiring-l2p rewiring j))
                                       0))))
        (setf (magicl:tref m shuffled-i i) 1)))
    m))

(defun rewiring-to-permutation-matrix-p2l (rewiring)
  (check-type rewiring full-rewiring)
  (let* ((size (expt 2 (rewiring-length rewiring)))
         (m (zeros (list size size))))
    (dotimes (i size)
      (let ((shuffled-i (loop :for j :below (rewiring-length rewiring)
                              :sum (if (logbitp j i)
                                       (ash 1 (apply-rewiring-p2l rewiring j))
                                       0))))
        (setf (magicl:tref m shuffled-i i) 1)))
    m))

(defun trim-rewiring (rewiring)
  "Selects the minimum prefix of wires on which REWIRING acts. Useful for controlling the size of the matrices generated by REWIRING-TO-PERMUTATION-MATRIX-*, which for even small chip specifications can result in human-unreadable output."
  (check-type rewiring full-rewiring)
  (let ((l2p (rewiring-l2p rewiring))
        (p2l (rewiring-p2l rewiring)))
    (do ((i (length l2p) (1- i)))
        ((or (zerop i)
              (/= (aref l2p (1- i)) (1- i)))
         (init-rewiring
          :l2p (subseq l2p 0 i)
          :p2l (subseq p2l 0 i))))))

(defun fill-rewiring (rewiring)
  "Assigns the unassigned logical qubits to the remaining physical qubits."
  (check-type rewiring bijective-rewiring)
  (loop
    :with l2p := (rewiring-l2p rewiring)
    :and p2l := (rewiring-p2l rewiring)
    :and physical := 0

    :for current :across l2p
    :and logical :from 0

    :unless current
      :do (setf physical (position nil p2l :start physical)
                (aref l2p logical) physical
                (aref p2l physical) logical))
  rewiring)

(defun rewire-l2p-instruction (rewiring instr)
  "Remaps the resources of INSTRuction in-place to their physical counterparts under REWIRING.

Returns NIL. This mutates the instruction."
  (typecase instr
    (measurement
     (setf (measurement-qubit instr)
           (qubit (apply-rewiring-l2p rewiring
                                      (qubit-index (measurement-qubit instr))
                                      :assert-wired t))))
    (gate-application
     (setf (application-arguments instr)
           (mapcar (lambda (q) (qubit (apply-rewiring-l2p rewiring (qubit-index q)
                                                          :assert-wired t)))
                   (application-arguments instr))))
    (reset-qubit
     (setf (reset-qubit-target instr)
           (qubit (apply-rewiring-l2p rewiring
                                      (qubit-index (reset-qubit-target instr))
                                      :assert-wired t))))
    (otherwise
     (error "Requested to rewire ~/cl-quil:instruction-fmt/, but we don't know how to do this."
            instr)))
  ;; Return nil to emphasize side effect.
  nil)

(defmacro with-update-rewiring (rewiring q0 q1 &body body)
  "Temporarily assigns the LOGICAL to PHYSICAL in REWIRING, and then executes
BODY as an implicit PROGN."
  (let ((rewiring-sym (gensym))
        (q0-sym (gensym))
        (q1-sym (gensym)))
    `(let ((,rewiring-sym ,rewiring)
           (,q0-sym ,q0)
           (,q1-sym ,q1))
       (update-rewiring ,rewiring-sym ,q0-sym ,q1-sym)
       (unwind-protect (progn ,@body)
         (update-rewiring ,rewiring-sym ,q0-sym ,q1-sym)))))

(defmacro with-rewiring-assign (rewiring logical physical &body body)
  "Temporarily assigns the LOGICAL to PHYSICAL in REWIRING, and then executes
BODY as an implicit PROGN."
  (let ((rewiring-sym (gensym))
        (logical-sym (gensym))
        (physical-sym (gensym)))
    `(let ((,rewiring-sym ,rewiring)
           (,logical-sym ,logical)
           (,physical-sym ,physical))
       (rewiring-assign ,rewiring-sym ,logical-sym ,physical-sym)
       (unwind-protect (progn ,@body)
         (rewiring-unassign ,rewiring-sym ,logical-sym)))))

(defun rewiring-assigned-for-qubit-p (rewiring lq)
  "Test whether REWIRING contains a non-nil rewiring for logical qubit LQ."
  (and lq (aref (rewiring-l2p rewiring) lq)))

(defun rewiring-assigned-for-instruction-qubits-p (rewiring instr)
  "Test whether every logical qubit in INSTR has been rewired in REWIRING."
  (every (a:compose (a:curry #'rewiring-assigned-for-qubit-p rewiring)
                    #'qubit-index)
         (application-arguments instr)))

(defun rewiring-distance (rewiring target-rewiring qq-distances)
  "A measure of the distance between a given REWIRING and a TARGET-REWIRING.

It is expectred that REWIRING and PARTIAL-REWIRING should be defined on the same set of logical qubits. The resulting distance value is based on the qubit-qubit distance array QQ-DISTANCES."
  (loop :for i :across (rewiring-l2p rewiring)
        :for j :across (rewiring-l2p target-rewiring)
        :when i
          :sum (aref qq-distances i j)))
