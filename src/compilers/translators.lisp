;;;; translators.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

;; a helper macro for building translator functions

(defmacro define-translator (fn-name (bind-expression gate-var) &body body)
  "Defines a function whose single argument is passed to operator-bind, which wraps BODY. In the event of a match failure, GIVE-UP-COMPILATION is called."
  `(defun ,fn-name (,gate-var)
     (operator-match
      (((,bind-expression ,gate-var))
       ,@body)
      (_
       (give-up-compilation)))))

;; standard 1Q gate translators
;;
;; NOTE: a lot of this is already baked into #'euler-compile
;; NOTE: some standard gate sets include
;;       * H, X, Y, Z
;;       * ±X, ±X/2, ±Y, ±Y/2
;;       * RX, RY
;;       * RX, RY, RZ
;;       these are **not** mutually translatable. we listed them in increasing
;;       order of expressiveness.

(define-translator H-to-YX (("H" () q) H-gate)
  (list (build-gate "RY" '(#.(/ pi 2)) q)
        (build-gate "X"  ()            q)))

(define-translator Z-to-XYX (("Z" () q) Z-gate)
  (list (build-gate "RX" '(#.(/ pi -2)) q)
        (build-gate "Y"  ()             q)
        (build-gate "RX" '(#.(/ pi 2))  q)))

(define-translator Yhalf-to-HX (("RY" (#. (/ pi 2)) q) yhalf-gate)
  (list (build-gate "H" () q)
        (build-gate "X" () q)))

(define-translator RY-to-XZX (("RY" (alpha) q) ry-gate)
  (list (build-gate "RX" '(#.(/ pi 2))  q)
        (build-gate "RZ" `(,alpha)      q)
        (build-gate "RX" '(#.(/ pi -2)) q)))

(define-translator RX-to-ZXZXZ (("RX" (alpha) q) rx-gate)
  (list (build-gate "RZ" '(#.(/ pi 2))  q)
        (build-gate "RX" '(#.(/ pi 2))  q)
        (build-gate "RZ" `(,alpha)      q)
        (build-gate "RX" '(#.(/ pi -2)) q)
        (build-gate "RZ" '(#.(/ pi -2)) q)))

;; standard 2Q gate translators

(define-translator CZ-to-CPHASE (("CZ" () q1 q0) CZ-gate)
  (list (build-gate "CPHASE" '(#.pi) q1 q0)))

(define-translator CPHASE-to-CNOT (("CPHASE" (alpha) p q) CPHASE-gate)
  (list
   (build-gate "CNOT" ()                            p q)
   (build-gate "RZ"   (list (param-* alpha -0.5d0)) q)
   (build-gate "CNOT" ()                            p q)
   (build-gate "RZ"   (list (param-* alpha  0.5d0)) q)
   (build-gate "RZ"   (list (param-* alpha  0.5d0)) p)))

(define-translator CNOT-to-CZ (("CNOT" () q1 q0) cnot-gate)
  (list (build-gate "RY" '(#.(/ pi 2))  q0)
        (build-gate "CZ" ()             q0 q1)
        (build-gate "RY" '(#.(/ pi -2)) q0)
        (build-gate "Z"  ()             q1)))

(define-translator iSWAP-to-CNOT (("ISWAP" () q1 q0) iswap-gate)
  (list (build-gate "RY"   '(#.(/ pi 2))   q1)
        (build-gate "RZ"   '(#.(/ pi -2))  q1)
        (build-gate "RY"   '(#.(/ pi 2))   q0)
        (build-gate "RZ"   '(#.(/ pi 2))   q0)
        (build-gate "CNOT" ()              q1 q0)
        (build-gate "RX"   '(#.(/ pi 2))   q1)
        (build-gate "RZ"   '(#.(/ pi -2))  q0)
        (build-gate "CNOT" ()              q1 q0)
        (build-gate "RZ"   '(#.(* pi 3/2)) q1)
        (build-gate "RY"   '(#.(/ pi 2))   q1)
        (build-gate "RZ"   '(#.pi)         q1)
        (build-gate "RZ"   '(#.(* pi 5/2)) q0)
        (build-gate "RY"   '(#.(/ pi 2))   q0)
        (build-gate "RZ"   '(#.pi)         q0)))

(define-translator CZ-to-CNOT (("CZ" () q1 q0) cz-gate)
  (list (build-gate "RY"   '(#.(/ pi 2))  q1)
        (build-gate "CNOT" ()             q0 q1)
        (build-gate "RY"   '(#.(/ pi -2)) q1)))

(define-translator iSWAP-to-PSWAP (("ISWAP" () q1 q0) iswap-gate)
  (list (build-gate "PSWAP" '(#.(/ pi 2)) q1 q0)))

(define-translator ISWAP-to-PISWAP (("ISWAP" () q1 q0) iswap-gate)
  (list (build-gate "PISWAP" '(#.pi) q1 q0)))

;; thanks, optimal 2Q compiler
(define-translator PSWAP-to-CNOT (("PSWAP" (theta) q0 q1) pswap-gate)
  (list (build-gate "RY"   '(#.(/ pi 2))  q1)
        (build-gate "RZ"   '(#.pi)        q0)
        (build-gate "RY"   '(#.(/ pi 2))  q0)
        (build-gate "RZ"   '(#.(/ pi -2)) q0)
        (build-gate "CNOT" ()             q1 q0)
        (build-gate "CNOT" ()             q0 q1)
        (build-gate "RY"   `(,(param-* -1d0 theta)) q1)
        (build-gate "CNOT" ()             q1 q0)
        (build-gate "RZ"   '(#.(/ pi -2)) q1)
        (build-gate "RY"   '(#.(/ pi 2))  q1)
        (build-gate "RZ"   '(#.pi)        q0)
        (build-gate "RY"   '(#.(/ pi 2))  q0)
        (build-gate "RZ"   '(#.pi)        q0)))

;; TODO: the first half of this is really an inverse ISWAP, and if we had native
;;       access to this gate (e.g., as U(pi, -pi)), we could shorten this.
(define-translator CNOT-to-iSWAP (("CNOT" () q0 q1) cnot-gate)
  (list (build-gate "RZ"    '(#.(/ pi -2)) q0)
        (build-gate "Z"     ()             q0)
        (build-gate "Z"     ()             q1)
        (build-gate "ISWAP" ()             q0 q1)
        (build-gate "RY"    '(#.(/ pi -2)) q0)
        (build-gate "ISWAP" ()             q0 q1)
        (build-gate "RX"    '(#.(/ pi -2)) q1)))

(define-translator SWAP-to-CNOT (("SWAP" () q0 q1) swap-gate)
  (list (build-gate "CNOT" () q0 q1)
        (build-gate "CNOT" () q1 q0)
        (build-gate "CNOT" () q0 q1)))

(define-translator SWAP-to-CZ (("SWAP" () _ _) swap-gate)
  (reduce #'append
          (mapcar #'CNOT-to-CZ
                  (SWAP-to-CNOT swap-gate))))

(define-translator SWAP-to-PSWAP (("SWAP" () q0 q1) swap-gate)
  (list (build-gate "PSWAP" '(0d0) q0 q1)))

(define-translator SWAP-to-iSWAP (("SWAP" () q0 q1) swap-gate)
  (list (build-gate "RZ"    '(#.(/ pi 2)) q1)
        (build-gate "RY"    '(#.(/ pi 2)) q1)
        (build-gate "RZ"    '(#.(/ pi 2)) q1)
        (build-gate "RY"    '(#.(/ pi 2)) q0)
        (build-gate "RZ"    '(#.(/ pi 2)) q0)
        (build-gate "ISWAP" ()            q0 q1)
        (build-gate "RY"    '(#.(/ pi 2)) q0)
        (build-gate "ISWAP" ()            q0 q1)
        (build-gate "RY"    '(#.(/ pi 2)) q1)
        (build-gate "ISWAP" ()            q0 q1)
        (build-gate "RZ"    '(#.(/ pi 2)) q1)
        (build-gate "RY"    '(#.(/ pi 2)) q1)
        (build-gate "RZ"    '(#.pi)       q1)
        (build-gate "RZ"    '(#.(/ pi 2)) q0)
        (build-gate "RY"    '(#.(/ pi 2)) q0)
        (build-gate "RZ"    '(#.pi)       q0)))

;; there are other standard algorithms that do this with better run-times, but i
;; don't know where to find (e.g.) a standard implementation of a priority queue
;; in Lisp, and i'm not presently interested in writing one. so, you get a naive
;; implementation of Dijkstra's.
(defun find-shortest-path-on-chip-spec (chip-spec start-node target-node)
  "Returns a sequence of link addresses that reach from START-NODE to TARGET-NODE on CHIP-SPEC."
  (let* ((num-qubits (length (vnth 0 (chip-specification-objects chip-spec))))
         (computed-paths (make-array num-qubits :initial-element -1))
         (current-node start-node)
         (unvisited-nodes (make-array num-qubits :initial-element t)))
    ;; the trivial path reaches the start node.
    (setf (aref computed-paths current-node) nil)
    ;; loop until we reach the target node
    (loop :until (= current-node target-node) :do
      (progn
        ;; set the current node as visited
        (setf (aref unvisited-nodes current-node) nil)
        ;; at the current node, look at all its neighbors
        (loop :for cxn :across (vnth 1 (hardware-object-cxns (chip-spec-nth-qubit chip-spec current-node))) :do
          (let ((other-qubit (let ((pair (chip-spec-qubits-on-link chip-spec cxn)))
                               (if (= (vnth 0 pair) current-node) (vnth 1 pair) (vnth 0 pair)))))
            (if (or
                 ;; if we haven't visited the other qubit yet
                 (typep (aref computed-paths other-qubit) 'integer)
                 ;; or if our current path is shorter than the existing path
                 (< (1+ (length (aref computed-paths current-node)))
                    (length (aref computed-paths other-qubit))))
                ;; then store this new shorter path
                (setf (aref computed-paths other-qubit)
                      (cons cxn (aref computed-paths current-node))))))
        ;; now we're looking for a new current node.
        ;; over all of the nodes...
        (let ((shortest-distance most-positive-fixnum))
          (dotimes (new-node num-qubits)
            (when (and
                   ;; if we haven't visited new-node yet
                   (aref unvisited-nodes new-node)
                   ;; and it's adjacent to a visited node
                   (typep (aref computed-paths new-node) 'list)
                   ;; and it's the shortest path we've seen yet
                   (< (length (aref computed-paths new-node)) shortest-distance))
              ;; then pick this as our new current node (so far)
              (setf shortest-distance (1+ (length (aref computed-paths new-node))))
              (setf current-node new-node))))))
    ;; finally, reply with the computed shortest path.
    (nreverse (aref computed-paths target-node))))

(defun SWAP-to-native-SWAPs (chip-spec swap-gate)
  (unless (operator-match-p swap-gate '("SWAP" () _ _))
    (give-up-compilation))
  (let* ((q0 (qubit-index (first (application-arguments swap-gate))))
         (q1 (qubit-index (second (application-arguments swap-gate))))
         (computed-path (find-shortest-path-on-chip-spec chip-spec q1 q0))
         (f-list (mapcar (lambda (link-cxn)
                           (apply #'build-gate "SWAP" '() 
                                  (map 'list #'qubit (chip-spec-qubits-on-link chip-spec link-cxn))))
                         computed-path)))
    (append f-list (rest (reverse f-list)))))

(defun CNOT-to-native-CNOTs (chip-spec cnot-gate)
  (unless (operator-match-p cnot-gate '("CNOT" () _ _))
    (give-up-compilation))
  (let* ((q1 (qubit-index (first (application-arguments cnot-gate))))
         (q0 (qubit-index (second (application-arguments cnot-gate))))
         ;; find a shortest path between the two qubits in the swap gate
         (computed-path (find-shortest-path-on-chip-spec chip-spec q1 q0)))
    (labels
        ((build-CNOT-string (index-list prev-qubit)
           (let* ((unoriented-qubit-indices (coerce (chip-spec-qubits-on-link chip-spec (first index-list))
                                                    'list))
                  (oriented-qubits (if (= prev-qubit (first unoriented-qubit-indices))
                                       (mapcar #'qubit unoriented-qubit-indices)
                                       (mapcar #'qubit (reverse unoriented-qubit-indices)))))
             (cond
               ;; base case
               ((= 1 (length index-list))
                (list (apply #'build-gate "CNOT" '() oriented-qubits)))
               ;; recursive case
               (t
                (let ((temp-string (build-CNOT-string (rest index-list)
                                                      (qubit-index (second oriented-qubits)))))
                  (append
                   (list
                    (apply #'build-gate "CNOT" '() oriented-qubits))
                   temp-string
                   (list
                    (apply #'build-gate "CNOT" '() oriented-qubits))
                   temp-string)))))))
      (build-CNOT-string computed-path q1))))

(defun CZ-to-native-CZs (chip-spec cz-gate)
  (unless (operator-match-p cz-gate '("CZ" () _ _))
    (give-up-compilation))
  (let ((q1 (qubit-index (first (application-arguments cz-gate))))
        (q0 (qubit-index (second (application-arguments cz-gate)))))
    (nconc
     (list (build-gate "RY" '(#.(/ pi 2)) q0))
     (CNOT-to-native-CNOTs chip-spec
                           (build-gate "CNOT" () q1 q0))
     (list (build-gate "RY" '(#.(/ pi -2)) q0)))))

(defun ISWAP-to-native-ISWAPs (chip-spec iswap-gate)
  (let* ((cnot-equivalent (iSWAP-to-CNOT iswap-gate))
         (cnots-on-chip (loop :for instr :in cnot-equivalent
                              :nconc (if (string= "CNOT" (application-operator-name instr))
                                         (CNOT-to-native-CNOTs chip-spec instr)
                                         (list instr))))
         (iswaps-on-chip (loop :for instr :in cnots-on-chip
                               :nconc (if (string= "CNOT" (application-operator-name instr))
                                          (CNOT-to-iSWAP instr)
                                          (list instr)))))
    iswaps-on-chip))

(defun CPHASE-to-native-CPHASEs (chip-spec cphase-gate)
  (let* ((cnot-equivalent (CPHASE-to-CNOT cphase-gate)))
    (mapcan (lambda (g)
              (cond
                ((string= "CNOT" (application-operator-name g))
                 (CNOT-to-native-CNOTs chip-spec g))
                (t
                 (list g))))
            cnot-equivalent)))

(defun PISWAP-to-native-PISWAPs (chip-spec piswap-gate)
  (unless (operator-match-p piswap-gate '("PISWAP" (_) _ _))
    (give-up-compilation))
  (let ((theta (first (application-parameters piswap-gate)))
        (p (qubit-index (first (application-arguments piswap-gate))))
        (q (qubit-index (second (application-arguments piswap-gate)))))
    ;; find a shortest path between the two qubits in the swap gate
    (let* ((computed-path (find-shortest-path-on-chip-spec chip-spec p q)))
      (labels
          ((build-PISWAP-string (index-list prev-qubit)
             (let* ((unoriented-qubit-indices (coerce (chip-spec-qubits-on-link chip-spec (first index-list))
                                                      'list))
                    (oriented-qubits (if (= (first unoriented-qubit-indices) prev-qubit)
                                         unoriented-qubit-indices
                                         (reverse unoriented-qubit-indices))))
               (cond
                 ;; base case
                 ((= 1 (length index-list))
                  (list (build-gate "PISWAP" `(,theta) (first oriented-qubits) (second oriented-qubits))))
                 ;; recursive case
                 (t
                  (let ((temp-string (build-PISWAP-string (rest index-list)
                                                          (second oriented-qubits))))
                    (append
                     (list
                      (build-gate "SWAP" () (first oriented-qubits) (second oriented-qubits)))
                     temp-string
                     (list
                      (build-gate "SWAP" () (first oriented-qubits) (second oriented-qubits))))))))))
        (build-PISWAP-string computed-path p)))))
