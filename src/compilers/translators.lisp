;;;; translators.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)


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

(define-compiler H-to-YX ((H-gate ("H" () q)))
  (list (build-gate "RY" '(#.(/ pi 2)) q)
        (build-gate "X"  ()            q)))

(define-compiler Z-to-XYX ((Z-gate ("Z" () q)))
  (list (build-gate "RX" '(#.(/ pi -2)) q)
        (build-gate "Y"  ()             q)
        (build-gate "RX" '(#.(/ pi 2))  q)))

(define-compiler Yhalf-to-HX ((yhalf-gate ("RY" (#.(/ pi 2)) q)))
  (list (build-gate "H" () q)
        (build-gate "X" () q)))

(define-compiler RY-to-XZX ((ry-gate ("RY" (alpha) q)))
  (list (build-gate "RX" '(#.(/ pi 2))  q)
        (build-gate "RZ" `(,alpha)      q)
        (build-gate "RX" '(#.(/ pi -2)) q)))

(define-compiler RX-to-ZXZXZ ((rx-gate ("RX" (alpha) q)))
  (list (build-gate "RZ" '(#.(/ pi 2))  q)
        (build-gate "RX" '(#.(/ pi 2))  q)
        (build-gate "RZ" `(,alpha)      q)
        (build-gate "RX" '(#.(/ pi -2)) q)
        (build-gate "RZ" '(#.(/ pi -2)) q)))

(define-compiler PHASE-to-RZ ((phase-gate ("PHASE" (alpha) q)))
  (list (build-gate "RZ" `(,alpha) q)))

;; standard 2Q gate translators

(define-compiler CZ-to-CPHASE ((CZ-gate ("CZ" () q1 q0)))
  (list (build-gate "CPHASE" '(#.pi) q1 q0)))

(define-compiler CPHASE-to-CNOT ((CPHASE-gate ("CPHASE" (alpha) p q)))
  (list
   (build-gate "CNOT" ()                            p q)
   (build-gate "RZ"   (list (param-* alpha -0.5d0)) q)
   (build-gate "CNOT" ()                            p q)
   (build-gate "RZ"   (list (param-* alpha  0.5d0)) q)
   (build-gate "RZ"   (list (param-* alpha  0.5d0)) p)))

(define-compiler CNOT-to-CZ ((cnot-gate ("CNOT" () q1 q0)))
  (list (build-gate "RY" '(#.(/ pi 2))  q0)
        (build-gate "CZ" ()             q0 q1)
        (build-gate "RY" '(#.(/ pi -2)) q0)
        (build-gate "Z"  ()             q1)))

(define-compiler CNOT-to-flipped-CNOT ((cnot-gate ("CNOT" () control target)))
  (list (build-gate "H"    () control)
        (build-gate "H"    () target)
        (build-gate "CNOT" () target control) ; !!
        (build-gate "H"    () control)
        (build-gate "H"    () target)))

(define-compiler iSWAP-to-CNOT ((iswap-gate ("ISWAP" () q1 q0)))
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

(define-compiler CZ-to-CNOT ((cz-gate ("CZ" () q1 q0)))
  (list (build-gate "RY"   '(#.(/ pi 2))  q1)
        (build-gate "CNOT" ()             q0 q1)
        (build-gate "RY"   '(#.(/ pi -2)) q1)))

(define-compiler iSWAP-to-PSWAP ((iswap-gate ("ISWAP" () q1 q0)))
  (list (build-gate "PSWAP" '(#.(/ pi 2)) q1 q0)))

(define-compiler ISWAP-to-PISWAP ((iswap-gate ("ISWAP" () q1 q0)))
  (list (build-gate "PISWAP" '(#.pi) q1 q0)))

;; thanks, optimal 2Q compiler
(define-compiler PSWAP-to-CNOT ((pswap-gate ("PSWAP" (theta) q0 q1)))
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

(define-compiler CNOT-to-iSWAP ((cnot-gate ("CNOT" () q0 q1)))
  (list (build-gate "RZ"    '(#.(/ pi -2)) q0)
        (build-gate "Z"     ()             q0)
        (build-gate "Z"     ()             q1)
        (build-gate "ISWAP" ()             q0 q1)
        (build-gate "RY"    '(#.(/ pi -2)) q0)
        (build-gate "ISWAP" ()             q0 q1)
        (build-gate "RX"    '(#.(/ pi -2)) q1)))

(define-compiler SWAP-to-CNOT ((swap-gate ("SWAP" () q0 q1)))
  (list (build-gate "CNOT" () q0 q1)
        (build-gate "CNOT" () q1 q0)
        (build-gate "CNOT" () q0 q1)))

(define-compiler SWAP-to-PSWAP ((swap-gate ("SWAP" () q0 q1)))
  (list (build-gate "PSWAP" '(0d0) q0 q1)))

(define-compiler SWAP-to-ISWAP ((swap-gate ("SWAP" () q0 q1)))
  (list (build-gate "ISWAP" () q0 q1)
        (build-gate "RX"    '(#.(/ pi 2)) q1)
        (build-gate "ISWAP" () q0 q1)
        (build-gate "RX"    '(#.(/ pi 2)) q0)
        (build-gate "ISWAP" () q0 q1)
        (build-gate "RX"    '(#.(/ pi 2)) q1)))

(define-compiler CCNOT-to-CNOT ((ccnot-gate ("CCNOT" () q0 q1 q2)))
  (list (build-gate "H"    ()             q2)
        (build-gate "CNOT" ()             q1 q2)
        (build-gate "RZ"   '(#.(/ pi -4)) q2)
        (build-gate "CNOT" ()             q0 q2)
        (build-gate "RZ"   '(#.(/ pi 4))  q2)
        (build-gate "CNOT" ()             q1 q2)
        (build-gate "RZ"   '(#.(/ pi -4)) q2)
        (build-gate "CNOT" ()             q0 q2)
        (build-gate "RZ"   '(#.(/ pi 4))  q1)
        (build-gate "RZ"   '(#.(/ pi 4))  q2)
        (build-gate "CNOT" ()             q0 q1)
        (build-gate "H"    ()             q2)
        (build-gate "RZ"   '(#.(/ pi 4))  q0)
        (build-gate "RZ"   '(#.(/ pi -4)) q1)
        (build-gate "CNOT" ()             q0 q1)))

(defun find-shortest-path-on-chip-spec (chip-spec start-node target-node)
  "Returns a sequence of qubit indices that reach from START-NODE to TARGET-NODE on CHIP-SPEC, or NIL if no path can be found.

Note that if (= START-NODE TARGET-NODE) then (list START-NODE) is returned."
  (assert (<= 0 start-node (1- (chip-spec-n-qubits chip-spec))) ()
          "Can't possibly find a path if START-NODE is not found within the CHIP-SPEC qubits.")
  (assert (<= 0 target-node (1- (chip-spec-n-qubits chip-spec))) ()
          "Can't possibly find a path if END-NODE is not found within the CHIP-SPEC qubits.")
  (labels ((make-graph ()
             "Builds a graph ((ni . (nj nk ...)) (...) ...) from CHIP-SPEC where ni are the nodes (qubits), and the (nj nk ...) are the nodes adjacent to ni."
             (loop :for i :below (chip-spec-n-qubits chip-spec)
                   :collect (cons i (chip-spec-adj-qubits chip-spec i))))
           (neighbors (graph node seen)
             "Find the neighbors of NODE in GRAPH that are not in SEEN."
             (let* ((all-neighbors (cdr (find node graph :key #'car))))
               (remove-if (lambda (neighbor) (find neighbor seen))
                          all-neighbors)))
           (mindistance (distances seen)
             "Find the node with smallest distance in DISTANCES that is not in SEEN."
             ;; dislike
             (cdr (reduce (lambda (a b) (if (<= (car a) (car b)) a b))
                          (loop :for i :below (length distances)
                                :unless (find i seen)
                                  :collect (cons (nth i distances) i)))))
           (dijkstra (graph)
             "Compute shortest distances to each node in GRAPH from START-NODE."
             (let ((nodes (loop :for n :below (chip-spec-n-qubits chip-spec) :collect n))
                   (seen nil)
                   (distances (loop :for n :below (chip-spec-n-qubits chip-spec)
                                    :collect (if (= n start-node) 0 most-positive-fixnum)))
                   (prev-nodes '()))
               (loop :until (every #'null nodes) :do
                 (let ((next-node (mindistance distances seen)))
                   (setf (nth next-node nodes) nil)
                   (push next-node seen)
                   (loop :for neighbor :in (neighbors graph next-node seen)
                         :for next-node-dist := (nth next-node distances)
                         :for neighbor-dist := (nth neighbor distances) :do
                           (when (>= neighbor-dist (1+ next-node-dist))
                             (setf (nth neighbor distances) (1+ next-node-dist))
                             (push (cons neighbor next-node) prev-nodes)))))
               (values distances prev-nodes)))
           (path (prev)
             "Compute the shortest path from START-NODE to TARGET-NODE given path information PREV."
             (let ((path nil)
                   (node target-node))
               (when (or (find node prev :key #'car)
                         (= node start-node))
                 (loop :while node :do
                   (progn (push node path)
                          (setq node (cdr (find node prev :key #'car))))))
               path)))
    (multiple-value-bind (distances prev)
        (dijkstra (make-graph))
      (declare (ignore distances))
      (path prev))))

(define-compiler SWAP-to-native-SWAPs ((swap-gate ("SWAP" () q0 q1)))
  (with-compilation-context (:chip-specification chip-spec)
    (let ((computed-path (find-shortest-path-on-chip-spec chip-spec q1 q0)))
      (unless computed-path
        (give-up-compilation))
      (let* ((f-list (mapcar (lambda (q1 q2) (build-gate "SWAP" '() q1 q2))
                             computed-path (rest computed-path))))
        (append f-list (rest (reverse f-list)))))))

(define-compiler CNOT-to-native-CNOTs ((cnot-gate ("CNOT" () q1 q0)))
  ;; find a shortest path between the two qubits in the swap gate
  (with-compilation-context (:chip-specification chip-spec)
    (let* ((computed-path (find-shortest-path-on-chip-spec chip-spec q1 q0)))
      (labels
          ((build-CNOT-string (qubit-string)
             (cond
               ;; base case
               ((= 2 (length qubit-string))
                (list (apply #'build-gate "CNOT" '() qubit-string)))
               ;; recursive case
               (t
                (let ((inner-string (build-CNOT-string (rest qubit-string)))
                      ;; one could also make a deep copy instead of running build-CNOT-string again
                      (inner-string-copy (build-CNOT-string (rest qubit-string)))
                      (first-two-qubits (list (first qubit-string) (second qubit-string))))
                  (append
                   (list
                    (apply #'build-gate "CNOT" '() first-two-qubits))
                   inner-string
                   (list
                    (apply #'build-gate "CNOT" '() first-two-qubits))
                   inner-string-copy))))))
        (build-CNOT-string computed-path)))))

(define-compiler CZ-to-native-CZs ((cz-gate ("CZ" () q0 q1)))
  (with-compilation-context (:full-context context)
    (nconc
     (list (build-gate "RY" '(#.(/ pi 2)) q1))
     (CNOT-to-native-CNOTs (build-gate "CNOT" () q0 q1) :context context)
     (list (build-gate "RY" '(#.(/ pi -2)) q1)))))

(define-compiler ISWAP-to-native-ISWAPs ((iswap-gate ("ISWAP" _ _ _)))
  (with-compilation-context (:full-context context)
    (let* ((cnot-equivalent (iSWAP-to-CNOT iswap-gate))
           (cnots-on-chip (loop :for instr :in cnot-equivalent
                                :nconc (if (string= "CNOT" (application-operator-name instr))
                                           (CNOT-to-native-CNOTs instr :context context)
                                           (list instr))))
           (iswaps-on-chip (loop :for instr :in cnots-on-chip
                                 :nconc (if (string= "CNOT" (application-operator-name instr))
                                            (CNOT-to-iSWAP instr)
                                            (list instr)))))
      iswaps-on-chip)))

(define-compiler CPHASE-to-native-CPHASEs ((cphase-gate ("CPHASE" _ _ _)))
  (with-compilation-context (:full-context context)
    (let* ((cnot-equivalent (CPHASE-to-CNOT cphase-gate)))
      (mapcan (lambda (g)
                (cond
                  ((string= "CNOT" (application-operator-name g))
                   (CNOT-to-native-CNOTs g :context context))
                  (t
                   (list g))))
              cnot-equivalent))))

(define-compiler PISWAP-to-native-PISWAPs ((piswap-gate ("PISWAP" (theta) q0 q1)))
  (with-compilation-context (:chip-specification chip-spec)
    (let ((computed-path (find-shortest-path-on-chip-spec chip-spec q0 q1)))
      (labels
          ((build-PISWAP-string (index-list)
             (let* ((oriented-qubits (mapcar #'qubit index-list))
                    (a (first oriented-qubits))
                    (b (second oriented-qubits)))
               (cond
                 ;; base case
                 ((= 2 (length index-list))
                  (list (build-gate "PISWAP" `(,theta) a b)))
                 ;; recursive case
                 (t
                  (let ((temp-string (build-PISWAP-string (rest index-list)))
                        (gate (list (build-gate "SWAP" () a b))))
                    (append gate temp-string gate)))))))
        (build-PISWAP-string computed-path)))))
