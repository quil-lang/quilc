;;;; approx.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)


(defun orthogonal-decomposition (m)
  (declare (optimize (debug 3) (speed 0) (safety 3)))
  (let* ((a (diagonalizer-in-e-basis m))
         (db (reduce #'magicl:multiply-complex-matrices
                     (list (magicl:transpose a) +edag-basis+ m +e-basis+)))
         (diag (loop :for j :below 4
                     :collect (let ((mag 0d0)
                                    phase)
                                (dotimes (i 4)
                                  (when (>= (abs (magicl:ref db j i)) mag)
                                    (setf mag (abs (magicl:ref db j i)))
                                    (setf phase (mod (phase (magicl:ref db j i)) pi))))
                                (exp (* #C(0 1) phase)))))
         (d (magicl:diag 4 4 diag))
         (b (magicl:multiply-complex-matrices
             (magicl:conjugate-transpose d) db)))
    ;; it could be the case that b has negative determinant. if that's the case, we'll
    ;; swap two of its columns that live in the same eigenspace.
    (when (double~ -1d0 (magicl:det b))
      (setf d (magicl:multiply-complex-matrices
               d
               (magicl:diag 4 4 (list -1 1 1 1))))
      (setf b (magicl:multiply-complex-matrices
               (magicl:diag 4 4 (list -1 1 1 1))
               b)))
    (assert (double~ 1d0 (magicl:det m)))
    (assert (double~ 1d0 (magicl:det a)))
    (assert (double~ 1d0 (magicl:det b)))
    (assert (double~ 1d0 (magicl:det d)))
    (assert (matrix-equals-dwim (magicl:diag 4 4 '(1d0 1d0 1d0 1d0))
                                (magicl:multiply-complex-matrices a (magicl:transpose a))))
    (assert (matrix-equals-dwim (magicl:diag 4 4 '(1d0 1d0 1d0 1d0))
                                (magicl:multiply-complex-matrices b (magicl:transpose b))))
    (assert (matrix-equals-dwim (reduce #'magicl:multiply-complex-matrices
                                        (list +edag-basis+ m +e-basis+))
                                (reduce #'magicl:multiply-complex-matrices
                                        (list a d b))))
    (values a d b)))

(defun trace-distance (m1 m2)
  (assert (= 4 (magicl:matrix-rows m1) (magicl:matrix-rows m2)))
  (let* ((prod (magicl:multiply-complex-matrices m1 (magicl:conjugate-transpose m2)))
         (tr (loop :for j :below 4 :sum (magicl:ref prod j j))))
    (/ (+ 4 (abs (* tr tr))) 20)))

(defun fidelity-of-straight-quil (instrs chip-spec)
  (let ((ls (make-lscheduler)))
    (append-instructions-to-lschedule ls instrs)
    (lscheduler-calculate-fidelity ls chip-spec)))

(defun get-canonical-coords-from-diagonal (d)
  (assert (= 4 (magicl:matrix-rows d) (magicl:matrix-cols d)))
  (let* ((angles (mapcar #'phase (loop :for i :below 4 :collect (magicl:ref d i i))))
         (first  (mod (+ (third angles) (fourth angles)) pi))
         (second (mod (- (+ (third angles) (first angles)))  pi))
         (third  (mod (+ (third angles) (second angles)) pi))
         (first  (if (double= pi first)  0d0 first))
         (second (if (double= pi second) 0d0 second))
         (third  (if (double= pi third)  0d0 third))
         (option-1 (sort (list first second third) #'>))
         (option-2 (sort (list (- pi first) (- pi second) third) #'>))
         (option-3 (sort (list first (- pi second) (- pi third)) #'>))
         (option-4 (sort (list (- pi first) second (- pi third)) #'>)))
    (flet ((test (seq)
             ;; TODO: is this a utils kind of thing?
             (and (>= (+ (/ pi 2) +double-comparison-threshold-strict+)
                      (- (first seq) +double-comparison-threshold-strict+))
                  (>= (+ (first seq) +double-comparison-threshold-strict+)
                      (- (second seq) +double-comparison-threshold-strict+))
                  (>= (+ (second seq) +double-comparison-threshold-strict+)
                      (- (abs (third seq)) +double-comparison-threshold-strict+)))))
      (cond
        ((test option-1) option-1)
        ((test option-2) option-2)
        ((test option-3) option-3)
        ((test option-4) option-4)
        (t (error "uh-oh (cf. ~a)" (list first second third)))))))

(defun get-fidelity-distance-from-canonical-coords (coords1 coords2)
  (let* ((deltas  (mapcar #'- coords1 coords2))
         (cosines (mapcar #'cos deltas))
         (sines   (mapcar #'sin deltas)))
    (/ (+ 4 (abs (+ (reduce #'* cosines) (* #C(0 1) (reduce #'* sines))))) 20)))

(defun make-signed-permutation-matrix (sigma signs)
  (let ((o (magicl:make-zero-matrix 4 4)))
    (loop :for i :below 4
          :for j :in (cl-permutation:permute sigma (alexandria:iota 4))
          :for sign :in signs
          :do (setf (magicl:ref o i j) sign))
    o))

(defun match-matrix-to-an-e-basis-diagonalization (m a d b)
  ;; we're going to decompose e^* m' e = a' d' b'.
  ;; our goal is then to maximize over signed permutation matrices o so that
  ;;     d' approx o^T d o 
  ;; is as good an approximation as possible.
  ;; then, setting u = e (a d b) e^*, we know that
  ;;     u approx a o a'^T m b'^T o^T b
  ;; is as good an approximation as possible.
  ;; we return the SU(2) (x) SU(2) versions of the triple products
  ;; (a o a'^T) and (b'^T o^T b) as well as the fidelity as a values triple
  (multiple-value-bind (aprime dprime bprime)
      (orthogonal-decomposition (magicl:scale (expt (magicl:det m) -1/4) m))
    (let (o
          (d-as-list      (loop :for j :below 4 :collect (magicl:ref d j j)))
          (dprime-as-list (loop :for j :below 4 :collect (magicl:ref dprime j j)))
          (max-fidelity 0d0))
      ;; maximize the trace over signed permutations
      (cl-permutation:doperms (sigma 4)
        (dolist (signs (list (list  1  1  1  1)
                             (list -1 -1  1  1)
                             (list -1  1 -1  1)
                             (list -1  1  1 -1)))
          (when (= -1 (cl-permutation:perm-sign sigma))
            (setf (first signs) (* -1 (first signs))))
          (let* ((new-trace
                   (loop :for x :in (cl-permutation:permute sigma d-as-list)
                         :for y :in dprime-as-list
                         :for sign :in signs
                         :sum (* x sign (conjugate y))))
                 (new-fidelity (/ (+ 4 (abs (* new-trace new-trace))) 20)))
            (when (>= new-fidelity max-fidelity)
              (setf max-fidelity new-fidelity)
              (setf o (make-signed-permutation-matrix sigma signs))))))
      #+ignore
      (progn
        (print a) (terpri) (print d) (terpri) (print b) (terpri)
        (print (reduce #'magicl:multiply-complex-matrices
                       (list +e-basis+ a d b +edag-basis+))) (terpri)
        (print aprime) (terpri) (print dprime) (terpri) (print bprime) (terpri)
        (print (reduce #'magicl:multiply-complex-matrices
                       (list +e-basis+ aprime dprime bprime +edag-basis+))) (terpri)
        (print o) (terpri))
      (assert (matrix-equals-dwim (magicl:diag 4 4 '(1d0 1d0 1d0 1d0))
                                  (magicl:multiply-complex-matrices a (magicl:transpose a))))
      (assert (matrix-equals-dwim (magicl:diag 4 4 '(1d0 1d0 1d0 1d0))
                                  (magicl:multiply-complex-matrices b (magicl:transpose b))))
      (assert (matrix-equals-dwim (magicl:diag 4 4 '(1d0 1d0 1d0 1d0))
                                  (magicl:multiply-complex-matrices aprime (magicl:transpose aprime))))
      (assert (matrix-equals-dwim (magicl:diag 4 4 '(1d0 1d0 1d0 1d0))
                                  (magicl:multiply-complex-matrices bprime (magicl:transpose bprime))))
      (assert (matrix-equals-dwim (magicl:diag 4 4 '(1d0 1d0 1d0 1d0))
                                  (magicl:multiply-complex-matrices o (magicl:transpose o))))
      (assert (double~ 1d0 (magicl:det a)))
      (assert (double~ 1d0 (magicl:det aprime)))
      (assert (double~ 1d0 (magicl:det b)))
      (assert (double~ 1d0 (magicl:det bprime)))
      (assert (double~ 1d0 (magicl:det o)))
      (values (reduce #'magicl:multiply-complex-matrices
                      (list +e-basis+ a o (magicl:transpose aprime) +edag-basis+))
              (reduce #'magicl:multiply-complex-matrices
                      (list +e-basis+ (magicl:transpose bprime) (magicl:transpose o) b +edag-basis+))
              max-fidelity))))

;; IMPORTANT TODO: spend time refactoring this so that it blends seemlessly with optimal 2q.
;; it should be possible to have 'circuit templates' that have a membership test, a projector
;; to the appropriate polyhedron, and a circuit manufacturer for points that pass the
;; membership test.  a chip can provide which templates it cares about, and we can additionally
;; thread logic to do/don't bother with projection based on the approximation flag.

(defun approximate-2Q-compiler (instr &key (target ':cz) (chip-spec nil))
  (check-type instr gate-application)
  (check-type target optimal-2q-target)
  (check-type chip-spec chip-specification)
  
  ;; TODO: remove this first restriction
  (unless (optimal-2q-target-meets-requirements target ':cz)
    (give-up-compilation))
  (unless (= 2 (length (application-arguments instr)))
    (give-up-compilation))
  (unless *enable-approximate-compilation*
    (give-up-compilation))
  
  ;; extract matrix, canonical decomposition
  (let* ((q1 (qubit-index (first (application-arguments instr))))
         (q0 (qubit-index (second (application-arguments instr))))
         (m (gate-matrix instr))
         (m (magicl:scale (expt (magicl:det m) -1/4) m)))
    (multiple-value-bind (a d b) (orthogonal-decomposition m)
      ;; now we manufacture a bunch of candidate circuits
      (let* ((candidate-pairs nil)
             (coord (get-canonical-coords-from-diagonal d)))
        (flet
            ((build-depth-0-circuit (coord q1 q0)
               (declare (ignore coord))
               (list (build-gate "I" () q0)
                     (build-gate "I" () q1)))
             (build-depth-1-circuit (coord q1 q0)
               (declare (ignore coord))
               (list (build-gate "CZ" () q1 q0)))
             ;; TODO: fix this circuit, which is wrong.
             (build-depth-2-circuit (coord q1 q0)
               (list (build-gate "CZ" () q1 q0)
                     (build-gate "RY" (list (/ (+ (first coord) (second coord)) 2)) q1)
                     (build-gate "RY" (list (/ (- (first coord) (second coord)) 2)) q1)
                     (build-gate "CZ" () q1 q0)))
             ;; TODO: fix this circuit, which is wrong.
             (build-depth-3-circuit (coord q1 q0)
               (list (build-gate "RY" '(#.(/ pi 2))         q0)
                     (build-gate "CZ" '()                   q0 q1)
                     (build-gate "RY" '(#.(/ pi -2))        q0)
                     (build-gate "RY" (list (second coord)) q1)
                     (build-gate "RZ" (list (third coord))  q0)
                     (build-gate "RY" '(#.(/ pi 2))         q1)
                     (build-gate "CZ" '()                   q0 q1)
                     (build-gate "RY" '(#.(/ pi -2))        q1)
                     (build-gate "RY" (list (first coord))  q1)
                     (build-gate "RY" '(#.(/ pi 2))         q0)
                     (build-gate "CZ" '()                   q0 q1)
                     (build-gate "RY" '(#.(/ pi -2))        q0))))
          (dolist (circuit-crafter (list #'build-depth-0-circuit
                                         #'build-depth-1-circuit
                                         #'build-depth-2-circuit
                                         #'build-depth-3-circuit))
            
            (let* ((center-circuit (apply circuit-crafter coord (mapcar #'qubit-index
                                                                        (application-arguments instr))))
                   (ls (append-instructions-to-lschedule (make-lscheduler) center-circuit))
                   (circuit-cost (lscheduler-calculate-fidelity ls chip-spec)))
              (multiple-value-bind (ua ub fidelity)
                  (match-matrix-to-an-e-basis-diagonalization
                   (make-matrix-from-quil center-circuit)
                   a d b)
                (format t "APPROXIMATE-2Q-COMPILER: ~a resulted in
    ... lone fidelity ~a
    ... and cost ~a,
    ... hence adjusted fidelity ~a.~%"
                        circuit-crafter
                        fidelity
                        circuit-cost
                        (* circuit-cost fidelity))
                (multiple-value-bind (b1 b0) (convert-su4-to-su2x2 ub)
                  (multiple-value-bind (a1 a0) (convert-su4-to-su2x2 ua)
                    (push (cons (* circuit-cost fidelity) 
                                (append (list (anon-gate "B0" b0 q0)
                                              (anon-gate "B1" b1 q1))
                                        center-circuit
                                        (list (anon-gate "A0" a0 q0)
                                              (anon-gate "A1" a1 q1))))
                          candidate-pairs)))))))
        ;; now vomit the results
        (cdr (alexandria:extremum candidate-pairs #'> :key #'car))))))

(defun approximate-2Q-compiler-for (target chip-spec)
  (lambda (instr)
    (approximate-2Q-compiler instr
                             :target target
                             :chip-spec chip-spec)))
