;;;; cs-compile.lisp
;;;;
;;;; Author: Eric Peterson
;;;; Algorithm based off of _Quantum Gate Decomposition Algorithms_ by Alexander Slepoy
;;;;
;;;; This file contains attic routines of how we used to do generic compilation
;;;; of multi-qubit programs. This routine is completely outperformed by "quantum
;;;; Shannon decomposition", src/compilers/qs-compile.lisp, and the non-historian
;;;; user is advised to look there instead for how we do this in the modern age.

(in-package #:cl-quil)

;;;; REMEMBER: THIS FILE IS NO LONGER MAINTAINED!
;;;;           PERUSE AT YOUR OWN RISK.
;;;;           PROBABLY DON'T MODIFY.

(define-global-counter **csc-counter** generate-csc-tag)

;; zips up a compatible pair of lists of UCRs into a single such list by
;; letting each list act on a block factor
#+#:pedagogical-purposes-only
(defun ucr-zipper (string0 string1 control-qubit)
  (mapcar
       (lambda (left right)
         ;; combine
         ;;   left  = UCR(g, t, c(n-1), ..., c0, alpha1, ..., alpha(2^n))
         ;;   right = UCR(g, t, c(n-1), ..., c0, beta1, ..., beta(2^n))
         ;; into UCR(g, t, c, c(n-1), ..., c0, alpha, beta)
         (assert (string= (UCR-application-roll-type left)
                          (UCR-application-roll-type right)))
         (assert (equalp (application-arguments left)
                         (application-arguments right)))
         (make-instance 'UCR-application
                        :roll-type (UCR-application-roll-type left)
                        :parameters (append (application-parameters left)
                                            (application-parameters right))
                        :arguments (list*
                                    ;; extract the target qubit
                                    (first (application-arguments left))
                                    ;; add the new control qubit at
                                    ;; the highest order position
                                    control-qubit
                                    ;; finally, the rest of the controls
                                    (rest (application-arguments left)))))
       string0
       string1))

#+#:pedagogical-purposes-only
(defun cs-compiler (instr)
  "Performs Cosine-Sine Compilation on an instruction, emitting a list of UCR instructions that describe an equivalent circuit."
  (when (zerop (length (application-arguments instr)))
    (return-from cs-compiler (list)))
  (when (adt:match operator-description (application-operator instr)
          ((named-operator name) (find name (standard-gate-names) :test #'string=))
          (_ nil))
    (give-up-compilation))
  (let ((m (gate-matrix instr))
        (tag (generate-csc-tag)))
    (unless m
      (give-up-compilation))
    (setf m (magicl:scale m (expt (magicl:det m) (/ -1 (magicl:nrows m)))))

    ;; first, a utility function.
    (let ((n (/ (magicl:nrows m) 2)))
      (multiple-value-bind (u0 u1 v0 v1 thetas) (magicl:csd-blocks m n n)
        ;; rebalance the u and v matrices so that u0 (+) u1 and v0 (+) v1
        ;; are special unitary.
        (let ((neg-nth-root-detu
                (expt (* (magicl:det u0) (magicl:det u1))
                      (/ -0.5 n))))
          (setf u0 (magicl:scale neg-nth-root-detu u0))
          (setf u1 (magicl:scale neg-nth-root-detu u1))
          (setf v0 (magicl:scale (/ neg-nth-root-detu) v0))
          (setf v1 (magicl:scale (/ neg-nth-root-detu) v1)))
        (let* ( ;; give a name to the top qubit
               (control-qubit (first (application-arguments instr)))
               ;; compute the rebalancing values
               (Lphi (realpart (/ (log (magicl:det u0)) #C(0 1) n)))
               (Rphi (realpart (/ (log (magicl:det v0)) #C(0 1) n)))
               ;; do the left recursion
               (lstring0 (cs-compiler (make-instance 'gate-application
                                                     :operator (named-operator (format nil "CSC-U0-~D" tag))
                                                     :arguments (rest (application-arguments instr))
                                                     :gate (magicl:scale (exp (* #C(0 -1) Lphi)) u0))))
               (lstring1 (cs-compiler (make-instance 'gate-application
                                                     :operator (named-operator (format nil "CSC-U1-~D" tag))
                                                     :arguments (rest (application-arguments instr))
                                                     :gate (magicl:scale (exp (* #C(0 1) Lphi)) u1))))
               ;; zip up the left-side results
               (lstring (ucr-zipper lstring0 lstring1 control-qubit))
               ;; do the right recursion
               (rstring0 (cs-compiler (make-instance 'gate-application
                                                     :operator (named-operator (format nil "CSC-V0-~D" tag))
                                                     :arguments (rest (application-arguments instr))
                                                     :gate (magicl:scale (exp (* #C(0 -1) Rphi)) v0))))
               (rstring1 (cs-compiler (make-instance 'gate-application
                                                     :operator (named-operator (format nil "CSC-V1-~D" tag))
                                                     :arguments (rest (application-arguments instr))
                                                     :gate (magicl:scale (exp (* #C(0 1) Rphi)) v1))))
               ;; zip up the right-side results
               (rstring (ucr-zipper rstring0 rstring1 control-qubit)))
          ;; build the output circuit.
          ;; note that the order here is reversed from the matrix decomp.
          ;; since the composition order of instructions is backwards
          (nconc
           ;; the right-hand recursion results
           rstring
           (list
            ;; right Z-balancer
            (make-instance 'UCR-application
                           :roll-type "RZ"
                           :arguments (list control-qubit)
                           :parameters (list (constant (* -2 Rphi))))
            ;; central thetas
            (make-instance 'UCR-application
                           :roll-type "RY"
                           :arguments (application-arguments instr)
                           :parameters (mapcar (lambda (a) (constant (* 2 a))) thetas))
            ;; left Z-balancer
            (make-instance 'UCR-application
                           :roll-type "RZ"
                           :arguments (list control-qubit)
                           :parameters (list (constant (* -2 Lphi)))))
           ;; the left-hand recursion results
           lstring))))))
