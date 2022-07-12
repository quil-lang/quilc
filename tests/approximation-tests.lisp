;;;; approximation-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-tests)


(deftest test-approximate-compilation ()
  (let* ((chip (cl-quil::build-nq-linear-chip 3 :architecture ':cz))
         (fidelity-hash (a:plist-hash-table (list "fCZ" 0.50d0
                                                  "f1QRB" 0.99d0)
                                            :test #'equalp))
         (pp (cl-quil::parse-quil "CPHASE(pi/4) 0 1"))
         (m-in (cl-quil::make-matrix-from-quil (coerce (cl-quil::parsed-program-executable-code pp) 'list)))
         cpp m-out)
    (loop
      :for obj :across (cl-quil::vnth 1 (cl-quil::chip-specification-objects chip))
      :do (setf (gethash "specs" (cl-quil::hardware-object-misc-data obj)) fidelity-hash))
    (let ((cl-quil::*enable-approximate-compilation* t))
      (setf cpp (cl-quil::compiler-hook pp chip)))
    ;; check: results are approximately correct
    (setf m-out (cl-quil::make-matrix-from-quil (coerce (cl-quil::parsed-program-executable-code cpp) 'list)))
    (is (< 0.95d0 (cl-quil::trace-distance m-in m-out)))
    ;; check: reduction actually happened
    (is (> 13 (length (cl-quil::parsed-program-executable-code cpp))))))

(deftest test-fidelity-calculation-agreement ()
  (let* ((quil (list (cl-quil::build-gate "ISWAP" () 0 1)
                     (cl-quil::build-gate "RY" '(#.(/ pi 4)) 0)
                     (cl-quil::build-gate "RY" '(#.(/ pi 4)) 1)
                     (cl-quil::build-gate "CPHASE" '(#.pi) 0 1)))
         (swap-gate (cl-quil::build-gate "SWAP" () 0 1))
         (m (cl-quil::make-matrix-from-quil quil))
         (mprime (cl-quil::make-matrix-from-quil (list swap-gate))))
    (multiple-value-bind (a d b) (cl-quil::orthogonal-decomposition m)
      (multiple-value-bind (aprime dprime bprime) (cl-quil::orthogonal-decomposition mprime)
        (declare (ignore aprime bprime))
        (let* ((coord-d (cl-quil::get-canonical-coords-from-diagonal d))
               (coord-dprime (cl-quil::get-canonical-coords-from-diagonal dprime))
               (fast-distance (cl-quil::fidelity-coord-distance 
                               coord-d
                               coord-dprime)))
          (multiple-value-bind (circ slow-nearness)
              (cl-quil::sandwich-with-local-gates (list swap-gate) a d b 1 0)
            (declare (ignore circ))
            (is (cl-quil::double= fast-distance (- 1 slow-nearness)))))))))
