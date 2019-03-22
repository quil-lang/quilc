;;;; approximation-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-tests)


(deftest test-approximate-compilation ()
  (let* ((chip (quil::build-nq-linear-chip 3 :architecture ':cz))
         (fidelity-hash (alexandria:plist-hash-table (list "fCZ"   0.50d0
                                                           "f1QRB" 0.99d0)
                                                     :test #'equalp))
         (pp (quil::parse-quil-string "CPHASE(pi/4) 0 1"))
         (m-in (quil::make-matrix-from-quil (coerce (quil::parsed-program-executable-code pp) 'list)))
         cpp m-out)
    (loop
       :for obj :across (quil::vnth 1 (quil::chip-specification-objects chip))
       :do (setf (gethash "specs" (quil::hardware-object-misc-data obj)) fidelity-hash))
    (let ((quil::*enable-approximate-compilation* t))
      (setf cpp (quil::compiler-hook pp chip)))
    ;; check: results are approximately correct
    (setf m-out (quil::make-matrix-from-quil (coerce (quil::parsed-program-executable-code cpp) 'list)))
    (is (< 0.95d0 (quil::trace-distance m-in m-out)))
    ;; check: reduction actually happened
    (is (> 13 (length (quil::parsed-program-executable-code cpp))))))

(deftest test-fidelity-calculation-agreement ()
  (let* ((quil (list (quil::build-gate "ISWAP" () 0 1)
                     (quil::build-gate "RY" '(#.(/ pi 4)) 0)
                     (quil::build-gate "RY" '(#.(/ pi 4)) 1)
                     (quil::build-gate "CPHASE" '(#.pi) 0 1)))
         (swap-gate (quil::build-gate "SWAP" () 0 1))
         (m (quil::make-matrix-from-quil quil))
         (mprime (quil::make-matrix-from-quil (list swap-gate))))
    (multiple-value-bind (a d b) (quil::orthogonal-decomposition m)
      (multiple-value-bind (aprime dprime bprime) (quil::orthogonal-decomposition mprime)
        (declare (ignore aprime bprime))
        (let* ((coord-d (quil::get-canonical-coords-from-diagonal d))
               (coord-dprime (quil::get-canonical-coords-from-diagonal dprime))
               (fast-distance (quil::fidelity-coord-distance 
                               coord-d
                               coord-dprime)))
          (multiple-value-bind (circ slow-nearness)
              (quil::sandwich-with-local-gates (list swap-gate) a d b 1 0)
            (declare (ignore circ))
            (is (quil::double= fast-distance (- 1 slow-nearness)))))))))
