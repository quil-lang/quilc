;;;; ucr-recognize.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

;; NOTE: the loops in this function do exactly twice as much work as is needed,
;; but rewriting them to do minimal work makes the loop structure nastier.
(define-compiler recognize-ucr ((instr
                                 :where (anonymous-gate-application-p instr)))
  "Checks whether an anonymous gate is a UCRY or UCRZ instruction, in which case it relabels it as such."
  (let* ((matrix (handler-case (gate-matrix instr)
                   (unknown-gate-parameter (c)
                     (declare (ignore c))
                     (give-up-compilation))))
         (dimension (magicl:nrows matrix))
         (log-dimension (length (application-arguments instr)))
         angles)
    (cond
      ;; are we a diagonal matrix?
      ((loop :for i :below (magicl:nrows matrix)
             :always (double= 1d0 (abs (magicl:tref matrix i i))))
       ;; if so, we are potentially of the form UCRZ.  the extra check
       ;; we need to do is to see if the matrix has the required extra
       ;; symmetry: there has to be a fixed target bit d about which
       ;; the matrix satisfies
       ;;
       ;;     j'  = j & !(2^d),
       ;;     j'' = j | 2^d,
       ;;     m_j'j' = conj(m_j''j'').
       ;;
       ;; in this case, the angles are given by (phase ...).
       (loop :for d :below log-dimension
             :do (setf angles (make-list (/ dimension 2)))
                 (when (loop :for j :below dimension
                             :always (let* ((jp  (dpb 0 (byte 1 d) j))
                                            (jpp (dpb 1 (byte 1 d) j))
                                            (i (+ (mod jp (ash 1 d))
                                                  (ash (- jp (mod jp (ash 1 d))) -1))))
                                       (setf (nth i angles)
                                             (constant (* -2 (phase (magicl:tref matrix jp jp)))))
                                       (double= (magicl:tref matrix jp jp)
                                                (conjugate (magicl:tref matrix jpp jpp)))))
                   (inst* (repeatedly-fork (named-operator "RZ") (1- log-dimension))
                          angles
                          (append (subseq (application-arguments instr)
                                          0
                                          (- log-dimension d 1))
                                  (subseq (application-arguments instr)
                                          (- log-dimension d))
                                  (list (nth (- log-dimension d 1)
                                             (application-arguments instr)))))
                   (finish-compiler)))
       (give-up-compilation))
      ;; are we a UCRY matrix? these have three salient properties:
      ;;
      ;;  (1) they are completely real
      ;;  (2) each column and row has only two nonzero entries
      ;;  (3) these nonzero entries arrange into squares of width 2^d for some d:
      ;;      setting j' and j'' as before, the nonzero entries have the form
      ;;          m_j'j' = m_j''j'' and m_j'j'' = -m_j''j' .
      ;;
      ;; in this case, the angles are given by (atan ...).
      (t
       (loop :for d :below log-dimension
             :do (setf angles (make-list (/ dimension 2)))
                 (when (loop :for j :below dimension
                             :always (let* ((jp  (dpb 0 (byte 1 d) j))
                                            (jpp (dpb 1 (byte 1 d) j))
                                            (m-jp-jp (magicl:tref matrix jp jp))
                                            (m-jpp-jp (magicl:tref matrix jpp jp))
                                            (m-jp-jpp (magicl:tref matrix jp jpp))
                                            (m-jpp-jpp (magicl:tref matrix jpp jpp))
                                            (i (+ (mod jp (ash 1 d))
                                                  (ash (- jp (mod jp (ash 1 d))) -1))))
                                       (setf (nth i angles)
                                             (constant (* 2 (atan (realpart m-jpp-jp)
                                                                  (realpart m-jp-jp)))))
                                       (and (double= 1d0 (+ (* m-jp-jp m-jp-jp)
                                                            (* m-jp-jpp m-jp-jpp)))
                                            (double= 1d0 (+ (* m-jpp-jp m-jpp-jp)
                                                            (* m-jpp-jpp m-jpp-jpp)))
                                            (double= m-jp-jp (realpart m-jp-jp))
                                            (double= m-jp-jpp (realpart m-jp-jpp))
                                            (double= m-jpp-jp (realpart m-jpp-jp))
                                            (double= m-jpp-jpp (realpart m-jpp-jpp))
                                            (double= m-jp-jp m-jpp-jpp)
                                            (double= m-jp-jpp (- m-jpp-jp)))))
		   (inst* (repeatedly-fork (named-operator "RY") (1- log-dimension))
			  angles
			  (append (subseq (application-arguments instr)
					  0
					  (- log-dimension d 1))
				  (subseq (application-arguments instr)
					  (- log-dimension d))
				  (list (nth (- log-dimension d 1)
                                             (application-arguments instr)))))
		   (finish-compiler)))
       (give-up-compilation)))))
