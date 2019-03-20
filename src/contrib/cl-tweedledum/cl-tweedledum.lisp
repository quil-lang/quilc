;;;; cl-tweedledum.lisp

(in-package #:cl-tweedledum)

;; (push #P"/Users/mark.skilbeck/hackery/cl-tweedledum/" *foreign-library-directories*)
;; (define-foreign-library libtweedledum (t (:default "libtweedledum")))
;; (use-foreign-library libtweedledum)

(defcfun (%synthesis-dbs "tweedledum_synthesis_dbs")
    :string
  (perm (:pointer :uint32))
  (size :int))

(defun synthesis-dbs (permutation)
  (with-foreign-object (perm :uint32 (length permutation)) 
    (loop :for i :below (length permutation)
          :for p_i :in permutation :do
            (setf (cffi:mem-aref perm :uint32 i) p_i))
    (%synthesis-dbs perm (length permutation))))

;; (setf *perm* '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 15 14))
;; (quil::record-standard-gate "PERMGATE" (apply #'quil::make-permutation-gate "PERM0231" "" *perm*))
;; (let ((lschedule (quil::make-lscheduler)))
;;   (loop :for instr
;;           :across (quil::parsed-program-executable-code
;;                    (compiler-hook (parse-quil-string "PERMGATE 3 2 1 0")
;;                                   (quil::build-nQ-trivalent-chip 1 1 8 4 '(:cz :iswap))))
;;         :when (and (typep instr 'quil::gate-application)
;;                    (<= 2 (length (quil::application-arguments instr))))
;;           :do (quil::append-instruction-to-lschedule lschedule instr))
;;   (quil::lscheduler-calculate-depth lschedule))
;; (let ((lschedule (quil::make-lscheduler)))
;;   (loop :for instr
;;           :across (quil::parsed-program-executable-code
;;                    (compiler-hook (parse-quil-string (cl-tweedledum:synthesis-dbs *perm*))
;;                                   (quil::build-nQ-trivalent-chip 1 1 8 4 '(:cz :iswap))))
;;         :when (and (typep instr 'quil::gate-application)
;;                    (<= 2 (length (quil::application-arguments instr))))
;;           :do (quil::append-instruction-to-lschedule lschedule instr))
;;   (quil::lscheduler-calculate-depth lschedule))
