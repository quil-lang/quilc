;;;; pragmas.lisp

(in-package #:cl-quil/frontend)

(define-pragma "PRESERVE_BLOCK" pragma-preserve-block
  (:documentation "PRAGMA denoting the start of a basic-block immune to the optimizing compiler.

Expected syntax: PRAGMA PRESERVE_BLOCK"))

(define-pragma "END_PRESERVE_BLOCK" pragma-end-preserve-block
  (:documentation "PRAGMA denoting the end of a basic-block immune to the optimizing compiler.

Expected syntax: PRAGMA END_PRESERVE_BLOCK"))

(define-pragma "COMMUTING_BLOCKS" pragma-commuting-blocks
  (:documentation "PRAGMA denoting the start of a COMMUTING_BLOCKS region, which contains several sub-BLOCK regions.  Used to inform the formation of logical schedules about extra commutativity relations.

Expected syntax: PRAGMA COMMUTING_BLOCKS"))

(define-pragma "END_COMMUTING_BLOCKS" pragma-end-commuting-blocks
  (:documentation "PRAGMA denoting the end of a COMMUTING_BLOCKS region."))

(define-pragma "BLOCK" pragma-block
  (:documentation "PRAGMA denoting the start of a BLOCK region, a generic child of a parent BLOCKS region (e.g., COMMUTING_BLOCKS).

Expected syntax: PRAGMA BLOCK"))

(define-pragma "END_BLOCK" pragma-end-block
  (:documentation "PRAGMA denoting the end of a BLOCK region.

Expected syntax: PRAGMA END_BLOCK"))

(define-pragma "ADD-KRAUS" pragma-add-kraus
  (:documentation
   "PRAGMA that introduces a Kraus noise channel on a fixed instruction.

Expected syntax: PRAGMA ADD-KRAUS operator qubit-list \"(matrix entries)\"")
  (:global t)
  (:slots operator-name qubit-arguments matrix-entries)
  (:words (operator string) &rest (args integer-list))
  (:freeform-string freeform-string)
  (:initialization
   (setf operator-name operator)
   (setf qubit-arguments args)
   (setf matrix-entries (parse-element-list freeform-string
                                            (expt 4 (length args)))))
  (:display-string
   (format nil "~A~{ ~A~} \"~A\""
           operator-name
           qubit-arguments
           (format-complex-list matrix-entries))))

(define-pragma "READOUT-POVM" pragma-readout-povm
  (:documentation "PRAGMA that introduces measurement noise on a particular qubit.

Expected syntax: PRAGMA READOUT-POVM qubit \"(matrix entries)\"")
  (:global t)
  (:slots qubit-index matrix-entries)
  (:words (index integer))
  (:freeform-string freeform-string)
  (:initialization
   (setf qubit-index index)
   (setf matrix-entries
         (parse-element-list freeform-string 4 'double-float)))
  (:display-string
   (format nil "~D \"~A\""
           qubit-index
           (format-complex-list matrix-entries))))

(define-pragma "EXPECTED_REWIRING" pragma-expected-rewiring
  (:documentation "PRAGMA denoting a desired logical-to-physical remapping state, typically found at the start of a control block. This has no effect on Quil code and is included only for human readability, where the human is expected to have as reference logically-specified Quil code for comparison.

Expected syntax: PRAGMA EXPECTED_REWIRING \"#(qubit permutation vector)\"")
  (:slots (rewiring rewiring))
  (:freeform-string rewiring-string)
  (:initialization
   (setf rewiring (let ((*read-eval* nil))
                    (make-rewiring-from-l2p (read-from-string rewiring-string)))))
  (:display-string
   (let ((*print-pretty* nil))
     (prin1-to-string (prin1-to-string (rewiring-l2p rewiring))))))

(define-pragma "CURRENT_REWIRING" pragma-current-rewiring
  (:documentation "PRAGMA denoting the current state of a logical-to-physical remapping, typically found at the end of a control block. This has no effect on Quil code and is included only for human readability, where the human is expected to have as reference logically-specified Quil code for comparison.

Expected syntax: PRAGMA CURRENT_REWIRING \"#(qubit permutation vector)\"")
  (:slots (rewiring rewiring))
  (:freeform-string rewiring-string)
  (:initialization
   (setf rewiring (let ((*read-eval* nil))
                    (make-rewiring-from-l2p (read-from-string rewiring-string)))))
  (:display-string
   (let ((*print-pretty* nil))
     (prin1-to-string (prin1-to-string (rewiring-l2p rewiring))))))

(define-pragma "INITIAL_REWIRING" pragma-initial-rewiring
  (:documentation "PRAGMA denoting what style of initial rewiring the compiler
  should use. This can only appear at the start of a program.

Expected syntax: PRAGMA INITIAL_REWIRING [NAIVE|PARTIAL|GREEDY|RANDOM]")
  (:global t)
  (:slots (rewiring-type (member :naive :random :partial :greedy)))
  (:freeform-string rewiring-type-string)
  (:initialization
   (setf rewiring-type
         (cond
           ((string= rewiring-type-string "NAIVE")
            ':naive)
           ((string= rewiring-type-string "RANDOM")
            ':random)
           ((string= rewiring-type-string "PARTIAL")
            ':partial)
           ((string= rewiring-type-string "GREEDY")
            ':greedy)
           (t
            (error "Invalid PRAGMA INITIAL_REWIRING: ~A" rewiring-type-string)))))
  (:display-string
   (prin1-to-string (symbol-name rewiring-type))))

(defun parsed-program-has-pragma-p (parsed-program &optional (pragma-type 'pragma))
  "Return T if PARSED-PROGRAM's executable code contains any pragma. Optionally use PRAGMA-TYPE to restrict to a particular pragma type."
  (some (a:rcurry #'typep pragma-type)
        (parsed-program-executable-code parsed-program)))

(defun parsed-program-has-preserve-blocks-p (parsed-program)
  "Return T if PARSED-PROGRAM's executable code contains a pragma of type PRAGMA-PRESERVE-BLOCK."
  (parsed-program-has-pragma-p parsed-program 'pragma-preserve-block))
