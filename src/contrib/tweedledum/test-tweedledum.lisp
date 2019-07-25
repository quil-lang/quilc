(in-package #:cl-quil.tweedledum)

(defun random-ucr (qubits)
  (let* ((n-forks (1- (length qubits)))
         (params (loop :repeat (expt 2 n-forks) :collect (random 1.0d0))))
    (loop :for operator := (quil::named-operator "RZ")
            :then (quil::forked-operator operator)
          :repeat n-forks
          :finally
             (return (apply #'quil::build-gate operator params qubits)))))

(defun scaled-matrix-from-ucr (ucr)
  (let ((m (quil::make-matrix-from-quil (list ucr))))
    (magicl:scale (/ (magicl:ref m 0 0)) m)))

(defun nativize-ucr (ucr chip)
  (quil::expand-to-native-instructions (list ucr) chip))

(defun parsed-program-from-instruction-list (instrs)
  (quil:parse-quil
   (with-output-to-string (*standard-output*)
     (loop :for instr :in instrs :do
       (format t "~/cl-quil::instruction-fmt/~%" instr)))))

(defun test-tweedledum-diagonal (qubits)
  (let* ((ucr (random-ucr qubits))
         (m (scaled-matrix-from-ucr ucr))
         (chip (quil::build-nq-fully-connected-chip (length qubits))))
    (let ((default-pp (parsed-program-from-instruction-list (nativize-ucr ucr chip)))
          (tweedle-pp (parsed-program-from-instruction-list (compile-diagonal-gate-with-tweedledum m))))
      (let ((default-comp (quil::compiler-hook default-pp chip))
            (tweedle-comp (quil::compiler-hook tweedle-pp chip)))
        (print (quil::matrix-equals-dwim
                (quil::parsed-program-to-logical-matrix default-comp :compress-qubits t)
                (quil::parsed-program-to-logical-matrix tweedle-comp :compress-qubits t)))
        (list
         :quilc (length (quil::parsed-program-executable-code default-pp))
         :tweed (length (quil::parsed-program-executable-code tweedle-pp))
         :quilc-compiled (length (quil::parsed-program-executable-code default-comp))
         :tweed-compiled (length (quil::parsed-program-executable-code tweedle-comp)))))))


