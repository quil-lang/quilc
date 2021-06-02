;;; circuit-diagram.lisp
;;;
;;; Author: Erik Davis

(in-package #:cl-quil.visualization)

;;; This file contains routines to render a subset of Quil programs as
;;; quantikz circuits (cf. https://ctan.org/pkg/quantikz).

(defparameter *impute-missing-qubits* nil
  "Include qubit lines with indices between those explicitly referenced in the Quil program.

For example, if T, the diagram for `CNOT 0 2` would have three qubit lines: 0, 1, 2.")

(defparameter *label-qubit-lines* t
  "Add labels to qubit lines.")

(defparameter *right-align-measurements* nil
  "Align measurement operations at the right of the diagram.")

(defparameter *qubit-line-open-wire-length* 1
  "The length by which qubit lines should be extended with open wires at the right of the diagram.")

(defparameter *pdflatex-exe*
  "pdflatex"
  "The 'pdflatex' executable.")

(defparameter *pdf2svg-exe*
  "pdf2svg"
  "The 'pdf2svg' executable.")


;;; quantikz commands

(defun tikz-left-ket (q)
  (format nil "\\lstick{\\ket{q_~D}}" q))

(defun tikz-control (control offset)
  (declare (ignore control))
  (format nil "\\ctrl{~D}" offset))

(defun tikz-swap (source offset)
  (declare (ignore source))
  (format nil "\\swap{~D}" offset))

(defun tikz-cnot-target ()
  "\\targ{}")

(defun tikz-cphase-target ()
  "\\control{}")

(defun tikz-swap-target ()
  "\\targX{}")

(defun tikz-measure ()
  "\\meter{}")

(defun tikz-nop ()
  "\\qw")

(defun tikz-gate (name &key (size 1) params dagger)
  ;; TeXify names
  (let* ((cl-quil::*pi-literal* "\\pi")
         (texified-name
           (format nil "~A~:[~;^{\\dagger}~]~@[(~{~/quil:instruction-fmt/~^, ~})~]"
                   (cond
                     ((string= "RX" name) "R_x")
                     ((string= "RY" name) "R_y")
                     ((string= "RZ" name) "R_z")
                     (t name))
                   dagger
                   params)))
    (format nil "\\gate[wires=~D]{~A}" size texified-name)))


;;; Diagrams

(defclass qubit-line ()
  ((length :initform 0
           :accessor qubit-line-length
           :type integer
           :documentation "The number of operations associated with this line.")
   (data :initform nil
         :accessor qubit-line-data
         :type list
         :documentation "The operations on the line, as a (reversed) list."))
  (:documentation "A line of operations for a single qubit."))

(defun push-onto-qubit-line (value line)
  "Add VALUE to the end of the qubit line LINE."
  (push value (qubit-line-data line))
  (incf (qubit-line-length line)))

(defclass circuit-diagram ()
  ((qubit-lines :initarg :qubit-lines
                :initform (make-hash-table)
                :reader qubit-lines
                :documentation "The lines of the diagram."))
  (:documentation "An abstract circuit diagram."))


(defun diagram-qubits (diagram)
  "The qubit indices associated with DIAGRAM."
  (sort (loop :for q :being :the :hash-keys :of (qubit-lines diagram)
              :collect q)
        #'<))

(defun add-qubit-line (diagram q)
  "Add a line for qubit Q in DIAGRAM."
  (when (gethash q (qubit-lines diagram))
    (error "Attempted to overwrite existing qubit line on ~D" q))
  (setf (gethash q (qubit-lines diagram))
        (make-instance 'qubit-line)))

(defun append-to-diagram (diagram q value)
  "Append VALUE to line Q of DIAGRAM."
  (unless (gethash q (qubit-lines diagram))
    (add-qubit-line diagram q))
  (push-onto-qubit-line
   value
   (gethash q (qubit-lines diagram))))

(defun extend-lines-to-common-edge (diagram qubits &optional (offset 0))
  "Advance the lines of DIAGRAM on the indicated QUBITS to a common length.

OFFSET indicates the number of additional no-operations to append to each of these lines."
  (let ((max-length
          (+ offset
             (loop :for q :in qubits
                   :for line := (gethash q (qubit-lines diagram))
                   :maximizing (qubit-line-length line)))))
    (loop :for q :in qubits
          :for line := (or (gethash q (qubit-lines diagram))
                           (add-qubit-line q diagram))
          :for length := (qubit-line-length line)
          :do (loop :repeat (- max-length length)
                    :do (push-onto-qubit-line (tikz-nop) line)))))

(defun interval-indices (diagram i j)
  "Get all qubit indices between I and J."
  (loop :with min := (min i j)
        :with max := (max i j)
        :for q :being :the :hash-keys :of (qubit-lines diagram)
        :when (<= min q max)
          :collect q))


(defvar custom-source-target-ops
  (alexandria:alist-hash-table
   (list (list "CNOT" #'tikz-control #'tikz-cnot-target)
         (list "SWAP" #'tikz-swap #'tikz-swap-target)
         (list "CZ" #'tikz-control (lambda () (tikz-gate "Z")))
         (list "CPHASE" #'tikz-control #'tikz-cphase-target))
   :test 'equal)
  "A map from gate names to constructors for their quantikz source and target operations.

The convention is that the source operation takes two arguments: the qubit index, and a numeric offset for its target. The target operation takes no arguments.")


(defun append-custom-source-target-gate (diagram name source target)
  "Update DIAGRAM with the gate NAME from SOURCE to TARGET."
  (destructuring-bind (source-op target-op)
      (gethash name custom-source-target-ops)
    (unless (and source-op target-op)
      (error "Unknown source-target operation ~A" name))
    (let* ((displaced (interval-indices diagram source target))
           (offset (if (< target source)
                       (- (1- (length displaced)))
                       (1- (length displaced)))))
      (extend-lines-to-common-edge diagram displaced)
      (append-to-diagram diagram source
                         (funcall source-op source offset))
      (append-to-diagram diagram target
                         (funcall target-op))
      (extend-lines-to-common-edge diagram displaced))))


(defgeneric render-instruction (diagram instr)
  (:documentation "Draw instruction INSTR on DIAGRAM.")
  (:method (diagram instr)
    (error "Unable to update diagram with instruction ~A" instr))
  
  (:method (diagram (instr measurement))
    (append-to-diagram diagram
                       (qubit-index (measurement-qubit instr))
                       (tikz-measure)))
  
  (:method (diagram (instr pragma))
    nil)

  (:method (diagram (instr reset))
    nil)

  (:method (diagram (instr gate-application))
    (let ((qubits (mapcar #'qubit-index
                          (application-arguments instr))))
      ;; special case: 2Q operator with special SOURCE-TARGET structure
      (adt:match operator-description (application-operator instr)
        ((named-operator name)
         (when (gethash name custom-source-target-ops)
           (destructuring-bind (source target) qubits
             (append-custom-source-target-gate diagram name source target))
           (return-from render-instruction nil)))
        (_ nil))

      ;; general case
      ;; note: DAGGER and CONTROL commute, so we can just tally counts
      (let ((dagger nil)
            (num-controls 0))
        (labels ((destruct-application (od)
                   (adt:match operator-description od
                     ((named-operator name)
                      name)
                     ((dagger-operator inner-od)
                      (setf dagger (not dagger))
                      (destruct-application inner-od))
                     ((controlled-operator inner-od)
                      (incf num-controls)
                      (destruct-application inner-od))
                     ((forked-operator _)
                      (error "LaTeX output does not currently support FORKED modifiers: ~A" instr)))))
          (let ((name
                  (destruct-application (application-operator instr))))
            (let ((control-qubits (subseq qubits 0 num-controls))
                  (target-qubits (subseq qubits num-controls)))
              (when (not (adjacent-lines-p diagram target-qubits))
                (error "Unable to render instruction ~A which targets non-adjacent qubits." instr))
              (extend-lines-to-common-edge diagram qubits)
              ;; draw controls
              (loop :for q :in control-qubits
                    :for offset := (- (first target-qubits) q)
                    :do (append-to-diagram diagram q (tikz-control q offset)))
              ;; per quantikz: we put the gate on first target line, with correct size
              (append-to-diagram diagram
                                 (first target-qubits)
                                 (tikz-gate name
                                            :size (length target-qubits)
                                            :params (application-parameters instr)
                                            :dagger dagger))
              ;; and then NOP on the rest
              (loop :for q :in (rest target-qubits)
                    :do (append-to-diagram diagram q (tikz-nop))))))))))


(defun adjacent-lines-p (diagram qubits)
  "Do the QUBITS index adjacent lines in DIAGRAM?"
  (search (sort qubits #'<)
          (diagram-qubits diagram)))


(defun build-diagram (instructions)
  "Construct a DIAGRAM from the provided INSTRUCTIONS."
  (let* ((instructions (coerce instructions 'list))
         (terminal-measurements nil)
         (diagram (make-instance 'circuit-diagram))
         (qubits
           (cl-quil::qubits-in-instr-list
            instructions)))

    (when *right-align-measurements*
      (loop :with seen-measure := nil
            :for instr :in instructions
            :if (typep instr 'measurement)
              :do (setf seen-measure t)
              :and :collect instr :into measurements
            :else
              :do (when seen-measure
                    (error "Instruction ~A follows a nonterminal MEASURE" instr))
              :and :collect instr :into gates
            :finally (setf instructions gates
                           terminal-measurements measurements)))
    
    (dolist (q qubits)
      (add-qubit-line diagram q))
    
    ;; draw left fringe
    (cond (*label-qubit-lines*
           (dolist (q qubits)
             (append-to-diagram diagram q (tikz-left-ket q))))
          (t
           (extend-lines-to-common-edge diagram qubits 1)))

    ;; handle instructions
    (dolist (instr instructions)
      (render-instruction diagram instr))

    ;; align and draw measure ops
    (when terminal-measurements
      (extend-lines-to-common-edge diagram qubits)
      (dolist (instr terminal-measurements)
        (render-instruction diagram instr)))


    (extend-lines-to-common-edge diagram
                                 qubits
                                 (max *qubit-line-open-wire-length* 0))

    diagram))


(defun print-quantikz-header (&optional stream)
  (let ((package-lines
          '("\\documentclass[convert={density=300,outext=.png}]{standalone}"
            "\\usepackage[margin=1in]{geometry}"
            "\\usepackage{tikz}"
            "\\usepackage{quantikz}"))
        (init-lines
          '("\\begin{document}"
            "\\begin{tikzcd}")))
    (format stream "~{~A~%~}~%~{~A~%~}"
            package-lines init-lines)))

(defun print-quantikz-footer (&optional stream)
  (let ((footer-lines
          '("\\end{tikzcd}"
            "\\end{document}")))
    (format stream "~{~A~%~}"
            footer-lines)))


(defun print-quantikz-diagram (diagram &optional stream)
  (loop :for q :in (diagram-qubits diagram)
        :for line := (gethash q (qubit-lines diagram))
        :do (format stream "~{ ~A~^ & ~} \\\\~%"
                    (reverse (qubit-line-data line)))))


(defun print-parsed-program-as-quantikz (pp &optional (stream *standard-output*))
  (let ((diagram
          (build-diagram (parsed-program-executable-code pp))))    
    (print-quantikz-header stream)
    (print-quantikz-diagram diagram stream)
    (print-quantikz-footer stream)))


;;; entry point


(defun plot-circuit-diagram (pp &key svg-file
                                  (label-qubit-lines *label-qubit-lines*)
                                  (right-align-measurements *right-align-measurements*)
                                  (qubit-line-open-wire-length *qubit-line-open-wire-length*))
  "Plot a parsed program PP as a circuit diagram. 

Returns a JUPYTER:SVG value, which may be rendered by a Jupyter notebook.

Keyword Arguments:
  * SVG-FILE: if not null, then output is saved here.
  * LABEL-QUBIT-LINES: If T, then the qubit lines will be labeled with qubit indices.
  * RIGHT-ALIGN-MEASUREMENTS: If T, attempt to align all measurements at the right of the diagram.
  * QUBIT-LINE-OPEN-WIRE-LENGTH: The amount of extra space to attach to qubit lines, at the right."
  (let ((*label-qubit-lines* label-qubit-lines)
        (*right-align-measurements* right-align-measurements)
        (*qubit-line-open-wire-length* qubit-line-open-wire-length))
    (uiop:with-current-directory ((uiop:temporary-directory))    
      (uiop:with-temporary-file (:stream tex-stream :pathname tex-file :directory (uiop:temporary-directory))
        (flet ((filename-by-extension (ext)
                 (namestring (merge-pathnames (make-pathname :type ext) tex-file))))
          (print-parsed-program-as-quantikz pp tex-stream)
          (finish-output tex-stream)
          (let ((%pdf-file (filename-by-extension "pdf")) ; generated by pdflatex
                (%aux-file (filename-by-extension "aux")) ; generated by pdflatex
                (%log-file (filename-by-extension "log")) ; generated by pdflatex
                (%svg-file (or svg-file (filename-by-extension "svg"))))
            (unwind-protect
                 (progn
                   (uiop:run-program (list *pdflatex-exe* "-halt-on-error" (namestring tex-file)))
                   (assert (uiop:file-exists-p %pdf-file))
                   (uiop:run-program (list *pdf2svg-exe*
                                           %pdf-file
                                           %svg-file))
                   (jupyter:svg
                    (alexandria:read-file-into-string %svg-file)))
              (dolist (file (list %aux-file %log-file %pdf-file))
                (uiop:delete-file-if-exists file))
              (unless svg-file
                (uiop:delete-file-if-exists %svg-file)))))))))
