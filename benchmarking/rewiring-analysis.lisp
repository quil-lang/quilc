;;;; rewiring-analysis.lisp
;;;;
;;;; Author: Corwin de Boor
;;;;
;;;; Get information about the performance of different rewiring methods.

(in-package #:cl-quil-benchmarking)

(defun make-graph (num-nodes &rest paths)
  "Creates a graph from paths"
  (let ((adj (make-array num-nodes :initial-element nil)))
    (flet ((connect (a b)
             (push b (aref adj a))
             (push a (aref adj b))))
      (dolist (path paths)
        (map nil #'connect path (rest path)))
      (map-into adj #'delete-duplicates adj))))

(defun graph-edges (g)
  (loop
    :for a :below (length g)
    :nconc (loop :for b :in (aref g a) :when (< a b) :collect (cons a b))))

(defun init-chip (&key (architecture ':cz))
  "Initialize a chip from a given architecture with no objects"
  (let ((chip-spec (quil::make-chip-specification
                    :objects (vector (quil::make-adjustable-vector)
                                     (quil::make-adjustable-vector))
                    :generic-rewriting-rules (coerce (quil::global-rewriting-rules) 'vector))))
    (quil::install-generic-compilers chip-spec architecture)
    chip-spec))

(defun make-graph-chip (graph &key (architecture ':cz))
  "Make a chip from a graph"
  (let* ((chip-spec (init-chip :architecture architecture))
         (qubits
           (loop :for i :below (length graph) :collect (quil::build-qubit i :type '(:RZ :X/2 :MEASURE))))
         (qubit-array (make-array (length graph) :initial-contents qubits))
         (links
           (loop
             :for (a . b) :in (graph-edges graph)
             :for link-index :from 0
             :collect (quil::build-link a b :type architecture)
             :do (vector-push-extend link-index (quil::vnth 1 (quil::hardware-object-cxns (aref qubit-array a))))
             :do (vector-push-extend link-index (quil::vnth 1 (quil::hardware-object-cxns (aref qubit-array b)))))))
    (setf (quil::chip-specification-objects chip-spec)
          (make-array 2 :initial-contents (list qubit-array
                                                (coerce links 'vector))))
    (quil:warm-hardware-objects chip-spec)))

;; 0 -- 1
;; |    |
;; 2 -- 3
;; |    |
;; 4 -- 5
(defvar *chip-6q-2ring* (make-graph-chip (make-graph 6 '(0 1 3 2 0) '(3 5 4 2 3))))
(defvar *chip-0008q* (quil::build-8q-chip))
(defvar *chip-0016q* (quil::build-nq-trivalent-chip 1 1 4 8))
(defvar *chip-0020q-linear* (quil::build-nq-linear-chip 20))
(defvar *chip-0020q-skew-rect* (quil::build-skew-rectangular-chip 0 4 5))
;; We need this as symbols because we will want to iterate through
;; them at macro-expansion time and get their values dynamically.
(defparameter *rewiring-explicit-test-chips*
  '((:0006q-2ring *chip-6q-2ring*)
    (:0008q *chip-0008q*)
    (:0016q *chip-0016q*)
    (:0020q-linear *chip-0020q-linear*)
    (:0020q-skew-rect *chip-0020q-skew-rect*))
  "Explicit layouts for testing the initial rewiring performance")

;; FIXME This will correspond to the path on which the binary is
;; compiled. If this binary is e.g. the SDK then the benchmarking
;; files will not be locatable on the resident system.
(defparameter *rewiring-test-chip-directory*
  (asdf:system-relative-pathname :cl-quil "benchmarking/qpu-rewiring/"))

(defun rewiring-test-chips ()
  "Chip layouts for testing the initial rewiring performance"
  (sort
   (append
    (loop
      :for file :in (uiop:directory-files *rewiring-test-chip-directory* #P"*.qpu")
      :collect (list (intern (string-upcase (pathname-name file)) 'KEYWORD)
                     (quil::read-chip-spec-file file)))
    (loop :for (name sym) :in *rewiring-explicit-test-chips*
          :collect (list name (symbol-value sym))))
   #'<
   :key (lambda (cell)
          (quil::chip-spec-n-qubits (second cell)))))

(defparameter *rewiring-test-file-directory*
  (asdf:system-relative-pathname :cl-quil "benchmarking/quil-rewiring/"))

(defun rewiring-test-files ()
  "Test files for testing the initial rewiring performance"
  (uiop:directory-files *rewiring-test-file-directory* #P"*.quil"))

(defun quil-file-prefix (file)
  "Gets the information prefix for a quil file"
  (let* ((pp     (quil::read-quil-file file))
         (used-q (quil::prog-used-qubits pp))
         (instrs (parsed-program-executable-code pp))
         (multiq (loop
                   :for inst :across instrs
                     :thereis (and (typep inst 'application)
                                   (< 1 (length (application-arguments inst)))))))
    (format nil "~4,'0Dq-~7,'0Di-~:[s~;m~]"
            (1+ (reduce #'max used-q))
            (length instrs)
            multiq)))

(defun chip-spec-file-prefix (file)
  (let ((chip-spec (quil::read-chip-spec-file file)))
    (format nil "~4,'0Dq-~4,'0DL"
            (quil::chip-spec-n-qubits chip-spec)
            (quil::chip-spec-n-links chip-spec))))

(defmacro with-stopwatch (elapsed-var &body body)
  (let ((start-time (gensym)))
    `(let ((,start-time (get-internal-real-time)))
       (symbol-macrolet ((,elapsed-var (- (get-internal-real-time) ,start-time)))
         ,@body))))

(defun measure-performance (assignments
                            &key (progs (rewiring-test-files))
                                 (chips (rewiring-test-chips))
                                 (break-on-error t))
  (labels
      ((get-prog (prog-source)
         (typecase prog-source
           (function
            (funcall prog-source))
           (otherwise
            (read-quil-file prog-source))))

       (by-assignment (prog-source chip assn)
         (handler-bind
             ((simple-error
               (lambda (e)
                 (when break-on-error
                   (break "~A" e))
                 (return-from by-assignment (list nil nil 1)))))
           (funcall assn (lambda ()
                           (multiple-value-list (compiler-hook (get-prog prog-source) chip))))))

       (by-chip (prog-source chip)
         (loop
           :for (label assn) :on assignments :by #'cddr
           :for (compiled-program swaps duration elapsed) := (by-assignment prog-source chip assn)
           :for fidelity := (quil::calculate-instructions-fidelity (coerce (quil::parsed-program-executable-code compiled-program) 'list) chip)
           :nconc (list label (list swaps duration elapsed fidelity))))

       (by-prog (prog-source)
         (loop
           :with max-needed := (apply #'max (quil::prog-used-qubits (get-prog prog-source)))
           :for (label . chip) :in chips
           :for n-qubits := (quil::chip-spec-n-qubits chip)
           :when (< max-needed (quil::chip-spec-n-qubits chip))
             :nconc (list label (by-chip prog-source chip)))))

    (loop
      :for prog-source :in progs
      :collect (list prog-source (by-prog prog-source)))))

(defmacro make-assignments (fixed-vars changing-vars &body assignments)
  "Create a plist of functions that each act like apply with an additional scope. The scope will contain all bindings from fixed-vars in addition to a binding each variable in changing-vars to the corresponding value in the current assignment. "
  `(list
    ,@(loop
        :with fn-sym := (gensym)
        :with args-sym := (gensym)
        :for (label values) :on assignments :by #'cddr
        :do (assert (= (length changing-vars) (length values)) (values)
                    "Different number of assignment variables and values: ~A /= ~A"
                    changing-vars
                    values)
        :collect label
        :collect `(lambda (,fn-sym &rest ,args-sym)
                    (let (,@fixed-vars ,@(mapcar #'list changing-vars values))
                      (apply ,fn-sym ,args-sym))))))

(defun make-rewiring-prog (target &optional (initial (quil::make-rewiring (quil::rewiring-length target))))
  "Generate a program that forces a change from the initial rewiring to the target rewiring."
  (make-instance
   'parsed-program
   :executable-code (concatenate
                     'vector
                     ;; force start with the identity rewiring
                     (list (make-instance 'quil::application-force-rewiring :target initial))

                     ;; force end with the desired rewiring
                     (list (make-instance 'quil::application-force-rewiring :target target)))))

(defun generate-random-rewiring-prog (n-qubits state)
  (let ((*random-state* #+sbcl (sb-ext:seed-random-state state)
                        #+ecl  (make-random-state state)
                        #-(or sbcl ecl) (error "don't know how to seed random state")))
    (make-rewiring-prog (quil::generate-random-rewiring n-qubits))))

(defun measure-rewiring-swap-search (assn &rest args
                                          &key break-on-error include-runtime
                                               (rewiring-qubits 20)
                                               (chips (rewiring-test-chips)))
  (declare (ignore break-on-error include-runtime))
  (remf args :rewiring-qubits)
  (setf (getf args :chips)
        (loop :for (name chip) :in chips
              :when (= (quil::chip-spec-n-qubits chip) rewiring-qubits) :collect (cons name chip)))
  (apply 'measure-performance assn
         :progs (loop
                  :for i :below 20
                  :collect (let ((curval i))
                             (lambda () (generate-random-rewiring-prog rewiring-qubits curval))))
         args))

(defvar *basic-swap-search-assn*
    (make-assignments
        ((*random-state* #+sbcl (sb-ext:seed-random-state 1)
                         #+ecl  (make-random-state 1)
                         #-(or sbcl ecl) (error "don't know how to seed random state"))
         (quil::*compressor-passes* 0))
        (quil::*addresser-swap-search-type*
         quil::*addresser-move-to-rewiring-swap-search-type*)
      :control   (:greedy-qubit :greedy-qubit)
      :path      (:greedy-path  :greedy-path)
      :a*        (:a*           :a*)
      ))

(defvar *2q-tiers-assn*
  (make-assignments
      ((*random-state* #+sbcl (sb-ext:seed-random-state 1)
                       #+ecl  (make-random-state 1)
                       #-(or sbcl ecl) (error "don't know how to seed random state"))
       (quil::*compressor-passes* 0))
      (quil::*initial-rewiring-default-type*
       quil::*addresser-swap-search-type*
       quil::*addresser-move-to-rewiring-swap-search-type*
       quil::*addresser-a*-swap-search-heuristic-scale*
       quil::*addresser-use-2q-tiers*)
    :control (:random  :greedy-qubit :greedy-qubit 1d0 nil)
    :naive   (:naive   :greedy-qubit :greedy-qubit 1d0 nil)
    :partial (:partial :greedy-qubit :greedy-qubit 1d0 nil)
    :pa*     (:partial :a*           :a*           1d0 nil)
    :path    (:partial :greedy-path  :greedy-path  1d0 nil)
    :2q-nve  (:naive   :greedy-qubit :greedy-qubit 1d0 t)
    :2q-ptl  (:partial :greedy-qubit :greedy-qubit 1d0 t)
    :2q-pa*  (:partial :a*           :a*           1d0 t)
    :2q-path (:partial :greedy-path  :greedy-path  1d0 t)
    ))

(defvar *swap-search-assn*
  (make-assignments
      ((*random-state* #+sbcl (sb-ext:seed-random-state 1)
                       #+ecl  (make-random-state 1)
                       #-(or sbcl ecl) (error "don't know how to seed random state"))
       (quil::*compressor-passes* 0))
      (quil::*initial-rewiring-default-type*
       quil::*addresser-swap-search-type*
       quil::*addresser-move-to-rewiring-swap-search-type*
       quil::*addresser-a*-swap-search-heuristic-scale*)
    :control   (:random  :greedy-qubit :greedy-qubit 1d0)
    :path      (:partial :greedy-path  :greedy-path  1d0)
    :naive     (:naive   :greedy-qubit :greedy-qubit 1d0)
    :a-1       (:naive   :a*           :a*           1d0)
    :partial   (:partial :greedy-qubit :greedy-qubit 1d0)
    :pa-2      (:partial :a*           :a*           2d0)
    :pa-1      (:partial :a*           :a*           1d0)
    ))

(defvar *initial-rewiring-assn*
  (make-assignments
      ((*random-state* #+sbcl (sb-ext:seed-random-state 1)
                       #+ecl  (make-random-state 1)
                       #-(or sbcl ecl) (error "don't know how to seed random state"))
       (quil::*compressor-passes* 0)
       (quil::*addresser-swap-search-type* :greedy-qubit))
      (quil::*initial-rewiring-default-type*)
    :control   (:random)
    :naive     (:naive)
    :initial   (:greedy)
    :partial   (:partial)
    ))

(defvar *depth-vs-swaps-assn*
  (make-assignments
      ((*random-state* #+sbcl (sb-ext:seed-random-state 1)
                       #+ecl  (make-random-state 1)
                       #-(or sbcl ecl) (error "don't know how to seed random state"))
       (quil::*compressor-passes* 0))
      (quil::*initial-rewiring-default-type*
       quil::*addresser-swap-search-type*
       quil::*addresser-move-to-rewiring-swap-search-type*
       quil::*addresser-a*-swap-search-heuristic-scale*
       quil::*addresser-a*-distance-metric*
       )
    :partial   (:partial :greedy-qubit :greedy-qubit 1d0 nil)
    :a*-swaps  (:partial :a*           :a*           1d0 :size)
    :a*-depth  (:partial :a*           :a*           1d0 :depth)
    :a*-sum    (:partial :a*           :a*           1d0 :sum)
    ))

(defvar *cost-fn-weight-style-assn*
    (make-assignments
        ((*random-state* #+sbcl (sb-ext:seed-random-state 1)
                         #+ecl  (make-random-state 1)
                         #-(or sbcl ecl) (error "don't know how to seed random state"))
         (quil::*compressor-passes* 1))
        (quil::*cost-fn-weight-style*)
      :duration   (:duration)
      :fidelity   (:fidelity)
      ))
