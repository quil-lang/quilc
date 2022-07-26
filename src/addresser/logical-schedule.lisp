;;;; logical-schedule.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

;;; This file implements the LOGICAL-SCHEDULER data structure, which is used by the addresser
;;; to organize and manage resource dependencies between instructions on 'logical' qubits.
;;;
;;; Quil instructions rely on some mixture of classical (memory regions) and
;;; quantum (qubits) resources. In the general case, it is these resource dependencies,
;;; rather than the strict linearization of straight line quil, which dictate the sequencing of
;;; physical instructions. For example, in the program
;;;
;;;   X 0          (i)
;;;   H 3          (ii)
;;;   CNOT 0 1     (iii)
;;;   X 3          (iv)
;;;   CNOT 1 3     (v)
;;;
;;; the instructions (iii) and (iv) could have equivalently been transposed. The
;;; partial ordering imposed by resource constraints is
;;;
;;;            ---> CNOT 0 1 ---> X 0
;;;           /
;;;   CNOT 1 3
;;;           \
;;;            -----> X 3 ------> H 3
;;;
;;; where the arrow A ---> B means that A logically follows B.
;;;
;;; The logical scheduler (defined below) holds a set of instructions and their
;;; resource dependencies. In particular, it maintain a list of 'first' or 'top'
;;; instructions (corresponding to the right fringe of the above diagram, i.e. X
;;; 0 and H 3), a set of 'last' or 'bottom' instructions (corresponding to the
;;; left fringe, i.e. CNOT 1 3), as well as hash tables storing information on
;;; the dependencies. It works almost as a queue: instructions may be added to
;;; the left fringe via APPEND-INSTRUCTIONS-TO-LSCHEDULE, and may be removed
;;; from the right fringe by LSCHEDULER-DEQUEUE-INSTRUCTION.

(defgeneric instruction-resources (instr)
  (:documentation "Returns the resources used by INSTR."))

;;;
;;; this pseudoinstruction is how COMMUTING_BLOCKS regions communicate with the
;;; addresser loop.
;;;

(defclass application-thread-invocation (application)
  ((thread :initarg :thread
           :accessor application-thread-invocation-thread
           :documentation "The thread referenced by this invocation pseudoinstruction.")
   (region :initarg :region
           :accessor application-thread-invocation-region
           :documentation "The region referenced by this invocation pseudoinstruction.")
   (resources :initarg :resources
              :accessor instruction-resources
              :documentation "The resources manipulated in this region."))
  (:documentation "Pseudo-instruction that captures a BLOCK from a COMMUTING_BLOCKS region. Carries an instruction sequence as a compilation payload."))

;;;
;;; some utility functions that assign different instruction types to their
;;; respective collections of qubit/classical resources
;;;

(defun address-resources (address)
  (typecase address
    ;; I wish this could be made more delicate, so that only the SHARING region
    ;; were marked as used, but without knowing the widths of all the classical
    ;; memory primitives, this isn't possible.
    (memory-descriptor
     (if (memory-descriptor-sharing-parent address)
         (address-resources (memory-descriptor-sharing-parent address))
         (make-resource-range (memory-descriptor-name address)
                              0
                              (memory-descriptor-length address))))
    (memory-ref
     (make-resource-range (memory-ref-name address)
                          (memory-ref-position address)
                          (1+ (memory-ref-position address))))
    (memory-name
     (address-resources (memory-name-descriptor address)))
    (otherwise
     (make-null-resource))))

(defun qubit-resources (qubit)
  (make-qubit-resource (qubit-index qubit)))

(defgeneric parameter-resources (param)
  (:method ((param param))
    (make-null-resource))
  (:method ((param constant))
    (make-null-resource))
  (:method ((param memory-ref))
    (address-resources param))
  (:method ((param memory-name))
    (instruction-resources param))
  (:method ((param memory-offset))
    (make-null-resource))
  (:method ((param delayed-expression))
    (parameter-resources (delayed-expression-expression param)))
  (:method ((param number))
    (make-null-resource))
  (:method ((param cons))
    (reduce #'resource-union
            (mapcar #'parameter-resources (rest param)))))

(defmethod instruction-resources ((inst unary-classical-instruction))
  (address-resources (classical-target inst)))

(defmethod instruction-resources ((inst binary-classical-instruction))
  (resource-union (address-resources (classical-left-operand inst))
                  (address-resources (classical-right-operand inst))))

(defmethod instruction-resources ((inst trinary-classical-instruction))
  (reduce #'resource-union
          (list (address-resources (classical-left-operand inst))
                (address-resources (classical-right-operand inst))
                (address-resources (classical-target inst)))))

(defmethod instruction-resources ((inst measurement))
  (qubit-resources (measurement-qubit inst)))

(defmethod instruction-resources ((inst measure))
  (resource-union (call-next-method)
                  (address-resources (measure-address inst))))

(defmethod instruction-resources ((inst application))
  (let ((param-res (if (application-parameters inst)
                       (reduce #'resource-union (application-parameters inst) :key #'parameter-resources)
                       (make-null-resource)))
        (arg-res (if (application-arguments inst)
                     (reduce #'resource-union (application-arguments inst) :key #'qubit-resources)
                     (make-null-resource))))
    (cond
      ((not (or (resource-null-p param-res) (resource-null-p arg-res)))
       (resource-union param-res arg-res))
      ((not (resource-null-p param-res))
       param-res)
      ((not (resource-null-p arg-res))
       arg-res)
      (t
       (make-null-resource)))))

(defmethod instruction-resources ((inst instruction))
  (if (global-instruction-p inst) (make-all-resource) (make-null-resource)))

(defmethod instruction-resources ((inst reset-qubit))
  (qubit-resources (reset-qubit-target inst)))

;; the addresser sorts instructions in a different way than the CLOS hierarchy
(defun local-classical-instruction-p (instr)
  (or (typep instr 'unary-classical-instruction)
      (typep instr 'binary-classical-instruction)
      (typep instr 'trinary-classical-instruction)))

(defun local-classical-quantum-instruction-p (instr)
  (or (typep instr 'measure)))

(defun global-instruction-p (instr)
  (or (typep instr 'halt)
      (typep instr 'reset)
      (typep instr 'wait)
      (typep instr 'jump)
      (typep instr 'pragma-expected-rewiring)))

(defun instruction-resources-intersect-p (instr1 instr2)
  (resources-intersect-p (instruction-resources instr1)
                         (instruction-resources instr2)))

;;;
;;; the core logical scheduler class and some of its utilities
;;;

(defclass logical-scheduler ()
  ((first-instrs :initform nil
                 :accessor lscheduler-first-instrs
                 :documentation "List of the instructions appearing at the \"top\" of a logical scheduler.  Excepting COMMUTING_BLOCKS threads, these instructions are guaranteed to occupy disjoint collections of resources.")
   (last-instrs :initform nil
                :accessor lscheduler-last-instrs
                :documentation "List of the instructions appearing at the \"bottom\" of a logical scheduler.  These are sorted topologically ascending: earlier items in the list come logically after deeper items in the list.")
   (later-instrs :initform (make-instr-hash-table)
                 :accessor lscheduler-later-instrs
                 :documentation "Hash table mapping instruction to a list of instructions after it.")
   (earlier-instrs :initform (make-instr-hash-table)
                   :accessor lscheduler-earlier-instrs
                   :documentation "Hash table mapping instruction to a list of instructions before it."))
  (:documentation "Data structure used to track the logical precedence of instructions in a straight-line Quil program."))

(defun make-instr-hash-table (&optional size)
  "Make a hash table for which each key is an instruction (instr),
i.e., an EQ hash table. If SIZE is optionally given, it's passed along
as the size keyword arg to make-hash-table."
  (if size 
      (make-hash-table :test #'eq :size size)
      (make-hash-table :test #'eq)))

(defun make-lscheduler ()
  (make-instance 'logical-scheduler))

(defun print-lschedule (lschedule &optional (stream *standard-output*))
  (format stream "First instructions:~%")
  (dolist (instr (lscheduler-first-instrs lschedule))
    (format stream "    ~/cl-quil:instruction-fmt/~%" instr))
  (format stream "Last instructions:~%")
  (dolist (instr (lscheduler-last-instrs lschedule))
    (format stream "    ~/cl-quil:instruction-fmt/~%" instr))
  (dohash ((key val) (lscheduler-later-instrs lschedule))
    (format stream "Instrs beneath ~/cl-quil:instruction-fmt/: ~{~/cl-quil:instruction-fmt/~^, ~}~%"
            key
            val))
  (dohash ((key val) (lscheduler-earlier-instrs lschedule))
    (format stream "Instrs above ~/cl-quil:instruction-fmt/: ~{~/cl-quil:instruction-fmt/~^, ~}~%"
            key
            val)))

;;;
;;; routines for incrementally forming a logical-scheduler object
;;;

(defun lscheduler-clean-up-last-instrs (lschedule)
  "Removes instructions from (lscheduler-later-instructions LSCHEDULE) whose resources are fully covered by preceding instructions in the list."
  (let ((new-bottommost nil)
        (traversed-instructions nil)
        (traversed-resources (make-null-resource)))
    (dolist (instr (lscheduler-last-instrs lschedule))
      (let ((resources (instruction-resources instr)))
        (cond
          ;; in the global case, we're definitely done
          ((resource-all-p traversed-resources)
           (return))

          ((resource-all-p resources)
           (push instr new-bottommost)
           (return))

          ;; in the thread case, we have to recompute the blocking resources to
          ;; exclude other threads from the same region
          ((typep instr 'application-thread-invocation)
           (let ((limited-traversed-resources (make-null-resource)))
             (dolist (other-instr traversed-instructions)
               (unless (and (typep other-instr 'application-thread-invocation)
                            (equal (application-thread-invocation-region other-instr)
                                   (application-thread-invocation-region instr)))
                 (setf limited-traversed-resources
                       (resource-union limited-traversed-resources
                                       (instruction-resources other-instr)))))
             (cond
               ((resource-subsetp resources limited-traversed-resources)
                nil)
               (t
                (setf traversed-resources (resource-union traversed-resources resources))
                (push instr new-bottommost)))))
          ;; if we're already covered up, skip this instruction
          ((resource-subsetp resources traversed-resources)
           nil)
          ;; otherwise, this instruction contributes something new to the cover
          (t
           (setf traversed-resources (resource-union traversed-resources resources))
           (push instr new-bottommost)))
        (push instr traversed-instructions)))
    (setf (lscheduler-last-instrs lschedule) (nreverse new-bottommost))))

(defun append-instruction-to-lschedule (lschedule instr)
  ;; if we touch anything in the old bottom, we come after it.
  (let ((resources (instruction-resources instr)))
    (cond
      ((resource-all-p resources)
       (dolist (bottom-instr (lscheduler-last-instrs lschedule))
         (push bottom-instr (gethash instr (lscheduler-earlier-instrs lschedule)))
         (push instr (gethash bottom-instr (lscheduler-later-instrs lschedule)))))
      (t
       (dolist (bottom-instr (lscheduler-last-instrs lschedule))
         (let ((bottom-resources (instruction-resources bottom-instr)))
           (when (resources-intersect-p resources bottom-resources)
             (push bottom-instr (gethash instr (lscheduler-earlier-instrs lschedule)))
             (push instr (gethash bottom-instr (lscheduler-later-instrs lschedule)))))))))
  ;; are we a topmost instr?
  (unless (gethash instr (lscheduler-earlier-instrs lschedule))
    (push instr (lscheduler-first-instrs lschedule)))
  (push instr (lscheduler-last-instrs lschedule))
  (lscheduler-clean-up-last-instrs lschedule))

(defun consume-commuting-blocks-region (lschedule instrs)
  "Helper function for APPEND-INSTRUCTIONS-TO-LSCHEDULE, called when the top instruction of INSTRS is an instance of PRAGMA COMMUTING_BLOCKS. Consumes items from INSTRS until PRAGMA END_COMMUTING_BLOCKS is encountered, and inserts the intervening BLOCKs into LSCHEDULE as instances of APPLICATION-THREAD-INVOCATION."
  (labels ((process-blocks-into-lists (instrs acc)
             (cond
               ((endp instrs)
                (error "Unexpected end inside of PRAGMA COMMUTING_BLOCKS."))
               ((typep (first instrs) 'pragma-commuting-blocks)
                nil)
               ((typep (first instrs) 'pragma-end-commuting-blocks)
                (return-from process-blocks-into-lists
                  (values acc (rest instrs))))
               ((typep (first instrs) 'pragma-block)
                (push nil acc))
               ((typep (first instrs) 'pragma-end-block)
                (check-type acc cons)
                (setf (first acc) (nreverse (first acc))))
               ((typep acc 'cons)
                (push (first instrs) (first acc)))
               (t
                (error "No matching PRAGMA END_COMMUTING_BLOCKS found.")))
             (process-blocks-into-lists (rest instrs) acc)))
    (multiple-value-bind (blocks rest) (process-blocks-into-lists instrs nil)
      (let ((comm-blocks-layer-bottom nil)
            (comm-blocks-layer-top nil))
        ;; process blocks into pseudoinstructions
        (dolist (block blocks)
          (let* ((resources (make-null-resource))
                 (arguments (remove-duplicates (apply #'append
                                                      (mapcar #'application-arguments block))
                                               :test #'equalp))
                 (pseudoinstruction (make-instance 'application-thread-invocation
                                                   :region (first instrs)
                                                   :thread block
                                                   :operator #.(named-operator "THREAD")
                                                   :arguments arguments)))
            (dolist (instr block)
              (setf resources (resource-union resources (instruction-resources instr))))
            (assert (not (resource-all-p resources)) ()
                    "Unsupported: global instruction encountered inside of COMMUTING_BLOCKS.")
            (setf (instruction-resources pseudoinstruction) resources)
            (push pseudoinstruction comm-blocks-layer-bottom)
            (when (notany (lambda (i) (instruction-resources-intersect-p i pseudoinstruction))
                          (lscheduler-last-instrs lschedule))
              (push pseudoinstruction comm-blocks-layer-top))))
        ;; merge the old top layer with the new top layer
        (setf (lscheduler-first-instrs lschedule)
              (append comm-blocks-layer-top
                      (loop :for instr :in (lscheduler-first-instrs lschedule)
                            :when (notany (lambda (i) (instruction-resources-intersect-p i instr))
                                          comm-blocks-layer-top)
                              :collect instr)))
        ;; merge the old bottom layer with the new bottom layer
        (let ((all-resources (make-null-resource))
              (new-bottom-layer comm-blocks-layer-bottom))
          (dolist (instr comm-blocks-layer-bottom)
            (setf all-resources (resource-union all-resources (instruction-resources instr)))
            (dolist (old-bottom-instr (lscheduler-last-instrs lschedule))
              (when (instruction-resources-intersect-p instr old-bottom-instr)
                (push instr (gethash old-bottom-instr (lscheduler-later-instrs lschedule)))
                (push old-bottom-instr (gethash instr (lscheduler-earlier-instrs lschedule))))))
          (dolist (instr (lscheduler-last-instrs lschedule))
            (unless (resource-subsetp (instruction-resources instr) all-resources)
              (push instr new-bottom-layer)))
          (setf (lscheduler-last-instrs lschedule) new-bottom-layer)))
      ;; return the current state
      (values lschedule rest))))

(defun append-instructions-to-lschedule (lschedule instrs)
  (cond
    ((endp instrs)
     lschedule)
    ((typep (first instrs) 'pragma-commuting-blocks)
     (multiple-value-bind (lschedule instrs)
         (consume-commuting-blocks-region lschedule instrs)
       (append-instructions-to-lschedule lschedule instrs)))
    (t
     (append-instruction-to-lschedule lschedule (first instrs))
     (append-instructions-to-lschedule lschedule (rest instrs)))))

(defun lscheduler-dequeue-instruction (lschedule instr)
  "Removes INSTR from the top of LSCHEDULE."
  ;; remove us from the topmost and bottommost
  (setf (lscheduler-first-instrs lschedule)
        (remove instr (lscheduler-first-instrs lschedule)))
  (setf (lscheduler-last-instrs lschedule)
        (remove instr (lscheduler-last-instrs lschedule)))
  ;; for each later instruction
  (dolist (later-instr (gethash instr (lscheduler-later-instrs lschedule)))
    ;; remove the upward link
    (let ((stripped-uplinks (remove instr
                                    (gethash later-instr
                                             (lscheduler-earlier-instrs lschedule)))))
      (cond
        ((endp stripped-uplinks)
         (remhash later-instr (lscheduler-earlier-instrs lschedule)))
        (t
         (setf (gethash later-instr (lscheduler-earlier-instrs lschedule))
               stripped-uplinks))))
    ;; if there are no other upward links
    (unless (gethash later-instr (lscheduler-earlier-instrs lschedule))
      ;; YTMND
      (push later-instr (lscheduler-first-instrs lschedule))))
  ;; clear the later instructions
  (remhash instr (lscheduler-later-instrs lschedule)))

(defun lscheduler-topmost-instructions (lschedule)
  (lscheduler-first-instrs lschedule))

(defun lscheduler-replace-instruction (lschedule instr new-sequence)
  "Replaces a topmost instruction INSTR in LSCHEDULE with a nonempty NEW-SEQUENCE of instructions.

NOTE: Assumes that the resource utilization of NEW-SEQUENCE is no more than that of INSTR.
NOTE: Assumes no nested COMMUTING_BLOCKS regions."
  ;; special behavior: if NEW-SEQUENCE contains just a NOP, then this method
  ;; should behave identically to lscheduler-dequeue-instruction
  (when (or (zerop (length new-sequence))
            (and (= 1 (length new-sequence))
                 (typep (first new-sequence) 'no-operation)))
    (return-from lscheduler-replace-instruction
      (lscheduler-dequeue-instruction lschedule instr)))
  ;; push instr above all its peers in the topmost instrs
  (dolist (top-instr (lscheduler-first-instrs lschedule))
    (when (and (not (equal instr top-instr))
               (instruction-resources-intersect-p instr top-instr))
      (push instr (gethash top-instr (lscheduler-earlier-instrs lschedule)))
      (push top-instr (gethash instr (lscheduler-later-instrs lschedule)))
      (setf (lscheduler-first-instrs lschedule)
            (remove top-instr (lscheduler-first-instrs lschedule)))))
  ;; make a new lschedule out of the incoming sequence
  (let ((new-lschedule (make-lscheduler)))
    (append-instructions-to-lschedule new-lschedule new-sequence)
    ;; sew together the new lschedule and the old one along instr
    (dohash ((key val) (lscheduler-earlier-instrs new-lschedule))
             (setf (gethash key (lscheduler-earlier-instrs lschedule)) val))
    (dohash ((key val) (lscheduler-later-instrs new-lschedule))
             (setf (gethash key (lscheduler-later-instrs lschedule)) val))
    (setf (lscheduler-first-instrs lschedule)
          (append (lscheduler-first-instrs new-lschedule)
                  (loop :for i :in (lscheduler-first-instrs lschedule)
                        :unless (instruction-resources-intersect-p i instr)
                          :collect i)))
    (loop :for i :in (lscheduler-last-instrs new-lschedule)
          :do (push instr (gethash i (lscheduler-later-instrs lschedule)))
              (push i (gethash instr (lscheduler-earlier-instrs lschedule))))
    ;; patch the up/down links through the old instruction
    (dolist (earlier-instr (gethash instr (lscheduler-earlier-instrs lschedule)))
      (dolist (later-instr (gethash instr (lscheduler-later-instrs lschedule)))
        (when (instruction-resources-intersect-p earlier-instr later-instr)
          (push earlier-instr (gethash later-instr (lscheduler-earlier-instrs lschedule)))
          (push later-instr (gethash earlier-instr (lscheduler-later-instrs lschedule))))))
    ;; unlink earlier instructions from instr
    (dolist (earlier-instr (gethash instr (lscheduler-earlier-instrs lschedule)))
      (setf (gethash earlier-instr (lscheduler-later-instrs lschedule))
            (remove instr (gethash earlier-instr (lscheduler-later-instrs lschedule)))))
    ;; unlink later instructions from instr
    (dolist (later-instr (gethash instr (lscheduler-later-instrs lschedule)))
      (let ((punctured-seq (remove instr (gethash later-instr
                                                  (lscheduler-earlier-instrs lschedule)))))
        (setf (gethash later-instr (lscheduler-earlier-instrs lschedule))
              punctured-seq)
        ;; prevent anyone from getting orphaned
        (when (endp punctured-seq)
          (push later-instr (lscheduler-first-instrs lschedule)))))
    ;; remove the old instruction from lschedule
    (remhash instr (lscheduler-later-instrs lschedule))
    (remhash instr (lscheduler-earlier-instrs lschedule))
    (setf (lscheduler-last-instrs lschedule)
          (remove instr (lscheduler-last-instrs lschedule)))
    ;; see if any sequence instrs need to be added to the bottommost list
    (dolist (instr new-sequence)
      (unless (gethash instr (lscheduler-later-instrs lschedule))
        (push instr (lscheduler-last-instrs lschedule))))))

(defun lschedule-splice-1q-instruction (lschedule lo-inst new-inst hi-inst)
  "Insert the 1Q NEW-INST between LO-INST and HI-INST.

NEW-INST must use only a single individual RESOURCE. LO-INST and HI-INST must
also both use this resource (not necessarily only that one).
If LO-INST is NIL, then HI-INST should be a first instruction.
If HI-INST is NIL, then LO-INST should be a last instruction.
If both are non-NIL, then LO-INST should be the last predecessor of HI-INST to
use RESOURCE."
  (with-slots (earlier-instrs later-instrs last-instrs first-instrs) lschedule
    (cond
      ((and lo-inst hi-inst)
       ;; connect after
       (setf (gethash hi-inst earlier-instrs) (substitute new-inst lo-inst (gethash hi-inst earlier-instrs)))
       (push hi-inst (gethash new-inst later-instrs))
       ;; connect before
       (setf (gethash lo-inst later-instrs)   (substitute new-inst hi-inst (gethash lo-inst later-instrs)))
       (push lo-inst (gethash new-inst earlier-instrs)))
      (lo-inst
       ;; connect before
       (push new-inst (gethash lo-inst later-instrs))
       (push lo-inst (gethash new-inst earlier-instrs))
       ;; connect after
       (push new-inst last-instrs)
       (lscheduler-clean-up-last-instrs lschedule))
      (hi-inst
       ;; connect after
       (push new-inst (gethash hi-inst earlier-instrs))
       (push hi-inst (gethash new-inst later-instrs))
       ;; connect before
       (a:removef first-instrs hi-inst)
       (push new-inst first-instrs))
      (t
       ;; connect before
       (push new-inst first-instrs)
       ;; connect after
       (push new-inst last-instrs)
       (lscheduler-clean-up-last-instrs lschedule)))))

;;;
;;; read-only statistical routines for logical-scheduler objects
;;;



(defun map-in-reverse-topological-order (adjacencies start-nodes function)
  "For a DAG in hash-table ADJACENCIES and a list of START-NODES, call
   FUNCTION on every node in reverse topological order. Nodes are
   assumed to be instruction (instr) instances."
  ;; This runs a depth-first search with an explicit stack as opposed
  ;; to using a recursive function. A recursive function might be
  ;; clearer and even a bit faster, but we use the iterative approach
  ;; because the function calling stack is typically limited in our
  ;; Lisp implementations (e.g., SBCL), and this may be called with
  ;; very large graphs (many 100,000's of nodes).
  (let* ((visitations (make-instr-hash-table (hash-table-count adjacencies)))
         (topo-stack start-nodes)
         node)
    (loop :until (null topo-stack)
          :do (setq node (first topo-stack))
              (let* ((visitation (gethash node visitations))
                     ;; visitation: nil => never visited; T => visited
                     ;; and now done w/chilren; or cons (list of
                     ;; unvisited children)
                     (children
                       (cond
                         ((null visitation)
                          (let ((children? (gethash node adjacencies)))
                            (setf (gethash node visitations)
                                  (or children? t))
                            children?))
                         ((consp visitation) visitation)
                         (t             ; i.e., visitation is T
                          '()))))
                (loop :while children
                      :do (let ((child (pop children)))
                            (when (null (gethash child visitations))
                              ;; fix up node's entry in visitations,
                              ;; to be children, if non-nil, else T:
                              (setf (gethash node visitations)
                                    (or children t))
                              ;; push child for further processing
                              ;; (we're not done with node)
                              (push child topo-stack)
                              (return)))
                      :finally
                         ;; done w/every child of node
                         (funcall function node)
                         (pop topo-stack))))))


;;; About lscheduler-absolutely-latest-instrs in comparison with
;;; lscheduler-last-instrs:
;;; 
;;;   lscheduler-last-instrs is a list such that, for any resource
;;;   used, the last instruction on that resource is in the list.
;;;
;;;   lscheduler-absolutely-latest-instrs is a list such that no
;;;   instructions in it have any later instructions
;;;
;;; There is currently (Sept 2021) some uncertainty about whether this
;;; should remain the case; see issue
;;; https://github.com/quil-lang/quilc/issues/728. So for now, work
;;; around this by using lscheduler-absolutely-latest-instrs when you
;;; need the true last instrs, i.e., the terminals of the DAG. In
;;; particular, therefore, lscheduler-absolutely-latest-instrs is
;;; supplied as start-nodes arg of map-in-reverse-topological-order.

(defun lscheduler-absolutely-latest-instrs (lschedule)
  (let ((last-instrs (lscheduler-last-instrs lschedule))
        (laters (lscheduler-later-instrs lschedule)))
    (if (loop :for instr :in (lscheduler-last-instrs lschedule)
                :thereis (gethash instr laters))
        (loop :for instr :in (lscheduler-last-instrs lschedule)
              :unless (gethash instr laters)
                :collect instr)
        ;; Otherwise, last-instrs is just fine to return.
        last-instrs)))

(defun map-lschedule-in-topological-order (lschedule function)
  "Call FUNCTION on every instr in LSCHEDULE in topological order."
  ;; Here's the idea: LSCHEDULE holds a directed acyclic graph (DAG)
  ;; with start nodes being the value of
  ;; (lscheduler-absolutely-latest-instrs LSCHEDULE) and adjacency
  ;; mappings in its earlier-instrs slot. We can call this DAG a
  ;; "reverse DAG" as it is the reverse of the DAG with starts and
  ;; adjacency mappings in first-instrs and later-instrs slots,
  ;; respectively.  Thus, by mapping in reverse topological order on
  ;; the "reverse DAG", we are actually mapping in topological order
  ;; on the DAG. We could have simply created a topologically sorted
  ;; list and then mapcar'd on it, but when no list is required per
  ;; se, using this avoids one extra pass over the list as well as
  ;; saving the space and time required to cons up the list and later
  ;; GC it.
  (map-in-reverse-topological-order
   (lscheduler-earlier-instrs lschedule)
   (lscheduler-absolutely-latest-instrs lschedule)
   function))


(defun topo-list-lschedule (lschedule)
  "Return a list of the instructions of LSCHEDULE in topological order."
  (let ((result-list '()))
    (map-lschedule-in-topological-order
     lschedule #'(lambda (instr) (push instr result-list)))
    result-list))

;; The above is not currently used but perhaps could be one day. Note:
;; consider as an optimization storing topo lists on lschedules so
;; they could be used directly, e.g., in the walker below. It could be
;; cached and, for most operations, be easily maintained incrementally
;; in constant time. Also consider storing the exact count of
;; instructions so that it would not need to be computed. The size is
;; passed to make-hash-table as size to avoid need to rehash.


(defun lscheduler-walk-graph (lschedule
                              &key
                                (base-value 0)
                                (bump-value (lambda (instr value)
                                              (declare (ignore instr))
                                              (1+ value)))
                                (combine-values #'max))
  "Walks the edges in the directed, acyclic graph underlying LSCHEDULE in topological order.

All instructions begin with a value of BASE-VALUE. When we visit an instruction INSTR, we first compute SOURCE-BUMP, which is obtained by applying COMBINE-VALUES to the values associated with all instructions with a directed edge to INSTR. Finally, INSTR's value is overwritten by (BUMP-VALUE INSTR SOURCE-BUMP).

Returns the reduction of all bumped values by COMBINE-VALUES, and a hash table mapping instructions to their values. "
  (let* ((max-distance base-value)
         (laters (lscheduler-later-instrs lschedule))
         (distances (make-instr-hash-table (hash-table-count laters))))
    ;; Initialize distances of start instrs to base-value, leaving
    ;; distances of other instrs nil.
    (dolist (instr (lscheduler-first-instrs lschedule))
      (setf (gethash instr distances) base-value))
    ;; Process instrs in the topological order.
    (map-lschedule-in-topological-order
     lschedule
     #'(lambda (instr)
         (let* ((d (gethash instr distances))
                (bumped-value (funcall bump-value instr d)))
           (setq max-distance
                 (funcall combine-values max-distance bumped-value))
           (loop :for later :in (gethash instr laters)
                 :as later-d := (gethash later distances)
                 :do (setf (gethash later distances)
                           (if (null later-d)
                               bumped-value
                               (funcall
                                combine-values bumped-value later-d)))))))
    (values max-distance distances)))

(defun lscheduler-calculate-duration (lschedule chip-spec)
  (flet ((duration-bumper (instr value)
           (multiple-value-bind (order address obj)
               (lookup-hardware-address chip-spec instr)
             (declare (ignore order address))
             (let (duration)
               (when obj
                 (setf duration (hardware-object-native-instruction-p obj instr)))
               (if duration
                   (+ duration value)
                   value)))))
    (or (lscheduler-walk-graph lschedule
                               :base-value 0
                               :bump-value #'duration-bumper
                               :combine-values #'max)
        0)))

(defun lscheduler-calculate-depth (lschedule)
  (or (lscheduler-walk-graph lschedule
                             :base-value 0
                             :bump-value (lambda (instr value)
                                           (declare (ignore instr))
                                           (1+ value))
                             :combine-values #'max)
      0))

(defun lscheduler-calculate-volume (lschedule)
  "Compute the count of instructions in LSCHEDULE."
  (+ (length (lscheduler-topmost-instructions lschedule))
     (hash-table-count (lscheduler-earlier-instrs lschedule))))

(defun lscheduler-calculate-log-fidelity (lschedule chip-spec)
  (labels
      ((get-fidelity (instr)
         (labels ((warn-and-skip (instr)
                    (format-noise "Unknown fidelity for ~/cl-quil::instruction-fmt/. Skipping." instr)
                    (return-from get-fidelity 0d0)))
           (let (fidelity)
             (typecase instr
               (measurement
                (let* ((qubit-index (qubit-index (measurement-qubit instr)))
                       (qubit-obj (chip-spec-nth-qubit chip-spec qubit-index))
                       (specs-obj (gethash (make-measure-binding :qubit qubit-index :target '_)
                                           (hardware-object-gate-information qubit-obj)))
                       (measure-fidelity (and specs-obj (gate-record-fidelity specs-obj))))
                  (unless specs-obj
                    (warn-and-skip instr))
                  (setf fidelity measure-fidelity)))
               (application
                (let ((obj (lookup-hardware-object chip-spec instr)))
                  (unless obj
                    (warn-and-skip instr))
                  (let ((specs-hash (hardware-object-gate-information obj)))
                    (unless specs-hash (warn-and-skip instr))
                    (when (> (hash-table-count specs-hash) 0)
                      (let ((binding (binding-from-instr instr)))
                        (dohash ((key val) specs-hash)
                          (when (binding-subsumes-p key binding)
                            (setf fidelity (gate-record-fidelity val))))))
                    (unless fidelity (warn-and-skip instr)))))
               (otherwise
                (warn-and-skip instr)))
             (expt (log fidelity) 2)))))
    (let ((running-fidelity 0d0))
      (dolist (instr (lscheduler-first-instrs lschedule))
        (unless (gethash instr (lscheduler-earlier-instrs lschedule))
          (incf running-fidelity (get-fidelity instr))))
      (maphash (lambda (instr val)
                 (declare (ignore val))
                 (incf running-fidelity (get-fidelity instr)))
               (lscheduler-earlier-instrs lschedule))
      (sqrt running-fidelity))))

(defun lscheduler-calculate-fidelity (lschedule chip-spec)
  "Calculate fidelity as the minimum fidelity of the individual instructions.

  This relies on the fact that the function $\exp\{-\sqrt{\log(x)^2 + \log(y)^2}\}$ is approximately equal to $\min\{x, y\}$ for $x, y \in (0, 1]$."
  (multiple-value-bind (max-value value-hash)
      (lscheduler-calculate-log-fidelity lschedule chip-spec)
    (values (exp (- max-value)) value-hash)))

(defun lscheduler-all-instructions (lschedule)
  "Return a list of the instructions of LSCHEDULE."
  (let* ((laters (lscheduler-later-instrs lschedule))
         (result-list (a:hash-table-keys laters)))
    ;; Last instrs may not be in laters, so add here.
    (dolist (instr (lscheduler-last-instrs lschedule))
      (unless (gethash instr laters)
        (push instr result-list)))
    result-list))
    
