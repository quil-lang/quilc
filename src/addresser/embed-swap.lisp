;;;; embed-swap.lisp
;;;;
;;;; Author: Eric Peterson, Erik Davis

(in-package #:cl-quil)

;;; This file contains routines related to the selection and insertion of a
;;; SWAP into a schedule, usually as part of migrating an active addresser
;;; instance from one state to another.

(defvar *addresser-move-to-rewiring-swap-search-type* :a*
  "The type of swap search the addresser should use when doing move-to-rewiring.")

;;; See *ADDRESSER-SWAP-SEARCH-TYPE* as well.


;;; This stuff is all at the service of SELECT-AND-EMBED-A-PERMUTATION below.
;;;
;;; Also, these need to know about COST-FUNCTION, but otherwise they're independent
;;; of the addresser-state business, which is why they live in a separate file.

(defun rewiring-distance (rewiring target-rewiring qq-distances)
  "A measure of the distance between a given REWIRING and a TARGET-REWIRING, based on
the qubit-qubit distance array QQ-DISTANCES."
  (loop :for i :across (rewiring-l2p rewiring)
        :for j :across (rewiring-l2p target-rewiring)
        :when i
          :sum (aref qq-distances i j)))

(defun cost-lowering-candidates (rewiring cost-function rewirings-tried chip-spec depth)
  "Given a rewiring and a cost function, returns a list of swap links for which
the cost of the rewiring is reduced."
  (let ((best-cost-so-far nil)
        (potential-first-links nil))
    (labels ((depth-first-traversal (depth topmost-link)
               (when (plusp depth)
                 (let ((links-to-search
                         (if topmost-link
                             (chip-spec-adj-links chip-spec topmost-link)
                             (a:iota (chip-spec-n-links chip-spec)))))
                   (dolist (link-index links-to-search)
                     (let ((topmost-link (or topmost-link link-index))
                           (swapped-qubits (chip-spec-qubits-on-link chip-spec link-index)))
                       (with-update-rewiring rewiring (aref swapped-qubits 0) (aref swapped-qubits 1)
                         ;; make sure we haven't been here before
                         (unless (member rewiring rewirings-tried :test #'equalp)
                           ;; compute the cost for this rewiring
                           (let ((new-cost (funcall cost-function rewiring)))
                             (cond
                               ((and best-cost-so-far
                                     (cost-= new-cost best-cost-so-far))
                                (push topmost-link potential-first-links))
                               ((or (not best-cost-so-far)
                                    (cost-< new-cost best-cost-so-far))
                                (setf best-cost-so-far new-cost)
                                (setf potential-first-links (list topmost-link)))
                               (t nil)))
                           ;; recurse on SWAP chains of one lower length
                           (depth-first-traversal (1- depth) topmost-link)))))))))
      (depth-first-traversal depth nil)
      (assert (not (endp potential-first-links)))
      (remove-duplicates potential-first-links :test #'=))))

(defun select-cost-lowering-swap (rewiring chip-spec cost-function rewirings-tried
                                  &optional
                                    (depth *addresser-swap-lookahead-depth*))
  "Seaches for a 'SWAP' instruction that lowers the objective COST-FUNCTION. Returns such an
instruction if it exists, and errors otherwise."
  (format-noise "SELECT-COST-LOWERING-SWAP: Entrance.")
  (let* ((best-cost-so-far nil)
         (potential-first-links (cost-lowering-candidates rewiring
                                                          cost-function
                                                          rewirings-tried
                                                          chip-spec
                                                          depth))
         (link-index (first potential-first-links)))
    (dolist (index potential-first-links)
      (let* ((swapped-qubits (chip-spec-qubits-on-link chip-spec index)))
        (with-update-rewiring rewiring (aref swapped-qubits 0) (aref swapped-qubits 1)
          ;; compute the new cost value
          ;; TODO Maybe fill in the rewiring? When is it better?
          ;;
          ;; cost-function expects its instruction to be
          ;; logically-addressed, and will then use the provided
          ;; rewiring to map back to physical addresses before
          ;; nativizing the instruction.
          (a:when-let* ((control (apply-rewiring-p2l rewiring (aref swapped-qubits 0)))
                        (target (apply-rewiring-p2l rewiring (aref swapped-qubits 1)))
                        (new-cost
                         (funcall cost-function rewiring
                                  :instr (build-gate "SWAP" () control target))))
            ;; TODO: this assumes only SWAPs exist in the permutation list
            (when (or (null best-cost-so-far) ; we have to make progress.
                      (cost-< new-cost best-cost-so-far))
              (setf best-cost-so-far new-cost)
              (setf link-index index))))))
    ;; if we have a nil swap, the greedy scheduler has failed to operate. scary!
    (assert link-index
            nil
            "Failed to select a SWAP instruction. This can be caused by a disconnected qubit graph, a program with a lot of symmetry, or even random chance. You might simply try again, or you might try requesting a different addressing strategy.")
    (format-noise
     "SELECT-COST-LOWERING-SWAP: SWAP ~d ~d is best, lowering cost from ~d to ~d."
     (vnth 0 (chip-spec-qubits-on-link chip-spec link-index))
     (vnth 1 (chip-spec-qubits-on-link chip-spec link-index))
     (funcall cost-function rewiring)
     best-cost-so-far)
    link-index))

(defun move-to-expected-rewiring (rewiring target-rewiring addresser-state
                                  &key
                                    (use-free-swaps nil)
                                    (rewirings-tried nil))
  "This function inserts the necessary SWAP instructions to move from the working logical-to-physical rewiring REWIRING to the TARGET-REWIRING."
  (with-slots (qq-distances chip-spec chip-sched initial-l2p) addresser-state
    (flet ((done-rewiring (rewiring)
             (zerop (rewiring-distance rewiring target-rewiring qq-distances)))
           (update-rewiring (rewiring)
             (loop
               :for logical :from 0
               :for physical :across (rewiring-l2p target-rewiring)
               :unless (apply-rewiring-l2p rewiring logical)
                 :do (rewiring-assign rewiring logical physical))))
      (loop :do (format-noise "MOVE-TO-EXPECTED-REWIRING: Moving~%~a~%~a" rewiring target-rewiring)
            :until (done-rewiring rewiring)
            :do (assert (> *addresser-max-swap-sequence-length* (length rewirings-tried)) ()
                        "Too many rewirings tried: ~a" (length rewirings-tried))
            :do (let ((links (select-swaps-for-rewiring
                              *addresser-move-to-rewiring-swap-search-type*
                              rewiring target-rewiring addresser-state rewirings-tried)))
                  (dolist (link-index links)
                    (embed-swap link-index
                                initial-l2p
                                rewiring
                                chip-spec
                                chip-sched
                                :use-free-swaps use-free-swaps)))
            :finally (update-rewiring rewiring)))))

(defmethod select-swaps-for-rewiring ((search-type (eql ':greedy-qubit)) rewiring target-rewiring addresser-state rewirings-tried)
  (push (copy-rewiring rewiring) rewirings-tried)
  (with-slots (chip-spec qq-distances) addresser-state
    (flet ((cost-function (rewiring &key instr gate-weights)
             (declare (ignore instr gate-weights))
             (rewiring-distance rewiring target-rewiring qq-distances)))
      (let ((link-index (select-cost-lowering-swap rewiring chip-spec #'cost-function rewirings-tried)))
        (values (list link-index) rewirings-tried)))))

(defmethod select-swaps-for-rewiring ((search-type (eql ':a*)) rewiring target-rewiring addresser-state rewirings-tried)
  (with-slots (chip-spec chip-sched qq-distances) addresser-state
    (flet ((cost-function (rewiring &key instr gate-weights)
             (declare (ignore instr gate-weights))
             (rewiring-distance rewiring target-rewiring qq-distances))
           (done-moving (rewiring)
             (zerop (rewiring-distance rewiring target-rewiring qq-distances))))
      (values (search-rewiring chip-spec rewiring
                               (chip-schedule-qubit-times chip-sched)
                               #'cost-function
                               #'done-moving
                               :max-iterations *addresser-a*-swap-search-max-iterations*)
              rewirings-tried))))

(defun embed-swap (link-index initial-l2p working-l2p chip-spec chip-sched &key use-free-swaps)
  "Safely insert a SWAP selected by LINK-INDEX into CHIP-SCHED, accounting for the possibility of virtualization."
  ;; we now insert the SWAP selected by LINK-INDEX.
  (destructuring-bind (q0 q1) (coerce (chip-spec-qubits-on-link chip-spec link-index) 'list)
    ;; can we make it a virtual SWAP?
    (cond
     ((and use-free-swaps
           (zerop (chip-schedule-resource-end-time
                   chip-sched
                   (make-qubit-resource q0 q1))))
      ;; yes, we can. apply the link swap to initial-l2p and to working-l2p
      (update-rewiring initial-l2p q0 q1)
      (update-rewiring working-l2p q0 q1)
      (format-noise
       "EMBED-SWAP: This is a free swap. :)~%~
        EMBED-SWAP: New rewiring: ~a~%~
        EMBED-SWAP: New initial rewiring: ~a"
       working-l2p initial-l2p))
     (t
      ;; in this case, this swap has to be performed by the QPU.
      ;; apply the link swap to working-l2p
      (update-rewiring working-l2p q0 q1)
      (format-noise "EMBED-SWAP: New rewiring: ~a" working-l2p)
      ;; insert the relevant 2q instruction
      (chip-schedule-append chip-sched (build-gate "SWAP" '() q0 q1))))))
