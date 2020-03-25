(in-package :cl-quil)

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

(defmethod select-swaps-for-rewiring ((search-type (eql ':greedy-qubit)) rewiring target-rewiring addresser-state rewirings-tried)
  (push (copy-rewiring rewiring) rewirings-tried)
  (with-slots (chip-spec qq-distances) addresser-state
    (flet ((cost-function (rewiring &key instr gate-weights)
             (declare (ignore instr gate-weights))
             (rewiring-distance rewiring target-rewiring qq-distances)))
      (let ((link-index (select-cost-lowering-swap rewiring chip-spec #'cost-function rewirings-tried)))
        (values (list link-index) rewirings-tried)))))

(defmethod select-swaps-for-gates ((search-type (eql ':greedy-qubit)) rewiring gates-in-waiting addresser-state rewirings-tried)
  (flet ((cost-function (rewiring &key instr (gate-weights gates-in-waiting))
           (let ((modified-state (copy-instance addresser-state)))
             (setf (addresser-state-working-l2p modified-state) rewiring)
             (cost-function modified-state
                            :gate-weights gate-weights
                            :instr instr))))
    (push (copy-rewiring rewiring) rewirings-tried)
    (let ((link-index
            (select-cost-lowering-swap rewiring
                                       (addresser-state-chip-specification addresser-state)
                                       #'cost-function
                                       rewirings-tried)))
      (values (list link-index) rewirings-tried))))
