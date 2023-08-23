;;;; backward-match.lisp
;;;;
;;;; Author: Brennen Hill

(in-package #:cl-quil.match)

;;;; backward-match finds all possible matches from nodes that
;;;; commutted to the left of the starting node

;; Returns a list of possible nodes to be matched next from the
;; pattern, which are unmatched, unblocked, and have a greater label
;; than yet are not successors to the starting node from the pattern
(defun find-backward-candidates (pat-canon pat-start-node scenario)
  ;;Search every node in the pattern for possible canddidates that meet the following criteria
  (loop :for node :across (nodes pat-canon)
        :unless (or 
                 (matched-p node scenario) ;Nodes must be unmatched
                 (blocked-p node scenario) ;Nodes must be unblocked
                 ;;Nodes must have a greater label than yet are not successors to the starting node from the pattern
                 (< (label node) (label pat-start-node))
                 (find node (succs  pat-start-node)
                       :test (lambda (node1 node2)
                               (= (label node1) (label node2)))))
          :collect node into backward-candidates
        ;;Sort the backward candidates in descending order
        :finally (setf backward-candidates (sort backward-candidates (lambda (node1 node2)
                                                                       (> (label node1) (label node2)))))
                 (return backward-candidates)))

;; Returns a list of the sets of matches with the highest cost, as
;; derived from their gates
(defun get-best-matches (match-sequences circ-canon)
  "Finds the match sequence with the highest cost"
  (loop :for sequence :in match-sequences
        ;;Calculate the total cost of the match sequence
        :for sequence-cost = (gate-cost (loop :for match :in sequence
                                                   :collect (gate (get-node circ-canon (first match)))))
        ;;If this sequence has the greatest cost yet found, clear the best-sequences already found and update the highest cost
        :when (> sequence-cost highest-cost)
          :do (setf best-sequences NIL)
        :maximize sequence-cost :into highest-cost
        ;;Collect this sequence if it has a cost equal to the highest-cost
        :when (= sequence-cost highest-cost)
          :collect sequence :into best-sequences
        :finally (return best-sequences))) ;;Return the sequences found that had the highest cost

;; Builds a tree of possible matches. For each node in the pattern not
;; matched by forward-match, tries matching with each possible node in
;; the circuit, left-blocking by blocking predecessors, and
;; right-blocking by blocking successors
(defun backward-match (circ-canon pat-canon pat-start-node initial-scenario)
  (let ((matchable-nodes '()) ;List of the labels of the cirucit that are not blocked or already matched with, sorted in descending order
        (num-nodes-left-to-match ;The maximum number of nodes that can be matched in backward-match
          (- (length (nodes pat-canon)) (label pat-start-node) (length (matches initial-scenario))))
        (matching-scenarios (list initial-scenario)) ;A stack of branched off matching scenarios to work through
        (forward-matches (copy-list (matches initial-scenario))) ;The list of matches found in forward-match
        (matched-sequences ;The list of all sequences of matches found from the given starting nodes in the circuit and pattern
          (list (matches initial-scenario)))) ;The first matched-sequence found was from forward-match
    
    ;;Block all successors of the starting node, as we are matching in the backward direction
    (loop :for node :in (succs pat-start-node)
          :do (blocked node initial-scenario))
    
    ;;Build a sorted list of the labels of the matchable nodes in the circuit
    (setf matchable-nodes
          (loop :for node :across (nodes circ-canon)
                ;;Collect the nodes that are not blocked or matched
                :unless (or
                         (blocked-p node initial-scenario)
                         (matched-p node initial-scenario))
                  :collect node into collected-nodes
                ;;Sort the collected nodes in descending order by label
                :finally (return 
                           (make-array (length collected-nodes)
                                       :initial-contents (sort collected-nodes
                                                               #'> :key (lambda (node) (label node)))))))
    
    ;;The while loop that loops through and progresses each matching scenario
    (tagbody next-iteration
       (when matching-scenarios
         (let* ((scenario (pop matching-scenarios)) ;The current scenario, including the list of matches and which nodes are blocked or matched
                (next-node NIL) ;The next node in the circuit to consider for matching
                (back-matches ;A list of matches in the matching scenario that were found in the backward direction, so not found in forward-match
                  (set-difference (matches scenario) forward-matches
                                  :test (lambda (match1 match2)
                                          (and (= (first match1) (first match2))
                                               (= (second match1) (second match2))))))
                (always-removed-match T) ;Boolean that keeps track of whether backwardMatch has always had to remove any previously made matches
                                         ;Used in Case 2 to see if Case 2.2C can be reached
                (matched-candidates '())) ;A list of nodes in the pattern that have matched with the next-node

           ;;The following labels represent checks and cases that are used in both case 2 and case 3, so are stored to avoid repetition
           (labels
               ;;Check whether a node can be blocked without blocking other nodes
               ((simple-block-p ()
                  (or
                   (null (preds next-node)) ;If the node has no predecessors, it can be left blocked easily
                   (not (find-if (lambda (succ) ;If the node has no matched successors, it can be right-blocked easily
                                   (matched-p succ scenario))
                                 (succs next-node)))))

                ;;Check if any fixed matches have been blocked, being matches already made in backward-match and the initial match
                (blocked-fixed-p (scenario)
                  (let ((result 
                          (or (blocked-p pat-start-node scenario)
                              (not (loop :for back-match :in back-matches
                                         :always (find-if (lambda (match)
                                                            (and (= (first match) (first back-match))
                                                                 (= (second match) (second back-match))))
                                                          (matches scenario)))))))
                    result))

                ;;Right block a node by blocking its successors and unmatching them if needed
                (right-block ()
                  (let ((scenario-copy (next-scenario scenario NIL)))
                    ;;Block and unmatch all successors to next-node
                    (loop :for succ :in (succs next-node)
                          :do (blocked succ scenario-copy)
                              (unmatched succ scenario-copy))
                    ;;As long as we didn't block any fixed matches, this is a valid option and we push the scenario to be progressed
                    (unless (blocked-fixed-p scenario-copy)
                      (push (next-scenario scenario-copy)  matching-scenarios))))

                ;;Left block a node by blocking its predecessors
                (left-block ()
                  (loop :for pred :in (preds next-node)
                        :do (blocked pred scenario))
                  (push (next-scenario scenario) matching-scenarios))) ;Push the scenario to be progressed
             
             #|||   CASE 1: Trivial Cases   |||
             Go through the cases that are simple|#
             ;;If the counter is past the last node or the number of backward matches made is equal to the available backward matches,
             ;;then add the matches found as a set.
             ;;Note that this is the only case where a sequence of matches is added to the list of matchSeqences
             (when (or
                    (equalp (length matchable-nodes) (counter scenario))
                    (equalp (length back-matches) num-nodes-left-to-match))
               (when (matches scenario)
                 (push (matches scenario) matched-sequences)) ;Only add the sequence of matches if it is non-empty
               (go next-iteration))
             
             ;;We can now safely create next-node, as the first trivial case would skip this iteration if it was invalid
             (setf next-node (svref matchable-nodes (counter scenario)))
             
             ;;If the node is blocked, skip it and create a new scenario with the following node
             (when (blocked-p next-node scenario)
               (push (next-scenario scenario) matching-scenarios)
               (go next-iteration))
             
             #|||   CASE 2: Try to Match, Otherwise Block   |||
             Try to match the gate corresponding to the next-node from the circuit with the gate from a node in the pattern
             If we cannot match the gate, then it is blocked|#
             ;;Build a list of nodes from the pattern that form valid matches with the next-node in the circuit
             (setf matched-candidates
                   (loop :with back-candidates = (find-backward-candidates pat-canon pat-start-node scenario)
                         :for candidate :in back-candidates
                         :when (and
                                (gate-matched-p next-node candidate) ; The gates are a match
                                (not (find-if (lambda (node) ;Ensure no two matched nodes have gates that can commute and lead to duplicate matches
                                                (gates-commute-p node candidate))
                                              matched)))
                           :collect candidate :into matched
                         :finally (return matched)))
             
             (cond (matched-candidates
                    #|||   CASE 2.1: Match|||
                    Possible matches for the next node have been found and stored in matched-candidates
                    We match the next node and add the result to the stack of matching scenarios|#
                    ;;Create a new scenario for each matched node to progress each option
                    (loop :for matched-candidate :in matched-candidates
                          :do (let ((scenario-copy (next-scenario scenario NIL)) ;Duplicate of the current scenario to explore this branch
                                    ;;Keep track of whether a match was removed during this iteration, to see if Case 2.2C can be reached
                                    (removed-match-this-iteration NIL))
                                
                        ;;Block all successors of the matched node and then all successors of all blocked nodes in the pattern
                        (loop :for succ-of-candidate :in (succs matched-candidate)
                              :unless (matched-p succ-of-candidate scenario-copy)
                                :do (blocked succ-of-candidate scenario-copy)
                                    (loop :for succ-of-blocked :in (succs succ-of-candidate)
                                          :do (blocked succ-of-blocked scenario-copy)
                                              ;;Unmatch any nodes that have been blocked
                                              (setf removed-match-this-iteration
                                                    (or (unmatched succ-of-blocked scenario-copy)
                                                        removed-match-this-iteration))))
                                
                        ;;Update whether a match was removed, to determine whether case 2.2C can be reached
                        (unless removed-match-this-iteration
                          (setf always-removed-match NIL))
                                
                        ;;Only progress this scenario if we haev not blocked a fixed match
                        (unless (blocked-fixed-p scenario-copy)
                          ;; If this point is reached, no nodes that were previously matched in backward-match nor starting node were blocked
                          ;; and there is a succesful match between the next node and the potential matching node
                          ;; Note that this is the only time when a match is added to a sequence of matches in backwardMatch
                          (matched next-node matched-candidate scenario-copy)
                          ;;Progress this scenario
                          (push (next-scenario scenario-copy) matching-scenarios))))
                    
                    #|||   CASE 2.2: Block|||
                    We try blocking the next-node|#
                    (blocked next-node scenario)
                    
                    (cond ((simple-block-p)
                           #|||   Case 2.2A: Simple Block   |||
                           The node has no predecessors, or none of its successors are matches
                           We have blocked the next-node without interfering with previously matched nodes and can progress the scenario|#
                           (push (next-scenario scenario) matching-scenarios))                      

                          (T
                           #|||   Case 2.2B: Right Block   |||
                           Blocking the next-node requires unmatching some of the previously matched node
                           We first try right blocking next-node|#
                           (right-block)

                           (when always-removed-match
                             #|||   CASE 2.2C Left Block   |||
                             Where we left block the next-node
                             This option will only be taken if matching the next-node and blocking the next-node without
                             blocking previously matched nodes is impossible|#
                             (left-block)))))
                         
                   (T
                    #|||   CASE 3.1: Block the Unmatchable   |||
                    We only block in this case
                    No matches were found for next-node, so we block it
                    The options to consider will be similiar to Case 2.2|#
                    (blocked next-node scenario)

                    (cond
                      ((simple-block-p)
                       #|||   CASE 3.1A: Simple Block   |||
                       The node has no predecessors, or none of its successors are matches
                       We have blocked the next-node without interfering with previously matched nodes and can progress the scenario|#
                       (push (next-scenario scenario) matching-scenarios))                      

                      (T
                       #|||   CASE 3.1B: Right Block   |||
                       Blocking the next-node requires unmatching some of the previously matched nodes
                       We first try right blocking next-node|#
                       (right-block)

                       #|||   CASE 3.1C: Left Block   |||
                       Where we left block the nextNode
                       Unlike when leftblocking may be undergone in 2.2C, this instance of left-blocking has no extra requirements|#
                       (left-block)))))))

         (go next-iteration))) ;;Reached the end of the loop, repeat iteration
    (get-best-matches matched-sequences circ-canon))) ;;Only return the matches with the greatest cost
