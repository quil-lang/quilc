;;;; forward-match.lisp
;;;;
;;;; Author: Brennen Hill

(in-package #:cl-quil.match)

;;;; forward-match greedily finds matches in the forward direction
;;;; with nodes that could not commute behind the starting node

;; Returns a list of the nodes from the pattern that can be matched
;; next. Possible nodes are direct-successors of the previously
;; matched node 'pred', are not blocked, are not matched, and are not
;; successors of the direct-successors of another previously matched
;; node.
(defun find-forward-candidates (pat-canon pred scenario)
  (let ((matched-labels '())                  ;A set of the labels of the nodes in the pattern that have already matched
        (excluded-labels '()))                ;A set of the labels of nodes to exclude as candidates
    
    ;;Extract the labels of nodes in the pattern that have a match
    (loop :for match :in (matches scenario)
          :do (setf matched-labels (cons (second match) matched-labels)))
    
    ;;Gathers the labels of nodes that branch into paths that weren't chosen and must be excluded
    (loop :for matched-label :in matched-labels
          :unless (= matched-label (label pred))
            :do ;Don't prune the direct-successors of the pred node, as it does not yet have a successor
                (loop :for direct-succ :in (direct-succs (get-node pat-canon matched-label))
                      :unless (find (label direct-succ) matched-labels)
                        :do ;Only prune paths that weren't chosen and added to match
                            (setf excluded-labels (union excluded-labels
                                                         (loop :for succ :in (succs direct-succ)
                                                               :collect (label succ))))))
    
    ;; Return the direct successors of the predecessor node, excluding
    ;; previously matched nodes and unchosen branches from previously
    ;; matched nodes
    (loop :for direct-succ :in (direct-succs pred)
          :unless (or
                   (find (label direct-succ) matched-labels)
                   (find (label direct-succ) excluded-labels))
            :collect direct-succ)))

;; The algorithm that will match nodes in the forward direction of a
;; circuit from a starting node
(defun forward-match (circ-canon pat-canon circ-start-node pat-start-node)
  (let ((scenario (make-instance 'matching-scenario ;An empty matching scenario, that will store the list of matches and which nodes are blocked or matched
                                 :matches '()                                                                       
                                 :blocked-track-circ (make-array (length (nodes circ-canon)) :initial-element NIL)  
                                 :matched-track-circ (make-array (length (nodes circ-canon)) :initial-element NIL)  
                                 :blocked-track-pat  (make-array (length (nodes pat-canon)) :initial-element NIL)   
                                 :matched-track-pat  (make-array (length (nodes pat-canon)) :initial-element NIL)))
        ;;;A priority queue of matched nodes who's successors will be checked for matches, oredered by label of the first succ-to-visit of each node
        (already-matched-nodes (list circ-start-node)))
    
    ;;Prepare the initial match and gather its direct-successors to prepare to visit them to attempt matching
    (matched circ-start-node pat-start-node scenario)
    (loop :for direct-succ :in (direct-succs circ-start-node)
          :do (push-succ-to-visit circ-start-node direct-succ))
    
    ;;Beginning of the while loop that loops through the list of matched vertices to greedily add more matches following their successors
    (tagbody next-iteration ;;Tag for the beginning of the loop
       (progn
         (let ((matched-pred (pop already-matched-nodes))) ;Get the predecessor of the next node to visit
           (unless matched-pred
             (go loop-end)) ;Terminate the loop if every matched-node has been iterated over
           
           (let ((next-node (pop-succ-to-visit matched-pred)) ;The next node to visit, which holds the smallest label of possible next nodes
                 (forward-candidates '())                     ;The list of candidates for matches in the pattern
                 (matched-candidates '())                     ;The list of nodes from forward-candidates that form matches
                 (lowest-matched-label))                      ;The label of the match from matched-candidates with the lowest label
             
             (unless next-node (go next-iteration)) ;Skip this iteration if every direct-succ of the current matched-pred has been itterated over
             (add-matched-node already-matched-nodes matched-pred) ;Return the predecessor to next-node to the set of matched nodes that it was popped from

             ;;Skip this iteration if the next-node is unmatchable do to being blocked or already matched
             (when (or (blocked-p next-node scenario) (matched-p next-node scenario))
               (go next-iteration))
             
             ;;Find a list of valid matches for next-node in the pattern
             (setf forward-candidates (find-forward-candidates pat-canon (get-matched matched-pred scenario pat-canon) scenario))
             (setf matched-candidates (loop :for forward-candidate :in forward-candidates
                                            :when (gate-matched-p forward-candidate next-node)
                                              :collect forward-candidate))
             
             ;;If there are no matches, then block this node and its successors before going onto the next node
             (unless matched-candidates
               (blocked next-node scenario)
               (loop :for succ :in (succs next-node) :do
                 (blocked succ scenario))
               (go next-iteration))
             
             ;;Greedily choose the matched forward candidate with the lowest label, thus the earliest position in the pattern
             (setf lowest-matched-label (loop :for matched-candidate :in matched-candidates :minimizing (label matched-candidate)))
             (matched next-node (get-node pat-canon lowest-matched-label) scenario)
             
             ;;Add the successors of the mathed node to the list of nodes to visit
             (loop :for direct-succ :in (direct-succs next-node) :unless (or
                                                                          (blocked-p direct-succ scenario)
                                                                          (matched-p direct-succ scenario))
                   do (push-succ-to-visit next-node direct-succ))
             
             (add-matched-node already-matched-nodes next-node) ;Return the predecessor to next-node to the set of matched nodes that it was popped from
             (go next-iteration)))) ;Reached end of loop iteration, go to the next iteration
     loop-end) ;Tag for the end of the loop, used when the loop is to be terminated
    scenario)) ;Return the information collected on matches as well as matched and blocked nodes
