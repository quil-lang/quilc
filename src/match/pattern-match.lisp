;;;; pattern-match.lisp
;;;;
;;;; Author: Brennen Hill

(in-package #:cl-quil.match)

;;;; pattern-match primarilly calls forward-match and backward-match
;;;; with every combination of starting gate and qubit permutation

;; Gets all subsets of circ-qubits of a length equal to (length
;; pat-qubits), before removing all subsets that don't include any
;; qubits marked by consequential-qubits
(defun get-subsets (circ-qubits pat-qubits consequential-qubits)
  (remove-if (lambda (subset) (or
                               (null subset)
                               (loop :for qubit :in subset
                                     :never (svref consequential-qubits (position qubit circ-qubits))))) 
             (multiple-value-list
              (get-subsets-helper '() circ-qubits '() 0 (length pat-qubits)))))

;; The helper method that recursively progresses two subsets by either
;; including or not including the next possible number
(defun get-subsets-helper (progress nums results index size)
  (cond
    ((= (length progress) size)
     ;A full subset is made, so add it
     (push progress results)
     progress)
    ((< index (length nums))
     ;;Make sure that the choice to not include a value was not made so many times that no full subset can be made
     (multiple-value-call #'values
       (get-subsets-helper (copy-list progress) nums results (1+ index) size) ;Recursively get subsets that do not include the next value

       (get-subsets-helper (copy-list (push (nth index nums) progress)) nums results (1+ index) size))))) ;Recursively get subsets that include next value

;; Recursively gets all possible permutations from the passed set nums
;; of a length equal to (length nums)
(defun get-permutations (nums &optional progress)
  (cond ((null nums)
         (list progress)) ;If every value has been used in the permutation, return the permutation
        ((listp nums) ;If nums is a list, then 
         (loop :repeat (length nums)
               ;;Rotate each value in nums so that each is eventually in the front for an iteration
               :for nums-copy = (copy-list nums)
                 :then (append (rest nums-copy) (list (first nums-copy)))
               ;;Add the first value of nums into the permutation in progress before recursively getting further permutations with the rest of the values in nums
               :append (get-permutations
                        (rest nums-copy)
                        (cons (first nums-copy) progress))))
        (T nums))) ;If the list of values is onto the last value, simply return it
    
;; * pattern-match, the high level pattern matching algorithm
;;
;; *** Parameters:
;; * circ - a list of 'gates' representing a circuit on which pattern matching will be applied
;; * pat - a list of 'gates' representing a pattern that realise the identity function and that will be matched in the circuit

;; *** Returns two values:
;; * value0 - A list of the best matches of the pattern in the circuit
;; * value1 - A list of qubit permutations, with indices correlating to value0

;; *** Notes:
;; * the term 'gates' as used above are objects that are instances of the class gate or a subclass of the class gate
;; * this algorithm is efficient (polynomial) in the number of gates in the circuit and the number of qubits operated on by the circuit
;; * this algorithm is inefficient (exponential) in the number of gates in the pattern and the number of qubits operated on by the pattern
;; * no post-processing occurs in pattern-match. Some match-sets may contain just a single match and may be seemingly duplicates except for a different
;;   qubit permutation. For post-processing of matches, look at pattern-replace.
;;
(defun pattern-match (circ pat)  
  ;;Detect some forms of errorous input. The circuit and pattern must both be lists and may not be empty
  (unless (and circ pat (listp circ) (listp pat))
    ;;This warning may come up when recursively calling pattern-match, such as in pattern-replace and can generally be ignored
    (warn "The circuit or pattern is an empty list or is not a list, exiting pattern-match.")
    (return-from pattern-match (values '() '())))
  (let ((matches '())
        (match-permutations '())
        (circ-canon (make-instance 'canonical-form :circuit circ))
        (pat-canon (make-instance 'canonical-form :circuit pat :pattern T)))
    ;;If the pattern acts on more qubits than the circuit, it cannot be matched
    ;;Attempting to match would require changig the circuit to operate on as many qubits as the pattern, which is not part of the desired purpose
    (when (< (length (qubits circ-canon))
             (length (qubits pat-canon)))
      (warn "The circuit cannot act on less qubits than the pattern, exiting pattern-match.")
      (return-from pattern-match (values '() '())))

    ;;Loop through each possible starting gate in the circuit
    (loop :for circ-node :across (nodes circ-canon)
          ;;Any qubit affected by the gate held by circ-node is marked as consequential
          ;;This will be used to skip any permutations of qubits that cannot match with the next circ-node
          :for consequential-qubits = (make-array (length (qubits circ-canon))
                               :initial-contents (loop :for qubit :in (qubits circ-canon)
                                                       :collect (gate-act-consequentialy-on-qubit-p circ-node qubit)))
          ;;Get all possible permutations of the qubits operated on by the circuit, of a size equal to the number of qubits operated on by the pattern,
          ;;skipping any permutations that do not include a consequential qubit and cannot be matched with the next circ-node
          :for permutations-sets = (loop :for subset :in (get-subsets (qubits circ-canon) (qubits pat-canon) consequential-qubits)
                                         :collect (get-permutations subset))
          :do
             ;;Loop through each possible starting gate in the pattern
             (loop :for pat-node :across (nodes pat-canon)
                   :do ;;Check if this gate combination performs the same action, albeit on possibly different qubits
                       (when (equal-gate-operation-p circ-node pat-node)
                         ;;Permute the pattern to operate on the qubits held by each permutation set
                         (loop :for permutations :in permutations-sets
                               :do (loop :for permutation :in permutations
                                         :for mapped-pat-canon = (map-pat-canon pat-canon permutation)
                                         :with forward-matches :and backward-matches
                                         :do
                                            ;;When the next gates held by the next nodes in the pattern and circuit are a match, check for further
                                            ;;matches in the forward and backward directions
                                            (when (gate-matched-p circ-node (get-node mapped-pat-canon (label pat-node)))
                                              (setf forward-matches (forward-match circ-canon mapped-pat-canon circ-node pat-node))
                                              (setf backward-matches (backward-match circ-canon mapped-pat-canon pat-node forward-matches))
                                              ;;Record the permutation used for each match
                                              (loop :for match :in backward-matches
                                                    :for index :from 0
                                                    :do
                                                       (setf (elt backward-matches index) (append  match (list permutation))))
                                              (setf matches (append backward-matches matches))))))))
    ;;Clear up duplicates from the list of matches found
    (setf matches (remove-duplicates matches :test (lambda (match-set1 match-set2)
                                                     (and (= (length match-set1) (length match-set2))
                                                          
                                                          (loop :for match :in match-set1
                                                                :for index :from 0
                                                                :always (or (= index (1- (length match-set1))) ;Last value holds permutation
                                                                            (find match (subseq match-set2 0 (1- (length match-set2)))
                                                                                  :test (lambda (match1 match2)
                                                                                          (and (= (first match1) (first match2))
                                                                                               (= (second match1) (second match2)))))))
                                                          (loop :for qubit :in (first (last match-set1))
                                                                :always (find qubit (first (last match-set2))
                                                                              :test (lambda (qubit1 qubit2)
                                                                                      (same-qubit-p qubit1 qubit2))))))))
    (loop :for match-set :in matches
          :collect (first (last match-set)) :into collected-match-permutations
          :collect (subseq match-set 0 (1- (length match-set))) :into cleaned-matches
          :finally (setf matches cleaned-matches)
                   (setf match-permutations collected-match-permutations))
    ;;Return both the list of matches and the corresponding list of permutations
    (values matches
            match-permutations)))
