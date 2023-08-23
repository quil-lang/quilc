;;;; pattern-replace.lisp
;;;;
;;;; Author: Brennen Hill

(in-package #:cl-quil.match)

;;;; pattern-replace replaces patterns of gates found in the passed
;;;; circuit with a more efficient, yet operationally identical
;;;; version

;; Gets a list of the indices of the gates in the circuit stored in a
;; matched-circ that are not part of the matched section of that
;; circuit
(defun get-unmatched-gate-indices (matched-circ)
  (loop :for index
        :from (1+ (start-index matched-circ)) :below (end-index matched-circ)
        :unless (find index (matches matched-circ) :test (lambda (index match-index)
                                            (= index match-index)))
          :collect index))

;; Commute gates out of the way of the matched section of the circuit
;; by commuting each unmatched gate either left or right
(defun commute-gates (matched-circ unmatched-gate-indices)
  (loop :for gate-index-original :in unmatched-gate-indices ;Loop through each of the gates that must commute out of the way of the match
        :with offset = 0 ;Offset is increased each time a gate is commuted to the right, showing how far the gates that were positioed
                         ;to its right in the circuit are now offset
        :for index = (- gate-index-original offset) ;Calculate the position of the gate in the circuit
        :for commutes-left = (can-commute-left index (start-index matched-circ) (circ matched-circ))

        ;;If the gate can commute left, commute it left; otherwise, commute it right
        :when commutes-left
          :do (setf (circ matched-circ) (commute-left index (start-index matched-circ) (circ matched-circ)))
              (incf (start-index matched-circ)) ;Now that an unmatched gate has been commuted to the left of the match, the match starts further right

        :unless commutes-left
          :do (setf (circ matched-circ) (commute-right index (initial-end-index matched-circ) (circ matched-circ)))
              (decf (end-index matched-circ)) ;Now that an unmatched gate has been commuted to the right of the match, the match starts ;further left
              (incf offset))) ;A gate has been moved to the right of the gates that were previously to its, right, so offset must be increased

;; Get whether a gate can commute left to the start-index by checking
;; that it can commute with each gate between it and the start-index
(defun can-commute-left (gate-index start-index circ)
  (loop :with commuting-gate = (nth gate-index circ)
        :for index :from (1- gate-index) :downto start-index
        :for circ-gate = (nth index circ)
        :always (gates-commute-p commuting-gate circ-gate)))

;; Commute an unmatched gate left to the start-index of a match
(defun commute-left (gate-index start-index circ)
  (append
      (subseq circ 0 start-index)
      (list (nth gate-index circ))
      (subseq circ start-index gate-index)
      (subseq circ (1+ gate-index) (length circ))))

;; Commute an unmatched gate right to the end-index of a match
(defun commute-right (gate-index end-index circ)
  (append
   (subseq circ 0 gate-index)
   (subseq circ (1+ gate-index) (1+ end-index))
   (list (nth gate-index circ))
   (subseq circ (1+ end-index) (length circ))))

;;
;; * pattern-replace replaces patterns of gates found in the passed
;;   circuit with a more efficient, yet operationally identical
;;   version found from taking the inverse of the unmatched section of
;;   each of the matched patterns
;;
;; *** Parameters:
;;
;; * circ-original - a list of 'gates' representing a circuit on which
;;                   pattern matching will be applied and gates will
;;                   be replaced
;;
;; * pats - a list of patterns, where each of these patterns is a list
;;          of 'gates' that realise the identity function and that
;;          will be matched in the circuit
;;
;; *** Note: the term 'gates' as used above are objects that are
;;           instances of the class gate or a subclass of the class gate
;;
;; *** Returns: A circuit that is operationally equivalent to
;;              circ-original, but may have had gates replaced by
;;              operationally equivalent yet cheaper gates
(defun pattern-replace (circ-original pats)
  (let* ((circ (copy-list circ-original)) ;Copy the original circuit to ensure no changes affect the passed circuit
         (match-datas '()) ;The list of match-datas, made up of data on each match found
         (selected-match-datas '())) ;The elements chosen out of match-data to replace in the circuit, which does not include overlapping matches

    ;;Get match-datas by matching each pattern with the circuit
    (setf match-datas 
          (loop :for pat :in pats ;Loop through each pattern passed
                                  ;;Collect which qubits the pattern operates on
                :for pat-qubits = (loop :for gate :in pat
                                        :for qubits = (union qubits (qubits gate))
                                        :finally (return qubits))
                :with match-sets :and permutations
                
                ;;find the matches for the pattern and the circuit and the corresponding permutations
                :do (multiple-value-bind (match-sets-value permutations-value) (pattern-match circ pat) 
                      (setf match-sets match-sets-value)
                      (setf permutations permutations-value))
                    
                ;;Build a match-data off of each match where the cost of replacing the match is less than the cost of running the original gates
                :append (loop :for matches :in match-sets
                              :and permutation :in permutations
                              :for match-data = (make-instance 'match-data
                                                               :matches matches
                                                               :circ circ
                                                               :pat (map-pat pat pat-qubits permutation))
                              :when (< (replace-cost match-data) 0)
                                :collect match-data)
                  :into unsorted-match-datas
                
                :finally
                   ;;Sort the unsorted-match-datas to form a priority queue to make sure the best matches are chosen where overlaps occur
                   (return (sort (copy-list unsorted-match-datas)
                                 (lambda (match-data1 match-data2)
                                   ;;Try to sort the match-data by cost in ascending order
                                   ;;If the match-data has equal cost, sort by match length in ascending order, as a
                                   ;;lower match length means there is more space left for other matches in the circuit
                                   (if (= (replace-cost match-data1)
                                          (replace-cost match-data2))

                                       (< (- (end-index (matched-circ match-data1))
                                             (start-index (matched-circ match-data1)))
                                          (- (end-index (matched-circ match-data2))
                                             (start-index (matched-circ match-data2))))

                                       (< (replace-cost match-data1)
                                          (replace-cost match-data2))))))))
    
    ;;If no matches were found, return from pattern-replace
    ;;Return the circuit in case previouse recursive calls to pattern-replace have made changes to it from the original call to pattern-replace    
    (unless match-datas (return-from pattern-replace circ))

    ;;Choose non-overlapping matches, prioritizing matches with the greatest cost reduction, or small length in a tie
    ;;TODO: This may not provide the optimal combination of matches. Future work could be to build a tree of each selection chosen
    ;;and recursively calling pattern-match on each leaf of the tree to see which choices of matches eventually provide the greatest reduction
    (setf selected-match-datas
          (loop :for match-data :in match-datas
                :with gate-available = (make-array (length circ) :initial-element T)

                ;;Make sure that each gate that would be affected by the match is still available
                :when (loop :for index
                            :from (start-index (matched-circ match-data)) :to (end-index (matched-circ match-data))
                            :always (svref gate-available index))
                  :collect match-data :into selected-match-datas
                  ;;Flag the indices of the gates affected by the selected match to ensure they are not affected by future selected matches
                  :and :do (loop :for index
                                 :from (start-index (matched-circ match-data)) :to (end-index (matched-circ match-data))
                                 :do (setf (svref gate-available index) NIL))

                :finally
                   ;;Sort the selected matches by highest index as replacing matches in the circuit by highest index first will ensure the indices
                   ;;of other selected matches are not influenced
                   (return (sort (copy-list selected-match-datas)
                                 (lambda (match-data1 match-data2)
                                   (> (end-index (matched-circ match-data1)) (end-index (matched-circ match-data2))))))))

    ;;Commute unmatched gates out of the way of matches in the circuit and pattern held by each of the selected match-data
    (loop :for match-data :in selected-match-datas
          :do
             ;;Commute the unmatched gates in the circuit held by each selected match-data
             (commute-gates (matched-circ match-data)
                            (get-unmatched-gate-indices (matched-circ match-data)))
             ;;Commute the unmatched gates in the pattern held by each selected match-data
             (commute-gates (matched-pat match-data)
                            (get-unmatched-gate-indices (matched-pat match-data))))
    
    ;;Replace the matches found in the circuit with their cheaper equivalent
    (loop :for match-data :in selected-match-datas
          ;;The section of the circuit before the match
          :for circ-start = (subseq circ 0 (initial-start-index (matched-circ match-data)))
          ;;The gates in the match that were commuted to the start
          :and match-prologue = (subseq (circ (matched-circ match-data)) (initial-start-index (matched-circ match-data)) (start-index (matched-circ match-data)))
          ;;The gates in the match that were commuted to the end
          :and match-epilogue = (rest (subseq (circ (matched-circ match-data)) (end-index (matched-circ match-data)) (1+ (initial-end-index (matched-circ match-data)))))
          ;;The section of the circuit after the match
          :and circ-end = (rest (subseq circ (initial-end-index (matched-circ match-data)) (length circ)))
          ;;The section of the circuit where the matches occur
          :and match-body = (append (loop :for index
                                          :from (1- (start-index (matched-pat match-data))) :downto 0
                                          :collect (gate-inverse (nth index (circ (matched-pat match-data)))))
                                    (loop :for index
                                          :from (1- (length (circ (matched-pat match-data)))) :above (end-index (matched-pat match-data))
                                          :collect (gate-inverse (nth index (circ (matched-pat match-data))))))
          :do (setf circ (append circ-start match-prologue match-body match-epilogue circ-end)))

    ;;Recursively call pattern-replace to find more matches in the new state of the circuit
    (pattern-replace circ pats)))
