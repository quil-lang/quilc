;;;; macros.lisp
;;;;
;;;; Author: Brennen Hill

(in-package #:cl-quil.match)

;;;; macros holds the macros used by the pattern-matching algorithm
;;;; and thus should always be compiled first

;; Adds matched-node to the priority queue already-matched-nodes,
;; before ensuring the priority queue is ordered by labels of
;; direc-succs of nodes in desending order
(defmacro add-matched-node (already-matched-nodes matched-node)
  `(when (peek-succ-to-visit ,matched-node) ;Only add the node if it has remaining successors to visit
     (push ,matched-node ,already-matched-nodes)
     (setf ,already-matched-nodes (sort ,already-matched-nodes #'<
                                        :key (lambda (node) (label (peek-succ-to-visit node)))))))

;; Records the match between circ-node and the matched node in the
;; passed scenario
(defmacro matched-expansion (circ-node match scenario)
  `(progn
     ;;Sets the position in the matched-track for the circuit and patten to the label of the corresponding matched node
     (setf
      (svref (matched-track-circ ,scenario) (label ,circ-node))
      (label ,match))

     (setf
      (svref (matched-track-pat ,scenario) (label ,match))
      (label ,circ-node))

     ;;Records the match between the nodes in the scenario's list of matches
     (push
      (list (label ,circ-node) (label ,match)) (matches ,scenario))))

;; Removes the match that incorporates the given node from the
;; scenario
(defmacro unmatched-expansion (node matched-track scenario location)
  ;;If the node is previously matched, remove the match
  `(cond ((matched-p ,node ,scenario)
          
          ;;Sets the position in the vector holding data about matches to NIL, removing any previous match
          (setf
           (svref ,matched-track (label ,node))
           NIL)

          ;;Remove the pair that included the node from the scenario's list of matches
          (setf
           (matches ,scenario)
           (remove-if (lambda (match)
                        (= (funcall ,location match) (label ,node)))
                      (matches ,scenario)))
          T) ;Return T, to show that a match was removed

         (T ;If the node is not matched, do nothing and return NIL
          NIL)))
