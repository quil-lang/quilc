;;;; path-heuristic.lisp
;;;;
;;;; Author: Corwin de Boor

(in-package #:cl-quil)

;;; This provides a method for selecting the best link to swap first in doing a
;;; rewiring search. This heuristic is based on the idea that you should swap
;;; links that will help you as soon as possible.
;;;
;;; When trying to move towards being able to apply some gates, we benefit
;;; links that will move qubits that need to be adjacent closer together. We
;;; give greater benefit to sooner gates.
;;;
;;; When trying to move directly to a target rewiring, each qubit has a
;;; shortest path to its destination. We benefit links on that direct path and
;;; punish links that move qubits away from that shortest path. The hope is
;;; that this will perform links that benefit multiple qubits first, before
;;; going towards other links.

(in-package #:cl-quil)

(defun update-incident-link-values (qubit-from qubit-to link-values chip-spec qq-distances
                                    &key weight-on (weight-off weight-on) (path-type :move-onto))
  "Update LINK-VALUES for links which are incident to QUBIT-FROM and are on the path to QUBIT-TO.

If PATH-TYPE is :MOVE-ONTO, then we are trying to move the qubit onto the target. If PATH-TYPE is :MOVE-ADJ, we only need to move the qubit adjacent to the target. These correspond to move-to-rewiring and select-cost-lowering-swap, respectively."
  (loop :for link-index :across (chip-spec-links-on-qubit chip-spec qubit-from)
        :for other := (loop :for q :across (chip-spec-qubits-on-link chip-spec link-index)
                            :when (/= q qubit-from) :return q)
        :do (cond
              ((and (eq path-type :move-adj) (= other qubit-to))
               nil)
              ((double= (aref qq-distances qubit-from qubit-to)
                        (+ (aref qq-distances qubit-from other)
                           (aref qq-distances other qubit-to)))
               ;; on shortest path
               (incf (aref link-values link-index) weight-on))
              (t
               ;; not on shortest path
               (decf (aref link-values link-index) weight-off)))))

(defun best-available-link-index (link-values current-rewiring chip-spec rewirings-tried)
  "Get the index maximizing LINK-VALUES, excluding those links which induce a rewiring in REWIRINGS-TRIED."
  (flet ((link-yields-duplicate-rewiring (link-index)
           (destructuring-bind (q0 q1)
               (coerce (chip-spec-qubits-on-link chip-spec link-index) 'list)
             (with-update-rewiring current-rewiring q0 q1
               (member current-rewiring rewirings-tried :test #'equalp))))
         (link-value (idx) (aref link-values idx)))
    (a:extremum
     (delete-if #'link-yields-duplicate-rewiring (a:iota (length link-values)))
     #'>
     :key #'link-value)))

(defun select-swap-for-rewiring (rewiring target-rewiring chip-spec qq-distances rewirings-tried)
  "Determine a link to swap in order to bring REWIRING closer to TARGET-REWIRING."
  (loop
    :with n-links := (chip-spec-n-links chip-spec)
    :with link-values := (make-array n-links :initial-element 0d0)

    :for qubit :below (chip-spec-n-qubits chip-spec)
    :for src := (apply-rewiring-l2p rewiring qubit)
    :for dst := (apply-rewiring-l2p target-rewiring qubit)
    :when src
      :do (update-incident-link-values src dst link-values chip-spec qq-distances
                                       :weight-on (+ (random 0.01) 1.0d0)
                                       :weight-off (if (= src dst) 0.2d0 0.6d0))
    :finally (format-noise "SELECT-SWAP-FOR-REWIRING: Link-values ~A" link-values)
             (return (best-available-link-index link-values rewiring chip-spec rewirings-tried))))

(defun select-swap-for-gates (rewiring gates-in-waiting chip-spec qq-distances rewirings-tried)
  "Determine a link to swap in order to schedule GATES-IN-WAITING with respect to the current REWIRING."
  (loop
    :with n-links := (chip-spec-n-links chip-spec)
    :with link-values := (make-array n-links :initial-element 0d0)
    :for gate :being :the :hash-keys :of gates-in-waiting
    :for tier := (gethash gate gates-in-waiting)
    :for tier-weight := (expt 0.5d0 tier)
    :when (and (typep gate 'gate-application)
               (= 2 (length (application-arguments gate))))
      :do (destructuring-bind (q0 q1) (mapcar #'qubit-index (application-arguments gate))
            (let* ((p0 (apply-rewiring-l2p rewiring q0))
                   (p1 (apply-rewiring-l2p rewiring q1)))
              (unless p0 (rotatef p0 p1) (rotatef q0 q1))
              ;; if both are unassigned, then we gain nothing by changing the
              ;; rewiring, so ignore this gate
              (when p0
                ;; otherwise at least one is assigned
                (unless p1
                  ;; find a position for the other qubit
                  (setf p1 (a:extremum
                            (delete-if (lambda (p) (apply-rewiring-p2l rewiring p))
                                       (a:iota (rewiring-length rewiring)))
                            #'<
                            :key (lambda (p) (aref qq-distances p0 p))))
                  (rewiring-assign rewiring q1 p1))

                (update-incident-link-values p0 p1 link-values chip-spec qq-distances
                                             :weight-on tier-weight :path-type :move-adj)
                (update-incident-link-values p1 p0 link-values chip-spec qq-distances
                                             :weight-on tier-weight :path-type :move-adj))))
    :finally (format-noise "SELECT-SWAP-FOR-GATES: Link-values ~A" link-values)
             (return (best-available-link-index link-values rewiring chip-spec rewirings-tried))))
