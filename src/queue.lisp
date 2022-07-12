(in-package #:cl-quil/frontend)

;;;; Queues, taken with permission from https://bitbucket.org/tarballs_are_good/lisp-random/raw/a595eb662c2dce87f362dd0dd2541a93efe9c902/stack-queue.lisp

(defstruct (queue (:constructor %make-queue)
                  (:predicate queuep))
  (elements nil :type list)
  (last nil :type (or null (cons t null))))

(defun make-queue ()
  "Create a new empty queue."
  (%make-queue))

(defun queue-empty-p (queue)
  "Is the queue QUEUE empty?"
  (null (queue-elements queue)))

(defun enqueue (queue obj)
  "Add an element OBJ to the end of the queue QUEUE."
  (let ((last (list obj)))
    (if (queue-empty-p queue)
        ;; Set up the queue with the first element. Note that the same
        ;; reference to the singleton list is shared by both
        ;; QUEUE-ELEMENTS and QUEUE-LAST.
        (setf (queue-elements queue) last
              (queue-last queue)     last)

        ;; We can now append elements to QUEUE-ELEMENTS simply by
        ;; modifying QUEUE-LAST, whose reference is shared by
        ;; QUEUE-ELEMENTS,
        ;;
        ;; We do this instead of a single SETF for type safety of
        ;; QUEUE-LAST.
        (let ((old (queue-last queue)))
          (setf (queue-last queue) last
                (cdr old)          last))))
  queue)

(defun dequeue (queue)
  "Remove and return an element from the queue QUEUE."
  (pop (queue-elements queue)))


(defmethod print-object ((q queue) stream)
  (print-unreadable-object (q stream :type t)
    (format stream "~S"
            (queue-elements q))))
