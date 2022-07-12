;;;; hasse-scheduler.lisp
;;;;
;;;; Author: Mark David

(in-package #:cl-quil/tools)

;;; This file implements utilities for creating a Hasse Diagram for a
;;; logical-scheduler instance. The resulting diagram indicates the
;;; partial ordering of the instructions. The diagram is in the form
;;; of a Graphviz source file, called a "dot file", conventionally
;;; named with file type ending ".gv".

;;; The dot file can be readily transformed into various kinds of
;;; image files. Given a file such as "example-1.gv", it can be
;;; converted to various viewable formats and then viewed in numerous
;;; possible ways. Here are some example command lines that should
;;; work on any sort Linux or MacOS with Graphviz installed. (Note
;;; that 'dot' is the Graphviz program for drawing directed graphs.)
;;;
;;;   dot -Tpdf example-1.gv -o g.pdf
;;;     -- convert the dot file to a PDF file named "g.pdf"
;;;
;;;   dot -Tpng example-1.gv -o g.png
;;;      -- convert the dot file to a PNG file named "g.png"
;;;
;;; The resulting image file can then be shown in various software,
;;; such as image viewers, PDF viewers, and web browsers.

(defun write-hasse-dot (logical-scheduler &key stream)
  (when (null stream)
    (setq stream *standard-output*))
  (format stream "digraph G {~%")
  (format stream "    rankdir=BT;~%")
  (write-hasse-node-statements logical-scheduler stream)
  (format stream "}~%"))

(defun write-hasse-node-statements (logical-scheduler stream)
  (let ((node-statements (get-hasse-node-statements logical-scheduler)))
    (dolist (stmt node-statements)
      (format stream "    ~a~%" stmt))))


(defvar *hasse-counter*)
(setf (documentation '*hasse-counter* 'variable)
      "Bound by get-hasse-node-statements initially to 1, read and
      incremented by hasse-instr-to-label on each node's creation.")

(defvar *hasse-hash-table*)
(setf (documentation '*hasse-hash-table* 'variable)
      "Bound by get-hasse-node-statements to a hash table mapping an
      instr to hasse-label-info, which gets created by
      hasse-instr-to-name.")


;;; The structure hasse-label-info has two fields:
;;;   label, a string of the form "<instruction text>", e.g., "X 2", and
;;;   name, a string of the form "<instruction text> [<unique integer>], e.g., "X 2 [15]"

(defstruct hasse-label-info
  label
  name)


(defun create-hasse-label-info (instr)
  "Create a hasse-label-info structure corresponding to instr, increment *hasse-counter*, and return the new structure."
  (prog1 (make-hasse-label-info
          :name (format-hasse-instr-name nil instr *hasse-counter*)
          :label (format-hasse-instr-label nil instr *hasse-counter*))
    (incf *hasse-counter*)))

(defun hasse-instr-to-label (instr)
  (or (first (gethash instr *hasse-hash-table*))
      (error "Programming error: no mapping from instr ~s to name" instr)))

(defun hasse-instr-to-name (instr)
  (let ((label-info
          (or (gethash instr *hasse-hash-table*)
              (setf (gethash instr *hasse-hash-table*)
                    (create-hasse-label-info instr)))))
    (hasse-label-info-name label-info)))

(defparameter *show-instr-instance-counters* nil
  "Defaults to nil. If true, the unique integer associated with each
   unique instruction is included in the visible labels of the graph.
   This is in most cases NOT needed, but possibly could be useful for
   debugging, particularly by developers of this tool.")


(defun format-hasse-instr-label (format-dest instr counter)
  (format format-dest
          (if *show-instr-instance-counters*
              "~/cl-quil::instruction-fmt/ [~d]"
              "~/cl-quil::instruction-fmt/")
          instr counter))

(defun format-hasse-instr-name (format-dest instr counter)
  (format format-dest
          "~/cl-quil::instruction-fmt/ [~d]"
          instr counter))
      
(defun hasse-instr-visited-p (instr)
  (gethash instr *hasse-hash-table*))

(defun get-hasse-node-statements (logical-scheduler)
   (let* ((*hasse-counter* 1)
          (*hasse-hash-table* (make-hash-table))
          (queue '())
          (result '())
          (earliers-hash-table
            (quil.si:lscheduler-earlier-instrs logical-scheduler)))
     ;; Go through all the last instructions. Any that have no
     ;; earliers go into the graph as singleton nodes. The rest get
     ;; pushed onto the queue of instructions.
     (loop :for instr :in (quil.si:lscheduler-last-instrs logical-scheduler)
           :as earliers := (gethash instr earliers-hash-table)
           :do (cond
                 ((null earliers)
                  (push (format nil "~s" (hasse-instr-to-name instr)) result))
                 (t
                  (push instr queue))))
     ;; While there's a queue of instructions, pop one instruction
     ;; INSTR and for each of its earliers E:
     ;; 
     ;;   - unless E's been done already, push E onto the queue
     ;;   
     ;;   - add INSTR and E to the graph with an edge from INSTR to E
     (loop :while queue
           :do (loop :with instr := (pop queue)
                     :with earliers := (gethash instr earliers-hash-table)
                     :as earlier :in earliers
                     :do (when (not (hasse-instr-visited-p earlier))
                           (push earlier queue))
                         (push
                          (format
                           nil
                           "~s -> ~s"
                           (hasse-instr-to-name instr)
                           (hasse-instr-to-name earlier))
                          result)))
     ;; Now all the nodes have been added to the graph, go through the
     ;; hash table and add label instructions so that labels show
     ;; rather than node names.  (Node names are graph-wide unique
     ;; because they contain a unique integer, but that should not
     ;; normally be seen by users. Therefore, we show a label instead
     ;; that is without the integer.)
     (maphash
      #'(lambda (instr label-info)
          (declare (ignore instr))
          (push
           (format nil "~s [label = ~s]"
                   (hasse-label-info-name label-info)
                   (hasse-label-info-label label-info))
           result))
      *hasse-hash-table*)
     result))

(defun quil-text-to-logical-scheduler (source-text)
  "Make a logical scheduler corresponding to SOURCE-TEXT."
  (quil-parse-to-logical-scheduler (cl-quil:parse-quil source-text)))


(defun quil-parse-to-logical-scheduler (parse)
  "Make a logical scheduler corresponding to PARSE."
  (let* ((instructions-vector (cl-quil:parsed-program-executable-code parse))
         (instructions-list (concatenate 'list instructions-vector))
         (lsched (quil.si:make-lscheduler)))
    (quil.si:append-instructions-to-lschedule lsched instructions-list)
    lsched))

;; Note: this was patterned after: calculate-instructions-2q-depth


(defun program-to-string (program)
  "Return a string representation of PROGRAM.

PROGRAM should be either a parsed program (i.e., a cl-quil:parsed-program
instance), a string, or a list of strings.  If a string, it is assumed
to have its own internal line breaks, and it is returned as is.  If a
list of strings, each string is assumed to represent a line, and this
returns a string that contains the contents of each string in the list
followed by a line break. A parsed program is printed, using
quil:print-parsed-program, to a string that is returned."
  (etypecase program
    (string program)
    (list
      (with-output-to-string (out)
        (loop :for line :in program
              :do (write-line line out))))
    (cl-quil:parsed-program
      (with-output-to-string (out)
        (cl-quil:print-parsed-program program out)))))


(defun write-program-to-graphviz-comment (program &key stream comment-line-start)
  "Write program lines in PROGRAM as comments.

PROGRAM should be an arg acceptable to program-to-string (see above),
which this uses to get the program text.

Each line is written out starting with COMMENT-LINE-START, which
defaults to \"# \".

Output goes to STREAM, which defaults to *standard-output*."
  (unless stream (setq stream *standard-output*))
  (unless comment-line-start (setq comment-line-start "# "))
  (with-input-from-string (in (program-to-string program))
    (loop :for line := (read-line in nil nil)
          :while line
          :do (format stream "~%# ~a" line))))


;;;; Design Questions

;;; Should there be other forms of output such as some kind of
;;; graphics in SLIME.  Should there be integration with SLIME and/or
;;; (plain) Emacs?

;;; Labels: currently, as each node is created it is assigned a label,
;;; which is simply the source text of the instruction (which may not
;;; be unique), and a unique label name, which is made up of the label
;;; text followed by a monotically increasing integer. This ensures
;;; that node is unique to the resulting graph, which is required,
;;; since each instruction represents a unique node, whereas the label
;;; text may be duplicated any number of times, just as any
;;; instruction may be duplicated multiple times in a quil program.
;;;
;;; There may be other good options to consider for labels. It's
;;; doubtful you would want the label name to show up (the label plus
;;; unique integer), since it is a randomish artifact of the graph
;;; search order, but conceivably you might want that for debugging
;;; (of this tool).  Are there other good options to consider for
;;; numbering?
;;;
;;; One appealing idea would be to show the line number of the
;;; instruction in the original listing. However, (a) we do not have
;;; access to the original source listing from the logical scheduler
;;; and therefore do not have access to line numbers from the logical
;;; scheduler; and (b) there may be more than one instruction per
;;; line, so what do you do then, subnumbering (10.3, etc.)?
;;;
;;; Another possibility would be an encoding of the form
;;; depth.position, where depth is the distance from the Hasse root
;;; (bottom), and position is a position (from left to right, say) in
;;; the horizontal span. One bit of hair to consider with that: it
;;; seems the left-to-right positioning in the horizontal span is left
;;; up to Graphviz, so how can you know that?  You could just order
;;; them effectively semi-randomly at each level, much as we do now,
;;; and users may not mind the order not being perfect.
;;;
;;; On the other hand, it may be possible to create really helpful
;;; visualization if the diagram combined with the program listing and
;;; integrated fully. That probably requires diagrammer to have access
;;; to both the logical scheduler and the parse (and possibly also the
;;; information about the source code).





;;;; Higher-Level Functions

;;; Here the exported functions: write-hasse-for-logical-scheduler and
;;; write-hasse-for-quil. Both put out a Hasse diagram as a Graphviz
;;; dot file (.gv). The former takes a logical scheduler (optionally
;;; with a program, which may improve the output), while the latter
;;; takes a quil program, for which a logical schedule is then
;;; derived.

(defun write-hasse-for-quil (program &key stream output-file)
  "Write a Hasse diagram for Quil program PROGRAM to the designated output.

PROGRAM should be an arg acceptable to program-to-string (see above),
which this uses to get the program text.

The output is in the format of a Graphviz DOT file, with a directed
graph program based on the logical scheduler corresponding to PROGRAM.

For convenient reference, the program text itself is written into the
DOT file as a comment.

If STREAM is non-nil, output goes to a stream. Otherwise, output-file
is used as the first arg to OPEN, and the file is written there
accordingly. If explicitly specifying a file type, note that the
default/expected type for a Graphviz DOT file is: gv

In the case of writing to a file, this returns the true name of the
resulting file a string.  Otherwise, this returns nil."
  (let* ((program-string (program-to-string program))
         (parse (cl-quil:parse program-string))
         (logical-scheduler (quil-parse-to-logical-scheduler parse)))
    (write-hasse-for-logical-scheduler
     logical-scheduler
     :program program-string
     :stream stream
     :output-file output-file)))


(defun write-hasse-for-logical-scheduler (logical-scheduler
                                          &key stream
                                               output-file
                                               program)
  "Wrote a Hasse diagram for LOGICAL-SCHEDULER to the designated output.

Details are as for WRITE-HASSE-FOR-QUIL, except that if PROGRAM is
nil, the program text is not emitted to the DOT file as a comment. See
above."
    (flet ((emit (stream)
             (write-hasse-dot logical-scheduler :stream stream)
             (when program
               (write-program-to-graphviz-comment program :stream stream))))
      (if stream
          (emit stream)
          (with-open-file (out output-file
                               :direction :output
                               :if-exists :supersede)
            (emit out)
            (namestring (truename out))))))
