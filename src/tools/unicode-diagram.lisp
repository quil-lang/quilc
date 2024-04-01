;;;; unicode-diagram.lisp

;;;; Prints a textual diagram of the a quantum circuit. Circuit wires
;;;; run vertically. The top is the start of the circuit and the
;;;; bottom is the end.

;;;; How the diagrammer works:

;;;; 1. A parsed-program instance is used to build a
;;;; logical-schedule.

;;;; 2. Instructions in the logical schedule are traversed in
;;;; topological order and added to diagram object by packing them
;;;; into rows such that the instructions do not overlap. An
;;;; instruction overlaps with another if its visual represnetation
;;;; would overlap with that of another instruction.

;;;; 3. Printing the diagram proceeds by operating on a reusable
;;;; "line", a vector of characters. Printing functions mutate this
;;;; line before printing it.
;;;;
;;;; 4. Every logical row of the digram prints as 4 textual lines. A
;;;; line of wires, the tops of containing boxes, the main content and
;;;; the bottoms of containing boxes.
;;;;
;;;; Finally, users may customize the characters used to print the
;;;; diagram elements by constructing a CHARMAP instance and passing
;;;; it to the PRINT-PROGRAM-DIAGRAM function.

(defpackage #:cl-quil.tools.unicode-diagram
  (:use #:cl)
  (:export
   #:print-program-diagram
   #:make-charmap
   #:+box-drawings-charmap+
   #:+ascii-charmap+))

(in-package #:cl-quil.tools.unicode-diagram)

;;; CHARACTER MAPS

(defstruct (charmap (:conc-name get-charmap-))
  (vertical #\BOX_DRAWINGS_LIGHT_VERTICAL)
  (horizontal #\BOX_DRAWINGS_LIGHT_HORIZONTAL)
  (top-left #\BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_RIGHT)
  (top-right #\BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_LEFT)
  (bottom-left #\BOX_DRAWINGS_LIGHT_ARC_UP_AND_RIGHT)
  (bottom-right  #\BOX_DRAWINGS_LIGHT_ARC_UP_AND_LEFT)
  (top-meet #\BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL)
  (bottom-meet #\BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL)
  (swap-char #\MULTIPLICATION_SIGN)
  (control-char #\MIDDLE_DOT)
  (cnot-target-char #\CIRCLED_PLUS))

(defparameter +box-drawings-charmap+ (make-charmap))
(defparameter +ascii-charmap+ (make-charmap
                               :vertical #\|
                               :horizontal #\-
                               :top-left #\-
                               :top-right #\-
                               :bottom-left #\-
                               :bottom-right #\-
                               :top-meet #\+
                               :bottom-meet #\+
                               :swap-char #\x
                               :control-char #\*
                               :cnot-target-char #\+))


(defvar *charmap* +box-drawings-charmap+
  "The charmap bound during printing.")

(defun vertical (&optional (map *charmap*))
   (get-charmap-vertical map))
(defun horizontal (&optional (map *charmap*))
  (get-charmap-horizontal map))
(defun top-left (&optional (map *charmap*))
  (get-charmap-top-left map))
(defun top-right (&optional (map *charmap*))
  (get-charmap-top-right map))
(defun bottom-left (&optional (map *charmap*))
  (get-charmap-bottom-left map))
(defun bottom-right (&optional (map *charmap*))
  (get-charmap-bottom-right map))
(defun top-meet (&optional (map *charmap*))
  (get-charmap-top-meet map))
(defun bottom-meet (&optional (map *charmap*))
  (get-charmap-bottom-meet map))
(defun swap-char (&optional (map *charmap*))
  (get-charmap-swap-char map))
(defun control-char (&optional (map *charmap*))
  (get-charmap-control-char map))
(defun cnot-target-char (&optional (map *charmap*))
  (get-charmap-cnot-target-char map))


;;; DIAGRAM TYPES


(defstruct figure
  "A representation of an operation in a diagram. Tracks information
needed to print the operation."
  name                                  ; gate name + paramers 
  boxed?                                ; does this figure have a box around it?
  swap?                                 ; a swap is a special unboxed figure
  control-wires                         ; list of integers, CCNOT 0 1 3 would contain (0 1)
  target-wires)                         ; list of integers, CCNOT 0 1 3 would contain (3)

(defclass diagram ()
  ((wire-count
    :accessor diagram-wire-count
    :initarg :wire-count
    :initform (error "required")
    :type integer
    :documentation "The number of wires (aka qubits) in this diagram. Each wire")
   (column-widths
    :accessor diagram-column-widths
    :type vector
    :documentation
    "A vector of integers containing wire-count entries. Each entry stores
     the character width of a column in the diagram.")
   (gap
    :accessor diagram-column-gap
    :initarg :gap
    :initform 2
    :documentation "Number of characters of blank space between columns.")
   (rows
    :accessor diagram-rows
    :type vector
    :documentation "A vector of non-overlapping figures. Time increases with vector index.")))

(defmethod initialize-instance :after ((diagram diagram) &key wire-count &allow-other-keys)
  (setf (diagram-column-widths diagram) (make-array wire-count :initial-element 1))
  (setf (diagram-rows diagram) (make-array 0 :adjustable t :fill-pointer 0)))

(defun diagram-width (diagram)
  "The total character width of the diagram."
  (+ 1 (* (diagram-column-gap diagram) (diagram-wire-count diagram))
     (reduce #'+ (diagram-column-widths diagram))))

(defstruct column
  "A representation of a column's boundaries across a line."
  start                                 ; the first character position of the column
  middle                                ; the canonical middle character position, cached b/c it's used often
  stop)                                 ; the last characer position.

(defun diagram-column-bounds (diagram)
  "Returns a list of COLUMNs for use in drawing the DIAGRAM."
  (with-slots (column-widths gap) diagram
    (loop :with pos := gap
          :for col :from 0
          :for width :across column-widths
          :for mid := (+ pos (ceiling width 2))
          :for stop := (+ pos width) 
          :collect (make-column
                    :start pos
                    :middle mid
                    :stop stop)
          :do (incf pos (+ width gap)))))

(defun instruction-figure-name (instr)
  "Return two values, the string name to use for this instruction and a
boolean indicationg whether or not to draw a box around the name."
  (typecase instr
    (cl-quil::reset-qubit (values "INIT" t))
    (cl-quil::measure-discard (values "MEASURE" t))
    (cl-quil::gate-application
     (let ((name (cl-quil::operator-description-root-name
                  (cl-quil::application-operator instr))))
       (cond ((string-equal "SWAP" name)
              (values (princ-to-string (swap-char)) nil t))
             ((member name '("CCNOT" "CNOT") :test #'equal)
              (values (princ-to-string (cnot-target-char)) nil))
             ((string-equal "CZ" name) (values "Z" t))
             ((cl-quil:application-parameters instr)
              (values 
               (format nil "~a(~{~/cl-quil:instruction-fmt/~^, ~})"
                       name
                       (cl-quil:application-parameters instr))
               t))
             (t (values name t)))))))

(defun control-depth (operator &optional (depth 0))
  (adt:match cl-quil::operator-description operator
    ((cl-quil::controlled-operator controlled)
     (control-depth controlled (1+ depth)))

    ((cl-quil::forked-operator forked)
     (control-depth forked (1+ depth)))

    ((cl-quil::dagger-operator conjugated)
     (control-depth conjugated depth))

    ((cl-quil::named-operator name)
     (+ depth
        (cond ((member name '("CNOT" "CZ" "CPHASE" "CSWAP") :test #'string-equal)
               1)
              ((member name '("CCNOT") :test #'string-equal)
               2)
              (t
               0))))))

(defun num-control-qubits (instr)
  "Returns the number of control qubits associated with this operator."
  (if (typep instr 'cl-quil::application)
      (control-depth (cl-quil:application-operator instr))
      0))

(defun partition-at (n xs)
  (loop :for (x . more) :on xs
        :repeat n
        :collect x :into front
        :finally (return (values front (cons x more)))))

(defun control-and-target (instr)
  "Returns two values CONTROL TARGET, the control qubits and target
qubit arguments to this instruction."
  (partition-at (num-control-qubits instr) (cl-quil:qubits-used instr)))

(defun new-figure (instruction)
  "Creates a figure from an instruction."
  (let ((qubits (cl-quil:qubits-used instruction)))
    (when qubits
      (multiple-value-bind (name boxed? swap?) (instruction-figure-name instruction)
        (multiple-value-bind (control target) (control-and-target instruction)
          (make-figure
           :name name
           :boxed? boxed?
           :swap? swap?
           :control-wires control
           :target-wires target))))))

(defun update-column-widths (diagram figure)
  "Ensures that the columns of DIAGRAM are wide enough to contain the
content in FIGURE."
  (let ((widths (diagram-column-widths diagram)))
    (with-slots (name target-wires boxed?) figure
      (let ((size (+ (if boxed? 2 0) (length name))))
        (loop :for wire :in target-wires
              :do (setf (elt widths wire) (max (elt widths wire) size)))))))

(defun add-new-row (diagram)
  "Inserts a new empty row into DIAGRAM's rows, and returns it. A row is
a vector of figures."
  (with-slots (rows wire-count) diagram
    (let ((new-row (make-array 0 :adjustable t :fill-pointer 0)))
      (vector-push-extend new-row rows)
      new-row)))

(defun get-last-row (diagram)
  "Returns the last row in DIAGRAM, creating it first if necessary."
  (with-slots (rows) diagram
    (if (plusp (length rows))
        (elt rows (1- (length rows)))
        (add-new-row diagram))))

(defun figure-wires (figure)
  "Get all wires in a figure - appends the control wires to the target
wires."
  (append (figure-control-wires figure)
          (figure-target-wires figure)))

(defun figure-wire-span (figure)
  "Returns two values, LEAST GREATEST where LEAST is the numerically
least wire/qubit mentioned in the figure and GREATEST is the
numerically greatest."
  (values (reduce #'min (figure-wires figure))
          (reduce #'max (figure-wires figure))))

(defun figures-intersect-p (f1 f2)
  "Two figures intersect if their wire spans overlap."
  (multiple-value-bind (left1 right1) (figure-wire-span f1)
    (multiple-value-bind (left2 right2) (figure-wire-span f2)
      (or (<= left1 left2 right1)
          (<= left1 right2 right1)
          (<= left2 left1 right2)
          (<= left2 right1 right2)))))

(defun row-has-room-for (row figure)
  "T if FIGURE does not intersect any members of ROW."
  (loop :for f :across row
        :never (figures-intersect-p f figure)))

(defun append-figure-to-row (row figure)
  (vector-push-extend figure row))

(defun add-instruction (diagram instr)
  "Adds an instruction to the diagram. Instruction is converted into a
figure, column widths are updated, and the figure is packed into the
rows."
  (let ((figure (new-figure instr)))
    (when figure
      (update-column-widths diagram figure)
      (let ((last-row (get-last-row diagram)))
        (if (row-has-room-for last-row figure)
            (append-figure-to-row last-row figure)
            (let ((new-row (add-new-row diagram)))
              (append-figure-to-row new-row figure)))))))

(defun make-diagram (program)
  "Given a cl-QUIL:PARSED-PROGRAM instance PROGRAM, return DIAGRAM
instance."
  (let* ((lschedule (cl-quil::make-lschedule))
         (qubits (length (cl-quil:qubits-used program)))
         (diagram (make-instance 'diagram :wire-count qubits)))

    (cl-quil::append-instructions-to-lschedule
     lschedule
     (map 'list #'identity (cl-quil::parsed-program-executable-code program)))

    (cl-quil::map-lschedule-in-topological-order
     lschedule
     (lambda (instr) (add-instruction diagram instr)))
    diagram))


;;; DIAGRAM PRINTING

;;; Printing functions operate on a reusable character vector, the
;;; line to be printed. Functions prepare the line before printing it.

(defun clear-line (line &key (char #\space) (from 0) (below (length line)))
  "FILL the LINE with CHAR, optionally between characer positions FROM
and BELOW."
  (loop :for idx :from from  :below below :do (setf (elt line idx) char)))

(defun write-wires-to-line (line columns)
  "Draw a wire at the center of each column."
  (loop :for col :in columns
        :do (setf (elt line (column-middle col)) (vertical))))

(defun write-box-top-to-line (line left right meets)
  "Draws the top of a box into LINE, includes LEFT and RIGHT corners,
and locations where the box MEETS a wire."
  (setf (elt line left) (top-left)
        (elt line right) (top-right))
  (loop :for idx :from (1+ left) :below right
        :do (setf (elt line idx) (horizontal)))
  (loop :for idx :in meets
        :do (setf (elt line idx) (top-meet))))

(defun write-box-bottom-to-line (line left right meets)
  "Draws the bottom of a box into LINE, includes LEFT and RIGHT corners,
and locations where the box MEETS a wire."
  (setf (elt line left) (bottom-left)
        (elt line right) (bottom-right))
  (loop :for idx :from (1+ left) :below right
        :do (setf (elt line idx) (horizontal)))
  (loop :for idx :in meets
        :do (setf (elt line idx) (bottom-meet))))

(defun box-bounds (figure columns gap)
  "Returns 3 values: LEFT RIGHT MEET-POSITIONS, the horizontal bounds
for a box that contains FIGURE. 

GAP is the containing diagram's GAP slot value. COLUMNS is a sequence
of COLUMN intances."
  (let* ((wires  (figure-target-wires figure))
         (fwidth (length (figure-name figure)))
         (fwidth/2 (ceiling fwidth 2)))
    (if (= 1 (length wires))
        (let* ((col (elt columns (first wires)))
               (mid (column-middle col))
               (start (- mid fwidth/2)))
          (values start (+ 1 start fwidth) (list mid)))
        (let (left right (meets nil))
          (loop :for wire :in wires
                :for wire-col := (elt columns wire)
                :for mid := (column-middle wire-col)
                :do (setf left (if left (min left (- mid gap)) (- mid gap))
                          right (if right (max right (+ mid gap)) (+ mid gap)))
                    (push mid meets))
          (values left
                  right
                  meets)))))

(defun write-around-row (columns row line &key top? (gap 2))
  "Draw the top or bottom of a ROW into LINE. 

COLUMNS is a sequence of column instances. ROW is a vector of
figures. LINE is vector of characters. If TOP? is T draw the top of a
row, otherwise draw the bottom."
  (clear-line line) 
  (write-wires-to-line line columns)
  (loop :for figure :across row
        :when (figure-boxed? figure)
          :do (multiple-value-bind (left right meets) (box-bounds figure columns gap)
                (if top?
                    (write-box-top-to-line line left right meets)
                    (write-box-bottom-to-line line left right meets)))))

(defun write-control-links (columns row line)
  "The control links are horizontal lines drawn between wires across
which controlled gates operate. This function simply draws these
links.

This funciton also draws the value of (CONTROL-CHAR) onto the wire
corresponding to the qubit passed as a control argument to a
controlled gate.

If a figure contains more than one control wire, then numbers are
indicating the wire's qubit's argument position are drawn instead,
starting with 1. 

E.g. CCNOT 3 1 2 would have a #\1 drawn on wire 3 and a #\2 drawn on
wire 1."
  (loop
    :for figure :across row
    :for control := (figure-control-wires figure)
    :for ctl-positions
      := (loop :for wire :in control :collect (column-middle (elt columns wire)))
    :when (or control (figure-swap? figure))
      :do (multiple-value-bind (left-wire right-wire) (figure-wire-span figure)
            (loop
              :with only-one? := (= 1 (length control))
              :for idx :from (column-middle (elt columns left-wire))
                :upto (column-middle (elt columns right-wire))
              :when (find idx ctl-positions :test #'=)
                :do (setf (elt line idx)
                          (if only-one?
                              (control-char)
                              (elt (princ-to-string (1+ (position idx ctl-positions :test #'=))) 0)))
              :else
                :do (setf (elt line idx) (horizontal))))))

(defun write-unboxed-names (columns row line)
  "Unboxed names are special characters representing SWAPed qubits or
the target of CNOTs. These are drawn onto the LINE at the corret wire
positions (i.e. the centers of corresponding columns)."
  (loop :for figure :across row
        :unless (figure-boxed? figure)
          :do (let* ((name (figure-name figure))
                     (len/2 (ceiling (length name) 2)))
                (loop :for wire :in  (figure-target-wires figure)
                      :for wire-pos := (column-middle (elt columns wire))
                      :do (replace line name :start1 (1+ (- wire-pos len/2)))))))

(defun write-boxed-names (columns row line &key (gap 2))
  "Boxed names are drawn into LINE so that they appearn in the centers
of the boxes that contain them.

COLUMNS is a sequence of COLUMN instances, ROW is a vector of FIGURE
instances."
  (loop
    :with centers := (loop :for col :in columns :collect (column-middle col))
    :for figure :across row
    :when (figure-boxed? figure)
      :do (multiple-value-bind (left right) (box-bounds figure columns gap)
            (clear-line line :from left :below right)
            (setf (elt line left) (vertical)
                  (elt line right) (vertical))
            (replace line (figure-name figure)
                     :start1 (- (+ left (floor (- right left) 2))
                                (floor (length (figure-name figure)) 2))))))

(defun write-row-content (columns row line)
  "The row content line is drawn."
  ;; we clear the line and process the row by drawing successive
  ;; layers. Some passes my overwrite the content of previous passes,
  ;; hence, order is significant.
  (clear-line line)
  (write-wires-to-line line columns)
  (write-control-links columns row line)
  (write-unboxed-names columns row line)
  (write-boxed-names columns row line))

(defun print-row (diagram row &key (stream *standard-output*) line columns)
  "Print a row to the stream.  A row will print as 4 lines:

1. A line of wires.

2. A line containing the top of a row (i.e. box tops).

3. A line containing the content of a row: control links, operation names

4. A line containing the bottom of a row (i.e. box bottoms).

DIAGRAM is a diagram instance, ROW is a vector of FIGURES, one of the
rows in DIAGRAM's ROWS slot.

LINE is a vector of characters - the width of the diagram. One will be
created if necessary.

COLUMNS is a sequence of column bounds. This list will be generated if
necessary."
  (let ((*standard-output* stream)
        (line (if line line (make-string (diagram-width diagram) :initial-element #\Space)))
        (columns (or columns (diagram-column-bounds diagram)))
        (gap (diagram-column-gap diagram)))
    (clear-line line)
    (write-wires-to-line line columns)
    (princ line)
    (terpri)
    (write-around-row columns row line :top? t :gap gap)
    (princ line)
    (terpri)
    (write-row-content columns row line)
    (princ line)
    (terpri)
    (write-around-row columns row line :top? nil :gap gap)
    (princ line)
    (terpri)))

(defun write-wire-labels (columns line)
  "Draw a line of column labels at the center of each column: integers
naming each qubit."
  (loop :for col :in columns
        :for num :from 0
        :do (replace line (princ-to-string num) :start1 (column-middle col))))

(defun write-wire-label-bottoms (columns line)
  "Draw the (BOTTOM-MEET) character at the center of each column."
  (loop :for col :in columns
        :for num :from 0
        :do (setf (elt line (column-middle col)) (bottom-meet))))

(defun print-diagram (diagram &optional (stream *standard-output*))
  "Prints the DIAGRAM to the stream."
  (let ((*standard-output* stream)
        (line (make-string (diagram-width diagram) :initial-element #\Space))
        (columns (diagram-column-bounds diagram)))
    (write-wire-labels columns line)
    (princ line)
    (terpri)
    (write-wire-label-bottoms columns line)
    (princ line)
    (terpri)
    (loop :for row :across (diagram-rows diagram)
          :do (print-row diagram row :line line :columns columns))))

(defun print-program-diagram (program &key (stream *standard-output*) (charmap +box-drawings-charmap+ ))
  "Given a PARSED-PROGRAM instance, create a diagram and print it to the
STREAM. You may characters used to print the diagram by passing a
custom CHARMAP."
  (let ((*charmap* charmap)) 
    (print-diagram (make-diagram program) stream)))
