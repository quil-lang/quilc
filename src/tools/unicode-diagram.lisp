;;;; unicode-diagram.lisp

(defpackage #:cl-quil.tools.unicode-diagram
  (:use #:cl)
  (:export #:print-program-diagram))

(in-package #:cl-quil.tools.unicode-diagram)


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

(defvar *charmap* (make-charmap))

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


(defstruct figure
  name
  boxed?
  swap?
  control-wires
  target-wires)

(defclass diagram ()
  ((wire-count
    :accessor diagram-wire-count
    :initarg :wire-count
    :initform (error "required")
    :type integer)
   (column-widths
    :accessor diagram-column-widths
    :type vector)
   (gap
    :accessor diagram-column-gap
    :initarg :gap
    :initform 2)
   (rows
    :accessor diagram-rows
    :type vector)))

(defmethod initialize-instance :after ((diagram diagram) &key wire-count &allow-other-keys)
  (setf (diagram-column-widths diagram) (make-array wire-count :initial-element 1))
  (setf (diagram-rows diagram) (make-array 0 :adjustable t :fill-pointer 0)))

(defun diagram-width (diagram)
  (+ 1 (* (diagram-column-gap diagram) (diagram-wire-count diagram))
       (reduce #'+ (diagram-column-widths diagram))))

(defstruct column
  start
  middle
  stop
  width)

(defun diagram-column-bounds (diagram)
  (with-slots (column-widths gap) diagram
    (loop :with pos := gap
          :for col :from 0
          :for width :across column-widths
          :for mid := (+ pos (ceiling width 2))
          :for stop := (+ pos width) 
          :collect (make-column
                    :start pos
                    :middle mid
                    :stop stop
                    :width width)
          :do (incf pos (+ width gap)))))

(defun instruction-figure-name (instr)
  "Return two values, the string name to use for this instruction and a
boolean indicationg whether or not to draw a box around the name."
  (typecase instr
    (cl-quil::reset-qubit (values "߀" t))
    (cl-quil::measure-discard (values "MEAS" t))
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
  (if (typep instr 'cl-quil::application)
      (control-depth (cl-quil:application-operator instr))
      0))

(defun partition-at (n xs)
  (loop :for (x . more) :on xs
        :repeat n
        :collect x :into front
        :finally (return (values front (cons x more)))))

(defun control-and-target (instr)
  (partition-at (num-control-qubits instr) (cl-quil:qubits-used instr)))


(defun new-figure (instruction)
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
  (let ((widths (diagram-column-widths diagram)))
    (with-slots (name target-wires boxed?) figure
      (let ((size (+ (if boxed? 2 0) (length name))))
        (loop :for wire :in target-wires
              :do (setf (elt widths wire) (max (elt widths wire) size)))))))

(defun add-new-row (diagram)
  (with-slots (rows wire-count) diagram
    (let ((new-row (make-array 0 :adjustable t :fill-pointer 0)))
      (vector-push-extend new-row rows)
      new-row)))

(defun get-last-row (diagram)
  (with-slots (rows) diagram
    (if (plusp (length rows))
        (elt rows (1- (length rows)))
        (add-new-row diagram))))

(defun figure-wires (figure)
  (append (figure-control-wires figure)
          (figure-target-wires figure)))

(defun figure-wire-span (figure)
  (values (reduce #'min (figure-wires figure))
          (reduce #'max (figure-wires figure))))

(defun figures-intersect-p (f1 f2)
  (multiple-value-bind (left1 right1) (figure-wire-span f1)
    (multiple-value-bind (left2 right2) (figure-wire-span f2)
      (or (<= left1 left2 right1)
          (<= left1 right2 right1)
          (<= left2 left1 right2)
          (<= left2 right1 right2)))))

(defun row-has-room-for (row figure)
  (loop :for f :across row
        :never (figures-intersect-p f figure)))

(defun append-figure-to-row (row figure)
  (vector-push-extend figure row))

(defun add-instruction (diagram instr)
  (let ((figure (new-figure instr)))
    (when figure
      (update-column-widths diagram figure)
      (let ((last-row (get-last-row diagram)))
        (if (row-has-room-for last-row figure)
            (append-figure-to-row last-row figure)
            (let ((new-row (add-new-row diagram)))
              (append-figure-to-row new-row figure)))))))

(defun make-diagram (program)
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

(defun clear-line (line &key (char #\space) (from 0) (below (length line)))
  (loop :for idx :from from  :below below :do (setf (elt line idx) char)))

(defun write-wires-to-line (line columns)
  (loop :for col :in columns
        :do (setf (elt line (column-middle col)) (vertical))))

(defun write-box-top-to-line (line left right meets)
  (setf (elt line left) (top-left)
        (elt line right) (top-right))
  (loop :for idx :from (1+ left) :below right
        :do (setf (elt line idx) (horizontal)))
  (loop :for idx :in meets
        :do (setf (elt line idx) (top-meet))))

(defun write-box-bottom-to-line (line left right meets)
  (setf (elt line left) (bottom-left)
        (elt line right) (bottom-right))
  (loop :for idx :from (1+ left) :below right
        :do (setf (elt line idx) (horizontal)))
  (loop :for idx :in meets
        :do (setf (elt line idx) (bottom-meet))))

(defun box-bounds (figure columns gap)
  "Returns 3 values: LEFT RIGHT MEET-POSITIONS"
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
  (clear-line line)
  (write-wires-to-line line columns)
  (loop :for figure :across row
        :when (figure-boxed? figure)
          :do (multiple-value-bind (left right meets) (box-bounds figure columns gap)
                (if top?
                    (write-box-top-to-line line left right meets)
                    (write-box-bottom-to-line line left right meets)))))

(defun write-control-links (columns row line)
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
  (loop :for figure :across row
        :unless (figure-boxed? figure)
          :do (let* ((name (figure-name figure))
                     (len/2 (ceiling (length name) 2)))
                (loop :for wire :in  (figure-target-wires figure)
                      :for wire-pos := (column-middle (elt columns wire))
                      :do (replace line name :start1 (1+ (- wire-pos len/2)))))))

(defun write-boxed-names (columns row line &key (gap 2))
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
  (clear-line line)
  (write-wires-to-line line columns)
  (write-control-links columns row line)
  (write-unboxed-names columns row line)
  (write-boxed-names columns row line))

(defun print-row (diagram row &key (stream *standard-output*) line columns)
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
  (loop :for col :in columns
        :for num :from 0
        :do (replace line (princ-to-string num) :start1 (column-middle col))))

(defun write-wire-label-bottoms (columns line)
  (loop :for col :in columns
        :for num :from 0
        :do (setf (elt line (column-middle col)) (bottom-meet))))

(defun print-diagram (diagram &optional (stream *standard-output*))
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

(defun print-program-diagram (program &optional (stream *standard-output*))
  (print-diagram (make-diagram program) stream))
