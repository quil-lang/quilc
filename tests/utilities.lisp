;;;; utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil-tests)

;;; Cribbed from QVM-TESTS
(defmacro with-output-to-quil (&body body)
  `(let ((quil:*allow-unresolved-applications* t))
     (quil:parse-quil
      (with-output-to-string (*standard-output*)
        ,@(loop :for form :in body
                :if (stringp form)
                  :collect `(write-line ,form)
                :else
                  :collect form)))))

(defun fiasco-assert-matrices-are-equal (m u)
  (is (= (magicl:matrix-rows u) (magicl:matrix-rows m)))
  (is (= (magicl:matrix-cols u) (magicl:matrix-cols m)))
  (setf u (quil::scale-out-matrix-phases u m))
  (is (loop :for i :below (magicl:matrix-rows m) :always
         (loop :for j :below (magicl:matrix-cols m) :always
            (< (abs (- (magicl:ref m i j) (magicl:ref u i j))) 0.01)))
      (with-output-to-string (s)
        (format s "Matrix comparison failed.~%Input matrix:")
        (magicl::pprint-matrix s m)
        (format s "~%Output matrix:~%")
        (magicl::pprint-matrix s u))))

(defun build-anonymous-gate (matrix &rest qubit-indices)
  (make-instance 'cl-quil::gate-application
                 :operator (cl-quil::named-operator "TEST")
                 :arguments (mapcar #'cl-quil::qubit qubit-indices)
                 :gate matrix))

(macrolet
    ((def (predicate-name container-type element-test)
       (check-type predicate-name symbol)
       (check-type container-type symbol)
       (check-type element-test symbol)
       (let ((docstring
               (format nil "Return true if OBJECT is a ~A and every element satisfies ~A."
                       container-type element-test)))
         `(defun ,predicate-name (object)
            ,docstring
            (and (typep object ',container-type)
                 (every #',element-test object))))))
  (def string-list-p list stringp)
  (def string-sequence-p sequence stringp))

(defun join-strings (strings &key (delimiter #\Newline) prefix-p suffix-p)
  "Join a sequence of STRINGS on the character or string DELIMITER.

If PREFIX-P is non-nil, prefix the returned string with DELIMITER.

If SUFFIX-P is non-nil, suffix the returned string with DELIMITER."
  (check-type strings (satisfies string-sequence-p))
  (check-type delimiter (or string character))
  (with-output-to-string (stream)
    (loop :with last := (length strings)
          :with delimiter-string := (string delimiter)
          :initially (and prefix-p (write-string delimiter-string stream))
          :for str :in (coerce strings 'list)
          :for i :upfrom 1
          :do (write-string str stream)
          :when (or (< i last) suffix-p)
            :do (write-string delimiter-string stream))))

;;;
;;; PARSING "GOLDEN" TEST FILES
;;;
;;; A golden file is a file that contains one or more alternating input and output sections, where
;;; each input section contains text meant to be passed to some function under test, and each output
;;; section corresponds to the expected output for the preceding input. Input sections start with a
;;; line containing the string "# Input" and likewise output sections begin with "# Output". For
;;; example:
;;;
;;; ----------- begin golden.txt ------------
;;; # Input
;;; DEFGATE TEST(%arg):
;;;     COS(%arg)^2, 0
;;;     0, SIN(%arg)^2
;;;
;;; TEST(0.5) 0
;;; # Output
;;; DEFGATE TEST(%arg):
;;;     (COS(%arg)^(2.0)), 0.0
;;;     0.0, (SIN(%arg)^(2.0))
;;;
;;;
;;; TEST(0.5) 0
;;;
;;; ----------- end golden.txt ------------
;;;
;;; In this example, both the input and output sections contain Quil code, but the golden file
;;; parser does not attribute any meaning to the contents of the sections. Any text between the
;;; section headers, including blank lines, is collected and returned.
;;;
;;; Golden files are useful for testing any function or system that can take text as input and
;;; produces text as output. See TEST-PRINT-PARSED-PROGRAM-GOLDEN-FILES in printer-tests.lisp for
;;; example usage.
;;;
;;;
;;; A NOTE ON TRAILING NEWLINES
;;;
;;; The final newline in an input or output section is considered part of the golden file syntax,
;;; not therefore not included in the input/output text returned for that section. Thus, if you want
;;; your input/output to include a trailing newline, then you need to add a trailing blank line to
;;; the section text. For example, given the following file:
;;;
;;; ----------- begin no-trailing-newline.txt ------------
;;; # Input
;;; Input 1
;;; # Output
;;; Output 1
;;; # Input
;;; Input 2
;;; # Output
;;; Output 2
;;; ----------- end no-trailing-newline.txt ------------
;;;
;;; calling (parse-golden-file "no-trailing-newline.txt") will return
;;;
;;;     (values ("Input 1" "Input 2") ("Output 1" "Output 2"))
;;;
;;; whereas parsing the file
;;;
;;; ----------- begin with-trailing-newlines.txt ------------
;;; # Input
;;; Input 1
;;;
;;; # Output
;;; Output 1
;;;
;;; # Input
;;; Input 2
;;; # Output
;;; Output 2
;;;
;;; ----------- end with-trailing-newlines.txt ------------
;;;
;;; will return (printf-style newline escapes for brevity, you get the idea)
;;;
;;;     (values ("Input 1\n" "Input 2") ("Output 1\n" "Output 2\n"))
;;;
;;;
;;; UPDATING GOLDEN FILES
;;;
;;; If you change the system under test such that it produces new or different output, you'll need
;;; to update the output sections of the associated golden files. The function
;;; UPDATE-GOLDEN-FILE-OUTPUT-SECTIONS does just that. See it's docstring for more info, or have a
;;; look at UPDATE-PRINT-PARSED-PROGRAM-GOLDEN-FILES in printer-tests.lisp for example usage.
;;;
;;; Of course, you are also free to update a golden file with your favorite text editor or by any
;;; other means. Just keep in mind the above comments about trailing newlines.
;;;
;;;
;;; ADDING NEW INPUT/OUTPUT pairs
;;;
;;; You can also use UPDATE-GOLDEN-FILE-OUTPUT-SECTIONS to add new outputs to a golden file without
;;; the need to copy/paste or fiddle with trailing-newline matching. For example, if you add the
;;; following to any golden file, either at the end or just after any existing output section:
;;;
;;;     # Input
;;;     My fresh new input test case
;;;     # Output
;;;
;;; then run
;;;
;;;     (update-golden-file-output-sections
;;;       "path/to/modified/golden/file"
;;;       #'some-function-that-generates-desired-output)
;;;
;;; then the previously-empty output section will be populated with the desired output. Note,
;;; however, that this will update ALL output sections in the file. In general, this is not a
;;; problem since the only sane way to use golden files is to assume that every input/output pair in
;;; the file is to be processed by the same output-generating function. Also note that the trailing
;;; "# Output" is required; otherwise, UPDATE-GOLDEN-FILE-OUTPUT-SECTIONS will complain that the
;;; number of input and output sections are not the same.

(define-condition golden-file-parse-error (alexandria:simple-parse-error)
  ((line-number
    :initarg :line-number
    :initform 0
    :type integer
    :reader golden-file-parse-error-line-number)
   (bad-text
    :initarg :bad-text
    :initform nil
    :type (or null string)
    :reader golden-file-parse-error-bad-text))
  (:report (lambda (condition stream)
             (format stream
                     "Error while parsing golden file at line ~D."
                     (golden-file-parse-error-line-number condition))
             (alexandria:when-let ((bad-text (golden-file-parse-error-bad-text condition)))
               (format stream "~&Invalid text: ~S" bad-text))
             (alexandria:when-let ((format-control (simple-condition-format-control condition)))
               (apply #'format stream
                      (concatenate 'string "~&" format-control)
                      (simple-condition-format-arguments condition)))))
  (:documentation "An error that occurred while parsing a golden file."))

(defun parse-golden-file-stream (stream)
  "Parse a \"golden\" file from STREAM and return (VALUES INPUTS OUTPUTS).

STREAM is an input stream and both INPUTS and OUTPUTS are lists of strings.

A \"golden\" file is a file that contains one or more alternating input and output sections, where
each input section contains text meant to be passed to some function under test, and each output
section corresponds to the expected output for the preceding input. Input sections start with a line
containing the string \"# Input\" (which is discarded) and likewise the output sections begin with
\"# Output\". Any text in between, including blank lines, is collected in the return values INPUTS
and OUTPUTS, respectively."
  (flet ((parse-error (line-number line format-control &rest format-arguments)
           (error 'golden-file-parse-error
                  :line-number line-number
                  :bad-text line
                  :format-control format-control
                  :format-arguments format-arguments)))
    (loop :with state := ':START
          :with input-header := "# Input"
          :with output-header := "# Output"
          :with pending-lines := '()
          :with inputs := '()
          :with outputs := '()
          :for line-number :upfrom 1
          :for line := (read-line stream nil)
          :while line
          :do (ecase state
                (:START
                 (unless (string= line "# Input")
                   (parse-error line-number line "Expected ~S" input-header))
                 (setf state ':READING-INPUT))
                ((:READING-INPUT :READING-OUTPUT)
                 (multiple-value-bind (bad-section next-section next-state)
                     (if (eq state ':READING-INPUT)
                         (values input-header output-header ':READING-OUTPUT)
                         (values output-header input-header ':READING-INPUT))
                   (cond
                     ((string= line bad-section)
                      (parse-error line-number line "Expected anything other than ~S" bad-section))
                     ((string= line next-section)
                      (let ((new-section (join-strings (nreverse pending-lines))))
                        (if (eq state ':READING-INPUT)
                            (push new-section inputs)
                            (push new-section outputs)))
                      (setf pending-lines '())
                      (setf state next-state))
                     (t
                      (push line pending-lines))))))
          :finally (progn
                     (unless (eq state ':READING-OUTPUT)
                       (parse-error line-number
                                    nil
                                    "Golden file must end with an ~S section"
                                    output-header))
                     (push (join-strings (nreverse pending-lines)) outputs)
                     (unless (= (length inputs) (length outputs))
                       (parse-error
                        line-number
                        nil
                        "Number of ~S sections (~D) does not match number of ~S sections (~D)."
                        input-header (length inputs)
                        output-header (length outputs)))
                     (return (values (nreverse inputs) (nreverse outputs)))))))

(defun parse-golden-file (file-name)
  "Convenience wrapper around PARSE-GOLDEN-STREAM."
  (check-type file-name (or string pathname))
  (with-open-file (f file-name)
    (parse-golden-file-stream f)))

(defun update-golden-file-output-sections (file-paths output-callback
                                           &key (if-exists ':supersede) skip-prompt)
  "Update all output sections of the golden files at FILE-PATHS.

If you call this function, examine the diffs of the old vs new output sections *carefully* before
committing the updated files, in order to ensure that all the changes are intended or expected.
Golden files are precious, and their sanctity must be preserved!

FILE-PATHS is either a single PATHNAME or NAMESTRING, or a list of them. All of the files should be
valid golden files that can be parsed by PARSE-GOLDEN-FILE.

OUTPUT-CALLBACK is function from STRING -> STRING. It will be called successively for each
golden-file input section, and should return the corresponding output string for the given input.

IF-EXISTS has the standard Common Lisp meaning. See http://l1sp.org/cl/open."
  (let ((file-paths-list (alexandria:ensure-list file-paths)))
    (when (or skip-prompt
              (y-or-n-p
               "Are you sure you want to clobber all the output sections of the following files?~%~@
                ~{~A~%~}~%"
               file-paths-list))
      (dolist (file file-paths-list)
        (format t "~&Updating ~A" file)
        (alexandria:write-string-into-file
         (join-strings (loop :for input :in (parse-golden-file file)
                             :for output := (funcall output-callback input)
                             :collect "# Input"
                             :collect input
                             :collect "# Output"
                             :collect output)
                       ;; Any trailing newline in the final output section is considered syntax and
                       ;; consumed by the parser, thus suffix the string with an additional newline
                       ;; here. This has the side benefit of mimicking Emacs' `require-final-newline'
                       ;; when the final output section does not end in a newline, thus preventing
                       ;; spurious diffs if someone visits the golden file in Emacs.
                       :suffix-p t)
         file
         :if-exists if-exists
         :if-does-not-exist ':error)))))
