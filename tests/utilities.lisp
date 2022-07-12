;;;; utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil-tests)

(define-condition retries-exceeded-error (error)
  ((condition :initarg :condition
              :reader retries-exceeded-error-condition)
   (retries :initarg :retries
            :reader retries-exceeded-error-retries))
  (:report (lambda (c s)
             (format s "After ~D retr~:@P, the condition:
    ~A
is still being signalled."
                     (retries-exceeded-error-retries c)
                     (retries-exceeded-error-condition c)))))
(defmacro with-retries (condition-name (&key (retries 3)) &body body)
  "Retry executing body up to RETRIES number of times (default 3, evaluated at run-time), tolerating CONDITION-NAME being signalled."
  (check-type condition-name symbol)
  (alexandria:with-gensyms (retries-body-fn c TRY-AGAIN num-retries retries-left)
    ;; We create a retries function for easier debuggability and nicer
    ;; macro-expansions.
    `(flet ((,retries-body-fn ()
              ,@body))
       (declare (dynamic-extent (function ,retries-body-fn)))
       (let* ((,num-retries ,retries)
              (,retries-left ,num-retries))
         (assert (typep ,num-retries 'unsigned-byte)
                 ()
                 "The RETRIES argument to WITH-RETRIES did not produce a non-negative integer.")
         (tagbody
            ,TRY-AGAIN
            (handler-case (,retries-body-fn)
              (,condition-name (,c)
                (cond
                  ;; We've exhausted our retries, so we just signal
                  ;; the error again.
                  ((zerop ,retries-left)
                   (error 'retries-exceeded-error :condition ,c
                                                  :retries ,num-retries))
                  (t
                   (decf ,retries-left)
                   (go ,TRY-AGAIN))))))))))

(defun matrix-mismatch (m u &key (test #'cl-quil::double~))
  "Return a LIST of the (ROW COL) indices of the first mismatch between M and U.

If M and U are equal under TEST, return NIL."
  (check-type m magicl:matrix)
  (check-type u magicl:matrix)
  (assert (and (= (magicl:nrows m) (magicl:nrows u))
               (= (magicl:ncols m) (magicl:ncols u))))
  (dotimes (r (magicl:nrows m))
    (dotimes (c (magicl:ncols m))
      (unless (funcall test (magicl:tref m r c) (magicl:tref u r c))
        (return-from matrix-mismatch (list r c))))))

(defun %print-matrix-mismatch (m u &key (test #'cl-quil::double~)
                                        stream)
  (a:if-let (mismatch-position (matrix-mismatch m u :test test))
    (format stream
            "First mismatch at position: ~A~@
             First matrix:~%~A~@
             Second matrix:~%~A"
            mismatch-position
            m u)
    (format stream "No matrix mismatch found.")))

(defun matrix-mismatch-fmt (stream arguments &optional colon-modifier at-modifier)
  "Like the function %PRINT-MATRIX-MISMATCH, but is compatible with format strings using the ~/.../ directive.

This function is intended to be used in FIASCO assertion error messages. FIASCO evaluates format
args regardless of whether a test passes or fails, but FORMAT is only called to print the error
message on failure. Thus, using MATRIX-MISMATCH-FMT in your FIASCO format control string allows you
to only compute the MATRIX-MISMATCH for failing tests, which is usually what you want.

For example,

    (format t \"~/cl-quil-tests::matrix-mismatch-fmt/\" (list matrix-a matrix-b))
    (format t \"~/cl-quil-tests::matrix-mismatch-fmt/\" (list matrix-a matrix-b #'test-function))
"
  (declare (ignore colon-modifier at-modifier))
  (destructuring-bind (m u &optional test) arguments
    (if (null test)
        (%print-matrix-mismatch m u :stream stream)
        (%print-matrix-mismatch m u :stream stream :test test))))

(defun fiasco-assert-matrices-are-equal (m u)
  (is (= (magicl:nrows u) (magicl:nrows m)))
  (is (= (magicl:ncols u) (magicl:ncols m)))
  (setf u (cl-quil::scale-out-matrix-phases u m))
  (flet ((test~ (a b)
           (< (abs (- a b)) 0.01)))
    (is (loop :for i :below (magicl:nrows m) :always
          (loop :for j :below (magicl:ncols m) :always
            (test~ (magicl:tref m i j) (magicl:tref u i j))))
        ;; FIASCO:IS always evaluates it's format arguments, even if the test assertion succeeds.
        ;; Formatting via MATRIX-MISMATCH-FMT will only compute the MATRIX-MISMATCH when/if the
        ;; above assertion actually fails.
        "Matrix comparison failed. ~/cl-quil-tests::matrix-mismatch-fmt/"
        (list m u #'test~))))

(defun build-anonymous-gate (matrix &rest qubit-indices)
  (make-instance 'cl-quil::gate-application
                 :operator (cl-quil::named-operator "TEST")
                 :arguments (mapcar #'cl-quil::qubit qubit-indices)
                 :gate matrix))

(defun join-strings (strings &key (delimiter #\Newline) prefix-p suffix-p)
  "Join a sequence of STRINGS on the character or string DELIMITER.

If PREFIX-P is non-nil, prefix the returned string with DELIMITER.

If SUFFIX-P is non-nil, suffix the returned string with DELIMITER."
  (check-type strings cl-quil::string-sequence)
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
;;; and therefore not included in the input/output text returned for that section. Thus, if you want
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
;;;     (#S(CL-QUIL-TESTS::GOLDEN-TEST-CASE
;;;         :FILE #1="no-trailing-newline.txt"
;;;         :LINE 1
;;;         :INPUT "input 1"
;;;         :OUTPUT "output 1")
;;;      #S(CL-QUIL-TESTS::GOLDEN-TEST-CASE
;;;         :FILE #1#
;;;         :LINE 5
;;;         :INPUT "input 2"
;;;         :OUTPUT "output 2"))
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
;;;     (#S(CL-QUIL-TESTS::GOLDEN-TEST-CASE
;;;         :FILE #1="with-trailing-newlines.txt"
;;;         :LINE 1
;;;         :INPUT "input 1\n"
;;;         :OUTPUT "output 1\n")
;;;      #S(CL-QUIL-TESTS::GOLDEN-TEST-CASE
;;;         :FILE #1#
;;;         :LINE 7
;;;         :INPUT "input 2"
;;;         :OUTPUT "output 2\n"))
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
;;; ADDING NEW INPUT/OUTPUT PAIRS
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
;;; "# Output" is required; otherwise, UPDATE-GOLDEN-FILE-OUTPUT-SECTIONS will fail when attempting
;;; to parse the file.

(define-condition golden-file-parse-error (a:simple-parse-error)
  ((line-number
    :initarg :line-number
    :initform 0
    :type unsigned-byte
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
             (a:when-let ((bad-text (golden-file-parse-error-bad-text condition)))
               (format stream "~&Invalid text: ~S" bad-text))
             (a:when-let ((format-control (simple-condition-format-control condition)))
               (apply #'format stream
                      (concatenate 'string "~&" format-control)
                      (simple-condition-format-arguments condition)))))
  (:documentation "An error that occurred while parsing a golden file."))

(defstruct golden-test-case
  "GOLDEN-TEST-CASE represents a single input/output pair read from a golden file by PARSE-GOLDEN-FILE.

FILE is a STRING representation of the file this test case was parsed from.

LINE is the line number in FILE where the given INPUT section begins.

INPUT is the text from this test case's Input section.

OUTPUT is the text from this test case's Output section."
  (file "" :type string)
  (line 0 :type unsigned-byte)
  (input "" :type string)
  (output "" :type string))

(defun parse-golden-file-stream (stream)
  "Parse a \"golden\" file from STREAM and return a list of GOLDEN-TEST-CASEs.

STREAM is an input stream and both INPUTS and OUTPUTS are lists of strings.

A \"golden\" file is a file that contains one or more alternating input and output sections, where
each input section contains text meant to be passed to some function under test, and each output
section corresponds to the expected output for the preceding input. Input sections start with a line
containing the string \"# Input\" (which is discarded) and likewise the output sections begin with
\"# Output\". Any text in between, including blank lines, is collected in a GOLDEN-TEST-CASE's INPUT
and OUTPUT slots, respectively."
  (assert (input-stream-p stream))
  (flet ((parse-error (line-number line format-control &rest format-arguments)
           (error 'golden-file-parse-error
                  :line-number line-number
                  :bad-text line
                  :format-control format-control
                  :format-arguments format-arguments))
         (stream-file-name (stream)
           (a:if-let ((path (uiop:truename* stream)))
             (enough-namestring path)
             ;; If STREAM is not a FILE-STREAM, it won't have an a TRUENAME. Just format the STREAM.
             (format nil "~A" stream))))
    (loop :with state := ':START
          :with input-header := "# Input"
          :with output-header := "# Output"
          :with file-name := (stream-file-name stream)
          :with pending-lines := '()
          :with pending-test-case := nil
          :with test-cases := '()
          :for line-number :upfrom 1
          :for line := (read-line stream nil)
          :while line
          :do (labels ((new-test-case ()
                         (make-golden-test-case :file file-name :line line-number)))
                (ecase state
                  (:START
                   (unless (string= line "# Input")
                     (parse-error line-number line "Expected ~S" input-header))
                   (setf pending-test-case (new-test-case))
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
                          (ecase state
                            (:READING-INPUT
                             (setf (golden-test-case-input pending-test-case) new-section))
                            (:READING-OUTPUT
                             (setf (golden-test-case-output pending-test-case) new-section)
                             (push pending-test-case test-cases)
                             (setf pending-test-case (new-test-case)))))
                        (setf pending-lines '())
                        (setf state next-state))
                       (t
                        (push line pending-lines)))))))
          :finally (progn
                     (unless (eq state ':READING-OUTPUT)
                       (parse-error line-number
                                    nil
                                    "Golden file must end with an ~S section"
                                    output-header))
                     (setf (golden-test-case-output pending-test-case)
                           (join-strings (nreverse pending-lines)))
                     (push pending-test-case test-cases)
                     (return (nreverse test-cases))))))

(defun parse-golden-file (file-name)
  "Convenience wrapper around PARSE-GOLDEN-STREAM."
  (check-type file-name (or string pathname))
  (with-open-file (f file-name)
    (parse-golden-file-stream f)))

(defun map-golden-files-and-test-cases (test-case-function golden-files &optional (stream t))
  "Call TEST-CASE-FUNCTION on every GOLDEN-TEST-CASE found in the specified GOLDEN-FILES.

This is a convenience wrapper for writing golden file tests that handles iterating over the
GOLDEN-FILES and their GOLDEN-TEST-CASEs as well as reporting status about the current file and test
case on the optional STREAM argument.

Note that unlike standard MAP* functions, this function performs a nested, double iteration.
Roughly:

    (dolist (f golden-files)
      (dolist (t (parse-golden-file f))
        (funcall test-case-function t)))

TEST-CASE-FUNCTION is function that takes two arguments:

   1. a GOLDEN-TEST-CASE

   2. a STRING that contains information about the GOLDEN-TEST-CASE-FILE and GOLDEN-TEST-CASE-LINE.
   This string can be passed as the optional MESSAGE argument to FIASCO assertion macros to include
   information about where the failing GOLDEN-TEST-CASE is located.

GOLDEN-FILES is a LIST of PATHNAMEs indicating golden files to test.

STREAM is an optional OUTPUT-STREAM for writing debug messages indicating the current file and test
case being processed. STREAM defaults to T."
  (dolist (file golden-files)
    (format stream "~&    Testing golden file ~A at line:" (pathname-name file))
    (dolist (test-case (parse-golden-file file))
      (format stream " ~D" (golden-test-case-line test-case))
      (let ((*always-show-failed-sexp* t))
        (funcall test-case-function test-case (format nil "~&Golden test case at (file:line): ~A:~D"
                                                      (golden-test-case-file test-case)
                                                      (golden-test-case-line test-case)))))))

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
  (let ((file-paths-list (a:ensure-list file-paths)))
    (when (or skip-prompt
              (y-or-n-p
               "Are you sure you want to clobber all the output sections of the following files?~%~@
                ~{~A~%~}~%"
               file-paths-list))
      (dolist (file file-paths-list)
        (format t "~&Updating ~A" file)
        (a:write-string-into-file
         (join-strings (loop :for test-case :in (parse-golden-file file)
                             :for input := (golden-test-case-input test-case)
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
