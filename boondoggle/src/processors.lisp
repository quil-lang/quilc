;;;; processors.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:boondoggle)

(defclass processor ()
  ())

(defgeneric apply-process (processor &rest data)
  (:documentation "Applies the transformation embodied by PROCESSOR to DATA."))

(defclass processor-quilc (processor)
  ((executable-path :initform ""
                    :initarg :executable-path
                    :reader processor-quilc-executable-path)
   (flags :initform ""
          :initarg :flags
          :reader processor-quilc-flags)))

(defmethod apply-process ((processor processor-quilc) &rest data)
  (let ((data (first data)))
    (cl-quil::parse-quil
     (uiop:run-program (processor-quilc-executable-path processor)
                       :input (make-string-input-stream
                               (with-output-to-string (s)
                                 (cl-quil::print-parsed-program data s)))
                       :output :string))))

(defclass processor-identity (processor)
  ())

(defmethod apply-process ((processor processor-identity) &rest data)
  (first data))

(defclass processor-L1-distance (processor)
  ())

(defmethod apply-process ((processor processor-L1-distance) &rest data)
  "Calculate the L1 distance between two arguments. &rest expects only 2 positional arguments, each of which is expected to be a normalized histogram count of bitstring outputs from the QVM."
  (reduce #'+ (mapcar (a:compose #'abs #'-) (first data) (second data))))

(defclass processor-two-sample-chi-squared (processor)
  ())

(defmethod apply-process ((processor processor-two-sample-chi-squared) &rest data)
  "Calculate a chi-squared statistic between two samples. &rest expects only 2 positional arguments, each of which is expected to be a histogram count of bitstring outputs from the QVM. This implementation of the two-sample chi-squared test is taken from https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/chi2samp.htm, where the following variables are defined as:

nq: number of qubits
c: The total number of categories (bitstrings), 2^Nq.
s: Histogram of bitstring counts for the first sample. Value of (first data).
r: Histogram of bitstring counts for the second sample. Value of (second data).

c-filtered: Number of bitstrings (categories) where (si + ri) > 0
s-filtered: Subset of s histogram, keeping only entries where (si + ri) > 0
r-filtered: Subset of r histogram, keeping only entries where (si + ri) > 0

k1 = Normalization factor for different sample sizes (see ref.)
k2 = (/ 1 k1)

Returns the chi-squared statistic value, as well as the calculated degrees of freedom, in a list.
"
  (let* ((k1 (sqrt (/ (reduce #'+ (first data)) (reduce #'+ (second data)))))
         (k2 (/ k1)))
    (multiple-value-bind (s-filtered r-filtered c-filtered) (filter-lists-by-loop (first data) (second data))
      (list (reduce #'+ (mapcar (lambda (si ri) (expt (/ (- (* k1 ri) (* k2 si)) (+ ri si)) 2)) s-filtered r-filtered))
            c-filtered))))

(defun filter-lists-by-loop (list1 list2)
  "Take two lists as input, list1 and list2, and returns

1. list1 filtered to the elements s.t. li1 + li2 > 0.
2. list2 filtered to the elements s.t. li1 + li2 > 0.
3. The length of the filtered lists above.
"
  (loop :for li1 :in list1
        :for li2 :in list2
        :for yes := (plusp (+ li1 li2))
        :when yes :collect li1 :into list1-filtered
        :when yes :collect li2 :into list2-filtered
        :finally (return (values list1-filtered list2-filtered (length list1-filtered)))))
