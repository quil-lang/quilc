;;;; consumers.lisp
;;;;
;;;; Author: Eric Peterson

;;; The main job of this file is to provide interfaces to different
;;; execution environments for Quil, currently just the local QVM.  We
;;; provide a generic `consume-quil` method that takes a
;;; `cl-quil::parsed-program` object and returns probability amplitudes
;;; as sterilized output.

(in-package #:boondoggle)

(defconstant +max-shots-through-canopy+ 10000)

(defgeneric consume-quil (consumer parsed-program)
  (:documentation
   "Runs PARSED-PROGRAM as a program on the Quil CONSUMER."))

(defclass consumer-local-qvm ()
  ((interface-mode :initform ':http
                   :initarg :interface-mode
                   :reader consumer-local-qvm-interface-mode)
   (url :initform "http://localhost:5000"
        :initarg :url
        :reader consumer-local-qvm-url)
   (trials :initform 1000
           :initarg :trials
           :reader consumer-local-qvm-trials)))

(defgeneric get-consumer-registers (consumer parsed-program)
  (:documentation "Returns a list of amplitude registered associated to a consumer.")
  (:method ((consumer consumer-local-qvm) parsed-program)
    (remove-duplicates
    (loop :for instr :across (cl-quil::parsed-program-executable-code parsed-program)
          :when (typep instr 'cl-quil::measure)
            :collect (cl-quil::memory-ref-position
                      (cl-quil::measure-address
                       instr))))))

(defmethod consume-quil ((consumer consumer-local-qvm) parsed-program)
  "This function runs a Quil program on the QVM, and returns a histogram of counts of each resulting bitstring ordered lexicographically.

For a program constructing a bell state with a large sampling count, the returned value would be (<large-number> 0 0 <large-number>)."
  (case (consumer-local-qvm-interface-mode consumer)
    (:http
     (let* ((quil-instructions
              (with-output-to-string (s)
                (cl-quil::print-parsed-program parsed-program s)))
            (classical-addresses
              (a:plist-hash-table
                (list "ro" (get-consumer-registers consumer parsed-program))))
            (qvm-payload (yason:encode
                          (a:plist-hash-table
                           (list "type" "multishot"
                                 "addresses" classical-addresses
                                 "trials" (consumer-local-qvm-trials consumer)
                                 "quil-instructions" quil-instructions))
                          (make-broadcast-stream)))
            (qvm-response
              (drakma:http-request (consumer-local-qvm-url consumer)
                                   :method ':post
                                   :content (with-output-to-string (s)
                                              (yason:encode qvm-payload s))
                                   :content-type "application/json; charset=utf-8"))
            (parsed-response (yason:parse qvm-response))
            (counts
              (loop :with ret := (make-list (expt 2 (length (gethash "ro" classical-addresses))) :initial-element 0)
                    :for result :in (gethash "ro" parsed-response)
                    :do (incf (nth
                               (let ((n 0))
                                 (dolist (x result)
                                   (setf n (+ x (* 2 n))))
                                 n)
                               ret))
                    :finally (return ret))))
       counts
       ))
    (:shared-memory
     (error "Shared memory mode not implemented."))))

