;;;; consumers.lisp
;;;;
;;;; Author: Eric Peterson

;;; The main job of this file is to provide interfaces to different
;;; execution environments for Quil, currently just the local QVM.  We
;;; provide a generic `consume-quil` method that takes a
;;; `quil::parsed-program` object and returns probability amplitudes
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
    (loop :for instr :across (quil::parsed-program-executable-code parsed-program)
          :when (typep instr 'quil::measure)
            :collect (quil::address-value
                      (quil::measure-address
                       instr))))))

(defmethod consume-quil ((consumer consumer-local-qvm) parsed-program)
  "Runs a Quil program on the local QVM."
  (case (consumer-local-qvm-interface-mode consumer)
    (:http
     (let* ((quil-instructions
              (with-output-to-string (s)
                (quil::print-parsed-program parsed-program s)))
            (classical-addresses
              (get-consumer-registers consumer parsed-program))
            (qvm-payload (yason:encode
                          (alexandria:plist-hash-table
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
            (counts
              (loop :with ret := (make-list (expt 2 (length classical-addresses)) :initial-element 0)
                    :for result :in (yason:parse qvm-response)
                    :do (incf (nth
                               (let ((n 0))
                                 (dolist (x result)
                                   (setf n (+ x (* 2 n))))
                                 n)
                               ret))
                    :finally (return ret))))
       (mapcar (lambda (n) (coerce (/ n (consumer-local-qvm-trials consumer)) 'double-float)) counts)))
    (:shared-memory
     (error "Shared memory mode not implemented."))))

