;;;; server.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; all the hunchentoot junk goes in here.

(in-package #:quilc)

(defvar *app* nil)
(defparameter *server-port* 6000)
(defparameter *server-host* "0.0.0.0")
(defparameter *time-limit* 60)

(declaim (special *protoquil*))


(defmacro with-timeout (&body body)
  (let ((f (gensym "TIME-LIMITED-BODY-")))
    `(flet ((,f () ,@body))
       (declare (dynamic-extent (function ,f)))
       (if (null *time-limit*)
           (,f)
           (sb-ext:with-timeout *time-limit*
             (,f))))))

(defun session-info ()
  (if (or (not (boundp 'tbnl:*session*))
          (null tbnl:*session*))
      ""
      (format nil
              "[~A Session:~D] "
              (tbnl:session-remote-addr tbnl:*session*)
              (tbnl:session-id tbnl:*session*))))

(defun format-server-log (fmt-string &rest args)
  (cond
    ((boundp 'tbnl:*acceptor*)
     (apply #'tbnl:log-message* ':INFO
            (concatenate 'string (session-info) fmt-string)
            args))
    (t
     (format t "[~A] ~?" (tbnl::iso-time) fmt-string args)
     (terpri))))


(defclass vhost (tbnl:acceptor)
  ((dispatch-table
    :initform '()
    :accessor dispatch-table
    :documentation "List of dispatch functions"))
  (:default-initargs
   :address *server-host*
   :document-root nil
   :error-template-directory nil
   :persistent-connections-p t))


(defmethod tbnl:acceptor-status-message ((acceptor vhost) http-status-code &key error &allow-other-keys)
  (if (eql http-status-code tbnl:+http-internal-server-error+)
      error
      (call-next-method)))

(defun create-prefix/method-dispatcher (prefix method handler)
  (lambda (request)
    (and (eq method (tbnl:request-method request))
         (let ((mismatch (mismatch (tbnl:script-name request) prefix
                                   :test #'char=)))
           (and (or (null mismatch)
                    (>= mismatch (length prefix)))
                handler)))))

(defmethod tbnl:acceptor-dispatch-request ((vhost vhost) request)
  (mapc (lambda (dispatcher)
          (let ((handler (funcall dispatcher request)))
            (when handler
              (return-from tbnl:acceptor-dispatch-request (funcall handler request)))))
        (dispatch-table vhost))
  (call-next-method))

(defun start-server ()
  (format-server-log "Starting server: ~a : ~d.~%" *server-host* *server-port*)
  
  (setq tbnl:*show-lisp-errors-p* t
        tbnl:*show-lisp-backtraces-p* t
        tbnl:*catch-errors-p* t)
  (setq *app* (make-instance 'vhost
                             :address *server-host*
                             :port *server-port*
                             :taskmaster (make-instance 'tbnl:one-thread-per-connection-taskmaster)))
  (when (null (dispatch-table *app*))
    (push
     (create-prefix/method-dispatcher "/" ':POST #'handle-post-request)
     (dispatch-table *app*)))
  (tbnl:start *app*)
  ;; let the hunchentoot thread take over
  (loop (sleep 1)))

(defun stop-server ()
  (tbnl:stop *app*))

(defun handle-post-request (request)
  (when (null tbnl:*session*)
    (tbnl:start-session))
  
  (let* ((api-key (tbnl:header-in* ':X-API-KEY request))
         (user-id (tbnl:header-in* ':X-USER-ID request))
         (data (hunchentoot:raw-post-data :request request
                                          :force-text t))
         (json (yason:parse data)))
    (format-server-log "Processing request from API-key/user-ID: ~s / ~s~%" api-key user-id)
    ;; we expect to get the guts of a Canopy POST:
    ;; { type: string,
    ;;   addresses: array,
    ;;   trials: integer,
    ;;   quil-instructions: string,
    ;;   isa: string }
    ;; we decode what we need, but we keep the object around to pass through.
    (let* ((quil-instructions (or (gethash "uncompiled-quil" json)
                                  (gethash "quil-instructions" json)))
           (quil-program (quil::safely-parse-quil-string quil-instructions))
           (chip-specification (cl-quil::qpu-hash-table-to-chip-specification
                                (gethash "isa" json)))
           (*protoquil* t)
           (*statistics-dictionary* (process-program quil-program chip-specification)))

      ;; update the program with the compiled version
      (setf (gethash "compiled-quil" json)
            (gethash "processed_program" *statistics-dictionary*))
      ;; remove the compiled program from the metadata
      (remhash "processed_program" *statistics-dictionary*)
      ;; if we're in protoquil mode, update the readout addresses
      (when *protoquil*
        (let ((l2p (gethash "final-rewiring" *statistics-dictionary*)))
          (setf (gethash "addresses" json)
                (mapcar (lambda (index) (quil::apply-rewiring-l2p l2p index))
                        (gethash "addresses" json)))))
      ;; store the statistics alongside the return data
      (setf (gethash "metadata" json)
            *statistics-dictionary*)
      ;; finally, return the string-ified JSON
      (with-output-to-string (s)
        (yason:encode json s)))))
