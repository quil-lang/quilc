;;;; server.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; all the hunchentoot junk goes in here.

(in-package #:quilc)

(defvar *app* nil)
(defparameter *server-port* 6000)
(defparameter *server-host* "0.0.0.0")
(defparameter *time-limit* 60
  "Timeout in seconds.")

(declaim (special *protoquil*
                  *statistics-dictionary*))


(defmacro with-timeout (&body body)
  (let ((f (gensym "TIME-LIMITED-BODY-")))
    `(flet ((,f () ,@body))
       (declare (dynamic-extent (function ,f)))
       (if (null *time-limit*)
           (,f)
           (bt:with-timeout (*time-limit*)
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
      (with-output-to-string (s)
        (yason:encode
         (alexandria:plist-hash-table
          (list* "error_type" "quilc_error"
                 #-forest-sdk
                 (list
                  "status" error)
                 #+forest-sdk
                 nil))
         s))
      (call-next-method)))

(defun create-prefix/method-dispatcher (prefix method handler)
  "Return a function such that given a request, return the handler function HANDLER only when the METHOD (e.g., :POST) matches, and when the request has a URI that is *prefixed* by the string PREFIX."
  (check-type prefix string)
  (check-type method symbol)
  (check-type handler function)
  (lambda (request)
    (and (eq method (tbnl:request-method request))
         (let ((mismatch (mismatch (tbnl:script-name request) prefix
                                   :test #'char=)))
           (if (or (null mismatch)
                   (>= mismatch (length prefix)))
               handler
               nil)))))

(defun create-exact/method-dispatcher (exact-prefix method handler)
  "Return a function such that given a request, return the handler function HANDLER only when the METHOD (e.g., :POST) matches, and when the request has a URI that exactly matches the string EXACT-PREFIX."
  (check-type exact-prefix string)
  (check-type method symbol)
  (check-type handler function)  
  (lambda (request)
    (and (eq method (tbnl:request-method request))
         (if (string= exact-prefix (tbnl:script-name request))
             handler
             nil))))

(defmethod tbnl:acceptor-dispatch-request ((vhost vhost) request)
  (mapc (lambda (dispatcher)
          (let ((handler (funcall dispatcher request)))
            (when handler
              (return-from tbnl:acceptor-dispatch-request (funcall handler request)))))
        (dispatch-table vhost))
  (call-next-method))

(defun request-handler (fn)
  "Given a function FN that takes DATA, JSON, API-KEY, and USER-ID, produce a new function which takes a request and properly unpacks it, calling the function."
  (lambda (request)
    (when (null tbnl:*session*)
      (tbnl:start-session))
    (let* ((data (hunchentoot:raw-post-data :request request :force-text t))
           (json (yason:parse data))
           (api-key (tbnl:header-in* ':X-API-KEY request))
           (user-id (tbnl:header-in* ':X-USER-ID request)))
      (format-server-log "Processing request from API-key/user-ID: ~s / ~s~%" api-key
                         user-id)
      ;; we expect to get the guts of a Canopy POST: { type: string, addresses:
      ;; array, trials: integer, quil-instructions: string, isa: string } we decode
      ;; what we need, but we keep the object around to pass through.
      (with-timeout (funcall fn data json api-key user-id)))))

(defun start-web-server ()
  (format-server-log "Starting server: ~a : ~d." *server-host* *server-port*)

  (setq tbnl:*show-lisp-errors-p* t
        tbnl:*show-lisp-backtraces-p* t
        tbnl:*catch-errors-p* t)

  #+forest-sdk
  (setq tbnl:*log-lisp-backtraces-p* nil
        tbnl:*log-lisp-errors-p* nil
        tbnl:*show-lisp-errors-p* nil
        tbnl:*show-lisp-backtraces-p* nil
        tbnl:*catch-errors-p* t)
  (tbnl:reset-session-secret)

  (unless (zerop *time-limit*)
    (setq tbnl:*default-connection-timeout* (/ *time-limit* 1000)))
  (setq *app* (make-instance 'vhost
                             :address *server-host*
                             :port *server-port*
                             :taskmaster (make-instance 'tbnl:one-thread-per-connection-taskmaster)))
  (when (null (dispatch-table *app*))
    (flet ((handle (uri method function &key (requestify t))
             (push
              (create-exact/method-dispatcher uri method (if requestify
                                                             (request-handler function)
                                                             function))
              (dispatch-table *app*))))
      (handle "/"                   ':POST 'compiler-post)
      (handle "/rb"                 ':POST 'rb-post)
      (handle "/apply-clifford"     ':POST 'apply-clifford-post)
      (handle "/rewrite-arithmetic" ':POST 'rewrite-arithmetic-post)
      (handle "/version"            ':GET  'version-get :requestify nil)))
  (tbnl:start *app*)
  ;; let the hunchentoot thread take over
  (loop (sleep 1)))

(defun stop-web-server ()
  (tbnl:stop *app*))

(defun version-get (request)
  (when (null tbnl:*session*)
    (tbnl:start-session))
  (let* ((api-key (tbnl:header-in* ':X-API-KEY request))
         (user-id (tbnl:header-in* ':X-USER-ID request)))
    (format-server-log "Processing request from API-key/user-ID: ~s / ~s~%"
                       api-key user-id)
    (with-timeout
        (with-output-to-string (s)
          (yason:encode (alexandria:alist-hash-table
                         `(("cl-quil" . ,+CL-QUIL-VERSION+)
                           ("quilc"   . ,+QUILC-VERSION+)
                           ("githash" . ,+GIT-HASH+)))
                        s)))))

(defun check-payload-for-keys (payload &rest keys)
  "Check that the parsed JSON payload PAYLOAD has all of the keys KEYS present."
  (check-type payload hash-table "Received a JSON payload that wasn't a JSON object.")
  (dolist (key keys)
    (multiple-value-bind (value exists?) (gethash payload key)
      (declare (ignore value))
      (unless exists?
        (error "For this endpoint, expected JSON with the key ~S, ~
                but it was nowhere to be found."
               key)))))

(defun rewrite-arithmetic-post (data json api-key user-id)
  "Rewrites the request program without arithmetic in gate parameters.  Expects a JSON payload of the form

{
    \"program\": original program as a raw Quil string
},

and replies with the JSON

{
    \"quil\": the rewritten program, again as a Quil string,
    \"original_memory_descriptors\": list of memory region descriptions as strings containing DECLARE directives,
    \"recalculation_table\": dictionary with keys memory references and values text containing parameter expressions
}."
  (declare (ignore data api-key user-id))
  (check-payload-for-keys json "program")
  (let ((program (quil::parse-quil (gethash "program" json))))
    (multiple-value-bind (rewritten-program original-memory-descriptors recalculation-table)
        (cl-quil::rewrite-arithmetic program)
      (let (reformatted-omd
            (reformatted-rt (make-hash-table)))
        (setf reformatted-omd (mapcar (lambda (memory-defn)
                                        (with-output-to-string (s)
                                          (format s "DECLARE ~a ~a"
                                                  (quil::memory-descriptor-name memory-defn)
                                                  (quil::quil-type-string (quil::memory-descriptor-type memory-defn)))))
                                      original-memory-descriptors))
        (maphash (lambda (key val)
                   (setf (gethash (quil::print-instruction key nil) reformatted-rt)
                         (quil::print-instruction val nil)))
                 recalculation-table)
        (with-output-to-string (s)
          (yason:encode
           (alexandria:plist-hash-table (list "quil" (with-output-to-string (s)
                                                       (quil::print-parsed-program rewritten-program s))
                                              "original_memory_descriptors" reformatted-omd
                                              "recalculation_table" reformatted-rt))
           s))))))

(defun rb-post (data json api-key user-id)
  "Handle a post request for generating a randomized benchmarking sequence. The keys of JSON are:
 * \"depth\": integer representing the desired circuit depth.
 * \"qubits\": integer representing the number of qubits involved in the circuit.
 * \"gateset\", list of strings, each representing a Clifford gate as a Quil program."
  (declare (ignore data api-key user-id))
  (check-payload-for-keys "depth" "qubits" "gateset")
  (let* ((k (gethash "depth" json))
         (n (gethash "qubits" json))
         (gateset (gethash "gateset" json))
         (seed (gethash "seed" json))
         (interleaver (gethash "interleaver" json)))
    #-sbcl
    (when seed
      (error "Unable to seed the random number generator."))
    (when (and seed (not (typep seed 'unsigned-byte)))
      (error "Seed must be a positive integer."))
    (when (> n 2)
      (error "Currently no more than two qubit randomized benchmarking is supported."))
    (let* ((cliffords (mapcar #'quil.clifford::clifford-from-quil gateset))
           (qubits-used (mapcar (alexandria:compose
                                 (alexandria:curry #'reduce #'union)
                                 #'cl-quil.clifford::extract-qubits-used
                                 #'cl-quil:parse-quil-string)
                                gateset))
           (qubits-used-by-interleaver
             (when interleaver
               (reduce #'union
                       (cl-quil.clifford::extract-qubits-used
                        (cl-quil:parse-quil-string interleaver)))))
           (qubits (union qubits-used-by-interleaver (reduce #'union qubits-used)))
           (embedded-cliffords (loop :for clifford :in cliffords
                                     :for i :from 0
                                     :collect
                                     (quil.clifford:embed clifford n
                                                          (loop :for index :in (nth i qubits-used)
                                                                :collect (position index qubits)))))
           (embedded-interleaver
             (when interleaver
               (quil.clifford:embed (quil.clifford::clifford-from-quil interleaver)
                                    n
                                    (loop :for index :in qubits-used-by-interleaver
                                          :collect (position index qubits)))))
           (rb-sequence
             (let ((*random-state*
                     #+sbcl (if seed (sb-ext:seed-random-state seed) *random-state*)
                     #-sbcl *random-state*))
               (quil.clifford::rb-sequence k n embedded-cliffords embedded-interleaver)))
           (gateset-label-sequence
             (loop :for clifford-element :in rb-sequence
                   :collect (loop :for generator :in clifford-element
                                  :collect (position generator embedded-cliffords :test #'quil.clifford:clifford=)))))
      (with-output-to-string (s) (yason:encode gateset-label-sequence s)))))

(defun apply-clifford-post (data json api-key user-id)
  "Handle a json post request for conjugating an element of the Pauli group by an element of the Clifford group. The Clifford element is specified as a quil program represented as a STRING and the element of the Pauli group is represented as a LIST whose first element is a LIST of qubit indices and the second element is a LIST of STRINGS, representing the Pauli operator acting on that index. e.g. ((1 2) (\"X\" \"Y\")) is the Pauli element IXY. JSON should be a HASHTABLE with keys \"pauli\" and \"clifford\", with values described above."
  (declare (ignore data api-key user-id))
  (let* ((indices-and-terms (gethash "pauli" json))
         (clifford-program (gethash "clifford" json))
         (pauli-indices (first indices-and-terms))
         (pauli-terms (second indices-and-terms))
         (clifford-indices (sort (reduce #'union (cl-quil.clifford::extract-qubits-used (cl-quil:parse-quil-string clifford-program))) #'<))
         (qubits (sort (union (copy-seq pauli-indices) (copy-seq clifford-indices)) #'<))
	 (pauli (quil.clifford:pauli-from-string
		 (with-output-to-string (s)
                   (dolist (i qubits)
                     (cond ((member i pauli-indices)
                            (write-string (nth (position i pauli-indices) pauli-terms) s))
                           (T (write-string "I" s)))))))
         (clifford (cl-quil.clifford::embed (quil.clifford::clifford-from-quil clifford-program)
                                            (length qubits)
                                            (loop :for index :in clifford-indices :collect (position index qubits))))
         (pauli-out (quil.clifford:apply-clifford clifford pauli)))
    (with-output-to-string (s)
      (yason:encode (list (quil.clifford::phase-factor pauli-out)
                          (apply #'concatenate 'string
                                 (mapcar (alexandria:compose #'symbol-name #'quil.clifford::base4-to-sym)
                                         (quil.clifford::base4-list pauli-out))))
                    s))))

(defun compiler-post (data json api-key user-id)
  "Handle a post request for compiling a quil circuit."
  (declare (ignore data api-key user-id))
  (let* ((quil-instructions (or (gethash "uncompiled-quil" json)
                                (gethash "quil-instructions" json)
                                (error "compiler-post requires the JSON to ~
                                        have \"uncompiled-quil\" or ~
                                        \"quil-instructions\", but they ~
                                        were nowhere to be found.")))
         (quil-program (quil::parse-quil quil-instructions))
         (chip-specification (cl-quil::qpu-hash-table-to-chip-specification
                              (gethash "target-device" json)))
         (*statistics-dictionary* (process-program quil-program chip-specification)))

    ;; update the program with the compiled version
    (setf (gethash "compiled-quil" json)
          (gethash "processed_program" *statistics-dictionary*))
    ;; remove the compiled program from the metadata
    (remhash "processed_program" *statistics-dictionary*)
    ;; if we're in protoquil mode, update the readout addresses
    (when (and *protoquil*
               (string= "MULTISHOT_MEASURE" (gethash "type" json))
               (gethash "qubits" json))
      (let ((l2p (gethash "final-rewiring" *statistics-dictionary*)))
        (setf (gethash "qubits" json)
              (mapcar (lambda (index) (quil::apply-rewiring-l2p l2p index))
                      (gethash "qubits" json)))))
    ;; store the statistics alongside the return data
    (setf (gethash "metadata" json)
          *statistics-dictionary*)
    ;; finally, return the string-ified JSON
    (with-output-to-string (s)
      (yason:encode json s))))
