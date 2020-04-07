(in-package :cl-quil)

(use-package :yacc)
(use-package :cl-lex)

;; Should this be a macro?
;; Careful. Here be conses.
(defun picker (&rest indices)
  (lambda (&rest args)
    (if indices
        (loop :for index :in indices :nconc (alexandria:ensure-list (elt args index)))
        nil)))

(declaim (inline binary-eval))
(defun binary-eval (head)
  (lambda (e1 op e2)
    (declare (ignore op))
    (if (and (numberp e1) (numberp e2))
        (eval (list head e1 e2))
        (list head e1 e2))))

(define-parser *quil-parser*
  (:start-symbol program)
  (:terminals (name integer complex semicolon colon newline left-paren right-paren left-bracket right-bracket comma expt times divide plus minus declare bit octet integer real sharing offset defgate indentation parameter))
  (:precedence ((:right expt)
                (:left times divide)
                (:left plus minus)))

  ;; TODO FIXME How to incorporate leading semicolons and newlines?
  ;; (i.e. those at the very beginning of a program) Maybe just do
  ;; that before lexing?
  (program
   quil
   (quil separator (lambda (a b) a))
   (quil separator program (lambda (a b c) (list a c))))

  (quil
   declare-mem
   gate-definition
   gate-application)
  
  (gate-definition
   (defgate name gate-definition-params colon newline gate-matrix
     (lambda (dg n p c nl gm)
       (let ((symbols (mapcar (a:compose #'intern #'param-name)
                              (flatten p))))
         (make-gate-definition n symbols gm)))))

  (gate-matrix
   (indentation gate-matrix-line newline gate-matrix (picker 1 3))
   ())

  (gate-matrix-line
   (param-expression #'list)
   (param-expression comma gate-matrix-line
                     (lambda (e c l) (append (list e) l))))

  (gate-definition-params
   (left-paren gate-definition-params-maybe-comma right-paren
               (lambda (a b c) (list b)))
   ())
  
  (gate-definition-params-maybe-comma
   parameter
   (parameter comma gate-definition-params-maybe-comma (picker 0 2)))

  (gate-application
   (name params-opt qubits
         (lambda (name params qubits)
           (make-instance 'unresolved-application
                          :operator (named-operator name)
                          :parameters (mapcar #'constant params)
                          :arguments (a:ensure-list qubits)))))

  (params-opt
   (left-paren param-expression-maybe-comma right-paren (picker 1))
   (left-paren right-paren (picker))
   ())

  (param-expression-maybe-comma
   param-expression
   (param-expression comma param-expression-maybe-comma
                     (lambda (a b c) (flatten (list a c)))))

  (param-expression
   (param-expression plus param-expression (binary-eval '+))
   (param-expression minus param-expression (binary-eval '-))
   (param-expression times param-expression (binary-eval '*))
   (param-expression divide param-expression (binary-eval '/))
   (param-expression expt param-expression (binary-eval 'expt))
   (left-paren param-expression right-paren (lambda (a b c) b))
   (name left-paren param-expression right-paren
         (lambda (n l p r)
           (declare (ignore l r))
           (let ((f (validate-function n)))
             (if (numberp p)
                 (eval (list f p))
                 (list f p)))))
   (parameter (lambda (p) (intern (param-name p))))
   (minus param-expression (lambda (m e)
                             (if (numberp e)
                                 (- e)
                                 (list '- e))))
   (number #'constant-value))
  
  ;; TODO Store names.
  ;; TODO Check name isn't already defined.
  (declare-mem
   (declare name vector-type vector-size
            (lambda (d n vt vs)
              (make-memory-descriptor
               :name n
               :type (parse-quil-type vt)
               :length (if vs (constant-value vs) 1))))
   ;; TODO Can this be folded into the above?
   ;; TODO Support multiple offsets
   (declare name vector-type vector-size sharing name offset integer vector-type
            (lambda (d n vt vs s sn o so sot)
              (make-memory-descriptor
               :name n
               :type (parse-quil-type vt)
               :length (if vs (constant-value vs) 1)
               :sharing-parent sn
               :sharing-offset-alist (list (cons (parse-quil-type sot) (constant-value so)))))))

  (vector-size
   (left-bracket integer right-bracket (lambda (a b c) b))
   ())

  (vector-type bit octet integer real)

  (qubits
   (qubit qubits (lambda (q qs) (list (qubit (constant-value q)) qs)))
   (qubit (lambda (q) (qubit (constant-value q)))))

  (qubit integer)

  (number integer complex)

  (separator?
   separator
   nil)

  (separator
   (semicolon separator (picker 0))
   (newline separator (picker 0))
   semicolon
   newline))

(defun yparse-quil (quil)
  (let ((raw-quil (flatten
                   (a:ensure-list
                    (parse-with-lexer (quil-lexer quil)
                                      *quil-parser*)))))
    (let ((pp (resolve-objects (raw-quil-to-unresolved-program raw-quil))))
      (dolist (xform *standard-post-process-transforms* pp)
        (setf pp (transform xform pp))))))

(defun make-rz-quil-string (length)
  (with-output-to-string (s)
    (loop :repeat length :do
      (format s "RZ(3*pi + 3/(2-4*pi^3)) 0; "))))

(define-string-lexer quil-lexer
  ("\\#[^\\n\\r]*" nil)
  ("(\\r\\n?|\\n)" (return (values 'newline nil)))
  ("\\;" (return (values 'semicolon nil)))
  ("\\:" (return (values 'colon nil)))
  ("\\(" (return (values 'left-paren nil)))
  ("\\)" (return (values 'right-paren nil)))
  ("\\[" (return (values 'left-bracket nil)))
  ("\\]" (return (values 'right-bracket nil)))
  ("\\," (return (values 'comma nil)))

  ("\\*" (return (values 'times  nil)))
  ("\\/" (return (values 'divide nil)))
  ("\\+" (return (values 'plus   nil)))
  ("\\-" (return (values 'minus  nil)))
  ("\\^" (return (values 'expt   nil)))

  ("DEFGATE|DECLARE|BIT|OCTET|INTEGER|REAL|SHARING|OFFSET"
   (return (values (intern $@) $@)))

  ("\\%([A-Za-z_]([A-Za-z0-9_\\-]*[A-Za-z0-9_])?)"
   (assert (not (null $1)))
   (return (values 'parameter (param $1))))
  
  ("(\\d*[.eE])(\\.?\\d)\\d*\\.?\\d*([eE][+-]?\\d+)?"
   (return (values 'complex (constant (quil::parse-complex $@ nil)))))
  ("\\d+" (return (values 'integer (constant (quil::parse-integer $@) quil-integer))))
  ("[A-Za-z_]([A-Za-z0-9_\\-]*[A-Za-z0-9_])?"
   (return
     (cond
       ((string= "pi" $@) (values 'complex (constant pi)))
       ((string= "i" $@) (values 'complex (constant #C(0.0d0 1.0d0))))
       (t (values 'name $@)))))

  ("(    |\\t)+" (return (values 'indentation (parse-indent-string $@))))
  ("[^\\S\\n\\r]+"
   nil))

;; (setf things (quil-lexer "RX 0"))
;; (parse-with-lexer things *quil-parser*)
;; (setf quilstr (make-rz-quil-string 100000))
;; (funcall things)
;; (time (progn (flatten (parse-with-lexer (quil-lexer quilstr) *quil-parser*)) 1))
;; (time (progn (parse-with-lexer (quil-lexer quilstr) *quil-parser*) 1))
;; (time (progn (quil::parse-quil-into-raw-program quilstr) 1))

;; (parse-with-lexer (quil-lexer "RX(0.2, 0.1) 1;") *quil-parser*)

;; Source: https://gist.github.com/westerp/cbc1b0434cc2b52e9b10
(defun flatten (lst &optional back acc)
  (loop
    (cond
      ((consp lst)
       (psetq lst (cdr lst)             ; parallel assignment
              back (cons (car lst) back)))
      (back
       (if (consp (car back))
           (psetq lst (cdar back)
                  back (cons (caar back) (cdr back)))
           (psetq acc (if (car back) (cons (car back) acc) acc)
                  back (cdr back))))
      (t
       (return acc)))))

(defun flatten-recursively (lst &aux (result '()))
  (labels ((rflatten (lst1)
             (dolist (el lst1 result)
               (if (listp el)
                   (rflatten el)
                   (push el result)))))
    (nreverse (rflatten lst))))
