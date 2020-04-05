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

(define-parser *quil-parser*
  (:start-symbol expression)
  (:terminals (name integer complex semicolon newline left-paren right-paren comma expt times divide plus minus))
  (:precedence ((:right expt)
                (:left times divide)
                (:left plus minus)))

  (expression
   (gate-application separator expression (lambda (a b c) (list a c)))
   (gate-application separator (picker 0))
   gate-application)

  (gate-application
   (name params-opt qubits
         (lambda (name params qubits)
           (apply #'quil::build-gate name
                  (alexandria:ensure-list params)
                  (alexandria:ensure-list qubits)))))

  (params-opt
   (left-paren param-value-maybe-comma right-paren (picker 1))
   (left-paren right-paren (picker))
   ())
  
  (param-value-maybe-comma
   (param-value comma param-value-maybe-comma (picker 0 2))
   param-value)
  
  (param-value number)

  (qubits
   (qubit qubits (picker 0 1))
   qubit)

  (qubit integer)

  (number integer complex)

  (separator
   (semicolon separator (picker 0))
   (newline separator (picker 0))
   semicolon
   newline))

(defun make-rz-quil-string (length)
  ;; (with-output-to-string (s)
  ;;   (quil:print-parsed-program
  ;;    (cl-quil-benchmarking::random-1q-program
  ;;     0 length :instruction-generators (list #'cl-quil-benchmarking::native-rz))
  ;;    s))
  (with-output-to-string (s)
    (loop :repeat length :do
      (format s "RZ(1.0) 0; "))))

(define-string-lexer quil-lexer
  ("\\#[^\\n\\r]*" nil)
  ("(\\r\\n?|\\n)" (return (values 'newline nil)))
  ("\\;" (return (values 'semicolon nil)))
  ("\\(" (return (values 'left-paren nil)))
  ("\\)" (return (values 'right-paren nil)))
  ("\\," (return (values 'comma nil)))

  ("\\*" (return (values 'times  nil)))
  ("\\/" (return (values 'divide nil)))
  ("\\*" (return (values 'plus   nil)))
  ("\\*" (return (values 'minus  nil)))
  ("\\^" (return (values 'expt   nil)))
  
  ("(\\d*[.eE])(\\.?\\d)\\d*\\.?\\d*([eE][+-]?\\d+)?"
   (return (values 'complex (quil::parse-complex $@ nil))))
  ("\\d+" (return (values 'integer (quil::parse-integer $@))))
  ("[A-Za-z_]([A-Za-z0-9_\\-]*[A-Za-z0-9_])?"
   (return
     (cond
       ((string= "pi" $@) (values 'complex (constant pi)))
       ((string= "i" $@) (values 'complex (constant #C(0.0d0 1.0d0))))
       (t (values 'name $@))))))

(setf things (quil-lexer "RX 0"))
(parse-with-lexer things *quil-parser*)
(setf quilstr (make-rz-quil-string 100000))
(funcall things)
(time (progn (flatten (parse-with-lexer (quil-lexer quilstr) *quil-parser*)) 1))
(time (progn (parse-with-lexer (quil-lexer quilstr) *quil-parser*) 1))
(time (progn (quil::parse-quil-into-raw-program quilstr) 1))

(parse-with-lexer (quil-lexer "RX(0.2, 0.1) 1;") *quil-parser*)

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
