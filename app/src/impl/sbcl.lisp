;;;; sbcl.lisp

(in-package #:quilc)

(defun disable-debugger ()
  (sb-ext:disable-debugger))

(defun enable-debugger ()
  (sb-ext:enable-debugger))

(deftype interactive-interrupt ()
  'sb-sys:interactive-interrupt)

(defun %zap-info (&key (purge-xref t))
  (sb-vm::map-allocated-objects
   (lambda (obj tag size)
     (declare (ignore size))
     (cond
       ((= tag sb-vm:code-header-widetag)
        (setf (sb-kernel:%code-debug-info obj) nil)
        (loop for fun = (sb-kernel:%code-entry-points obj)
                then (sb-kernel:%simple-fun-next fun)
              while fun
              do (cond
                   ((or purge-xref
                        (stringp (sb-kernel:%simple-fun-info fun)))
                    (setf (sb-kernel:%simple-fun-info fun) nil))
                   ((consp (sb-kernel:%simple-fun-info fun))
                    (setf (sb-kernel:%simple-fun-info fun)
                          (cdr (sb-kernel:%simple-fun-info fun)))))))
       ((= tag sb-vm:instance-widetag)
	(typecase obj
	  (method-combination
	   (setf (slot-value obj 'sb-pcl::%documentation) nil))
          (standard-method
           (setf (slot-value obj 'sb-pcl::%documentation) nil))
          (class
           (setf (slot-value obj 'sb-pcl::%documentation) nil))
          (sb-mop:standard-slot-definition
           (setf (slot-value obj 'sb-pcl::%documentation) nil))))
       ((and (= tag sb-vm:funcallable-instance-widetag)
             (typep obj 'generic-function))
        (setf (slot-value obj 'sb-pcl::%documentation) nil))))
   :dynamic)
  (sb-c::call-with-each-globaldb-name
   (lambda (name)
     (sb-int:clear-info :variable :documentation name)
     (sb-int:clear-info :type :documentation name)
     (sb-int:clear-info :type :source-location name)
     (sb-int:clear-info :typed-structure :documentation name)
     (sb-int:clear-info :setf :documentation name)
     (sb-int:clear-info :random-documentation :stuff name)
     (sb-int:clear-info :source-location :variable name)
     (sb-int:clear-info :source-location :constant name)
     (sb-int:clear-info :source-location :typed-structure name)
     (sb-int:clear-info :source-location :symbol-macro name)
     ;; Doesn't exist in SBCL 1.4.7
     #+#:ignore
     (sb-int:clear-info :source-location :vop name)
     (sb-int:clear-info :source-location :declaration name)
     (sb-int:clear-info :source-location :alien-type name)
     ;; Doesn't exist in SBCL 1.4.7
     #+#:ignore
     (unless (or (and (symbolp name)
                      (find (package-name (symbol-package name))
                            '("SB-KERNEL"
                              "SB-PCL"
                              "SB-IMPL")
                            :test #'string-equal))
                 (find name '((COMMON-LISP:SETF SB-PCL::CLOS-SLOTS-REF)
                              QVM::NAT-TUPLE-CARDINALITY
                              QVM::INJECT-BIT)
                       :test #'equalp))
       (sb-int:clear-info :function :inline-expansion-designator name))
     ;; TODO: package documentation
     ;;       package location
     )))

(defun zap-info ()
  (format t "~&Zapping info...~%")
  (%zap-info)
  (format t "~&Garbage collecting...~%")
  (sb-ext:gc :full t))
