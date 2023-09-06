
;  Common Lisp CLOS utilities
;
;  Written by:
;        Blake McBride
;        blake@mcbridemail.com
;
;  Version / Release 9/5/2023
;
;  Donated to the public domain.


(defmacro set-slot (i s v)
  `(setf (slot-value ,i ',s) ,v))

(defmacro get-slot (i s)
  `(slot-value ,i ',s))

(defclass metaclass (standard-class) ())

#+sbcl           (defmethod sb-mop:validate-superclass ((class metaclass) (superclass standard-class)) t)
#+(or cmu gcl)   (defmethod pcl:validate-superclass ((class metaclass) (superclass standard-class)) t)
#+abcl           (defmethod mop:validate-superclass ((class metaclass) (superclass standard-class)) t)
#+ccl            (defmethod ccl:validate-superclass ((class metaclass) (superclass standard-class)) t)
#+(or ecl mkcl)  (error "This system does not work in ECL or MKCL")

(defmacro define-class (class-name super-class-list class-variables instance-variables)
  "define-class defines a CLOS class with a parallel meta-class structure ala Smalltalk."
  (flet ((add-meta (sym)
		   (intern (concatenate 'string "META-" (symbol-name sym))))
	 (make-global (x)
		      (if (consp x)
			  (append x '(:allocation :class))
			(cons x '(:allocation :class)))))
	`(progn
	   (defclass ,(add-meta class-name)
	     ,(if (null super-class-list)
		  '(metaclass)
		(mapcar #'add-meta super-class-list))
	     ,(mapcar #'make-global class-variables)
	     (:metaclass metaclass))
	   (defclass ,class-name
	     ,(if (null super-class-list)
		  '(standard-object)
		super-class-list)
	     ,instance-variables
	     (:metaclass ,(add-meta class-name)))
	   (defvar ,class-name (find-class ',class-name))
;          (defvar ,(add-meta class-name) (find-class ',(add-meta class-name)))
	   )))

(defmacro define-method (method-name class-name arg-list &rest body)
  "define-method defines a fixed argument method and associates it to a variable argument generic.
   This allows the same method name in different classes to have a different number of fixed arguments."
  (let ((alist (gensym)))
    `(defmethod ,method-name ((self ,class-name) &rest ,alist)
       (apply #'(lambda (self ,@arg-list) ,@body)
	      (cons self ,alist)))))


