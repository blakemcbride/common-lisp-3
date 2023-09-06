
;  Common Lisp CLOS utilities
;
;  Written by:
;        Blake McBride
;        blake@mcbridemail.com
;
;  Version / Release 9/6/2023
;

(defvar *class-instances* (make-hash-table :test #'eq))

(defmacro define-class (name super-class-list class-variables instance-variables)
  "A macro to define a class and its parallel hierarchy."
  (let ((parallel-class-name (intern (concatenate 'string "CLASS-" (string name))))
        (parallel-super-class-list (mapcar (lambda (super) 
                                             (intern (concatenate 'string "CLASS-" (string super))))
                                           super-class-list)))
    `(progn
       ;; Define the parallel class
       (defvar ,parallel-class-name (defclass ,parallel-class-name ,parallel-super-class-list
		  ,class-variables))
       
       ;; Define the primary class
       (defvar ,name (defclass ,name ,super-class-list
                            (,@instance-variables
                             (class :initform (find-class ',parallel-class-name)))))
       
       ;; Initialize the instance table for the class
       (setf (gethash ,name *class-instances*)
             (make-instance (find-class ',parallel-class-name)))
       ,name)))

(defun get-class-object (cls)
  (gethash cls *class-instances*))

(defun get-slot (obj slot)
  (if (typep obj 'standard-class)
      (let ((ins (get-class-object obj)))
	(slot-value ins slot))
      (slot-value obj slot)))

(defun set-slot (obj slot val)
  (if (typep obj 'standard-class)
      (let ((ins (get-class-object obj)))
	(setf (slot-value ins slot) val))
      (setf (slot-value obj slot) val)))

(defmacro define-method (method-name class-name arg-list &rest body)
  "define-method defines a fixed argument method and associates it to a variable argument generic.
   This allows the same method name in different classes to have a different number of fixed arguments."
  (let ((alist (gensym)))
    `(defmethod ,method-name ((self ,class-name) &rest ,alist)
       (apply #'(lambda (self ,@arg-list) ,@body)
	      (cons self ,alist)))))


