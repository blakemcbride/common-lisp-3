

;  Lisp1
;  Makes Common Lisp operate like a Lisp1 Lisp
;  Version 9/5/23
;  
;  Written by:
;  	Blake McBride
;  	blake@mcbridemail.com
;  
;  
;  
; Special case primitives (primitives that don't act like regular function calls)
;
;  setq
;  let let* prog multiple-value-setq multiple-value-bind
;  lambda
;  defmacro
;  defun
;  cond
;  progn
;  quote
;  backquote  ???
;  do do*
;  dolist dotimes
;  defpackage
;  export import shadow
;  defmethod


(declaim (ftype function is-builtin convert convert-cond convert-progn is-let-like is-macro rep))

(defun repq ()
  (rep)
#+:sbcl (sb-ext:quit)
#+:clisp  (ext:quit)
)

(defmacro lisp1 (&rest argsz)
  (cons 'progn (convert-progn argsz)))

(defmacro define (name value)
  `(progn
     (defparameter ,name ,(convert value))
     (defmacro ,name (&rest args)
       `(funcall ,',name ,@args))))

(defmacro define-macro (name args &rest rest)
  (cons 'defmacro (cons name (cons args (convert-progn rest)))))

(defmacro define-method (name args &rest rest)
  `(progn
     ,(cons 'defmethod (cons name (cons args (convert-progn rest))))
     (defparameter ,name (symbol-function ',name))
     (defmacro ,name (&rest args)
       `(funcall ,',name ,@args))))
     

(defun rep ()
  (prog (e e1)
   loop
   (princ "lisp1> ")
   (finish-output)
   (setq e (read))
   (cond ((not (and (consp e) 
                    (atom (car e))
                    (or (equal (symbol-name (car e)) "QUIT")
                        (equal (symbol-name (car e)) "EXIT"))))
          (setq e1 (convert e))
;          (print e1)
;          (terpri)
	  (prin1 (common-lisp:eval e1))
	  (terpri)
	  (go loop)))))

(defun eval (x)
  (common-lisp:eval (convert x)))

(defun load (file)
  (prog ((stream (open file :direction :input)) sexp)
   loop
   (setq sexp (read stream nil 'eof))
   (cond ((not (eq sexp 'eof))
	  (common-lisp:eval (convert sexp))
	  (go loop)))
   (close stream)))

(defun compile-file (file)
  (prog ((stream (open (concatenate 'string file ".lisp") :direction :input)) 
         (ostream (open "tmp.lisp" :direction :output))
         sexp)
   loop
   (setq sexp (read stream nil 'eof))
   (cond ((not (eq sexp 'eof))
	  (print (convert sexp) ostream)
	  (go loop)))
   (close ostream)
   (close stream)
   (common-lisp:compile-file "tmp" :output-file file)
   (delete-file "tmp.lisp")))

(defun convert (x)
  (if (atom x) 
      x
      (let ((fun (car x)))
	(cond ((eq fun 'lambda)
	       (cons fun (cons (cadr x) (convert-progn (cddr x)))))
	      ((eq fun 'defun)
	       (cons 'defparameter (list (cadr x) (cons 'lambda (cons (caddr x) (convert-progn (cdddr x)))))))
	      ((eq fun 'defmacro)
	       (cons fun (cons (cadr x) (cons (caddr x) (convert-progn (cdddr x))))))
	      ((is-let-like fun)
	       (cons fun (cons (convert-cond (cadr x)) (convert-progn (cddr x)))))
	      ((eq fun 'cond)
	       (cons fun (convert-cond (cdr x))))
	      ((eq fun 'progn)
	       (cons fun (convert-progn (cdr x))))
	      ((eq fun 'setq)
	       (cons 'setq (convert-progn (cdr x))))
	      ((or (eq fun 'quote)
                   (eq fun 'defpackage)
                   (eq fun 'export)
                   (eq fun 'import)
                   (eq fun 'shadow)
                   (eq fun 'defclass))
	       x)
	      ((eq fun (car '`(,f))) ; portable way of getting the backquote symbol  
	       x)
              ((or (eq fun 'do)
		   (eq fun 'do*))
               (cons fun (cons (convert-cond (cadr x)) (cons (convert-progn (caddr x)) (convert-progn (cdddr x))))))
              ((or (eq fun 'dolist)
		   (eq fun 'dotimes))
               (cons fun (cons (convert-progn (cadr x)) (convert-progn (cddr x)))))
              ((eq fun 'defmethod)
	       (list 'progn 
		     (cons fun (cons (cadr x) (cons (caddr x) (convert-progn (cdddr x)))))
		     `(defparameter ,(cadr x) (symbol-function (quote ,(cadr x))))))
	      ((or (is-builtin fun)
		   (is-macro fun))
	       (cons fun (convert-progn (cdr x))))
	      (t
	       (cons 'funcall (cons (convert fun) (convert-progn (cdr x)))))))))

(defun is-builtin (x)
  (and (atom x)
       (multiple-value-bind (symbol status)
	   (find-symbol (string x) 'common-lisp)
         (declare (ignore symbol))
	 (eq status :external))))

(defun is-macro (x)
  (and (atom x)
       (macro-function x)))

(defun is-let-like (x)
  (and (atom x)
       (find x
	     '(let
	       let*
	       prog
	       multiple-value-setq
	       multiple-value-bind
	       ))))

(defun convert-progn (x)
  (if (consp x)
      (cons (convert (car x)) (convert-progn (cdr x)))
      x))

(defun convert-cond (x)
  (if (consp x)
      (cons (convert-progn (car x)) (convert-cond (cdr x)))
      x))
