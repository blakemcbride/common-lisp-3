
; Lisp1 examples
;
; Try to execute the stuff in comments


(eval-when (:compile-toplevel :execute)
  (require "LISP1" "lisp1")
  (use-package "LISP1"))

;  Compiled code doesn't have to have lisp1.lisp loaded to run
;  but the package must exist because the code creates local
;  variables in the LISP1 package.
(eval-when (:load-toplevel)
  (if (not (find-package "LISP1"))
      (make-package "LISP1")))

(define add
    (lambda (a b)
      (+ a b)))

(define var1 add)

(define var2 5)

(define var3 6)

;  var1
;  var2
;  (var1 var2 var3)

(define fun1
    (lambda (a b)
      (var1 a b)))

;  (fun1 7 8)

(define fun2
  (lambda (fun)
    (fun 5 6)))

; (fun2 var1)
; (fun2 fun1)
; (fun2 (lambda (a b) (* a b)))

(define fun3
    (lambda (a)
      (let ((fun2 (lambda (a b) (* a b)))
	    (c 100))
	(fun2 a c))))

; (fun3 22)

(define-macro mac (a b)
  `(var1 ,a , b))

; (mac 3 5)

(define fun4
    (lambda (x)
      (lambda (y)
	(+ x y))))

; (lisp1 ((fun4 1) 2))
