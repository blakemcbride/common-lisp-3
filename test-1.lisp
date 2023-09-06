(define abc 66)

(define ABC (lambda (a b) (+ a b)))

(define xx ABC)

(print abc)
(print xx)
(print (xx 5 6))
