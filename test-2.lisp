
(define-class MyClass
  nil
  (cv1 cv2)
  (iv1 iv2))

(defparameter i1 (make-instance MyClass))

(defparameter i2 (make-instance MyClass))

(set-slot i1 iv1 44)

(set-slot i2 iv1 55)

(set-slot MyClass cv1 77)

(print (get-slot i1 iv1))
(print (get-slot i2 iv1))
(print (get-slot MyClass cv1))


