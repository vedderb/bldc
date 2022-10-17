
(define a 10)

(= (let ((b 1))
     (eval `(+ b ,a)))
   11)
