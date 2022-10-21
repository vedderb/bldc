
(define a 10)

(= (eval (let ((b 1))
           `(+ ,b ,a)))
   11)
