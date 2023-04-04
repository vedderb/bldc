
(define a 10)

(check (= (eval (let ((b 1))
                  `(+ ,b ,a)))
          11))
