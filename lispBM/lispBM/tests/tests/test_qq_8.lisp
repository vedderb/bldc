
(define a 10)

(check (= (let ((b 1))
            (eval `(+ b ,a)))
          11))
