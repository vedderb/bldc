
(define r (eval-program (read-program "(+ 50 50)")))

(check (= r 100))

