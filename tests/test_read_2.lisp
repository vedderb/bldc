
(define prg "(define a 10) (define r (+ a 10))")

(eval-program (read-program prg))

(check (= r 20))
