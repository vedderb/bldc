
;; eval-program takes over the current context completely.

(define prg "(define a 10) (define r (+ a 10))")

(eval-program (read-program prg))

(= r 20)
