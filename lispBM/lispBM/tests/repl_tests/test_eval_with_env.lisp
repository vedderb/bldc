
(define r1 (eval (list '(a . 1) '(b . 2)) '(+ a b)))
(define r2 (eval-program (list '(define c 10) '(+ 1 2 c))))
(define r3 (trap (eval (list '(a . 1) '(b . 2)) '(+ a b) 1)))
(define r4 (trap (eval-program (list '(define c 10) '(+ 1 2 c)) 12)))

(if (and (eq r1 3)
         (eq r2 13)
         (eq r3 '(exit-error eval_error))
         (eq r4 '(exit-error eval_error)))
    (print "SUCCESS")
    (print "FAILURE"))

