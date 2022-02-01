
(define prg "(define a 10) (+ a 10)")

(= (eval-program (read-program prg)) 20)