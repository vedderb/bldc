
(define alist '((1 . apa) (2 . bepa)))

(define r1 (eq '(exit-error eval_error) (trap (cossa 1))))
(define r2 (eq nil (cossa alist 3)))
(define r3 (eq nil (cossa nil 2)))
(define r4 (eq '(exit-error eval_error) (trap (cossa 'apa 1))))
(define r5 (eq '(exit-error eval_error) (trap (cossa (list 1 2 3) 1))))

(check (and r1 r2 r3 r4 r5))
