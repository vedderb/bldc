(define sum (lambda (s ls) (if (eq ls nil) s (sum (+ s (car ls)) (cdr ls)))))

(check ( = (sum 0 (list 2.0 2.0 1.0 1.0 3.0 3.0)) 12.0))
