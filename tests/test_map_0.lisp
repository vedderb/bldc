;;(define map (lambda (f xs) (if (eq xs nil) nil (cons (f (car xs)) (map f (cdr xs))))))

(check (eq (map (lambda (x) (+ x 1)) (list 1 2 3 4 5)) (list 2 3 4 5 6)))

