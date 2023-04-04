
(define fold (lambda (f i xs)
               (if (eq xs nil)
                   i
                   (fold f (f i (car xs)) (cdr xs)))))

(check (= (fold '+ 0 (list 1 2 3 4 5 6 7 8 9 10)) 55))

