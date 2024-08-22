
(define f (lambda (x)
            (if (= x 0)
                x
              (check ( = 10 
                         (call-cc (lambda (k) (progn (define cc k) (f (- x 1))))))
                     't))))

(f 1)
(cc 10)


