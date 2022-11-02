
(define f (lambda (x)
            (if (= x 0)
                x
                ( = 10 
                (call-cc (lambda (k) (progn (define cc k) (f (- x 1)))))))))

(f 1)
(cc 10)


