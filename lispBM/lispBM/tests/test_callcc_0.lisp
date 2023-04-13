

(define f (lambda (cc x)
            (if (= x 0)
                (cc 1000)
                x)))

(define g (lambda (x y)
            (+ x (call-cc (lambda (cc)
                            (f cc y))))))


(check (and (= (g 1 0) 1001)
            (= (g 1 1) 2)))

