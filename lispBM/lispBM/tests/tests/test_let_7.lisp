(define f (lambda (x)
            (let ((a 4))
              (let ((b 5))
                (let ((c 6))
                  (if (= x 0) (+ a b c) (f (- x 1))))))))

(check (= (f 1000000) 15))
 
