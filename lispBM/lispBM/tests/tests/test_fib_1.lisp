(check (let ((fib (lambda (n) (if (> 2 n) n (+ (fib (- n 1)) (fib (- n 2))))))) (= (fib 10) 55)))

