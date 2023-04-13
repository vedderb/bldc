(define fib
    (lambda (n)
      (let ((fib0 (lambda (n a b)
                    (if (= n 0) a
                        (if (= n 1) b
                            (fib0 (- n 1) b (+ a b)))))))
        (fib0 n 0 1))))


(check (= (fib 10) 55))
