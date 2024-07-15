(define f (lambda (acc n) (if (= n 0) acc (f (+ acc n) (- n 1)))))

(check ( = (f 0 10000u32) 50005000u32))
