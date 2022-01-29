(define f (lambda (acc n) (if (num-eq n 0) acc (f (+ acc n) (- n 1)))))

( = (f 0 10000u32) 50005000u32)
