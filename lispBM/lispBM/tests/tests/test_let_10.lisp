(define ok
    (lambda ()
      (let ((b (+ 12 15)))
        (let ((c 25))
          (+ b c)))))

(define f (lambda (x)
            (progn
              (ok)
              (yield 10)
              (if (= x 0) (ok) (f (- x 1))))))

(check (= (f 1000) 52))
              
