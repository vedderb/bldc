
(define f (lambda ()
            (progn 
              (recv (_ nil))
              (f))))


(spawn f)


(yield 10000)
(check (= 1 1))
