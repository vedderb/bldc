
(define f (lambda ()
            (progn 
              (recv (_ nil))
              (f))))


(spawn f)


(yield 10000)
(= 1 1)
