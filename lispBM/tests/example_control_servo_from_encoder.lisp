(define f (lambda ()
   (progn
     (define encval (get-encoder)) ; So that lisp_stats shows the value     
     (set-servo (/ (- 360.0 encval) 360.0))
     (yield 20000)
     (f)
)))
     
(f)

