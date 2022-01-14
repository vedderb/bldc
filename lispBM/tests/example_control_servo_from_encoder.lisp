(define f (lambda ()
   (progn
     (define encval (enc-get)) ; So that lisp_stats shows the value
     (servo-set (/ encval 360.0))
     (yield 20000)
     (f)
)))
     
(f)

