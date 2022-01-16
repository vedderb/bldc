(define itcnt 0)

(define f (lambda ()
   (progn
     (define itcnt (+ itcnt 1))
     (canset-pos 124 (get-encoder))
     (yield 2000)
     (f)
)))
     
(f)

