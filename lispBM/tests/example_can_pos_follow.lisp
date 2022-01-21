(define itcnt 0)

(define f (lambda ()
   (progn
     (define itcnt (+ itcnt 1))
     (canset-pos 124 (get-encoder))
     (define canc (canget-current-dir 124))
     (set-servo (- 0.5 (* 0.02 canc)))
     (timeout-reset)
     (yield 2000)
     (f)
)))
     
(f)

