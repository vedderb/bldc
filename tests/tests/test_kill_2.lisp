
(define inf-loop (lambda ()         
                   (inf-loop)
                   ))

(define pid (spawn-trap inf-loop))

(kill pid 10)

(define r 0)

(recv ((exit-ok  (? tid) (? val)) (setq r val))
      ((exit-error (? tid) (? val) (setq r 0))))

 
(check (= r 10))
