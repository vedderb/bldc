
(define f (lambda (x)
            (if (= x 0) 0
              (f (- x 1)))))


(define apa 0)

(define g (lambda (x) {
            (f x)
            (setq apa 10)
            }
            ))

(define p1 (spawn "apa" 100 f 10000))
(define p2 (spawn "bepa" 50 f 10000))
(define p3 (spawn "cepa" 40 f 10000))
(define p4 (spawn "depa" 30 f 10000))
(define p5 (spawn "eepa" 20 g 10000))

(wait p5)

(check (= apa 10))
