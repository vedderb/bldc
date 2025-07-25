
(define glob 0)

(define inf-loop (lambda () {
                   (setq glob (+ glob 1))
                   (inf-loop)
                   }))

(define pid (spawn inf-loop))

(kill pid 10)

(define a glob)
(sleep 0.1)
(define b glob)

(check (= a b))
