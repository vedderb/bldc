
(define f (lambda (x) (+ x 1)))

{
(var a 10)
(setq a (f 1))
(check (= a 2))
}
