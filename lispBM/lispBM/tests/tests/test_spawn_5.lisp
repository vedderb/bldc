

(define f (lambda () 1))

(define r (trap (spawn 1000000 f)))
(define merr '(exit-error out_of_memory))

(check (eq r merr))
