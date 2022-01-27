(define set-auxtime (lambda (port state time)
  (progn
    (set-aux port state)
    (define aux-now (list port state time))
    (yield (* 1000 time))
)))

(let ((f (lambda ()
  (progn
    (set-auxtime 1 1 1500)
    (set-auxtime 1 0 500)
    (set-auxtime 2 1 1500)
    (set-auxtime 2 0 500)
    (yield 1000000)
    (f)
)))) (f))

