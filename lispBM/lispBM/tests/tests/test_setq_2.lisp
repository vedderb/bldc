

(define a 10)

(let ((a 100))
  (setq a 43000))

(check (= a 10))
