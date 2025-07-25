
(define err '(exit-error 1))

(define r1 (eq err (trap (exit-error 1))))

;; Oddly enough exit-ok is trickier to test.

(check (and r1))
