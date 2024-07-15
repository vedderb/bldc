
(let ((r '(1 2 3 4)))
  (def result (eq r (call-cc (fn (k) (define cc k))))))

(gc)

(cc '(1 2 3 4))

(check result)
