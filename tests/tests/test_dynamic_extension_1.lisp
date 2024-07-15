
(load-inc-i)

(define a 1)

(define b (ext-inc-i a))

(check (= b 2))
