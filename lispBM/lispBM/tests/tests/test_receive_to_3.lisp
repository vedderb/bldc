
(define g 0)
(send (self) 100)

(recv-to (+ 0.1 0.1)  ((? x) (setq g x)))

(check (= g 100))
