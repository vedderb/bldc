(send (self) 1)

(recv ((? x) (define a1 x)))

(check (= a1 1))
