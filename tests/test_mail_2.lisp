

(send (self) 1)
(send (self) 2)
(send (self) 3)
(send (self) 4)
(send (self) 5)
(send (self) 6)
(send (self) 7)
(send (self) 8)
(send (self) 9)
(send (self) 10)
(send (self) 11)
(send (self) 12)
(send (self) 13)

(recv ((? x) (define a1 x)))
(recv ((? x) (define a2 x)))
(recv ((? x) (define a3 x)))

(check (and (= a1 4)
            (= a2 5)
            (= a3 6)))
