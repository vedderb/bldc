

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


(recv ((? x) (define a1 x)))
(recv ((? x) (define a2 x)))
(recv ((? x) (define a3 x)))
(recv ((? x) (define a4 x)))
(recv ((? x) (define a5 x)))
(recv ((? x) (define a6 x)))
(recv ((? x) (define a7 x)))
(recv ((? x) (define a8 x)))
(recv ((? x) (define a9 x)))
(recv ((? x) (define a10 x)))

(check (eq (list a1 a2 a3 a4 a5 a6 a7 a8 a9 a10) (list 1 2 3 4 5 6 7 8 9 10)))
