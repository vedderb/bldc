
(define compute (lambda ()
                  (progn
                    (recv (((? pid) (? x)  (? y)) (send pid (+ x y))))
                    (compute))))


(define cpid (spawn compute))

(send cpid (list (self) 1 2))
(send cpid (list (self) 2 3))
(send cpid (list (self) 3 4))

(define a (recv ((? x) x)))
(define b (recv ((? x) x)))
(define c (recv ((? x) x)))

(and (= a 3)
     (= b 5)
     (= c 7))
