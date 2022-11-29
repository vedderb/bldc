
(define compute (lambda ()
                  (progn
                    (recv (((? pid) (? x)  (? y)) (send pid (+ x y))))
                    (compute))))


(define cpid (spawn compute))

(send cpid (list (self) 1 2))

(recv ((? x) (= x 3)))



