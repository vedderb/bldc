
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

(define elem (lambda (x xs)
               (if (eq xs 'nil) nil
                 (or (eq x (car xs)) (elem x (cdr xs))))))

(check (and (elem a '(3 5 7))
            (elem b '(3 5 7))
            (elem c '(3 5 7))))
           
               

