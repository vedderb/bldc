
(define i0 (let ( (a 10)
                  (b 20)
                  (c (lambda (x y) (+ x y))))
             (c a b)))

(define i1
  (progn 
    (var a 10)
    (var b 20)
    (var c (lambda (x y) (+ x y)))
    (c a b)
  ))

(check (= i0 i1))
