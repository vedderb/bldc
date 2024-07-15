
(define a 10)
(define b 20)
(define c 30)

(check (and (eq (not (eq a b c)) (not-eq a b c))
            (eq (not (eq a a c)) (not-eq a a c))
            (eq (not (eq a a a)) (not-eq a a a))
            (eq (not (eq b b c)) (not-eq b b c))
            (eq (not (eq b b b)) (not-eq b b b))
            (eq (not (eq c b c)) (not-eq c b c))
            (eq (not (eq c c c)) (not-eq c c c))))
     
