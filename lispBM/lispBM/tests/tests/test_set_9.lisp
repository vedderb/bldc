
(define to-bool (lambda (x) (if (eq x nil) nil t)))

(define myset-1 (list 1 2 3 4 5))
(define myset-2 nil)

(define u (set-union myset-1 myset-2))

(define r1 (= (length u) 5))
(define r2 (to-bool (member u 1)))
(define r3 (to-bool (member u 2)))
(define r4 (to-bool (member u 3)))
(define r5 (to-bool (member u 4)))
(define r6 (to-bool (member u 5)))


(check (and r1 r2 r3 r4 r5 r6))
