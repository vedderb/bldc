
(define to-bool (lambda (x) (if (eq x nil) nil t)))

(define myset-1 nil)
(define myset-2 (list 1 2 3 4 5))

(define u (set-union myset-1 myset-2))

(define r1 (= (length u) 5))
(define r2 (to-bool (member 1 u )))
(define r3 (to-bool (member 2 u )))
(define r4 (to-bool (member 3 u )))
(define r5 (to-bool (member 4 u )))
(define r6 (to-bool (member 5 u )))


(check (and r1 r2 r3 r4 r5 r6))
