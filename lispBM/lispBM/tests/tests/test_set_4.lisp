
(define to-bool (lambda (x) (if (eq x nil) nil t)))

(define myset-1 nil)
(define myset-2 (list 1))
(define myset-3 (list 1 2 3))

(define r0 (to-bool (member (set-insert myset-1 1) 1)))
(define r1 (not (to-bool (member (set-insert myset-1 1) 2))))
(define r2 (to-bool (member (set-insert myset-2 2) 1)))
(define r3 (to-bool (member (set-insert myset-2 2) 2)))
(define r4 (to-bool (member (set-insert myset-2 1) 1)))
(define r5 (not (to-bool (member (set-insert myset-2 1) 2))))
(define r6 (to-bool (member (set-insert myset-3 100) 100)))

(check (and r0 r1 r2 r3 r4 r5 r6))
