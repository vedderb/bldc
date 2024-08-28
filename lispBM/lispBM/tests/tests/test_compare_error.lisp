
(define err '(exit-error type_error))

(define r1 (eq err (trap (> 1 'apa))))
(define r2 (eq err (trap (> (list 1 2 3) 1))))
(define r3 (eq err (trap (< 1 'apa))))
(define r4 (eq err (trap (< (list 1 2 3) 1))))
(define r5 (eq err (trap (= 1 'apa))))
(define r6 (eq err (trap (= (list 1 2 3) 1))))
(define r7 (eq err (trap (>= 1 'apa))))
(define r8 (eq err (trap (>= (list 1 2 3) 1))))
(define r9 (eq err (trap (<= 1 'apa))))
(define r10 (eq err (trap (<= (list 1 2 3) 1))))
(define r11 (eq err (trap (!= 1 'apa))))
(define r12 (eq err (trap (!= (list 1 2 3) 1))))

(check (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12))
