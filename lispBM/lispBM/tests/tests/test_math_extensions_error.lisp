
(define err (lambda (x)
              (eq '(exit-error eval_error) x)))

(define r1 (err (trap (sin))))
(define r2 (err (trap (cos))))
(define r3 (err (trap (tan))))
(define r4 (err (trap (asin))))
(define r5 (err (trap (acos))))
(define r6 (err (trap (atan))))
(define r7 (err (trap (atan2))))
(define r8 (err (trap (pow))))
(define r9 (err (trap (exp))))
(define r10 (err (trap (sqrt))))
(define r11 (err (trap (log))))
(define r12 (err (trap (log10))))
(define r13 (err (trap (floor))))
(define r14 (err (trap (ceil))))
(define r15 (err (trap (round))))

(define r16 (eq (deg2rad) nil))
(define r17 (eq (rad2deg) nil))

(define r18 (err (trap (is-nan))))
(define r19 (err (trap (is-inf))))

(check (and r1 r2 r3 r4 r5
            r6 r7 r8 r9 r10
            r11 r12
            r13 r14
            r15 r16
            r17 r18
            r19))
