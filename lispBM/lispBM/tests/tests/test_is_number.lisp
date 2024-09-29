
(define r1 (number? 10))
(define r2 (number? 1u32))
(define r3 (number? 1i32))
(define r4 (number? 1.0f32))
(define r5 (number? 1.0f64))
(define r6 (number? 2u64))
(define r7 (number? 4i64))
(define r8 (not (number? 'apa)))
(define r9 (not (number? nil)))
(define r10 (not (number? (list 1 2 3))))
(define r11 (not (number? [1 2 3])))

(check (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11))
