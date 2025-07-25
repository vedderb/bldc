(define r1 (eq (type-of (sin (deg2rad 180))) type-float))
(define r2 (eq (type-of (cos (deg2rad 0))) type-float))
(define r3 (eq (type-of (tan (deg2rad 270))) type-float))
(define r4 (eq (type-of (asin 0.5)) type-float))
(define r5 (eq (type-of (acos 0.5)) type-float))
(define r6 (eq (type-of (atan 0.5)) type-float))
(define r7 (eq (type-of (atan2 0.4 1.0)) type-float))
(define r8 (eq (type-of (pow 2 4)) type-float))
(define r9 (eq (type-of (exp 0.75)) type-float))
(define r10 (eq (type-of (sqrt 4)) type-float))
(define r11 (eq (type-of (log 2)) type-float))
(define r12 (eq (type-of (log10 2)) type-float))
(define r13 (eq (type-of (floor 2.3)) type-float))
(define r14 (eq (type-of (ceil 2.3)) type-float))
(define r15 (eq (type-of (round 2.1)) type-float))
(define r16 (eq (type-of (log10 2)) type-float))
 
(define r17 (eq (type-of (deg2rad 2)) type-float))
(define r18 (eq (type-of (rad2deg 2)) type-float))

(define r19 (eq (is-nan 2) nil))
(define r20 (eq (is-inf 2) nil))

(define r21 (eq (is-nan 2.0f32) nil))
(define r22 (eq (is-inf 2.0f32) nil))

(define r23 (eq (is-nan 2.0f64) nil))
(define r24 (eq (is-inf 2.0f64) nil))

(check (and r1 r2 r3 r4 r5
            r6 r7 r8 r9 r10
            r11 r12
            r13 r14
            r15 r16
            r17 r18
            r19 r20
            r21 r22
            r23 r24))
