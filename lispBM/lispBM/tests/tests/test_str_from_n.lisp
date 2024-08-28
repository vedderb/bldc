

(define is-eerr (lambda (x)
                  (eq x '(exit-error eval_error))))

(define is-terr (lambda (x)
                  (eq x '(exit-error type_error))))

(define r1 (is-eerr (trap (str-from-n))))
(define r2 (is-eerr (trap (str-from-n 1 2 3))))

(define r3 (is-terr (trap (str-from-n (list 1 2 3) ))))
(define r4 (is-terr (trap (str-from-n 1 2))))

(define str (str-from-n (/ 1.0 10000000.0)))
(define str2 (str-from-n (/ 1.0f64 10000000.0f64)))

(define r5 (eq (type-of str) type-array))
(define r6 (eq (type-of str2) type-array))

(define r7 (eq (str-from-n 3.14f32) "3.14"))
(define r8 (eq (str-from-n 3.14f64) "3.14"))

(check (and  r1 r2 r3 r4 r5 r6 r7 r8))


                    



                    
