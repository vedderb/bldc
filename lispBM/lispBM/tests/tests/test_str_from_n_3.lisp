(define r1 (eq (str-from-n 10) "10"))

(define r2 (eq (str-from-n 2.5) "2.5"))

(define r3 (eq (str-from-n 2.5 "%.1f") "2.5"))

(define r4 (eq (str-from-n 10 "0x%04X") "0x000A"))

(define r5 (eq (str-from-n 0.023e3) "23"))


(check (and r1 r2 r3 r4 r5))
