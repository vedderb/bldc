(define r1 (eq (str-part "Hello World!" 6) "World!"))

(define r2 (eq (str-part "Hello World!" 6 2) "Wo"))

(define r3 (eq (str-part "Hello World!" 0 2) "He"))

(check (and r1 r2 r3))
