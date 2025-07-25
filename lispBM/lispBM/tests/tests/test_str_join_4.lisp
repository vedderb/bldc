(define r1 (eq (str-join '("a" "b" "c")) "abc"))

(define r2 (eq (str-join '("I" "love" "lispbm!") " ") "I love lispbm!"))

(define r3 (eq (str-join '() " ") ""))


(check (and r1 r2 r3))
