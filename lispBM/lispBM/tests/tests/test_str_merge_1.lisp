
(define str-merge str-join)

(define r1 (eq (str-merge (list "A" "bC" "D")) "AbCD"))

(define r2 (eq (str-merge (list "Num1: " (str-from-n 10) " Num2: " (str-from-n 2.1 "%.1f"))) "Num1: 10 Num2: 2.1"))

(check (and r1 r2))
