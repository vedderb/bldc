(define r1 (eq (str-split "This is a test" " ") (list "This" "is" "a" "test")))

(define r2 (eq (str-split "This_o_is_o_a_o_test" "_o_") (list "This" "is" "a" "test")))

(define r3 (eq (str-split "This is a test" 3) (list "Thi" "s i" "s a" " te" "st")))

(define r4 (eq (str-split "This is a test" 1) (list "T" "h" "i" "s" " " "i" "s" " " "a" " " "t" "e" "s" "t")))

(check (and r1 r2 r3 r4))
